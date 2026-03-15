open Symbols
open Collect
open Typed

let rec resolve_typ t (ty : Ast.typ) : r_typ =
  match ty with
  | Ast.TInt i -> TInt i
  | Ast.TFloat f -> TFloat f
  | Ast.TString -> TString
  | Ast.TBool -> TBool
  | Ast.TVoid -> TVoid
  | Ast.TPtr inner -> TPtr (resolve_typ t inner)
  | Ast.TArray inner -> TArray (resolve_typ t inner)
  | Ast.TTuple ts -> TTuple (List.map (resolve_typ t) ts)
  | Ast.TFunc (args, ret) ->
      TFunc (List.map (resolve_typ t) args, resolve_typ t ret)
  | Ast.TGeneric s ->
      let sid = resolve_or_error t s in
      TGeneric sid
  | Ast.TNamed path -> begin
      let name = String.concat "::" path in
      match resolve t name with
      | Some sid -> TNamed sid
      | None ->
          let last = List.nth path (List.length path - 1) in
          let sid = resolve_or_error t last in
          TNamed sid
    end
  | Ast.TApp (base, args) ->
      TApp (resolve_typ t base, List.map (resolve_typ t) args)

let rec resolve_pattern t (pat : Ast.pattern) : t_pattern =
  match pat with
  | Ast.PWildcard -> PWildcard
  | Ast.PVar name ->
      let sid = fresh t name SLocal in
      PVar sid
  | Ast.PTuple pats -> PTuple (List.map (resolve_pattern t) pats)
  | Ast.PVariant (path, pats) ->
      let name = String.concat "::" path in
      let sid = resolve_or_error t name in
      PVariant (sid, List.map (resolve_pattern t) pats)
  | Ast.PLiteral lit -> PLiteral (resolve_literal lit)

and resolve_literal (lit : Ast.literal) : t_literal =
  match lit with
  | Ast.LInt s -> IntLiteral (Common.I32, Int64.of_string s)
  | Ast.LFloat s -> FloatLiteral (Common.F32, float_of_string s)
  | Ast.LString s -> StringLiteral s
  | Ast.LChar c -> CharLiteral c
  | Ast.LBool b -> BoolLiteral b

let rec resolve_expr t (e : Ast.expr) : t_expr =
  match e with
  | Ast.Literal lit -> TLiteral (resolve_literal lit)
  | Ast.Unary (op, inner) -> TUnary (op, resolve_expr t inner)
  | Ast.Binary (op, l, r) -> TBinary (op, resolve_expr t l, resolve_expr t r)
  | Ast.Tuple es -> TTuple (List.map (resolve_expr t) es)
  | Ast.Array es -> TArray (List.map (resolve_expr t) es)
  | Ast.Path path ->
      let name = String.concat "::" path in
      let sid = resolve_or_error t name in
      if is_function t sid then TFunc sid else TLocal sid
  | Ast.Call (callee, args) ->
      TCall (resolve_expr t callee, List.map (resolve_expr t) args)
  | Ast.Index (arr, idx) -> TIndex (resolve_expr t arr, resolve_expr t idx)
  | Ast.Field (obj, field) ->
      (* field names are resolved later during type checking *)
      let field_sid = fresh t field SLocal in
      TField (resolve_expr t obj, field_sid)
  | Ast.If (cond, then_b, else_b) ->
      TIf (resolve_expr t cond, resolve_block t then_b, resolve_block t else_b)
  | Ast.Closure (params, body) ->
      push_scope t;
      let tparams = List.map (resolve_param t) params in
      let tbody = resolve_block t body in
      pop_scope t;
      TClosure (tparams, tbody)
  | Ast.Iterate (src, fn) -> TIterate (resolve_expr t src, resolve_expr t fn)
  | Ast.StructInit (path, fields) ->
      let name = String.concat "::" path in
      let sid = resolve_or_error t name in
      TStructInit (sid, List.map (resolve_init_field t) fields)
  | Ast.Match (scrutinee, arms) ->
      TMatch (resolve_expr t scrutinee, List.map (resolve_arm t) arms)

and resolve_param t (p : Ast.param) : t_param =
  let sid = fresh t p.name SParam in
  { sid; r_typ = p.typ; mode = p.mode }

and resolve_init_field t (f : Ast.init_field) : t_struct_init_field =
  let sid = fresh t f.init_name SLocal in
  { sf_init_sid = sid; sf_init_value = resolve_expr t f.init_value }

and resolve_arm t (arm : Ast.match_arm) : t_match_arm =
  push_scope t;
  let pat = resolve_pattern t arm.arm_pat in
  let body = resolve_block t arm.arm_body in
  pop_scope t;
  { arm_pat = pat; arm_body = body }

and resolve_stmt t (s : Ast.stmt) : t_stmt =
  match s with
  | Ast.Let (pat, typ, init) ->
      let tinit = Option.map (resolve_expr t) init in
      let _tpat = resolve_pattern t pat in
      TLet (pat, typ, tinit)
  | Ast.Expr e -> TExpr (resolve_expr t e)
  | Ast.Return e -> TReturn (Option.map (resolve_expr t) e)
  | Ast.Loop body -> TLoop (resolve_block t body)
  | Ast.Block body -> TBlock (resolve_block t body)
  | Ast.Break -> TBreak
  | Ast.Continue -> TContinue

and resolve_block t (b : Ast.block) : t_block =
  let stmts = List.map (resolve_stmt t) b.stmts in
  let tail = Option.map (resolve_expr t) b.tail in
  { stmts; tail }

let resolve_func_decl t (f : Ast.func_decl) : t_func_decl =
  let fname = resolve_or_error t f.fname in
  push_scope t;
  (* register generics *)
  let fgenerics = List.map (fun g -> fresh t g SGeneric) f.fgenerics in
  (* register params *)
  let fparams = List.map (resolve_param t) f.fparams in
  let fret = Some (resolve_typ t f.fret) in
  let fbody = resolve_block t f.fbody in
  pop_scope t;
  { fname; fgenerics; fparams; fret; fbody }

let resolve_struct_decl t (s : Ast.struct_decl) : t_struct_decl =
  let sname = resolve_or_error t s.sname in
  push_scope t;
  let sgenerics = List.map (fun g -> fresh t g SGeneric) s.sgenerics in
  let sfields =
    List.map
      (fun (f : Ast.struct_field) ->
        let sid = fresh t f.sfield_name SLocal in
        {
          sf_init_sid = sid;
          sf_init_value = TLiteral (BoolLiteral false) (* placeholder *);
        })
      s.sfields
  in
  let sfuncs =
    List.map
      (fun (f : Ast.func_decl) ->
        ignore (fresh t f.fname SFunc);
        resolve_func_decl t f)
      s.sfuncs
  in
  pop_scope t;
  { sname; sgenerics; sfields; sfuncs }

let resolve_enum_decl t (e : Ast.enum_decl) : t_enum_decl =
  let ename = resolve_or_error t e.ename in
  push_scope t;
  let egenerics = List.map (fun g -> fresh t g SGeneric) e.egenerics in
  let evariants =
    List.map
      (fun (v : Ast.enum_variant) ->
        let fields =
          List.map
            (fun (f : Ast.enum_field) ->
              let field_name =
                Option.map (fun n -> fresh t n SLocal) f.efield_name
              in
              { field_name; field_type = resolve_typ t f.efield_type })
            v.efields
        in
        { variant_name = v.evariant_name; fields })
      e.evariants
  in
  pop_scope t;
  { ename; egenerics; evariants }

(* ── register top-level names (first pass) ───────────────────────── *)

let qualify prefix name =
  match prefix with "" -> name | _ -> prefix ^ "::" ^ name

let rec register_sig_decl t prefix (sd : sig_decl) =
  match sd with
  | SigFunc f -> ignore (fresh t (qualify prefix f.sfname) SFunc)
  | SigStruct s ->
      let qname = qualify prefix s.ssname in
      ignore (fresh t qname SStruct);
      List.iter
        (fun (mf : sig_func) ->
          ignore (fresh t (qname ^ "::" ^ mf.sfname) SFunc))
        s.ssfuncs
  | SigEnum e ->
      let qname = qualify prefix e.sename in
      ignore (fresh t qname SEnum);
      List.iter
        (fun (v : Ast.enum_variant) ->
          ignore (fresh t (qualify prefix v.evariant_name) SFunc))
        e.sevariants
  | SigNamespace ns ->
      let ns_prefix = qualify prefix (String.concat "::" ns.snpath) in
      List.iter (register_sig_decl t ns_prefix) ns.sndecls

let rec register_decl t prefix (d : Ast.decl) =
  match d with
  | Ast.FuncDec f -> ignore (fresh t (qualify prefix f.fname) SFunc)
  | Ast.StructDec s ->
      let qname = qualify prefix s.sname in
      ignore (fresh t qname SStruct);
      List.iter
        (fun (mf : Ast.func_decl) ->
          ignore (fresh t (qname ^ "::" ^ mf.fname) SFunc))
        s.sfuncs
  | Ast.EnumDec e ->
      let qname = qualify prefix e.ename in
      ignore (fresh t qname SEnum);
      List.iter
        (fun (v : Ast.enum_variant) ->
          ignore (fresh t (qualify prefix v.evariant_name) SFunc))
        e.evariants
  | Ast.NamespaceDec ns ->
      let ns_prefix = qualify prefix (String.concat "::" ns.npath) in
      List.iter (register_decl t ns_prefix) ns.ndecls

(* ── full resolve pass ───────────────────────────────────────────── *)

let rec resolve_decl t (d : Ast.decl) : t_decl =
  match d with
  | Ast.FuncDec f -> TFuncDec (resolve_func_decl t f)
  | Ast.StructDec s -> TStructDec (resolve_struct_decl t s)
  | Ast.EnumDec e -> TEnumDec (resolve_enum_decl t e)
  | Ast.NamespaceDec ns ->
      TNamespaceDec
        { npath = ns.npath; ndecls = List.map (resolve_decl t) ns.ndecls }

let resolve (imports : mw_module list) (decls : Ast.decl list) :
    symbol_table * t_decl list =
  let t = create () in
  (* pass 1: register all imported names *)
  List.iter (fun m -> List.iter (register_sig_decl t "") m) imports;
  (* pass 2: register all local top-level names *)
  List.iter (register_decl t "") decls;
  (* pass 3: resolve bodies *)
  let resolved = List.map (resolve_decl t) decls in
  (t, resolved)
