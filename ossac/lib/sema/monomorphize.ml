open Common
open Symbols
open Typed

(* ── mangled name generation ─────────────────────────────────────── *)

let rec mangle_typ st = function
  | TInt I8 -> "i8"
  | TInt I16 -> "i16"
  | TInt I32 -> "i32"
  | TInt I64 -> "i64"
  | TInt U8 -> "u8"
  | TInt U16 -> "u16"
  | TInt U32 -> "u32"
  | TInt U64 -> "u64"
  | TFloat F32 -> "f32"
  | TFloat F64 -> "f64"
  | TString -> "string"
  | TBool -> "bool"
  | TVoid -> "void"
  | TChar -> "char"
  | TNamed s -> name st s
  | TPtr t -> "ptr." ^ mangle_typ st t
  | TArray (t, size) -> "arr." ^ mangle_typ st t ^ "." ^ string_of_int size
  | TTuple ts -> "tup." ^ String.concat "." (List.map (mangle_typ st) ts)
  | TFunc (args, ret) ->
      "fn."
      ^ String.concat "." (List.map (mangle_typ st) args)
      ^ ".to." ^ mangle_typ st ret
  | TGeneric s -> name st s
  | TApp (base, args) ->
      mangle_typ st base ^ "."
      ^ String.concat "." (List.map (mangle_typ st) args)

let mangle_name st base_name type_args =
  if type_args = [] then base_name
  else base_name ^ "." ^ String.concat "." (List.map (mangle_typ st) type_args)

(* ── substitution map: generic symbol -> concrete type ───────────── *)

type subst = (symbol, r_typ) Hashtbl.t

let make_subst generics type_args : subst =
  let tbl = Hashtbl.create (List.length generics) in
  List.iter2 (fun g t -> Hashtbl.replace tbl g t) generics type_args;
  tbl

let rec apply_subst sub = function
  | TGeneric s -> begin
      match Hashtbl.find_opt sub s with Some t -> t | None -> TGeneric s
    end
  | TPtr t -> TPtr (apply_subst sub t)
  | TArray (t, size) -> TArray (apply_subst sub t, size)
  | TTuple ts -> TTuple (List.map (apply_subst sub) ts)
  | TFunc (args, ret) ->
      TFunc (List.map (apply_subst sub) args, apply_subst sub ret)
  | TApp (base, args) ->
      TApp (apply_subst sub base, List.map (apply_subst sub) args)
  | t -> t

module SpecKey = struct
  type t = symbol * r_typ list

  let equal (s1, ts1) (s2, ts2) =
    s1 = s2
    && List.length ts1 = List.length ts2
    && List.for_all2 Analyzer.types_equal ts1 ts2

  let hash (s, ts) =
    let h = ref (Hashtbl.hash s) in
    List.iter (fun t -> h := (!h * 31) + Hashtbl.hash t) ts;
    !h
end

module SpecTbl = Hashtbl.Make (SpecKey)

type mono_ctx = {
  st : symbol_table;
  specs : symbol SpecTbl.t; (* already specialized: key -> new symbol *)
  mutable queue : specialization list; (* pending work *)
  mutable output : t_decl list; (* emitted monomorphic decls *)
  funcs : (symbol, t_func_decl) Hashtbl.t;
  structs : (symbol, t_struct_decl) Hashtbl.t;
  enums : (symbol, t_enum_decl) Hashtbl.t;
}

and specialization = {
  orig_sym : symbol;
  type_args : r_typ list;
  new_sym : symbol;
}

let create_ctx st =
  {
    st;
    specs = SpecTbl.create 32;
    queue = [];
    output = [];
    funcs = Hashtbl.create 32;
    structs = Hashtbl.create 32;
    enums = Hashtbl.create 32;
  }

(* ── collect generic declarations into lookup tables ──────────────── *)

let rec collect_decls ctx = function
  | [] -> ()
  | d :: rest ->
      begin match d with
      | TFuncDec f -> Hashtbl.replace ctx.funcs f.fname f
      | TStructDec s -> Hashtbl.replace ctx.structs s.sname s
      | TEnumDec e -> Hashtbl.replace ctx.enums e.ename e
      | TNamespaceDec ns -> collect_decls ctx ns.ndecls
      end;
      collect_decls ctx rest

(* ── request a specialization ────────────────────────────────────── *)

let request_spec ctx orig_sym type_args =
  let key = (orig_sym, type_args) in
  match SpecTbl.find_opt ctx.specs key with
  | Some new_sym -> new_sym
  | None ->
      let orig_name = name ctx.st orig_sym in
      let mangled = mangle_name ctx.st orig_name type_args in
      let new_sym = fresh ctx.st mangled (kind ctx.st orig_sym) in
      SpecTbl.replace ctx.specs key new_sym;
      ctx.queue <- { orig_sym; type_args; new_sym } :: ctx.queue;
      new_sym

(* ── substitute types in expressions, stmts, blocks ──────────────── *)

let rec subst_expr ctx sub = function
  | TLiteral _ as e -> e
  | TUnary (op, e) -> TUnary (op, subst_expr ctx sub e)
  | TBinary (op, l, r) ->
      TBinary (op, subst_expr ctx sub l, subst_expr ctx sub r)
  | TTuple es -> TTuple (List.map (subst_expr ctx sub) es)
  | TArray es -> TArray (List.map (subst_expr ctx sub) es)
  | TLocal s -> TLocal s
  | TFunc s -> TFunc s
  | TCall (callee, args) ->
      TCall (subst_expr ctx sub callee, List.map (subst_expr ctx sub) args)
  | TIndex (arr, idx) -> TIndex (subst_expr ctx sub arr, subst_expr ctx sub idx)
  | TIf (cond, tb, eb) ->
      TIf
        (subst_expr ctx sub cond, subst_block ctx sub tb, subst_block ctx sub eb)
  | TIterate (src, fn) ->
      TIterate (subst_expr ctx sub src, subst_expr ctx sub fn)
  | TClosure (params, body) ->
      let params' =
        List.map
          (fun (p : t_param) ->
            {
              p with
              r_typ = Option.map (fun _t -> p.r_typ) p.r_typ |> Option.join;
            })
          params
      in
      TClosure (params', subst_block ctx sub body)
  | TStructInit (sid, fields) ->
      TStructInit
        ( sid,
          List.map
            (fun f ->
              { f with sf_init_value = subst_expr ctx sub f.sf_init_value })
            fields )
  | TEnumInit (sid, fields) ->
      TEnumInit
        ( sid,
          List.map
            (fun f ->
              { f with ef_init_value = subst_expr ctx sub f.ef_init_value })
            fields )
  | TField (obj, fld) -> TField (subst_expr ctx sub obj, fld)
  | TMatch (scrut, arms) ->
      TMatch
        ( subst_expr ctx sub scrut,
          List.map
            (fun arm ->
              { arm with arm_body = subst_block ctx sub arm.arm_body })
            arms )

and subst_stmt ctx sub = function
  | TLet (pat, typ, init) ->
      TLet (pat, typ, Option.map (subst_expr ctx sub) init)
  | TExpr e -> TExpr (subst_expr ctx sub e)
  | TReturn e -> TReturn (Option.map (subst_expr ctx sub) e)
  | TLoop body -> TLoop (subst_block ctx sub body)
  | TBlock body -> TBlock (subst_block ctx sub body)
  | TBreak -> TBreak
  | TContinue -> TContinue

and subst_block ctx sub (b : t_block) =
  {
    stmts = List.map (subst_stmt ctx sub) b.stmts;
    tail = Option.map (subst_expr ctx sub) b.tail;
  }

(* ── specialize a function ───────────────────────────────────────── *)

let specialize_func ctx spec =
  match Hashtbl.find_opt ctx.funcs spec.orig_sym with
  | None -> ()
  | Some f ->
      let sub = make_subst f.fgenerics spec.type_args in
      let fparams =
        List.map
          (fun (p : t_param) ->
            {
              p with
              r_typ = Option.map (fun _t -> p.r_typ) p.r_typ |> Option.join;
            })
          f.fparams
      in
      let fret = apply_subst sub f.fret in
      let fbody = subst_block ctx sub f.fbody in
      ctx.output <-
        TFuncDec { fname = spec.new_sym; fgenerics = []; fparams; fret; fbody }
        :: ctx.output

(* ── specialize a struct ─────────────────────────────────────────── *)

let specialize_struct ctx spec =
  match Hashtbl.find_opt ctx.structs spec.orig_sym with
  | None -> ()
  | Some s ->
      let sub = make_subst s.sgenerics spec.type_args in
      let sfields =
        List.map
          (fun (f : t_struct_init_field) ->
            { f with sf_init_value = subst_expr ctx sub f.sf_init_value })
          s.sfields
      in
      let sfuncs =
        List.map
          (fun (f : t_func_decl) ->
            let inner_sub = make_subst f.fgenerics spec.type_args in
            let fret = apply_subst inner_sub f.fret in
            let fbody = subst_block ctx inner_sub f.fbody in
            { f with fgenerics = []; fret; fbody })
          s.sfuncs
      in
      ctx.output <-
        TStructDec { sname = spec.new_sym; sgenerics = []; sfields; sfuncs }
        :: ctx.output

(* ── specialize an enum ──────────────────────────────────────────── *)

let specialize_enum ctx spec =
  match Hashtbl.find_opt ctx.enums spec.orig_sym with
  | None -> ()
  | Some e ->
      let sub = make_subst e.egenerics spec.type_args in
      let evariants =
        List.map
          (fun (v : t_enum_variant) ->
            let fields =
              List.map
                (fun (f : t_enum_field) ->
                  { f with field_type = apply_subst sub f.field_type })
                v.fields
            in
            { v with fields })
          e.evariants
      in
      ctx.output <-
        TEnumDec { ename = spec.new_sym; egenerics = []; evariants }
        :: ctx.output

(* ── process the work queue ──────────────────────────────────────── *)

let drain_queue ctx =
  while ctx.queue <> [] do
    let work = ctx.queue in
    ctx.queue <- [];
    List.iter
      (fun spec ->
        let k = kind ctx.st spec.orig_sym in
        match k with
        | SFunc -> specialize_func ctx spec
        | SStruct -> specialize_struct ctx spec
        | SEnum -> specialize_enum ctx spec
        | _ -> ())
      work
  done

(* ── scan for TApp usage and seed specializations ────────────────── *)

let rec scan_type ctx = function
  | TApp (TNamed sid, args) | TApp (TGeneric sid, args) ->
      List.iter (scan_type ctx) args;
      ignore (request_spec ctx sid args)
  | TPtr t | TArray (t, _) -> scan_type ctx t
  | TTuple ts -> List.iter (scan_type ctx) ts
  | TFunc (args, ret) ->
      List.iter (scan_type ctx) args;
      scan_type ctx ret
  | TApp (_, args) -> List.iter (scan_type ctx) args
  | _ -> ()

let rec scan_expr ctx = function
  | TLiteral _ | TLocal _ | TFunc _ -> ()
  | TUnary (_, e) -> scan_expr ctx e
  | TBinary (_, l, r) ->
      scan_expr ctx l;
      scan_expr ctx r
  | TTuple es | TArray es -> List.iter (scan_expr ctx) es
  | TCall (callee, args) ->
      scan_expr ctx callee;
      List.iter (scan_expr ctx) args
  | TIndex (a, b) ->
      scan_expr ctx a;
      scan_expr ctx b
  | TIf (c, t, e) ->
      scan_expr ctx c;
      scan_block ctx t;
      scan_block ctx e
  | TIterate (s, f) ->
      scan_expr ctx s;
      scan_expr ctx f
  | TClosure (_, body) -> scan_block ctx body
  | TStructInit (_, fields) ->
      List.iter (fun f -> scan_expr ctx f.sf_init_value) fields
  | TEnumInit (_, fields) ->
      List.iter (fun f -> scan_expr ctx f.ef_init_value) fields
  | TField (obj, _) -> scan_expr ctx obj
  | TMatch (scrut, arms) ->
      scan_expr ctx scrut;
      List.iter (fun arm -> scan_block ctx arm.arm_body) arms

and scan_stmt ctx = function
  | TLet (_, typ, init) ->
      Option.iter (fun t -> scan_type ctx (Resolver.resolve_typ ctx.st t)) typ;
      Option.iter (scan_expr ctx) init
  | TExpr e -> scan_expr ctx e
  | TReturn e -> Option.iter (scan_expr ctx) e
  | TLoop b -> scan_block ctx b
  | TBlock b -> scan_block ctx b
  | TBreak | TContinue -> ()

and scan_block ctx (b : t_block) =
  List.iter (scan_stmt ctx) b.stmts;
  Option.iter (scan_expr ctx) b.tail

let scan_func ctx (f : t_func_decl) =
  List.iter
    (fun (p : t_param) ->
      Option.iter
        (fun t -> scan_type ctx (Resolver.resolve_typ ctx.st t))
        p.r_typ)
    f.fparams;
  scan_type ctx f.fret;
  scan_block ctx f.fbody

let rec scan_decl ctx = function
  | TFuncDec f -> scan_func ctx f
  | TStructDec s -> List.iter (scan_func ctx) s.sfuncs
  | TEnumDec e ->
      List.iter
        (fun v -> List.iter (fun f -> scan_type ctx f.field_type) v.fields)
        e.evariants
  | TNamespaceDec ns -> List.iter (scan_decl ctx) ns.ndecls

(* ── main entry point ────────────────────────────────────────────── *)

let is_generic_decl = function
  | TFuncDec f -> f.fgenerics <> []
  | TStructDec s -> s.sgenerics <> []
  | TEnumDec e -> e.egenerics <> []
  | TNamespaceDec _ -> false

let monomorphize st (decls : t_decl list) : t_decl list =
  let ctx = create_ctx st in
  (* index all declarations *)
  collect_decls ctx decls;
  (* scan all code for generic instantiations *)
  List.iter (scan_decl ctx) decls;
  (* process until fixpoint *)
  drain_queue ctx;
  (* output: non-generic originals + specialized copies *)
  let non_generic = List.filter (fun d -> not (is_generic_decl d)) decls in
  non_generic @ List.rev ctx.output
