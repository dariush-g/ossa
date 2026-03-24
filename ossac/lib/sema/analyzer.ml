open Typed
open Common
open Symbols

type analyzer = {
  st : symbol_table;
  types : (symbol, r_typ) Hashtbl.t;
  mutable ret_typ : r_typ option;
  mutable errors : string list;
}

let create st = { st; types = Hashtbl.create 64; ret_typ = None; errors = [] }
let error a msg = a.errors <- msg :: a.errors
let set_type a sid typ = Hashtbl.replace a.types sid typ
let get_type a sid = Hashtbl.find_opt a.types sid

(* ── type display ────────────────────────────────────────────────── *)

let rec typ_to_string st = function
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
  | TNamed s -> Symbols.name st s
  | TPtr t -> "*" ^ typ_to_string st t
  | TArray t -> "[" ^ typ_to_string st t ^ "]"
  | TTuple ts -> "(" ^ String.concat ", " (List.map (typ_to_string st) ts) ^ ")"
  | TFunc (args, ret) ->
      "("
      ^ String.concat ", " (List.map (typ_to_string st) args)
      ^ ") -> " ^ typ_to_string st ret
  | TGeneric s -> Symbols.name st s
  | TApp (base, args) ->
      typ_to_string st base ^ "<"
      ^ String.concat ", " (List.map (typ_to_string st) args)
      ^ ">"

(* ── type equality ───────────────────────────────────────────────── *)

let rec types_equal a b =
  match (a, b) with
  | TInt a, TInt b -> a = b
  | TFloat a, TFloat b -> a = b
  | TString, TString | TBool, TBool | TVoid, TVoid | TChar, TChar -> true
  | TNamed a, TNamed b -> a = b
  | TPtr a, TPtr b -> types_equal a b
  | TArray a, TArray b -> types_equal a b
  | TTuple a, TTuple b ->
      List.length a = List.length b && List.for_all2 types_equal a b
  | TFunc (a1, r1), TFunc (a2, r2) ->
      List.length a1 = List.length a2
      && List.for_all2 types_equal a1 a2
      && types_equal r1 r2
  | TGeneric a, TGeneric b -> a = b
  | _ -> false

(* ── numeric helpers ─────────────────────────────────────────────── *)

(* ── type existence checks ───────────────────────────────────────── *)

let rec check_type_exists a = function
  | TNamed sid ->
      let k = Symbols.kind a.st sid in
      if k <> SStruct && k <> SEnum then
        error a
          (Printf.sprintf "type '%s' is not a struct or enum"
             (Symbols.name a.st sid))
  | TPtr t | TArray t -> check_type_exists a t
  | TTuple ts -> List.iter (check_type_exists a) ts
  | TFunc (args, ret) ->
      List.iter (check_type_exists a) args;
      check_type_exists a ret
  | TApp (base, args) ->
      check_type_exists a base;
      List.iter (check_type_exists a) args
  | TGeneric sid ->
      let k = Symbols.kind a.st sid in
      if k <> SGeneric then
        error a
          (Printf.sprintf "'%s' is not a type parameter" (Symbols.name a.st sid))
  | TInt _ | TFloat _ | TString | TBool | TVoid | TChar -> ()

let check_ast_type_exists a (ty : Ast.typ) =
  let resolved = Resolver.resolve_typ a.st ty in
  check_type_exists a resolved

(* ── numeric helpers ─────────────────────────────────────────────── *)

let is_int = function TInt _ -> true | _ -> false
let is_float = function TFloat _ -> true | _ -> false
let is_numeric t = is_int t || is_float t
let is_bool = function TBool -> true | _ -> false
let is_ptr = function TPtr _ -> true | _ -> false

(* ── infer expression type ───────────────────────────────────────── *)

let rec infer_type a expr =
  match expr with
  | TLiteral lit -> begin
      match lit with
      | IntLiteral (ty, _) -> TInt ty
      | FloatLiteral (ty, _) -> TFloat ty
      | StringLiteral _ -> TString
      | CharLiteral _ -> TChar
      | BoolLiteral _ -> TBool
    end
  | TLocal sym | TFunc sym -> begin
      match get_type a sym with Some t -> t | None -> TVoid
    end
  | TUnary (op, inner) ->
      let t = infer_type a inner in
      begin match op with
      | Neg -> t
      | Not -> TBool
      | Deref -> ( match t with TPtr inner -> inner | _ -> t)
      | AddrOf -> TPtr t
      end
  | TBinary (op, left, right) -> begin
      let lt = infer_type a left in
      let _rt = infer_type a right in
      match op with
      | Add | Sub | Mul | Div | Mod -> lt
      | Eq | Ne | Lt | Le | Gt | Ge -> TBool
      | And | Or -> TBool
      | Assign | AddAssign | SubAssign | MulAssign | DivAssign -> TVoid
      | Range -> TArray lt
    end
  | TTuple es -> TTuple (List.map (infer_type a) es)
  | TArray es -> begin
      match es with [] -> TArray TVoid | e :: _ -> TArray (infer_type a e)
    end
  | TCall (callee, _) -> begin
      let ct = infer_type a callee in
      match ct with TFunc (_, ret) -> ret | _ -> TVoid
    end
  | TIndex (arr, _) -> begin
      let t = infer_type a arr in
      match t with TArray inner -> inner | _ -> TVoid
    end
  | TIf (_, then_b, _) -> infer_block_type a then_b
  | TClosure (params, body) ->
      let param_types =
        List.filter_map
          (fun (p : t_param) ->
            match p.r_typ with
            | Some ty -> Some (Resolver.resolve_typ a.st ty)
            | None -> None)
          params
      in
      let ret = infer_block_type a body in
      TFunc (param_types, ret)
  | TIterate _ -> TVoid
  | TStructInit (sid, _) -> TNamed sid
  | TEnumInit (sid, _) -> TNamed sid
  | TField _ -> TVoid (* needs struct type info to resolve *)
  | TMatch (_, arms) -> begin
      match arms with
      | arm :: _ -> infer_block_type a arm.arm_body
      | [] -> TVoid
    end

and infer_block_type a (b : t_block) =
  match b.tail with Some e -> infer_type a e | None -> TVoid

(* ── check expressions ───────────────────────────────────────────── *)

let check_binary_op a op lt rt =
  match op with
  | Add | Sub | Mul | Div | Mod ->
      if not (is_numeric lt && is_numeric rt) then
        error a
          (Printf.sprintf "arithmetic on non-numeric types: %s and %s"
             (typ_to_string a.st lt) (typ_to_string a.st rt));
      if not (types_equal lt rt) then
        error a
          (Printf.sprintf "type mismatch in arithmetic: %s vs %s"
             (typ_to_string a.st lt) (typ_to_string a.st rt))
  | Eq | Ne ->
      if not (types_equal lt rt) then
        error a
          (Printf.sprintf "comparison between different types: %s vs %s"
             (typ_to_string a.st lt) (typ_to_string a.st rt))
  | Lt | Le | Gt | Ge ->
      if not (is_numeric lt && is_numeric rt) then
        error a
          (Printf.sprintf "ordering on non-numeric types: %s and %s"
             (typ_to_string a.st lt) (typ_to_string a.st rt))
  | And | Or ->
      if not (is_bool lt && is_bool rt) then
        error a "logical operators require bool operands"
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign -> ()
  | Range ->
      if not (is_int lt && is_int rt) then
        error a "range requires integer bounds"

let rec check_expr a expr =
  match expr with
  | TLiteral _ -> ()
  | TLocal _ | TFunc _ -> ()
  | TUnary (Neg, inner) ->
      check_expr a inner;
      let t = infer_type a inner in
      if not (is_numeric t) then error a "negation on non-numeric type"
  | TUnary (Not, inner) ->
      check_expr a inner;
      let t = infer_type a inner in
      if not (is_bool t) then error a "logical not on non-bool type"
  | TUnary (Deref, inner) ->
      check_expr a inner;
      let t = infer_type a inner in
      if not (is_ptr t) then error a "dereference of non-pointer type"
  | TUnary (AddrOf, inner) -> check_expr a inner
  | TBinary (op, left, right) ->
      check_expr a left;
      check_expr a right;
      check_binary_op a op (infer_type a left) (infer_type a right)
  | TTuple es | TArray es -> List.iter (check_expr a) es
  | TCall (callee, args) ->
      check_expr a callee;
      List.iter (check_expr a) args
  | TIndex (arr, idx) ->
      check_expr a arr;
      check_expr a idx;
      let it = infer_type a idx in
      if not (is_int it) then error a "array index must be integer"
  | TIf (cond, then_b, else_b) ->
      check_expr a cond;
      let ct = infer_type a cond in
      if not (is_bool ct) then error a "if condition must be bool";
      check_block a then_b;
      check_block a else_b
  | TClosure (params, body) ->
      List.iter
        (fun (p : t_param) ->
          match p.r_typ with
          | Some ty -> set_type a p.sid (Resolver.resolve_typ a.st ty)
          | None -> ())
        params;
      check_block a body
  | TIterate (src, fn) ->
      check_expr a src;
      check_expr a fn
  | TStructInit (sid, fields) ->
      let k = Symbols.kind a.st sid in
      if k <> SStruct then
        error a (Printf.sprintf "'%s' is not a struct" (Symbols.name a.st sid));
      List.iter (fun f -> check_expr a f.sf_init_value) fields
  | TEnumInit (sid, fields) ->
      let k = Symbols.kind a.st sid in
      if k <> SEnum then
        error a (Printf.sprintf "'%s' is not an enum" (Symbols.name a.st sid));
      List.iter (fun f -> check_expr a f.ef_init_value) fields
  | TField (obj, _) -> check_expr a obj
  | TMatch (scrutinee, arms) ->
      check_expr a scrutinee;
      let scrut_typ = infer_type a scrutinee in
      List.iter
        (fun (arm : t_match_arm) ->
          bind_t_pattern a arm.arm_pat scrut_typ;
          check_block a arm.arm_body)
        arms

(* ── check statements & blocks ───────────────────────────────────── *)

and bind_t_pattern a (pat : t_pattern) (typ : r_typ) =
  match (pat, typ) with
  | PWildcard, _ -> ()
  | PVar sid, t -> set_type a sid t
  | PTuple pats, TTuple ts when List.length pats = List.length ts ->
      List.iter2 (bind_t_pattern a) pats ts
  | PTuple pats, _ -> List.iter (fun p -> bind_t_pattern a p TVoid) pats
  | PVariant (sid, pats), _ ->
      (* check that the variant constructor exists as a known symbol *)
      let k = Symbols.kind a.st sid in
      if k <> SFunc then
        error a
          (Printf.sprintf "unknown enum variant '%s'" (Symbols.name a.st sid));
      List.iter (fun p -> bind_t_pattern a p TVoid) pats
  | PLiteral _, _ -> ()

and bind_ast_pattern a (pat : Ast.pattern) (typ : r_typ) =
  match (pat, typ) with
  | Ast.PWildcard, _ -> ()
  | Ast.PVar name, t -> begin
      (* look up the symbol the resolver created for this name *)
      match Symbols.resolve a.st name with
      | Some sid -> set_type a sid t
      | None -> ()
    end
  | Ast.PTuple pats, TTuple ts when List.length pats = List.length ts ->
      List.iter2 (bind_ast_pattern a) pats ts
  | Ast.PTuple pats, _ -> List.iter (fun p -> bind_ast_pattern a p TVoid) pats
  | Ast.PVariant (_, pats), _ ->
      List.iter (fun p -> bind_ast_pattern a p TVoid) pats
  | Ast.PLiteral _, _ -> ()

and check_stmt a stmt =
  match stmt with
  | TLet (pat, typ, init) ->
      Option.iter (check_expr a) init;
      (* check annotated type exists *)
      Option.iter (check_ast_type_exists a) typ;
      (* infer type from init expression *)
      let inferred =
        match init with Some e -> infer_type a e | None -> TVoid
      in
      (* if there's an explicit annotation, use it; otherwise use inferred *)
      let declared =
        match typ with
        | Some ty -> Resolver.resolve_typ a.st ty
        | None -> inferred
      in
      (* check annotation matches init if both present *)
      (match (typ, init) with
      | Some _, Some _ ->
          if (not (types_equal declared inferred)) && inferred <> TVoid then
            error a
              (Printf.sprintf "type mismatch in let: declared %s, got %s"
                 (typ_to_string a.st declared)
                 (typ_to_string a.st inferred))
      | _ -> ());
      (* bind the pattern variables to the resolved type *)
      let bind_typ = if typ <> None then declared else inferred in
      bind_ast_pattern a pat bind_typ
  | TExpr e -> check_expr a e
  | TReturn e_opt -> begin
      Option.iter (check_expr a) e_opt;
      match (a.ret_typ, e_opt) with
      | Some expected, Some e ->
          let actual = infer_type a e in
          if not (types_equal expected actual) then
            error a
              (Printf.sprintf "return type mismatch: expected %s, got %s"
                 (typ_to_string a.st expected)
                 (typ_to_string a.st actual))
      | _ -> ()
    end
  | TLoop body -> check_block a body
  | TBlock body -> check_block a body
  | TBreak | TContinue -> ()

and check_block a (b : t_block) =
  List.iter (check_stmt a) b.stmts;
  Option.iter (check_expr a) b.tail

(* ── check declarations ─────────────────────────────────────────── *)

let check_func a (f : t_func_decl) =
  (* push generics into scope so resolve_typ can find them *)
  Symbols.push_scope a.st;
  List.iter
    (fun g -> ignore (Symbols.fresh a.st (Symbols.name a.st g) SGeneric))
    f.fgenerics;
  (* check and register param types *)
  List.iter
    (fun (p : t_param) ->
      match p.r_typ with
      | Some ty ->
          check_ast_type_exists a ty;
          set_type a p.sid (Resolver.resolve_typ a.st ty)
      | None -> ())
    f.fparams;
  (* check return type exists *)
  Option.iter (check_type_exists a) f.fret;
  a.ret_typ <- f.fret;
  check_block a f.fbody;
  a.ret_typ <- None;
  Symbols.pop_scope a.st

let check_struct a (s : t_struct_decl) =
  Symbols.push_scope a.st;
  List.iter
    (fun g -> ignore (Symbols.fresh a.st (Symbols.name a.st g) SGeneric))
    s.sgenerics;
  List.iter (check_func a) s.sfuncs;
  Symbols.pop_scope a.st

let check_enum a (e : t_enum_decl) =
  Symbols.push_scope a.st;
  List.iter
    (fun g -> ignore (Symbols.fresh a.st (Symbols.name a.st g) SGeneric))
    e.egenerics;
  List.iter
    (fun (v : t_enum_variant) ->
      List.iter
        (fun (f : t_enum_field) -> check_type_exists a f.field_type)
        v.fields)
    e.evariants;
  Symbols.pop_scope a.st

let rec check_decl a (d : t_decl) =
  match d with
  | TFuncDec f -> check_func a f
  | TStructDec s -> check_struct a s
  | TEnumDec e -> check_enum a e
  | TNamespaceDec ns -> List.iter (check_decl a) ns.ndecls

let analyze st (decls : t_decl list) : string list =
  let a = create st in
  List.iter (check_decl a) decls;
  List.rev a.errors
