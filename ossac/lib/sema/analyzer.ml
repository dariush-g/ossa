open Typed
open Common

type analyzer = {
  symbol_table : Symbols.symbol_table;
  mutable errors : string list;
}

exception Analyzer_abort

let check_unary a op (right : 'a) =
  match op with
  | Deref ->
      match right with 
        | TArray _ -> ()
        | TLocal _ -> ()

  | _ -> ()

let check_expr a expr =
  match expr with
  | TUnary (op, right) -> check_unary a op right
  | TBinary (op, left, right) -> ()
  | TLiteral literal -> ()
  | TTuple exprs -> ()
  | TArray exprs -> ()
  | TLocal sym -> ()
  | TFunc sym -> ()
  | TCall (ident, params) -> ()
  | TStructInit (sym, (fields : t_struct_init_field list)) -> ()
  | TEnumInit (sym, (fields : t_enum_init_field list)) -> ()
  | TIndex (left, idx) -> ()
  | TIf (condition, t, f) -> ()
  | TIterate (left, right) -> ()
  | TClosure (params, block) -> ()
  | TField (ex, sym) -> ()
  | TMatch (matched, arms) -> ()

let check_stmt a stmt = ()
let check_block a block = ()
let analyze_func a func = ()

let error a msg : 'a =
  a.errors <- msg :: a.errors;
  raise Analyzer_abort

let analyze_struct _ (s_decl : Ast.struct_decl) =
  let _ = List.map (fun _ -> ()) s_decl.sfuncs in
  ()

(* type symbol_table = {
  mutable next_id : int;
  mutable symbols : (symbol, symbol_info) Hashtbl.t;
  mutable scopes : (string, symbol) Hashtbl.t list;
} *)

let rec analyze_decl a (p : Ast.decl) =
  let _ =
    match p with
    | Ast.FuncDec f -> analyze_func a f
    | Ast.StructDec s -> analyze_struct a s
    | Ast.NamespaceDec n ->
        let _ = List.map (fun d -> analyze_decl a d) n.ndecls in
        ()
    | _ -> ()
  in
  ()
