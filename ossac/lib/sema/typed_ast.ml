open Common
open Symbols

type r_typ =
  | TInt of int_type
  | TFloat of float_type
  | TString
  | TBool
  | TVoid
  | TNamed of symbol
  | TPtr of r_typ
  | TArray of r_typ
  | TTuple of r_typ list
  | TFunc of r_typ list * r_typ
  | TGeneric of symbol
  | TApp of r_typ * r_typ list

type t_literal =
  | IntLiteral of int_type * int64
  | FloatLiteral of float_type * float
  | StringLiteral of string
  | CharLiteral of char
  | BoolLiteral of bool

type t_pattern =
  | PWildcard
  | PVar of symbol
  | PTuple of t_pattern list
  | PVariant of symbol * t_pattern list
  | PLiteral of t_literal

type t_param = { sid : symbol; r_typ : Ast.typ option; mode : Ast.param_mode }

type t_expr =
  | Unary of unary_op * t_expr
  | Binary of binary_op * t_expr * t_expr
  | Literal of t_literal
  | Tuple of t_expr list
  | Array of t_expr list
  | Local of symbol
  | Func of symbol
  | Call of t_expr * t_expr list
  | StructInit of symbol * t_struct_init_field list
  | EnumInit of symbol * t_enum_init_field list
  | Index of t_expr * t_expr
  | If of t_expr * t_block * t_block
  | Iterate of t_expr * t_expr
  | Closure of t_param list * t_block
  | Field of t_expr * symbol
  | Match of t_expr * t_match_arm list

and t_struct_init_field = { sf_init_sid : symbol; sf_init_value : t_expr }
and t_enum_init_field = { ef_init_sid : symbol option; ef_init_value : t_expr }
and t_match_arm = { arm_pat : t_pattern; arm_body : t_block }
and t_block = { stmts : t_stmt list; tail : t_expr option }

and t_stmt =
  | Let of Ast.pattern * Ast.typ option * t_expr option
  | Expr of t_expr
  | Return of t_expr option
  | Loop of t_block
  | Block of t_block
  | Break
  | Continue

type t_func_decl = {
  fname : symbol;
  fgenerics : symbol list;
  fparams : t_param list;
  fret : r_typ option;
  fbody : t_block;
}

type t_struct_decl = {
  sname : symbol;
  sgenerics : symbol list;
  sfields : t_struct_init_field list;
  sfuncs : t_func_decl list;
}

type t_enum_variant = { variant_name : string; fields : t_enum_field list }
and t_enum_field = { field_name : symbol option; field_type : r_typ }

type t_enum_decl = {
  ename : symbol;
  egenerics : symbol list;
  evariants : t_enum_variant list;
}

type t_namespace_decl = { npath : Ast.path; ndecls : t_decl list }

and t_decl =
  | FuncDec of t_func_decl
  | StructDec of t_struct_decl
  | EnumDec of t_enum_decl
  | NamespaceDec of t_namespace_decl
