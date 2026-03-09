open Common
open Symbols

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

type t_expr =
  | Unary of unary_op * t_expr
  | Binary of binary_op * t_expr * t_expr
  | Literal of t_literal
  | Tuple of t_expr list
  | Array of t_expr list
  | Call of t_expr * t_expr list
  | StructInit of symbol * t_struct_init_field list
  | EnumInit of symbol * t_enum_init_field list
  | Index of t_expr * t_expr
  | If of t_expr * t_block * t_block

and t_struct_init_field = { sf_init_sid : symbol; sf_init_value : t_expr }
and t_enum_init_field = { ef_init_sid : symbol option; ef_init_value : t_expr }
and match_arm = { arm_pat : t_pattern; arm_body : t_block }
and t_block = { stmts : t_stmt list; tail : t_expr option }
and t_stmt = Let of Ast.pattern * Ast.typ option * t_expr option
