open Common
open Symbols

type r_typ =
  | TInt of int_type
  | TFloat of float_type
  | TString
  | TBool
  | TVoid
  | TChar
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
  | TUnary of unary_op * t_expr
  | TBinary of binary_op * t_expr * t_expr
  | TLiteral of t_literal
  | TTuple of t_expr list
  | TArray of t_expr list
  | TLocal of symbol
  | TFunc of symbol
  | TCall of t_expr * t_expr list
  | TStructInit of symbol * t_struct_init_field list
  | TEnumInit of symbol * t_enum_init_field list
  | TIndex of t_expr * t_expr
  | TIf of t_expr * t_block * t_block
  | TIterate of t_expr * t_expr
  | TClosure of t_param list * t_block
  | TField of t_expr * symbol
  | TMatch of t_expr * t_match_arm list

and t_struct_init_field = { sf_init_sid : symbol; sf_init_value : t_expr }
and t_enum_init_field = { ef_init_sid : symbol option; ef_init_value : t_expr }
and t_match_arm = { arm_pat : t_pattern; arm_body : t_block }
and t_block = { stmts : t_stmt list; tail : t_expr option }

and t_stmt =
  | TLet of Ast.pattern * Ast.typ option * t_expr option
  | TExpr of t_expr
  | TReturn of t_expr option
  | TLoop of t_block
  | TBlock of t_block
  | TBreak
  | TContinue

type t_func_decl = {
  fname : symbol;
  fgenerics : symbol list;
  fparams : t_param list;
  fret : r_typ;
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
  | TFuncDec of t_func_decl
  | TStructDec of t_struct_decl
  | TEnumDec of t_enum_decl
  | TNamespaceDec of t_namespace_decl
