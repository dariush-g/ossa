type ident = string

type typ =
  | TInt
  | TLong
  | TFloat
  | TDouble
  | TString
  | TBool
  | TVoid
  | TNamed of ident
  | TPtr of typ
  | TArray of typ
  | TTuple of typ list
  | TFunc of typ list * typ
  | TGeneric of ident
  | TApp of ident * typ list

type param_mode =
  | Value
  | Ref

type param = {
  name : ident;
  typ : typ option;
  mode : param_mode;
}

type literal =
  | LInt of string
  | LFloat of string
  | LString of string
  | LChar of char
  | LBool of bool

type struct_field = {
  field_name : ident;
  field_typ : typ;
}

type pattern =
  | PVar of ident
  | PTuple of pattern list

type expr =
  | Literal of literal
  | Tuple of expr list
  | Array of expr list
  | Var of ident

  | If of expr * block * block
  | Closure of param list * block
  | Iterator of expr * expr

  | StructInit of ident * init_field list
  | Field of expr * ident

and init_field = {
  init_name : ident;
  init_value : expr;
}

and block = {
  stmts : stmt list;
  tail : expr option;
}

and match_arm = {
  arm_pat : pattern;
  arm_body : block;
}

and stmt =
  | Let of pattern * typ option * expr option
  | Expr of expr
  | Return of expr option
  | Loop of block
  | Block of block
  | FuncDec of ident * ident list * param list * typ option * block
  | StructDec of ident * ident list * struct_field list
  | Break
  | Continue