type int_type = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64
type float_type = F32 | F64

type unary_op = Neg | Not | Deref | AddrOf

type binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Assign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | Range
