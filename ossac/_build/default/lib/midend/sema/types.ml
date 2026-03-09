type int_type = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 
type float_type = F32 | F64

type t = 
  | TInt of int_type
  | TFloat of float_type
  | TString of string
  | TChar of char
