open Typed

let named_types : (Symbols.symbol, Llvm.lltype) Hashtbl.t = Hashtbl.create 16
let context = Llvm.global_context ()
let _module = Llvm.create_module context "ossa"
let builder = Llvm.builder context

let rec llvm_type t =
  match t with
  | TInt I8 | TInt U8 -> Llvm.i8_type context
  | TInt I16 | TInt U16 -> Llvm.i16_type context
  | TInt I32 | TInt U32 -> Llvm.i32_type context
  | TInt I64 | TInt U64 -> Llvm.i64_type context
  | TFloat F32 -> Llvm.float_type context
  | TFloat F64 -> Llvm.double_type context
  | TBool -> Llvm.i1_type context
  | TVoid -> Llvm.void_type context
  | TPtr _ -> Llvm.pointer_type context
  | TString -> Llvm.pointer_type context
  | TChar -> Llvm.i8_type context
  | TArray t -> Llvm.array_type (llvm_type t) size
  | TTuple ts ->
      Llvm.struct_type context (Array.of_list (List.map llvm_type ts))
  | TFunc (params, ret) ->
      let param_types = Array.of_list (List.map llvm_type params) in
      let fn_type = Llvm.function_type (llvm_type ret) param_types in
      Llvm.pointer_type context
  | TNamed sym -> Hashtbl.find named_types sym
  | TGeneric _ -> failwith "bug: generic type in codegen"
  | TApp _ -> failwith "bug: type application in codegen"
