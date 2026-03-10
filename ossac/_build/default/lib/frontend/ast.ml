open Common

type ident = string
type path = string list

type typ =
  (* unsigned ints *)
  | TInt of int_type
  (* floats *)
  | TFloat of float_type
  | TString
  | TBool
  | TVoid
  | TNamed of path
  | TPtr of typ
  | TArray of typ
  | TTuple of typ list
  | TFunc of typ list * typ
  | TGeneric of string
  | TApp of typ * typ list

type param_mode = Value | Ref
type param = { name : string; typ : typ option; mode : param_mode }

type literal =
  | LInt of string
  | LFloat of string
  | LString of string
  | LChar of char
  | LBool of bool

type struct_field = { field_name : string; field_typ : typ }

type enum_variant = { variant_name : string; fields : enum_field list }
and enum_field = { field_name : string option; field_type : typ }

type pattern =
  | PWildcard
  | PVar of ident
  | PTuple of pattern list
  | PVariant of path * pattern list
  | PLiteral of literal

type expr =
  | Unary of unary_op * expr
  | Binary of binary_op * expr * expr
  | Literal of literal
  | Tuple of expr list
  | Array of expr list
  | Path of path
  | Call of expr * expr list
  | Index of expr * expr
  | If of expr * block * block
  | Closure of param list * block
  | Iterate of expr * expr
  | StructInit of path * init_field list
  | Field of expr * ident
  | Match of expr * match_arm list

and init_field = { init_name : ident; init_value : expr }
and block = { stmts : stmt list; tail : expr option }
and match_arm = { arm_pat : pattern; arm_body : block }

and stmt =
  | Let of pattern * typ option * expr option
  | Expr of expr
  | Return of expr option
  | Loop of block
  | Block of block
  | Break
  | Continue

type func_decl = {
  fname : ident;
  fgenerics : ident list;
  fparams : param list;
  fret : typ option;
  fbody : block;
}

type struct_decl = {
  sname : ident;
  sgenerics : ident list;
  sfields : struct_field list;
  sfuncs : func_decl list;
}

type enum_decl = {
  ename : ident;
  egenerics : ident list;
  evariants : enum_variant list;
}

type namespace_decl = { npath : path; ndecls : decl list }

and decl =
  | FuncDec of func_decl
  | StructDec of struct_decl
  | EnumDec of enum_decl
  | NamespaceDec of namespace_decl

let indent n = String.make (n * 2) ' '

let dump_path p =
  "[" ^ String.concat "; " (List.map (Printf.sprintf "%S") p) ^ "]"

let rec dump_typ = function
  | TInt U8 -> "Tu8"
  | TInt U16 -> "Tu16"
  | TInt U32 -> "Tu32"
  | TInt U64 -> "Tu64"
  | TInt I8 -> "Ti8"
  | TInt I16 -> "Ti16"
  | TInt I32 -> "Ti32"
  | TInt I64 -> "Ti64"
  | TFloat F32 -> "Tf32"
  | TFloat F64 -> "Tf64"
  | TString -> "TString"
  | TBool -> "TBool"
  | TVoid -> "TVoid"
  | TNamed p -> "TNamed(" ^ dump_path p ^ ")"
  | TPtr t -> "TPtr(" ^ dump_typ t ^ ")"
  | TArray t -> "TArray(" ^ dump_typ t ^ ")"
  | TTuple ts -> "TTuple([" ^ String.concat "; " (List.map dump_typ ts) ^ "])"
  | TFunc (args, ret) ->
      "TFunc(["
      ^ String.concat "; " (List.map dump_typ args)
      ^ "], " ^ dump_typ ret ^ ")"
  | TGeneric s -> "TGeneric(" ^ Printf.sprintf "%S" s ^ ")"
  | TApp (t, args) ->
      "TApp(" ^ dump_typ t ^ ", ["
      ^ String.concat "; " (List.map dump_typ args)
      ^ "])"

let dump_param_mode = function Value -> "Value" | Ref -> "Ref"

let dump_literal = function
  | LInt s -> "LInt(" ^ Printf.sprintf "%S" s ^ ")"
  | LFloat s -> "LFloat(" ^ Printf.sprintf "%S" s ^ ")"
  | LString s -> "LString(" ^ Printf.sprintf "%S" s ^ ")"
  | LChar c -> "LChar(" ^ Printf.sprintf "%C" c ^ ")"
  | LBool b -> "LBool(" ^ string_of_bool b ^ ")"

let dump_unary_op = function
  | Neg -> "Neg"
  | Not -> "Not"
  | Deref -> "Deref"
  | AddrOf -> "AddrOf"

let dump_binary_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Eq -> "Eq"
  | Ne -> "Ne"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | And -> "And"
  | Or -> "Or"
  | Assign -> "Assign"
  | AddAssign -> "AddAssign"
  | SubAssign -> "SubAssign"
  | MulAssign -> "MulAssign"
  | DivAssign -> "DivAssign"
  | Range -> "Range"

let dump_option f = function None -> "None" | Some x -> "Some(" ^ f x ^ ")"

let dump_param (p : param) =
  "{" ^ "name=" ^ Printf.sprintf "%S" p.name ^ "; typ="
  ^ dump_option dump_typ p.typ ^ "; mode=" ^ dump_param_mode p.mode ^ "}"

let dump_struct_field (f : struct_field) =
  "{" ^ "field_name="
  ^ Printf.sprintf "%S" f.field_name
  ^ "; field_typ=" ^ dump_typ f.field_typ ^ "}"

let dump_enum_field (f : enum_field) =
  "{" ^ "field_name="
  ^ (match f.field_name with
    | None -> "None"
    | Some s -> "Some(" ^ Printf.sprintf "%S" s ^ ")")
  ^ "; field_type=" ^ dump_typ f.field_type ^ "}"

let dump_enum_variant (v : enum_variant) =
  "{" ^ "variant_name="
  ^ Printf.sprintf "%S" v.variant_name
  ^ "; fields=["
  ^ String.concat "; " (List.map dump_enum_field v.fields)
  ^ "]" ^ "}"

let rec dump_pattern = function
  | PWildcard -> "PWildcard"
  | PVar x -> "PVar(" ^ Printf.sprintf "%S" x ^ ")"
  | PTuple ps ->
      "PTuple([" ^ String.concat "; " (List.map dump_pattern ps) ^ "])"
  | PVariant (p, ps) ->
      "PVariant(" ^ dump_path p ^ ", ["
      ^ String.concat "; " (List.map dump_pattern ps)
      ^ "])"
  | PLiteral lit -> "PLiteral(" ^ dump_literal lit ^ ")"

let rec dump_expr indent_level = function
  | Unary (op, e) ->
      Printf.printf "%sUnary(%s)\n" (indent indent_level) (dump_unary_op op);
      dump_expr (indent_level + 1) e
  | Binary (op, l, r) ->
      Printf.printf "%sBinary(%s)\n" (indent indent_level) (dump_binary_op op);
      dump_expr (indent_level + 1) l;
      dump_expr (indent_level + 1) r
  | Literal lit ->
      Printf.printf "%sLiteral(%s)\n" (indent indent_level) (dump_literal lit)
  | Tuple es ->
      Printf.printf "%sTuple[%d]\n" (indent indent_level) (List.length es);
      List.iter (dump_expr (indent_level + 1)) es
  | Array es ->
      Printf.printf "%sArray[%d]\n" (indent indent_level) (List.length es);
      List.iter (dump_expr (indent_level + 1)) es
  | Path p -> Printf.printf "%sPath(%s)\n" (indent indent_level) (dump_path p)
  | Call (f, args) ->
      Printf.printf "%sCall\n" (indent indent_level);
      Printf.printf "%sfn:\n" (indent (indent_level + 1));
      dump_expr (indent_level + 2) f;
      Printf.printf "%sargs[%d]:\n"
        (indent (indent_level + 1))
        (List.length args);
      List.iter (dump_expr (indent_level + 2)) args
  | Index (e, i) ->
      Printf.printf "%sIndex\n" (indent indent_level);
      dump_expr (indent_level + 1) e;
      dump_expr (indent_level + 1) i
  | If (cond, tb, eb) ->
      Printf.printf "%sIf\n" (indent indent_level);
      Printf.printf "%scond:\n" (indent (indent_level + 1));
      dump_expr (indent_level + 2) cond;
      Printf.printf "%sthen:\n" (indent (indent_level + 1));
      dump_block (indent_level + 2) tb;
      Printf.printf "%selse:\n" (indent (indent_level + 1));
      dump_block (indent_level + 2) eb
  | Closure (params, body) ->
      Printf.printf "%sClosure\n" (indent indent_level);
      Printf.printf "%sparams = [%s]\n"
        (indent (indent_level + 1))
        (String.concat "; " (List.map dump_param params));
      Printf.printf "%sbody:\n" (indent (indent_level + 1));
      dump_block (indent_level + 2) body
  | Iterate (src, f) ->
      Printf.printf "%sIterate\n" (indent indent_level);
      Printf.printf "%ssrc:\n" (indent (indent_level + 1));
      dump_expr (indent_level + 2) src;
      Printf.printf "%sfn:\n" (indent (indent_level + 1));
      dump_expr (indent_level + 2) f
  | StructInit (p, fields) ->
      Printf.printf "%sStructInit(%s)\n" (indent indent_level) (dump_path p);
      List.iter
        (fun f ->
          Printf.printf "%sfield %S =\n" (indent (indent_level + 1)) f.init_name;
          dump_expr (indent_level + 2) f.init_value)
        fields
  | Field (e, name) ->
      Printf.printf "%sField(%S)\n" (indent indent_level) name;
      dump_expr (indent_level + 1) e
  | Match (e, arms) ->
      Printf.printf "%sMatch\n" (indent indent_level);
      Printf.printf "%sexpr:\n" (indent (indent_level + 1));
      dump_expr (indent_level + 2) e;
      Printf.printf "%sarms[%d]:\n"
        (indent (indent_level + 1))
        (List.length arms);
      List.iter (dump_match_arm (indent_level + 2)) arms

and dump_match_arm indent_level (arm : match_arm) =
  Printf.printf "%sMatchArm\n" (indent indent_level);
  Printf.printf "%spat = %s\n"
    (indent (indent_level + 1))
    (dump_pattern arm.arm_pat);
  Printf.printf "%sbody:\n" (indent (indent_level + 1));
  dump_block (indent_level + 2) arm.arm_body

and dump_stmt indent_level = function
  | Let (pat, ty, init) -> (
      Printf.printf "%sLet\n" (indent indent_level);
      Printf.printf "%spattern = %s\n"
        (indent (indent_level + 1))
        (dump_pattern pat);
      Printf.printf "%stype = %s\n"
        (indent (indent_level + 1))
        (dump_option dump_typ ty);
      Printf.printf "%sinit = %s\n"
        (indent (indent_level + 1))
        (match init with None -> "None" | Some _ -> "<expr>");
      match init with None -> () | Some e -> dump_expr (indent_level + 2) e)
  | Expr e ->
      Printf.printf "%sExpr\n" (indent indent_level);
      dump_expr (indent_level + 1) e
  | Return eo -> (
      Printf.printf "%sReturn(%s)\n" (indent indent_level)
        (match eo with None -> "None" | Some _ -> "Some");
      match eo with None -> () | Some e -> dump_expr (indent_level + 1) e)
  | Loop b ->
      Printf.printf "%sLoop\n" (indent indent_level);
      dump_block (indent_level + 1) b
  | Block b ->
      Printf.printf "%sBlock\n" (indent indent_level);
      dump_block (indent_level + 1) b
  | Break -> Printf.printf "%sBreak\n" (indent indent_level)
  | Continue -> Printf.printf "%sContinue\n" (indent indent_level)

and dump_block indent_level (b : block) =
  Printf.printf "%sBlock {\n" (indent indent_level);
  Printf.printf "%sstmts[%d]:\n"
    (indent (indent_level + 1))
    (List.length b.stmts);
  List.iter (dump_stmt (indent_level + 2)) b.stmts;
  Printf.printf "%stail = %s\n"
    (indent (indent_level + 1))
    (match b.tail with None -> "None" | Some _ -> "Some");
  (match b.tail with None -> () | Some e -> dump_expr (indent_level + 2) e);
  Printf.printf "%s}\n" (indent indent_level)

let dump_func_decl indent_level (f : func_decl) =
  Printf.printf "%sFuncDec\n" (indent indent_level);
  Printf.printf "%sname = %S\n" (indent (indent_level + 1)) f.fname;
  Printf.printf "%sgenerics = [%s]\n"
    (indent (indent_level + 1))
    (String.concat "; " (List.map (Printf.sprintf "%S") f.fgenerics));
  Printf.printf "%sparams = [%s]\n"
    (indent (indent_level + 1))
    (String.concat "; " (List.map dump_param f.fparams));
  Printf.printf "%sret = %s\n"
    (indent (indent_level + 1))
    (dump_option dump_typ f.fret);
  Printf.printf "%sbody:\n" (indent (indent_level + 1));
  dump_block (indent_level + 2) f.fbody

let dump_struct_decl indent_level (s : struct_decl) =
  Printf.printf "%sStructDec\n" (indent indent_level);
  Printf.printf "%sname = %S\n" (indent (indent_level + 1)) s.sname;
  Printf.printf "%sgenerics = [%s]\n"
    (indent (indent_level + 1))
    (String.concat "; " (List.map (Printf.sprintf "%S") s.sgenerics));
  Printf.printf "%sfields = [%s]\n"
    (indent (indent_level + 1))
    (String.concat "; " (List.map dump_struct_field s.sfields));
  Printf.printf "%sfuncs:\n" (indent (indent_level + 1));
  List.iter (dump_func_decl (indent_level + 2)) s.sfuncs

let dump_enum_decl indent_level (e : enum_decl) =
  Printf.printf "%sEnumDec\n" (indent indent_level);
  Printf.printf "%sname = %S\n" (indent (indent_level + 1)) e.ename;
  Printf.printf "%sgenerics = [%s]\n"
    (indent (indent_level + 1))
    (String.concat "; " (List.map (Printf.sprintf "%S") e.egenerics));
  Printf.printf "%svariants = [%s]\n"
    (indent (indent_level + 1))
    (String.concat "; " (List.map dump_enum_variant e.evariants))

let rec dump_decl_at indent_level = function
  | FuncDec f -> dump_func_decl indent_level f
  | StructDec s -> dump_struct_decl indent_level s
  | EnumDec e -> dump_enum_decl indent_level e
  | NamespaceDec d ->
      Printf.printf "%sNamespaceDec(path=%s)\n" (indent indent_level)
        (dump_path d.npath);
      List.iter (dump_decl_at (indent_level + 1)) d.ndecls

let print_decl d = dump_decl_at 0 d
