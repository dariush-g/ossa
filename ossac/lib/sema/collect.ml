open Ast

type sig_func = {
  sfname : string;
  sfgenerics : string list;
  sfparams : param list;
  sfret : typ;
}

type sig_struct = {
  ssname : string;
  ssgenerics : string list;
  ssfields : struct_field list;
  ssfuncs : sig_func list;
}

type sig_enum = {
  sename : string;
  segregerics : string list;
  sevariants : enum_variant list;
}

type sig_namespace = { snpath : path; sndecls : sig_decl list }

and sig_decl =
  | SigFunc of sig_func
  | SigStruct of sig_struct
  | SigEnum of sig_enum
  | SigNamespace of sig_namespace

type mw_module = sig_decl list

let collect_func (f : Ast.func_decl) : sig_func =
  {
    sfname = f.fname;
    sfgenerics = f.fgenerics;
    sfparams = f.fparams;
    sfret = f.fret;
  }

let collect_struct (s : Ast.struct_decl) : sig_struct =
  {
    ssname = s.sname;
    ssgenerics = s.sgenerics;
    ssfields = s.sfields;
    ssfuncs = List.map collect_func s.sfuncs;
  }

let collect_enum (e : Ast.enum_decl) : sig_enum =
  { sename = e.ename; segregerics = e.egenerics; sevariants = e.evariants }

let rec collect_decl (d : Ast.decl) : sig_decl =
  match d with
  | Ast.FuncDec f -> SigFunc (collect_func f)
  | Ast.StructDec s -> SigStruct (collect_struct s)
  | Ast.EnumDec e -> SigEnum (collect_enum e)
  | Ast.NamespaceDec n ->
      SigNamespace
        { snpath = n.npath; sndecls = List.map collect_decl n.ndecls }

let get_mw_module (decls : Ast.decl list) : mw_module =
  List.map collect_decl decls

let emit_module_sig (path : string) (m : mw_module) =
  let oc = open_out_bin path in
  Marshal.to_channel oc m [];
  close_out oc

let load_module_sig (path : string) : mw_module =
  let ic = open_in_bin path in
  let m : mw_module = Marshal.from_channel ic in
  close_in ic;
  m

let rec dump_sig_decl = function
  | SigFunc f ->
      Printf.printf "func %s(%s) -> %s\n" f.sfname
        (String.concat ", " (List.map dump_param f.sfparams))
        (dump_typ f.sfret)
  | SigStruct s -> Printf.printf "struct %s { ... }\n" s.ssname
  | SigEnum e -> Printf.printf "enum %s { ... }\n" e.sename
  | SigNamespace n ->
      Printf.printf "namespace %s\n" (String.concat "::" n.snpath);
      List.iter dump_sig_decl n.sndecls

let dump_module (m : mw_module) = List.iter dump_sig_decl m
