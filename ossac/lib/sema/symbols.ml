type symbol = int
type symbol_kind = SLocal | SParam | SFunc | SStruct | SEnum | SGeneric
type symbol_info = { sid : symbol; sname : string; skind : symbol_kind }

type symbol_table = {
  mutable next_id : int;
  mutable symbols : (symbol, symbol_info) Hashtbl.t;
  mutable scopes : (string, symbol) Hashtbl.t list;
}

let is_function t id = (Hashtbl.find t.symbols id).skind = SFunc
let is_param t id = (Hashtbl.find t.symbols id).skind = SParam
let is_struct t id = (Hashtbl.find t.symbols id).skind = SStruct
let is_enum t id = (Hashtbl.find t.symbols id).skind = SEnum
let is_generic t id = (Hashtbl.find t.symbols id).skind = SGeneric
let is_local t id = (Hashtbl.find t.symbols id).skind = SLocal

let create () =
  { next_id = 0; symbols = Hashtbl.create 64; scopes = [ Hashtbl.create 16 ] }

let fresh t name kind =
  let id = t.next_id in
  t.next_id <- id + 1;
  let info = { sid = id; sname = name; skind = kind } in
  Hashtbl.replace t.symbols id info;
  (match t.scopes with
  | scope :: _ -> Hashtbl.replace scope name id
  | [] -> failwith "no scope");
  id

let push_scope t = t.scopes <- Hashtbl.create 16 :: t.scopes

let pop_scope t =
  match t.scopes with
  | _ :: rest -> t.scopes <- rest
  | [] -> failwith "no scope to pop"

let resolve t name =
  let rec go = function
    | [] -> None
    | scope :: rest -> (
        match Hashtbl.find_opt scope name with
        | Some id -> Some id
        | None -> go rest)
  in
  go t.scopes

let resolve_or_error t name =
  match resolve t name with
  | Some id -> id
  | None ->
      Printf.eprintf "error: undefined name '%s'\n%!" name;
      exit 1

let info t id = Hashtbl.find t.symbols id
let name t id = (info t id).sname
let kind t id = (info t id).skind
