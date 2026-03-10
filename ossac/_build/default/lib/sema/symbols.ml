type symbol = int

type symbol_kind =
  | SLocal
  | SParam
  | SFunc
  | SStruct
  | SEnum
  | SGeneric

type symbol_info = {
  sid : symbol;
  sname: string;
  skind: symbol_kind;
}

type symbol_table = {
  mutable next_id : int;
  mutable symbols : (symbol, symbol_info) Hashtbl.t;
  mutable scopes : (string, symbol) Hashtbl.t list;
}

let create () = {
  next_id = 0;
  symbols = Hashtbl.create 64;
  scopes = [Hashtbl.create 16];
}

let fresh t name kind =
  let id = t.next_id in
  t.next_id <- id + 1;
  let info = { sid = id; sname = name; skind = kind } in
  Hashtbl.replace t.symbols id info;
  (match t.scopes with
   | scope :: _ -> Hashtbl.replace scope name id
   | [] -> failwith "no scope");
  id

let push_scope t =
  t.scopes <- Hashtbl.create 16 :: t.scopes

let pop_scope t =
  match t.scopes with
    | _ :: rest -> t.scopes <- rest
    | [] -> failwith "no scope to pop"

let resolve t name =
  let rec go = function
    | [] -> None
    | scope :: rest ->
      match Hashtbl.find_opt scope name with
      | Some id -> Some id
      | None -> go rest
  in
  go t.scopes

let info t id = Hashtbl.find t.symbols id
let name t id = (info t id).sname
let kind t id = (info t id).skind
