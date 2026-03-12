let profile () =
  let result = Toml.Parser.from_filename "marrow.toml" in
  match result with
  | `Ok table -> (
      match Toml.Types.Table.find_opt (Toml.Min.key "profile") table with
      | Some (Toml.Types.TTable t) -> t
      | _ ->
          Printf.printf "error: [profile] section not found\n";
          exit 1)
  | `Error (_, _) ->
      Printf.printf "couldn't find marrow.toml\n";
      exit 1

let get_string table key =
  match Toml.Types.Table.find_opt (Toml.Min.key key) table with
  | Some (Toml.Types.TString s) -> s
  | _ -> failwith (Printf.sprintf "missing key: %s" key)

