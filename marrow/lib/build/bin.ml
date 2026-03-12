let bins () =
  let result = Toml.Parser.from_filename "marrow.toml" in
  match result with
  | `Ok table -> (
      match Toml.Types.Table.find_opt (Toml.Min.key "bin") table with
      | Some (Toml.Types.TTable t) ->
          Toml.Types.Table.fold
            (fun key value acc ->
              match value with
              | Toml.Types.TString s ->
                  (Toml.Types.Table.Key.to_string key, s) :: acc
              | _ -> acc)
            t []
      | _ ->
          Printf.printf "error: [bin] section not found\n";
          exit 1)
  | `Error (_, _) ->
      Printf.printf "couldn't find marrow.toml\n";
      exit 1
