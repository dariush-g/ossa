let init () =
  let name = Filename.basename (Sys.getcwd ()) in

  if Sys.file_exists "marrow.toml" then (
    prerr_endline "error: marrow.toml already exists";
    exit 1);

  if not (Sys.file_exists "src") then Sys.mkdir "src" 0o755;

  let oc = open_out "marrow.toml" in
  Printf.fprintf oc
    "[profile]\nname = \"%s\"\n\n[bin]\nmain = \"src/main.oa\"\n" name;
  close_out oc;

  let oc = open_out "src/main.oa" in
  Printf.fprintf oc "main :: () -> int {\n    return 0;\n}\n";
  close_out oc;

  Printf.printf "initialized project '%s'\n%!" name
