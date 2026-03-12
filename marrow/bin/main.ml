let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: marrow ...";
    exit 1);

  match Sys.argv.(1) with
  | "init" -> Marrow_lib.Init.init ()
  | "build" ->
      let entries = Marrow_lib.Bin.bins () in
      List.iter
        (fun (name, path) -> Printf.printf "building %s from %s\n%!" name path)
        entries
  | "run" -> ()
  | arg ->
      Printf.printf "unknown argument %s" arg;
      exit 1
