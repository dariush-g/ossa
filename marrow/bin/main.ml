let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: marrow ...";
    exit 1);

  match Sys.argv.(1) with
  | "init" -> ()
  | "build" -> ()
  | "run" -> ()
  | arg ->
      Printf.printf "unknown argument %s" arg;
      exit 1
