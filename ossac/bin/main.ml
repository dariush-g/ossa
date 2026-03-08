let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: ossa <file>";
    exit 1
  );

  let args = Array.to_list Sys.argv |> List.tl in

  let emit_tokens = List.mem "--emit-tokens" args in

  let filename = Sys.argv.(1) in

  let source =
    In_channel.with_open_text filename In_channel.input_all
  in

  let tokens = Ossa.Lexer.tokenize source in

  if emit_tokens then
    List.iter (fun ({ tok; span } : Ossa.Lexer.spanned_token) ->
    Printf.printf "[%d..%d] %s "
      span.start
      span.finish
    (Ossa.Lexer.token_to_string tok)
  ) tokens