let find_flag prefix args =
  List.find_map
    (fun arg ->
      if
        String.length arg > String.length prefix
        && String.sub arg 0 (String.length prefix) = prefix
      then
        Some
          (String.sub arg (String.length prefix)
             (String.length arg - String.length prefix))
      else None)
    args

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: ossa <file>";
    exit 1);

  let args = Array.to_list Sys.argv |> List.tl in
  let emit_tokens = List.mem "--emit-tokens" args in
  let emit_ast = List.mem "--emit-ast" args in
  let emit_sig = find_flag "--emit-sig=" args in
  let dump_sig = List.mem "--dump-sig" args in

  let filename = Sys.argv.(1) in

  let source = In_channel.with_open_text filename In_channel.input_all in

  let tokens, errors = Ossa.Lexer.tokenize source in

  List.iter
    (fun err ->
      Printf.eprintf "%s: %s\n" filename (Ossa.Lexer.error_to_string err))
    errors;

  if emit_tokens then
    List.iter
      (fun ({ tok; line; _ } : Ossa.Lexer.spanned_token) ->
        Printf.printf "%d: %s\n" line (Ossa.Lexer.token_to_string tok))
      tokens;

  if errors <> [] then exit 1;

  let decls, parse_errors = Ossa.Parser.parse tokens in

  if emit_ast || not emit_tokens then begin
    List.iter
      (fun err ->
        Printf.eprintf "%s: %s\n" filename (Ossa.Lexer.error_to_string err))
      parse_errors;

    if emit_ast then List.iter Ossa.Ast.print_decl decls;
    if parse_errors <> [] then exit 1
  end;

  match emit_sig with
  | Some path ->
      let sig_ = Ossa.Collect.get_mw_module decls in
      Ossa.Collect.emit_module_sig path sig_;
      if dump_sig then Ossa.Collect.dump_module sig_
  | None ->
      let st, resolved = Ossa.Resolver.resolve [] decls in
      let type_errors = Ossa.Analyzer.analyze st resolved in
      List.iter
        (fun msg -> Printf.eprintf "%s: error: %s\n" filename msg)
        type_errors;
      if type_errors <> [] then exit 1
