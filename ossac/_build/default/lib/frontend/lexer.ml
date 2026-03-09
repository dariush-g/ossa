type token =
  | IntLiteral of string
  | FloatLiteral of string
  | StringLiteral of string
  | CharLiteral of char
  | Identifier of string
  (* keywords *)
  | Let
  | If
  | Else
  | Match
  | Loop
  | Struct
  | Enum
  | Namespace
  | Ref
  | New
  | Return
  | With
  (* operators *)
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Equal
  | DoubleEqual
  | NotEqual
  | Bang
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | PlusEqual
  | MinusEqual
  | StarEqual
  | SlashEqual
  | Ampersand
  | DoubleAmpersand
  | Pipe
  | DoublePipe
  | Question
  (* punctuation *)
  | Arrow (* -> *)
  | FatArrow (* => *)
  | Range (* .. *)
  | Colon
  | DoubleColon (* :: *)
  | Comma
  | Dot
  | Semicolon
  (* delimiters *)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | EOF

type spanned_token = {
  tok : token;
  span : int * int; (* start_pos, end_pos *)
  line : int;
}

type error = { msg : string; line : int }

type lexer = {
  source : string;
  mutable pos : int;
  mutable line : int;
  length : int;
  mutable errors : error list;
}

let create source =
  { source; pos = 0; line = 1; length = String.length source; errors = [] }

let add_error l msg = l.errors <- { msg; line = l.line } :: l.errors
let peek l = if l.pos >= l.length then None else Some l.source.[l.pos]

let peek_next l =
  if l.pos + 1 >= l.length then None else Some l.source.[l.pos + 1]

let advance l =
  let c = l.source.[l.pos] in
  l.pos <- l.pos + 1;
  if c = '\n' then l.line <- l.line + 1;
  c

let match_char l expected =
  match peek l with
  | Some c when c = expected ->
      ignore (advance l);
      true
  | _ -> false

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_digit c || is_alpha c

let skip_whitespace_and_comments l =
  let rec go () =
    match peek l with
    | Some (' ' | '\t' | '\n' | '\r') ->
        ignore (advance l);
        go ()
    | Some '/' -> begin
        match peek_next l with
        | Some '/' ->
            ignore (advance l);
            ignore (advance l);
            let rec skip_line () =
              match peek l with
              | None | Some '\n' -> ()
              | _ ->
                  ignore (advance l);
                  skip_line ()
            in
            skip_line ();
            go ()
        | Some '*' ->
            let start_line = l.line in
            ignore (advance l);
            ignore (advance l);
            let rec skip_block () =
              match peek l with
              | None ->
                  l.errors <-
                    { msg = "unterminated block comment"; line = start_line }
                    :: l.errors
              | Some '*' -> (
                  ignore (advance l);
                  match peek l with
                  | Some '/' -> ignore (advance l)
                  | _ -> skip_block ())
              | _ ->
                  ignore (advance l);
                  skip_block ()
            in
            skip_block ();
            go ()
        | _ -> ()
      end
    | _ -> ()
  in
  go ()

let keyword_or_ident s =
  match s with
  | "let" -> Let
  | "if" -> If
  | "else" -> Else
  | "match" -> Match
  | "loop" -> Loop
  | "struct" -> Struct
  | "enum" -> Enum
  | "namespace" -> Namespace
  | "ref" -> Ref
  | "new" -> New
  | "return" -> Return
  | "with" -> With
  | _ -> Identifier s

let is_hex_digit c =
  is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let lex_number l =
  let start = l.pos in
  let is_hex = ref false in
  let is_bin = ref false in

  (match peek l with
  | Some '0' -> begin
      match peek_next l with
      | Some ('x' | 'X') ->
          is_hex := true;
          ignore (advance l);
          ignore (advance l);
          while
            match peek l with
            | Some c -> is_hex_digit c || c = '_'
            | None -> false
          do
            ignore (advance l)
          done
      | Some ('b' | 'B') ->
          is_bin := true;
          ignore (advance l);
          ignore (advance l);
          while
            match peek l with Some ('0' | '1' | '_') -> true | _ -> false
          do
            ignore (advance l)
          done
      | _ ->
          while
            match peek l with Some c -> is_digit c || c = '_' | None -> false
          do
            ignore (advance l)
          done
    end
  | _ ->
      while
        match peek l with Some c -> is_digit c || c = '_' | None -> false
      do
        ignore (advance l)
      done);

  let is_float =
    if !is_hex || !is_bin then false
    else
      match peek l with
      | Some '.' -> begin
          match peek_next l with
          | Some c when is_digit c ->
              ignore (advance l);
              while
                match peek l with
                | Some c -> is_digit c || c = '_'
                | None -> false
              do
                ignore (advance l)
              done;
              true
          | _ -> false
        end
      | _ -> false
  in

  (* Consume type suffix into the same string *)
  (match peek l with
  | Some ('f' | 'F' | 'u' | 'U' | 'i' | 'I') ->
      ignore (advance l);
      while match peek l with Some c -> is_digit c | None -> false do
        ignore (advance l)
      done
  | _ -> ());

  let text = String.sub l.source start (l.pos - start) in
  if is_float then FloatLiteral text else IntLiteral text

let lex_string l =
  let start_line = l.line in
  let buf = Buffer.create 64 in
  let rec go () =
    match peek l with
    | None ->
        l.errors <-
          { msg = "unterminated string literal"; line = start_line } :: l.errors;
        StringLiteral (Buffer.contents buf)
    | Some '"' ->
        ignore (advance l);
        StringLiteral (Buffer.contents buf)
    | Some '\\' -> (
        ignore (advance l);
        match peek l with
        | None ->
            l.errors <-
              { msg = "unterminated escape sequence in string"; line = l.line }
              :: l.errors;
            StringLiteral (Buffer.contents buf)
        | Some 'n' ->
            ignore (advance l);
            Buffer.add_char buf '\n';
            go ()
        | Some 't' ->
            ignore (advance l);
            Buffer.add_char buf '\t';
            go ()
        | Some 'r' ->
            ignore (advance l);
            Buffer.add_char buf '\r';
            go ()
        | Some '\\' ->
            ignore (advance l);
            Buffer.add_char buf '\\';
            go ()
        | Some '"' ->
            ignore (advance l);
            Buffer.add_char buf '"';
            go ()
        | Some '0' ->
            ignore (advance l);
            Buffer.add_char buf '\000';
            go ()
        | Some c ->
            add_error l (Printf.sprintf "unknown escape sequence '\\%c'" c);
            ignore (advance l);
            Buffer.add_char buf c;
            go ())
    | Some c ->
        ignore (advance l);
        Buffer.add_char buf c;
        go ()
  in
  go ()

let lex_char l =
  match peek l with
  | None ->
      add_error l "unterminated character literal";
      CharLiteral '\000'
  | Some '\\' ->
      ignore (advance l);
      let c =
        match peek l with
        | None ->
            add_error l "unterminated escape sequence in character literal";
            '\000'
        | Some 'n' ->
            ignore (advance l);
            '\n'
        | Some 't' ->
            ignore (advance l);
            '\t'
        | Some 'r' ->
            ignore (advance l);
            '\r'
        | Some '\\' ->
            ignore (advance l);
            '\\'
        | Some '\'' ->
            ignore (advance l);
            '\''
        | Some '0' ->
            ignore (advance l);
            '\000'
        | Some c ->
            add_error l (Printf.sprintf "unknown escape sequence '\\%c'" c);
            ignore (advance l);
            c
      in
      (match peek l with
      | Some '\'' -> ignore (advance l)
      | _ -> add_error l "unterminated character literal");
      CharLiteral c
  | Some '\'' ->
      add_error l "empty character literal";
      ignore (advance l);
      CharLiteral '\000'
  | Some c ->
      ignore (advance l);
      (match peek l with
      | Some '\'' -> ignore (advance l)
      | _ -> add_error l "unterminated character literal");
      CharLiteral c

let lex_identifier l =
  let start = l.pos in
  while match peek l with Some c -> is_alnum c | None -> false do
    ignore (advance l)
  done;
  let text = String.sub l.source start (l.pos - start) in
  keyword_or_ident text

let rec lex_token l =
  let start_pos = l.pos in
  let start_line = l.line in
  let tok =
    match advance l with
    | '(' -> LParen
    | ')' -> RParen
    | '{' -> LBrace
    | '}' -> RBrace
    | '[' -> LBracket
    | ']' -> RBracket
    | ',' -> Comma
    | ';' -> Semicolon
    | '?' -> Question
    | '+' -> if match_char l '=' then PlusEqual else Plus
    | '*' -> if match_char l '=' then StarEqual else Star
    | '%' -> Percent
    | '!' -> if match_char l '=' then NotEqual else Bang
    | '&' -> if match_char l '&' then DoubleAmpersand else Ampersand
    | '|' -> if match_char l '|' then DoublePipe else Pipe
    | '-' ->
        if match_char l '>' then Arrow
        else if match_char l '=' then MinusEqual
        else Minus
    | '=' ->
        if match_char l '>' then FatArrow
        else if match_char l '=' then DoubleEqual
        else Equal
    | ':' -> if match_char l ':' then DoubleColon else Colon
    | '.' -> if match_char l '.' then Range else Dot
    | '<' -> if match_char l '=' then LessEqual else Less
    | '>' -> if match_char l '=' then GreaterEqual else Greater
    | '/' -> if match_char l '=' then SlashEqual else Slash
    | '"' -> lex_string l
    | '\'' -> lex_char l
    | c when is_digit c ->
        l.pos <- l.pos - 1;
        lex_number l
    | c when is_alpha c ->
        l.pos <- l.pos - 1;
        lex_identifier l
    | c ->
        add_error l (Printf.sprintf "unexpected character '%c'" c);
        (* skip the bad character and try the next token *)
        lex_token l |> fun st -> st.tok
  in
  { tok; span = (start_pos, l.pos); line = start_line }

let lex_all l =
  let rec go acc =
    skip_whitespace_and_comments l;
    if l.pos >= l.length then
      List.rev ({ tok = EOF; span = (l.pos, l.pos); line = l.line } :: acc)
    else
      let tok = lex_token l in
      go (tok :: acc)
  in
  go []

let tokenize source =
  let l = create source in
  let tokens = lex_all l in
  (tokens, List.rev l.errors)

let token_to_string = function
  | IntLiteral s -> Printf.sprintf "INT(%s)" s
  | FloatLiteral s -> Printf.sprintf "FLOAT(%s)" s
  | StringLiteral s -> Printf.sprintf "STRING(%S)" s
  | CharLiteral c -> Printf.sprintf "CHAR(%C)" c
  | Identifier s -> Printf.sprintf "IDENT(%s)" s
  | Let -> "LET"
  | If -> "IF"
  | Else -> "ELSE"
  | Match -> "MATCH"
  | Loop -> "LOOP"
  | Struct -> "STRUCT"
  | Enum -> "ENUM"
  | Namespace -> "NAMESPACE"
  | Ref -> "REF"
  | New -> "NEW"
  | With -> "WITH"
  | Return -> "RETURN"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Percent -> "%"
  | Equal -> "="
  | DoubleEqual -> "=="
  | NotEqual -> "!="
  | Bang -> "!"
  | Less -> "<"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | PlusEqual -> "+="
  | MinusEqual -> "-="
  | StarEqual -> "*="
  | SlashEqual -> "/="
  | Ampersand -> "&"
  | DoubleAmpersand -> "&&"
  | Pipe -> "|"
  | DoublePipe -> "||"
  | Question -> "?"
  | Arrow -> "->"
  | FatArrow -> "=>"
  | Range -> ".."
  | Colon -> ":"
  | DoubleColon -> "::"
  | Comma -> ","
  | Dot -> "."
  | Semicolon -> ";"
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBracket -> "["
  | RBracket -> "]"
  | EOF -> "EOF"

let span_to_string (s, e) = Printf.sprintf "%d..%d" s e

let error_to_string (err : error) =
  Printf.sprintf "line %d: error: %s" err.line err.msg
