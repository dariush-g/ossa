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
  | Interface
  | Ref
  | New
  | Return
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
  | Arrow         (* -> *)
  | FatArrow      (* => *)
  | Range         (* .. *)
  | Colon
  | DoubleColon   (* :: *)
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

type span = {
  start : int;
  finish : int;
}

type spanned_token = {
  tok : token;
  span : span;
}

type lexer = {
  source : string;
  mutable pos : int;
  length : int;
}

let create source =
  { source; pos = 0; length = String.length source }

let peek l =
  if l.pos >= l.length then None
  else Some l.source.[l.pos]

let peek_next l =
  if l.pos + 1 >= l.length then None
  else Some l.source.[l.pos + 1]

let advance l =
  let c = l.source.[l.pos] in
  l.pos <- l.pos + 1;
  c

let match_char l expected =
  match peek l with
  | Some c when c = expected -> l.pos <- l.pos + 1; true
  | _ -> false

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c = is_digit c || is_alpha c

let skip_whitespace_and_comments l =
  let rec go () =
    match peek l with
    | Some (' ' | '\t' | '\n' | '\r') ->
      ignore (advance l); go ()
    | Some '/' -> begin
      match peek_next l with
      | Some '/' ->
        (* line comment *)
        ignore (advance l); ignore (advance l);
        let rec skip_line () =
          match peek l with
          | None | Some '\n' -> ()
          | _ -> ignore (advance l); skip_line ()
        in
        skip_line (); go ()
      | Some '*' ->
        (* block comment *)
        ignore (advance l); ignore (advance l);
        let rec skip_block () =
          match peek l with
          | None -> ()
          | Some '*' ->
            ignore (advance l);
            (match peek l with
             | Some '/' -> ignore (advance l)
             | _ -> skip_block ())
          | _ -> ignore (advance l); skip_block ()
        in
        skip_block (); go ()
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
  | "interface" -> Interface
  | "ref" -> Ref
  | "new" -> New
  | "return" -> Return
  | _ -> Identifier s

let lex_number l =
  let start = l.pos in
  while (match peek l with Some c -> is_digit c | None -> false) do
    ignore (advance l)
  done;
  let is_float =
    match peek l with
    | Some '.' -> begin
      match peek_next l with
      | Some c when is_digit c ->
        ignore (advance l);
        while (match peek l with Some c -> is_digit c | None -> false) do
          ignore (advance l)
        done;
        true
      | Some '.' ->
        (* this is a range like 0..10, not a float *)
        false
      | _ -> false
    end
    | _ -> false
  in
  let text = String.sub l.source start (l.pos - start) in
  if is_float then FloatLiteral text else IntLiteral text

let lex_string l =
  (* opening quote already consumed *)
  let buf = Buffer.create 64 in
  let rec go () =
    match peek l with
    | None -> failwith "unterminated string literal"
    | Some '"' -> ignore (advance l)
    | Some '\\' ->
      ignore (advance l);
      let c = match peek l with
        | None -> failwith "unterminated escape in string"
        | Some 'n' -> '\n'
        | Some 't' -> '\t'
        | Some 'r' -> '\r'
        | Some '\\' -> '\\'
        | Some '"' -> '"'
        | Some '0' -> '\000'
        | Some c -> c
      in
      ignore (advance l);
      Buffer.add_char buf c;
      go ()
    | Some c ->
      ignore (advance l);
      Buffer.add_char buf c;
      go ()
  in
  go ();
  StringLiteral (Buffer.contents buf)

let lex_char l =
  (* opening quote already consumed *)
  let c = match peek l with
    | None -> failwith "unterminated char literal"
    | Some '\\' ->
      ignore (advance l);
      (match peek l with
       | None -> failwith "unterminated escape in char"
       | Some 'n' -> ignore (advance l); '\n'
       | Some 't' -> ignore (advance l); '\t'
       | Some 'r' -> ignore (advance l); '\r'
       | Some '\\' -> ignore (advance l); '\\'
       | Some '\'' -> ignore (advance l); '\''
       | Some '0' -> ignore (advance l); '\000'
       | Some c -> ignore (advance l); c)
    | Some c -> ignore (advance l); c
  in
  (match peek l with
   | Some '\'' -> ignore (advance l)
   | _ -> failwith "unterminated char literal");
  CharLiteral c

let lex_identifier l =
  let start = l.pos in
  while (match peek l with Some c -> is_alnum c | None -> false) do
    ignore (advance l)
  done;
  let text = String.sub l.source start (l.pos - start) in
  keyword_or_ident text

let lex_token l =
  let start = l.pos in
  let tok = match advance l with
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
    | c -> failwith (Printf.sprintf "unexpected character: '%c' at position %d" c l.pos)
  in
  { tok; span = { start; finish = l.pos } }

let lex_all l =
  let rec go acc =
    skip_whitespace_and_comments l;
    if l.pos >= l.length then
      List.rev ({ tok = EOF; span = { start = l.pos; finish = l.pos } } :: acc)
    else
      let tok = lex_token l in
      go (tok :: acc)
  in
  go []

let tokenize source =
  let l = create source in
  lex_all l

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
  | Interface -> "INTERFACE"
  | Ref -> "REF"
  | New -> "NEW"
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
