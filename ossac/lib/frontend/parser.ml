open Lexer
open Ast
open Common

(* ── generic scopes ──────────────────────────────────────────────── *)

module StringSet = Set.Make (String)

exception Parse_abort

type parser_state = {
  tokens : spanned_token array;
  mutable pos : int;
  mutable errors : Lexer.error list;
  mutable generic_scopes : StringSet.t list;
}

(* ── helpers ─────────────────────────────────────────────────────── *)

let create tokens =
  { tokens = Array.of_list tokens; pos = 0; errors = []; generic_scopes = [] }

let current p =
  if p.pos < Array.length p.tokens then p.tokens.(p.pos)
  else p.tokens.(Array.length p.tokens - 1)

let current_tok p = (current p).tok
let current_line p = (current p).line

let error p msg : 'a =
  p.errors <- { msg; line = current_line p } :: p.errors;
  raise Parse_abort

let advance p =
  let t = current p in
  if p.pos < Array.length p.tokens then p.pos <- p.pos + 1;
  t

let eat p tok =
  if current_tok p = tok then (
    ignore (advance p);
    true)
  else false

let expect p tok =
  if current_tok p = tok then ignore (advance p)
  else
    error p
      (Printf.sprintf "expected %s, got %s" (token_to_string tok)
         (token_to_string (current_tok p)))

let expect_ident p =
  match current_tok p with
  | Identifier s ->
      ignore (advance p);
      s
  | _ ->
      error p
        (Printf.sprintf "expected identifier, got %s"
           (token_to_string (current_tok p)))

let push_scope p = p.generic_scopes <- StringSet.empty :: p.generic_scopes

let pop_scope p =
  match p.generic_scopes with _ :: rest -> p.generic_scopes <- rest | [] -> ()

let add_generic p name =
  match p.generic_scopes with
  | scope :: rest -> p.generic_scopes <- StringSet.add name scope :: rest
  | [] -> ()

let is_generic p name =
  List.exists (fun scope -> StringSet.mem name scope) p.generic_scopes

(* ── synchronization ─────────────────────────────────────────────── *)

let peek_tok p offset =
  let idx = p.pos + offset in
  if idx < Array.length p.tokens then p.tokens.(idx).tok else EOF

let rec synchronize_decl p =
  match current_tok p with
  | EOF -> ()
  | Struct | Enum -> ()
  | Identifier _ when peek_tok p 1 = DoubleColon -> ()
  | _ ->
      ignore (advance p);
      synchronize_decl p

(* ── types ───────────────────────────────────────────────────────── *)

let rec parse_type p =
  match current_tok p with
  | Star ->
      ignore (advance p);
      TPtr (parse_type p)
  | LParen -> (
      ignore (advance p);
      let tys = parse_comma_types p in
      expect p RParen;
      if eat p Arrow then
        let ret = parse_type p in
        TFunc (tys, ret)
      else match tys with [ t ] -> t | _ -> TTuple tys)
  | LBracket ->
      ignore (advance p);
      let t = parse_type p in
      expect p RBracket;
      TArray t
  | Identifier s ->
      ignore (advance p);
      let base = if is_generic p s then TGeneric s else resolve_named_type s in
      if current_tok p = Less then (
        ignore (advance p);
        let args = parse_comma_types p in
        expect p Greater;
        TApp (base, args))
      else base
  | _ ->
      error p
        (Printf.sprintf "expected type, got %s"
           (token_to_string (current_tok p)))

and resolve_named_type s =
  match s with
  | "u8" -> TInt U8
  | "u16" -> TInt U16
  | "u32" -> TInt U32
  | "u64" -> TInt U64
  | "i8" -> TInt I8
  | "i16" -> TInt I16
  | "i32" -> TInt I32
  | "i64" -> TInt I64
  | "int" -> TInt I32
  | "f32" -> TFloat F32
  | "f64" -> TFloat F64
  | "float" -> TFloat F32
  | "string" -> TString
  | "bool" -> TBool
  | "void" -> TVoid
  | _ -> TNamed [ s ]

and parse_comma_types p =
  if current_tok p = RParen || current_tok p = Greater then []
  else
    let first = parse_type p in
    let rec go acc =
      if eat p Comma then go (parse_type p :: acc) else List.rev acc
    in
    go [ first ]

(* ── patterns ────────────────────────────────────────────────────── *)

let rec parse_pattern p =
  match current_tok p with
  | Identifier "_" ->
      ignore (advance p);
      PWildcard
  | Identifier s ->
      ignore (advance p);
      if current_tok p = LParen then (
        ignore (advance p);
        let pats = parse_comma_patterns p in
        expect p RParen;
        PVariant ([ s ], pats))
      else PVar s
  | LParen ->
      ignore (advance p);
      let pats = parse_comma_patterns p in
      expect p RParen;
      PTuple pats
  | IntLiteral s ->
      ignore (advance p);
      PLiteral (LInt s)
  | FloatLiteral s ->
      ignore (advance p);
      PLiteral (LFloat s)
  | StringLiteral s ->
      ignore (advance p);
      PLiteral (LString s)
  | CharLiteral c ->
      ignore (advance p);
      PLiteral (LChar c)
  | _ ->
      error p
        (Printf.sprintf "expected pattern, got %s"
           (token_to_string (current_tok p)))

and parse_comma_patterns p =
  if current_tok p = RParen then []
  else
    let first = parse_pattern p in
    let rec go acc =
      if eat p Comma then go (parse_pattern p :: acc) else List.rev acc
    in
    go [ first ]

let prefix_binding_power = function
  | Minus -> 17
  | Bang -> 17
  | Star -> 17
  | Ampersand -> 17
  | _ -> -1

let infix_binding_power = function
  | Equal | PlusEqual | MinusEqual | StarEqual | SlashEqual -> (1, 0)
  | DoublePipe -> (2, 3)
  | DoubleAmpersand -> (4, 5)
  | DoubleEqual | NotEqual -> (6, 7)
  | Less | LessEqual | Greater | GreaterEqual -> (8, 9)
  | Range -> (10, 11)
  | Plus | Minus -> (12, 13)
  | Star | Slash | Percent -> (14, 15)
  | _ -> (-1, -1)

let postfix_binding_power = function
  | LParen -> Some 19
  | LBracket -> Some 19
  | Dot -> Some 19
  | Question -> Some 19
  | _ -> None

let token_to_unary_op = function
  | Minus -> Neg
  | Bang -> Not
  | Star -> Deref
  | Ampersand -> AddrOf
  | _ -> failwith "not a unary op"

let token_to_binary_op = function
  | Plus -> Add
  | Minus -> Sub
  | Star -> Mul
  | Slash -> Div
  | Percent -> Mod
  | DoubleEqual -> Eq
  | NotEqual -> Ne
  | Less -> Lt
  | LessEqual -> Le
  | Greater -> Gt
  | GreaterEqual -> Ge
  | DoubleAmpersand -> And
  | DoublePipe -> Or
  | Equal -> Assign
  | PlusEqual -> AddAssign
  | MinusEqual -> SubAssign
  | StarEqual -> MulAssign
  | SlashEqual -> DivAssign
  | Range -> Range
  | _ -> failwith "not a binary op"

let is_assign_tok = function
  | Equal | PlusEqual | MinusEqual | StarEqual | SlashEqual -> true
  | _ -> false

let rec parse_expr p = parse_expr_bp p 0

and parse_expr_bp p min_bp =
  let lhs = ref (parse_prefix p) in
  let continue_ = ref true in
  while !continue_ do
    let tok = current_tok p in
    match postfix_binding_power tok with
    | Some bp when bp >= min_bp -> begin
        match tok with
        | LParen ->
            ignore (advance p);
            let args = parse_call_args p in
            expect p RParen;
            lhs := Call (!lhs, args)
        | LBracket ->
            ignore (advance p);
            let idx = parse_expr p in
            expect p RBracket;
            lhs := Index (!lhs, idx)
        | Dot ->
            ignore (advance p);
            let field = expect_ident p in
            lhs := Field (!lhs, field)
        | Question ->
            ignore (advance p);
            let then_e = parse_expr p in
            expect p Colon;
            let else_e = parse_expr_bp p 0 in
            lhs :=
              If
                ( !lhs,
                  { stmts = []; tail = Some then_e },
                  { stmts = []; tail = Some else_e } )
        | _ -> continue_ := false
      end
    | _ ->
        let lbp, rbp = infix_binding_power tok in
        if lbp >= min_bp then (
          let rbp = if is_assign_tok tok then lbp else rbp in
          ignore (advance p);
          let rhs = parse_expr_bp p rbp in
          lhs := Binary (token_to_binary_op tok, !lhs, rhs))
        else continue_ := false
  done;
  if current_tok p = FatArrow then (
    ignore (advance p);
    let closure = parse_closure_or_paren_closure p in
    lhs := Iterate (!lhs, closure));
  !lhs

and parse_prefix p =
  let tok = current_tok p in
  let rbp = prefix_binding_power tok in
  if rbp > 0 then (
    ignore (advance p);
    let e = parse_expr_bp p rbp in
    Unary (token_to_unary_op tok, e))
  else parse_atom p

and parse_atom p =
  match current_tok p with
  | IntLiteral s ->
      ignore (advance p);
      Literal (LInt s)
  | FloatLiteral s ->
      ignore (advance p);
      Literal (LFloat s)
  | StringLiteral s ->
      ignore (advance p);
      Literal (LString s)
  | CharLiteral c ->
      ignore (advance p);
      Literal (LChar c)
  | Identifier "true" ->
      ignore (advance p);
      Literal (LBool true)
  | Identifier "false" ->
      ignore (advance p);
      Literal (LBool false)
  | Identifier _ -> parse_path_or_struct_init p
  | New -> parse_new p
  | If -> parse_if_expr p
  | Match -> parse_match_expr p
  | Pipe -> parse_pipe_closure p
  | LParen -> parse_paren_or_tuple p
  | LBracket -> parse_array_literal p
  | LBrace -> parse_block_expr p
  | _ ->
      error p
        (Printf.sprintf "expected expression, got %s"
           (token_to_string (current_tok p)))

(* ── path / struct init ──────────────────────────────────────────── *)

and parse_path_or_struct_init p =
  let path = parse_path p in
  match current_tok p with
  | LBrace ->
      let saved = p.pos in
      ignore (advance p);
      let is_struct_init =
        match current_tok p with
        | Identifier _ ->
            let saved2 = p.pos in
            ignore (advance p);
            let r = current_tok p = Equal in
            p.pos <- saved2;
            r
        | RBrace -> true
        | _ -> false
      in
      if is_struct_init then (
        let fields = parse_init_fields p in
        expect p RBrace;
        StructInit (path, fields))
      else (
        p.pos <- saved;
        Path path)
  | _ -> Path path

and parse_path p =
  let first = expect_ident p in
  let rec go acc =
    if current_tok p = DoubleColon then (
      ignore (advance p);
      let next = expect_ident p in
      go (next :: acc))
    else List.rev acc
  in
  go [ first ]

and parse_init_fields p =
  let rec go acc =
    match current_tok p with
    | RBrace -> List.rev acc
    | _ ->
        let name = expect_ident p in
        expect p Equal;
        let value = parse_expr p in
        ignore (eat p Comma);
        go ({ init_name = name; init_value = value } :: acc)
  in
  go []

(* ── new ─────────────────────────────────────────────────────────── *)

and parse_new p =
  expect p New;
  let path = parse_path p in
  expect p LBrace;
  let fields = parse_init_fields p in
  expect p RBrace;
  Unary (AddrOf, StructInit (path, fields))

(* ── if ──────────────────────────────────────────────────────────── *)

and parse_if_expr p =
  expect p If;
  let cond = parse_expr p in
  let then_block = parse_block p in
  let else_block =
    if eat p Else then
      if current_tok p = If then
        let e = parse_if_expr p in
        { stmts = []; tail = Some e }
      else parse_block p
    else { stmts = []; tail = None }
  in
  If (cond, then_block, else_block)

(* ── match ───────────────────────────────────────────────────────── *)

and parse_match_expr p =
  expect p Match;
  let scrutinee = parse_expr p in
  expect p LBrace;
  let arms = parse_match_arms p in
  expect p RBrace;
  Match (scrutinee, arms)

and parse_match_arms p =
  let rec go acc =
    match current_tok p with
    | RBrace | EOF -> List.rev acc
    | _ ->
        let pat = parse_pattern p in
        expect p FatArrow;
        let body = parse_block p in
        ignore (eat p Semicolon);
        go ({ arm_pat = pat; arm_body = body } :: acc)
  in
  go []

(* ── closures ────────────────────────────────────────────────────── *)

and parse_pipe_closure p =
  expect p Pipe;
  let params = parse_closure_params_until_pipe p in
  expect p Pipe;
  let body = parse_block p in
  Closure (params, body)

and parse_paren_closure p =
  expect p LParen;
  let params = parse_param_list p in
  expect p RParen;
  let body = parse_block p in
  Closure (params, body)

and parse_closure_or_paren_closure p =
  match current_tok p with
  | Pipe -> parse_pipe_closure p
  | LParen -> parse_paren_closure p
  | _ -> error p "expected closure after =>"

and parse_closure_params_until_pipe p =
  let rec go acc =
    match current_tok p with
    | Pipe -> List.rev acc
    | _ ->
        let mode = if eat p Ref then Ref else Value in
        let name = expect_ident p in
        let typ = if eat p Colon then Some (parse_type p) else None in
        ignore (eat p Comma);
        go ({ name; typ; mode } :: acc)
  in
  go []

(* ── paren / tuple ───────────────────────────────────────────────── *)

and parse_paren_or_tuple p =
  expect p LParen;
  if current_tok p = RParen then (
    ignore (advance p);
    Tuple [])
  else
    let first = parse_expr p in
    if eat p Comma then (
      let rest = parse_comma_exprs p in
      expect p RParen;
      Tuple (first :: rest))
    else (
      expect p RParen;
      first)

and parse_comma_exprs p =
  let rec go acc =
    match current_tok p with
    | RParen | EOF -> List.rev acc
    | _ ->
        let e = parse_expr p in
        ignore (eat p Comma);
        go (e :: acc)
  in
  go []

(* ── array ───────────────────────────────────────────────────────── *)

and parse_array_literal p =
  expect p LBracket;
  let rec go acc =
    match current_tok p with
    | RBracket | EOF -> List.rev acc
    | _ ->
        let e = parse_expr p in
        ignore (eat p Comma);
        go (e :: acc)
  in
  let elems = go [] in
  expect p RBracket;
  Array elems

(* ── block as expr ───────────────────────────────────────────────── *)

and parse_block_expr p =
  let b = parse_block p in
  match b.tail with
  | Some e when b.stmts = [] -> e
  | _ -> If (Literal (LBool true), b, { stmts = []; tail = None })

(* ── call args ───────────────────────────────────────────────────── *)

and parse_call_args p =
  let rec go acc =
    match current_tok p with
    | RParen | EOF -> List.rev acc
    | _ ->
        let e = parse_expr p in
        ignore (eat p Comma);
        go (e :: acc)
  in
  go []

(* ── blocks & statements ─────────────────────────────────────────── *)

and parse_block p =
  expect p LBrace;
  let stmts, tail = parse_stmts p in
  expect p RBrace;
  { stmts; tail }

and parse_stmts p =
  let rec go acc =
    match current_tok p with
    | RBrace | EOF -> (List.rev acc, None)
    | _ -> (
        let s = parse_stmt p in
        match s with
        | Expr e when current_tok p = RBrace -> (List.rev acc, Some e)
        | _ ->
            expect p Semicolon;
            go (s :: acc))
  in
  go []

and parse_stmt p =
  match current_tok p with
  | Let -> parse_let p
  | Return -> parse_return p
  | Loop -> parse_loop p
  | _ -> Expr (parse_expr p)

and parse_let p =
  expect p Let;
  let pat = parse_pattern p in
  let typ = if eat p Colon then Some (parse_type p) else None in
  let init = if eat p Equal then Some (parse_expr p) else None in
  Let (pat, typ, init)

and parse_return p =
  expect p Return;
  if current_tok p = Semicolon || current_tok p = RBrace then Return None
  else Return (Some (parse_expr p))

and parse_loop p =
  expect p Loop;
  Loop (parse_block p)

(* ── parameters ──────────────────────────────────────────────────── *)

and parse_param_list p =
  let rec go acc =
    match current_tok p with
    | RParen | EOF -> List.rev acc
    | _ ->
        let mode = if eat p Ref then Ref else Value in
        let name = expect_ident p in
        let typ = if eat p Colon then Some (parse_type p) else None in
        ignore (eat p Comma);
        go ({ name; typ; mode } :: acc)
  in
  go []

(* ── generics ────────────────────────────────────────────────────── *)

let parse_generics p =
  if eat p With then (
    push_scope p;
    let rec go acc =
      match current_tok p with
      | DoubleColon | LBrace | LParen | EOF -> List.rev acc
      | _ ->
          let name = expect_ident p in
          add_generic p name;
          ignore (eat p Comma);
          go (name :: acc)
    in
    go [])
  else []

let parse_func_decl p name =
  let generics = parse_generics p in
  expect p DoubleColon;
  expect p LParen;
  let params = parse_param_list p in
  expect p RParen;
  let ret = if eat p Arrow then parse_type p else TVoid in
  let body = parse_block p in
  if generics <> [] then pop_scope p;
  FuncDec
    {
      fname = name;
      fgenerics = generics;
      fparams = params;
      fret = ret;
      fbody = body;
    }

let parse_struct_decl p =
  expect p Struct;
  let name = expect_ident p in
  let generics = parse_generics p in
  expect p LBrace;
  let rec go fields funcs =
    match current_tok p with
    | RBrace | EOF -> (List.rev fields, List.rev funcs)
    | Identifier _
      when let saved = p.pos in
           ignore (advance p);
           let is_func = current_tok p = DoubleColon in
           p.pos <- saved;
           is_func ->
        let fname = expect_ident p in
        let decl = parse_func_decl p fname in
        begin match decl with
        | FuncDec fd -> go fields (fd :: funcs)
        | _ -> go fields funcs
        end
    | _ ->
        let field_name = expect_ident p in
        expect p Colon;
        let field_typ = parse_type p in
        ignore (eat p Comma);
        go ({ field_name; field_typ } :: fields) funcs
  in
  let fields, funcs = go [] [] in
  expect p RBrace;
  if generics <> [] then pop_scope p;
  StructDec
    { sname = name; sgenerics = generics; sfields = fields; sfuncs = funcs }

let parse_enum_decl p =
  expect p Enum;
  let name = expect_ident p in
  let generics = parse_generics p in
  expect p LBrace;

  let rec parse_tuple_fields acc =
    match current_tok p with
    | RParen | EOF -> List.rev acc
    | _ ->
        let ft = parse_type p in
        ignore (eat p Comma);
        parse_tuple_fields ({ field_name = None; field_type = ft } :: acc)
  in

  let rec parse_named_fields acc =
    match current_tok p with
    | RBrace | EOF -> List.rev acc
    | _ ->
        let fname = expect_ident p in
        expect p Colon;
        let ftyp = parse_type p in
        ignore (eat p Comma);
        parse_named_fields
          ({ field_name = Some fname; field_type = ftyp } :: acc)
  in

  let parse_variant_fields p =
    if eat p LParen then (
      let fs = parse_tuple_fields [] in
      expect p RParen;
      fs)
    else if eat p LBrace then (
      let fs = parse_named_fields [] in
      expect p RBrace;
      fs)
    else
      match current_tok p with
      | Comma | RBrace -> []
      | _ -> error p "expected '(', '{', ',' or '}' after enum variant name"
  in

  let rec go acc =
    match current_tok p with
    | RBrace -> List.rev acc
    | EOF -> List.rev acc
    | _ ->
        let vname = expect_ident p in
        let fields = parse_variant_fields p in
        ignore (eat p Comma);
        go ({ variant_name = vname; fields } :: acc)
  in

  let variants = go [] in
  expect p RBrace;
  if generics <> [] then pop_scope p;
  EnumDec { ename = name; egenerics = generics; evariants = variants }

(* ── program ─────────────────────────────────────────────────────── *)

let rec parse_namespace_decl p =
  expect p Namespace;
  let path = parse_path p in
  expect p LBrace;
  let decls = parse_program p in
  expect p RBrace;
  NamespaceDec { npath = path; ndecls = decls }

and parse_decl p =
  try
    match current_tok p with
    | Struct -> parse_struct_decl p
    | Enum -> parse_enum_decl p
    | Namespace -> parse_namespace_decl p
    | Identifier name ->
        ignore (advance p);
        parse_func_decl p name
    | _ ->
        error p
          (Printf.sprintf "expected declaration, got %s"
             (token_to_string (current_tok p)))
  with Parse_abort ->
    synchronize_decl p;
    FuncDec
      {
        fname = "_";
        fgenerics = [];
        fparams = [];
        fret = TVoid;
        fbody = { stmts = []; tail = None };
      }

and parse_program p =
  let rec go acc =
    match current_tok p with
    | EOF | RBrace -> List.rev acc
    | _ -> go (parse_decl p :: acc)
  in
  go []

let parse tokens =
  let p = create tokens in
  let decls = parse_program p in
  (decls, List.rev p.errors)
