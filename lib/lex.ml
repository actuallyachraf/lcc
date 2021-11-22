open Base

(*Integer regex*)
let int_regexp = Str.regexp_case_fold "\\(\\([0-9]+\\)\\|\\(0x[0-9a-f]+\\)\\)\\(\\b.*\\)"

(* C char regex*)
let char_regexp =
  Str.regexp
    "'\\([^'\\\\]\\|\\\\\\([abfenrtv'\"?\\\\]\\|[0-7]+\\|x[0-9a-fA-F]+\\)\\)'\\(.*\\)"
;;

(* C Identifier regex *)
let id_regexp = Str.regexp "\\([A-Za-z_][A-Za-z0-9_]*\\)\\(\\b.*\\)"

let is_digit c =
  let code = Char.to_int c in
  code >= Char.to_int '0' && code <= Char.to_int '9'
;;

let is_alpha c =
  let code = Char.to_int c in
  (code >= Char.to_int 'a' && code <= Char.to_int 'z')
  || (code >= Char.to_int 'A' && code <= Char.to_int 'Z')
;;

let get_identifier t =
  match t with
  | "return" -> Token.Return
  | "int" -> Token.KInt
  | "static" -> Token.Static
  | "extern" -> Token.Extern
  | "char" -> Token.KChar
  | "if" -> Token.If
  | "else" -> Token.Else
  | "for" -> Token.For
  | "do" -> Token.Do
  | "while" -> Token.While
  | "break" -> Token.Break
  | "continue" -> Token.Continue
  | _ -> Token.Ident t
;;

let lex_identifier input =
  if Str.string_match int_regexp input 0
  then (
    let int_token = Str.matched_group 1 input in
    let int_val = Int.of_string int_token in
    let rest = Str.matched_group 4 input in
    Token.Int int_val, rest)
  else if Str.string_match char_regexp input 0
  then (
    let _ = Str.string_match char_regexp input 0 in
    let char_token = Str.matched_group 1 input in
    let rest = Str.matched_group 3 input in
    Token.Char (String.get char_token 0), rest)
  else if Str.string_match id_regexp input 0
  then (
    (* it's an Identifier or possibly a keyword *)
    let ident = Str.matched_group 1 input in
    let rest = Str.matched_group 2 input in
    let id_token = get_identifier ident in
    id_token, rest)
  else failwith ("Syntax error: \"" ^ input ^ "\" is not valid.")
;;

let rec lex_ident tokens =
  let input = String.of_char_list tokens in
  let tok, rest = lex_identifier input in
  tok :: next_token (String.to_list rest)

and next_token input =
  match input with
  | '{' :: tail -> Token.LBrace :: next_token tail
  | '}' :: tail -> Token.RBrace :: next_token tail
  | '(' :: tail -> Token.LParen :: next_token tail
  | ')' :: tail -> Token.RParen :: next_token tail
  | ';' :: tail -> Token.Semicolon :: next_token tail
  | ',' :: tail -> Token.Comma :: next_token tail
  | '+' :: tail -> Token.Plus :: next_token tail
  | ':' :: tail -> Token.Colon :: next_token tail
  | '-' :: '-' :: _ -> failwith "decrement not yet implemented"
  | '<' :: '<' :: tail -> Token.Shl :: next_token tail
  | '>' :: '>' :: tail -> Token.Shr :: next_token tail
  | '!' :: '=' :: tail -> Token.Neq :: next_token tail
  | '<' :: '=' :: tail -> Token.Lte :: next_token tail
  | '>' :: '=' :: tail -> Token.Gte :: next_token tail
  | '<' :: tail -> Token.Lt :: next_token tail
  | '>' :: tail -> Token.Gt :: next_token tail
  | '-' :: tail -> Token.Minus :: next_token tail
  | '*' :: tail -> Token.Star :: next_token tail
  | '/' :: tail -> Token.Slash :: next_token tail
  | '&' :: '&' :: tail -> Token.And :: next_token tail
  | '&' :: tail -> Token.BitAnd :: next_token tail
  | '|' :: '|' :: tail -> Token.Or :: next_token tail
  | '|' :: tail -> Token.BitOr :: next_token tail
  | '^' :: tail -> Token.Xor :: next_token tail
  | '~' :: tail -> Token.Complement :: next_token tail
  | '!' :: tail -> Token.Bang :: next_token tail
  | '=' :: '=' :: tail -> Token.Equal :: next_token tail
  | '=' :: tail -> Token.Assign :: next_token tail
  | c :: tail -> if Char.is_whitespace c then next_token tail else lex_ident input
  | _ -> []
;;

let lex input =
  let input = String.strip input in
  next_token (String.to_list input)
;;
