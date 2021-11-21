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
  | "char" -> Token.KChar
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
  else Token.Static, input
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
  | c :: tail -> if Char.is_whitespace c then next_token tail else lex_ident input
  | _ -> []
;;

let lex input =
  let input = String.strip input in
  String.to_list input
;;
