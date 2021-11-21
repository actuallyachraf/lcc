open Base

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

let rec next_token input =
  match input with
  | '{' :: tail -> Token.LBrace :: next_token tail
  | '}' :: tail -> Token.RBrace :: next_token tail
  | '(' :: tail -> Token.LParen :: next_token tail
  | ')' :: tail -> Token.RParen :: next_token tail
  | ';' :: tail -> Token.Semicolon :: next_token tail
  | ',' :: tail -> Token.Comma :: next_token tail
  | _ -> []
;;

let lex input =
  let input = String.strip input in
  String.to_list input
;;
