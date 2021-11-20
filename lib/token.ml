(* A token in a C progrm *)
type token =
  | Ident of string
  | Return
  | Int of int
  | Char of char
  | Plus
  | Minus
  | Star
  | Slash
  | Semicolon
  | Comma
  | LParen
  | RParen
  | RBrace
  | LBrace

let token_of_string tok =
  match tok with
  | Ident id -> Printf.sprintf "IDENTIFIER<%s>" id
  | _ -> "Illegal"
;;
