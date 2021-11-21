(* A token in a C progrm *)
type token =
  | Ident of string (* Identifiers*)
  | Return (* Return keyword*)
  | Int of int (* C int type*)
  | Char of char (* C char type*)
  | Assign
  | Plus
  | Minus
  | Star
  | Slash
  | Semicolon
  | Comma
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Static

let token_of_string tok =
  match tok with
  | Ident id -> Printf.sprintf "IDENTIFIER<%s>" id
  | Return -> "RETURN"
  | Int t -> Printf.sprintf "INTEGER<%d>" t
  | Char t -> Printf.sprintf "CHAR<%c>" t
  | Assign -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Semicolon -> ";"
  | Comma -> ","
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | _ -> "ILLEGAL"
;;
