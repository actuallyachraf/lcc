(* A token in a C progrm *)
type token =
  | Ident
  | Return
  | Int
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

let token_of_string tok = match tok with
  | Ident -> "Ident"
  | _ -> "Not Implemented Yet"