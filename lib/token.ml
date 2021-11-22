(* A token in a C progrm *)
type token =
  | Return (* Return keyword*)
  | Void (* void keyword *)
  | KInt (* int keyword *)
  | KChar (* char keyword *)
  | If (* if *)
  | Else (* else *)
  | For (* for *)
  | While (* while *)
  | Do (* do *)
  | Break (* break *)
  | Continue (* continue *)
  | Ident of string (* Identifiers*)
  | Int of int (* C int type*)
  | Char of char (* C char type*)
  | Assign (* = *)
  | Equal (* == *)
  | Neq (* != *)
  | Gt (* > *)
  | Lt (* < *)
  | Gte (* >= *)
  | Lte (* <= *)
  | And (* && *)
  | Or (* || *)
  | BitAnd (* Bitwise And: &*)
  | BitOr (* Bitwise Or : |*)
  | Xor (* ^ *)
  | Shl (* Shiftleft << *)
  | Shr (* ShiftRight >> *)
  | Bang (* ! *)
  | Complement (* ~ *)
  | Plus (* + *)
  | Minus (* - *)
  | Star (* * *)
  | Slash (* / *)
  | Semicolon (* ; *)
  | Colon (* : *)
  | Comma (* , *)
  | LParen (* ( *)
  | RParen (* ) *)
  | LBrace (* { *)
  | RBrace (* } *)
  | Static
  | Extern
(* static keyword *)

let token_of_string tok =
  match tok with
  | Ident id -> Printf.sprintf "IDENTIFIER<%s>" id
  | Return -> "RETURN"
  | If -> "IF"
  | Else -> "ELSE"
  | For -> "FOR"
  | While -> "WHILE"
  | Do -> "DO"
  | KInt -> "INT"
  | KChar -> "CHAR"
  | Int t -> Printf.sprintf "INTEGER<%d>" t
  | Char t -> Printf.sprintf "CHAR<%c>" t
  | Void -> "VOID"
  | Break -> "BREAK"
  | Continue -> "CONTINUE"
  | Static -> "STATIC"
  | Extern -> "EXTERN"
  | Assign -> "="
  | Equal -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | And -> "&&"
  | Or -> "||"
  | Xor -> "^"
  | BitAnd -> "&"
  | BitOr -> "|"
  | Shl -> "<<"
  | Shr -> ">>"
  | Bang -> "!"
  | Complement -> "~"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Semicolon -> ";"
  | Colon -> ":"
  | Comma -> ","
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
;;
