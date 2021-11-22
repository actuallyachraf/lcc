let next tokens = List.hd tokens, List.tl tokens

let token_to_const = function
  | Token.Int i -> Ast.(Const (Int i))
  | Token.Char c -> Ast.(Const (Char c))
  | _ -> failwith "not a constant"
;;

let parse_expr tokens =
  match tokens with
  | Token.Int i :: rest -> Ast.(Const (Int i)), rest
  | [] -> failwith "no tokens to parse"
  | tok :: _ ->
    failwith ("unrecognized token " ^ Token.token_of_string tok ^ "in expression")
;;

let rec parse_statement_list tokens =
  match tokens with
  | Token.Semicolon :: rest -> parse_statement_list rest
  | Token.Return :: rest ->
    let expr, rest = parse_expr rest in
    let other_statement, rest = parse_statement_list rest in
    Ast.ReturnVal expr :: other_statement, rest
  | _ -> [], tokens
;;
