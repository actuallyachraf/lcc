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

let rec parse_fun_params tokens =
  match tokens with
  | Token.RParen :: rest -> [], rest
  | Token.KInt :: Token.Ident name :: rest ->
    let _params, rest = parse_fun_params rest in
    Ast.Param (Ast.IntType, Ast.Ident name) :: _params, rest
  | _ -> failwith "unknown token in function parameters"
;;

let rec parse_statement_list tokens =
  match tokens with
  | Token.Semicolon :: rest -> parse_statement_list rest
  | Token.Return :: Token.Semicolon :: rest ->
    let _stmts, rest = parse_statement_list rest in
    Ast.Return :: _stmts, rest
  | Token.Return :: rest ->
    let expr, rest = parse_expr rest in
    let _stmts, rest = parse_statement_list rest in
    Ast.ReturnVal expr :: _stmts, rest
  | _ -> [], tokens
;;

let parse_fun_body tokens =
  let stmts, rest = parse_statement_list tokens in
  match rest with
  | [ Token.RBrace ] -> Ast.Body stmts
  | _ -> failwith "Expected closing brace"
;;

let parse_function tokens =
  let fun_type, fun_name, rest =
    match tokens with
    | Token.KInt :: Token.Ident name :: Token.LParen :: rest ->
      Ast.IntType, Ast.Ident name, rest
    | _ -> failwith "bad function type or name"
  in
  let fun_params, rest = parse_fun_params rest in
  let fun_body =
    match rest with
    | Token.LBrace :: rest -> parse_fun_body rest
    | _ -> failwith "Expected opening brace"
  in
  Ast.FunDecl (fun_type, fun_name, fun_params, fun_body)
;;

let parse tokens = Ast.Prog (parse_function tokens)
