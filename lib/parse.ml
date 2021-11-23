open Batteries

let next tokens = List.hd tokens, List.tl tokens

let bin_op_map =
  Map.empty
  |> Map.add Token.Plus Ast.Add
  |> Map.add Token.Minus Ast.Sub
  |> Map.add Token.Star Ast.Mul
  |> Map.add Token.Slash Ast.Div
  |> Map.add Token.Lt Ast.Lt
  |> Map.add Token.Lte Ast.Lte
  |> Map.add Token.Gt Ast.Gt
  |> Map.add Token.Gte Ast.Gte
  |> Map.add Token.Equal Ast.Eq
  |> Map.add Token.Neq Ast.Neq
  |> Map.add Token.And Ast.And
  |> Map.add Token.Or Ast.Or
  |> Map.add Token.Shl Ast.Shl
  |> Map.add Token.Shr Ast.Shr
  |> Map.add Token.BitAnd Ast.BitAnd
  |> Map.add Token.BitOr Ast.BitOr
  |> Map.add Token.Xor Ast.Xor
;;

let token_to_const = function
  | Token.Int i -> Ast.(Const (Int i))
  | Token.Char c -> Ast.(Const (Char c))
  | _ -> failwith "not a constant"
;;

let parse_expr tokens =
  let parse_bin_expr parse_next_level op_tokens tokens =
    let lhs, rest = parse_next_level tokens in
    let rec add_terms lhs_expr tokens =
      let hd_token = List.hd tokens in
      if List.mem hd_token op_tokens
      then (
        let rhs_expr, rest = parse_next_level (List.tl tokens) in
        let bin_op = Map.find hd_token bin_op_map in
        let lhs_expr = Ast.BinOp (bin_op, lhs_expr, rhs_expr) in
        add_terms lhs_expr rest)
      else lhs, tokens
    in
    add_terms lhs rest
  in
  let rec parse_fun_call = function
    | Token.(Ident name :: LParen :: arg_tokens) ->
      let fun_name = Ast.Ident name in
      let args, rest = parse_fun_args arg_tokens in
      Ast.FunCall (fun_name, args), rest
    | _ -> failwith "Shouldn't have called parse_fun_call, this isn't a function call"
  and parse_fun_args = function
    | Token.RParen :: rest -> [], rest
    | toks ->
      let arg, rest = parse_exp toks in
      let args, rest =
        match rest with
        | Token.Comma :: more_args -> parse_fun_args more_args
        | Token.RParen :: after_fun_call -> [], after_fun_call
        | _ -> failwith "Invalid list of function arguments"
      in
      arg :: args, rest
  and parse_factor toks =
    let open Token in
    match toks with
    | LParen :: factor ->
      let exp, after_exp = parse_exp factor in
      (match after_exp with
      | RParen :: rest -> exp, rest
      | _ -> failwith "Syntax error: expected close paren")
    | Minus :: factor ->
      let num, rest = parse_factor factor in
      Ast.(UnOp (Neg, num)), rest
    | Plus :: factor ->
      let num, rest = parse_factor factor in
      Ast.(UnOp (Pos, num)), rest
    | Complement :: factor ->
      let num, rest = parse_factor factor in
      Ast.(UnOp (Complement, num)), rest
    | Bang :: factor ->
      let num, rest = parse_factor factor in
      Ast.(UnOp (Not, num)), rest
    | Ident _ :: LParen :: _ -> parse_fun_call toks
    | Ident name :: rest -> Ast.(Var (Ident name)), rest
    | Int i :: rest -> Ast.(Const (Int i)), rest
    | Char c :: rest -> Ast.(Const (Char c)), rest
    | _ -> failwith "Failed to parse factor"
  and parse_term toks =
    let open Token in
    parse_bin_expr parse_factor [ Star; Slash ] toks
  and parse_additive_exp toks =
    let open Token in
    parse_bin_expr parse_term [ Plus; Minus ] toks
  and parse_shift_expr toks =
    let open Token in
    parse_bin_expr parse_additive_exp [ Shl; Shr ] toks
  and parse_relational_exp toks =
    let open Token in
    parse_bin_expr parse_shift_expr [ Lt; Lte; Gt; Gte ] toks
  and parse_equality_expr toks =
    let open Token in
    parse_bin_expr parse_relational_exp [ Equal; Neq ] toks
  and parse_bitwise_and_expr toks =
    parse_bin_expr parse_equality_expr [ Token.BitAnd ] toks
  and parse_xor_expr toks = parse_bin_expr parse_bitwise_and_expr [ Token.Xor ] toks
  and parse_bitwise_or_expr toks = parse_bin_expr parse_xor_expr [ Token.BitOr ] toks
  and parse_and_expr toks = parse_bin_expr parse_bitwise_or_expr [ Token.And ] toks
  and parse_or_expr toks = parse_bin_expr parse_and_expr [ Token.Or ] toks
  and parse_ternary_expr toks =
    let exp_1, rest = parse_or_expr toks in
    match rest with
    | Token.Question :: branch1_tokens ->
      let branch1, rest = parse_exp branch1_tokens in
      (match rest with
      | Token.Colon :: branch2_tokens ->
        let branch2, rest = parse_ternary_expr branch2_tokens in
        Ast.TernOp (exp_1, branch1, branch2), rest
      | _ -> failwith "Expected colon after ternary operator")
    | _ -> exp_1, rest
  and parse_exp = function
    | Token.(Ident v :: Assign :: rest) ->
      (* assignment statement *)
      let var_id = Ast.Ident v in
      let exp, rest = parse_exp rest in
      Ast.(AssignOp (Assign, var_id, exp)), rest
    | tokens -> parse_ternary_expr tokens
  in
  parse_exp tokens
;;

let parse_optional_exp next_expected toks =
  if List.hd toks = next_expected
  then None, List.tl toks
  else (
    let e, rest = parse_expr toks in
    if List.hd rest = next_expected
    then Some e, List.tl rest
    else failwith "Didn't get expected token after exp")
;;

let parse_rest_of_declaration var_id var_type storage_class tokens =
  let init, rest =
    match tokens with
    | Token.Semicolon :: _ -> None, tokens
    | Token.Assign :: rest ->
      let exp, rest = parse_expr rest in
      Some exp, rest
    | _ -> failwith "Invalid initial value for variable"
  in
  let declaration = Ast.{ var_type; var_name = var_id; init; storage_class } in
  match rest with
  | Token.Semicolon :: rest -> declaration, rest
  | _ -> failwith "Expected semicolon after declaration"
;;

let parse_declaration tokens =
  let storage_class, rest =
    match tokens with
    | Token.Static :: rest -> Ast.Static, rest
    | Token.Extern :: rest -> Ast.Extern, rest
    | _ -> Ast.Nothing, tokens
  in
  match rest with
  | Token.KInt :: Token.Ident varname :: rest ->
    parse_rest_of_declaration (Ast.Ident varname) Ast.IntType storage_class rest
  | _ ->
    failwith
      (Printf.sprintf "Unexpected keyword %s" (Token.token_of_string (List.hd rest)))
;;

let parse_function_body toks =
  let parse_return_statement stmt =
    let exp, rest = parse_expr stmt in
    Ast.ReturnVal exp, rest
  in
  let rec parse_block = function
    | Token.LBrace :: more_tokens ->
      let block_items, rest = parse_block_item_list more_tokens in
      (match rest with
      | Token.RBrace :: rest -> block_items, rest
      | _ -> failwith "Expected closing brace at end of block")
    | _ -> failwith "Expected opening brace at start of block"
  and parse_if_statement = function
    | Token.LParen :: _ as toks ->
      let cond, rest = parse_expr toks in
      let if_body, rest = parse_statement rest in
      let else_body, rest =
        match rest with
        | Token.Else :: else_tokens ->
          let else_body, rest = parse_statement else_tokens in
          Some else_body, rest
        | _ -> None, rest
      in
      let if_statement = Ast.If { cond; if_body; else_body } in
      if_statement, rest
    | _ -> failwith "Expected '(' after 'if'"
  and parse_for_components toks =
    let cond, rest = parse_optional_exp Token.Semicolon toks in
    let cond =
      match cond with
      (* C11 6.8.5.3 - An omitted expression-2 is replaced by a nonzero constant *)
      | None -> Ast.Const (Int 1)
      | Some c -> c
    in
    let post, rest = parse_optional_exp Token.RParen rest in
    let body, rest = parse_statement rest in
    cond, post, body, rest
  and parse_for_statement = function
    | Token.(LParen :: (KInt :: _ as decl_toks)) ->
      (* for loop w/ variable declaration *)
      let init, rest = parse_declaration decl_toks in
      let cond, post, body, rest = parse_for_components rest in
      Ast.ForDecl { init; cond; post; body }, rest
    | Token.LParen :: toks ->
      let init, rest = parse_optional_exp Token.Semicolon toks in
      let cond, post, body, rest = parse_for_components rest in
      Ast.For { init; cond; post; body }, rest
    | _ -> failwith "PANIC: expected open paren at start of for loop"
  and parse_while_statement toks =
    let cond, rest = parse_expr toks in
    let body, rest = parse_statement rest in
    Ast.While { cond; body }, rest
  and parse_do_while_statement toks =
    let body, rest = parse_statement toks in
    match rest with
    | Token.While :: cond_tokens ->
      let cond, rest = parse_expr cond_tokens in
      (match rest with
      | Token.Semicolon :: rest -> Ast.DoWhile { body; cond }, rest
      | _ -> failwith "Expected semicolon after do-while")
    | _ -> failwith "Expected 'while' after body of do-while"
  (* TODO: actually pay attention to types *)
  and parse_statement toks =
    let open Token in
    match toks with
    | LBrace :: _ ->
      let block, rest = parse_block toks in
      Block block, rest
    | If :: tokens -> parse_if_statement tokens
    | For :: tokens -> parse_for_statement tokens
    | Return :: tokens ->
      let statement, rest = parse_return_statement tokens in
      (match rest with
      | Semicolon :: rest -> statement, rest
      | _ -> failwith "Expected semicolon after return statement")
    | While :: tokens -> parse_while_statement tokens
    | Do :: tokens -> parse_do_while_statement tokens
    | Break :: Semicolon :: rest -> Ast.Break, rest
    | Break :: _ -> failwith "Expected semicolon after break"
    | Continue :: Semicolon :: rest -> Ast.Continue, rest
    | Continue :: _ -> failwith "Expected semicolon after continue"
    | _ ->
      let exp, rest = parse_optional_exp Token.Semicolon toks in
      Ast.Expr exp, rest
  and parse_block_item tokens =
    let is_declaration =
      match tokens with
      | Token.Static :: _ -> true
      | Token.Extern :: _ -> true
      | Token.KInt :: _ -> true
      | _ -> false
    in
    if is_declaration
    then (
      let decl, rest = parse_declaration tokens in
      Ast.Decl decl, rest)
    else (
      let stmt, rest = parse_statement tokens in
      Ast.Statement stmt, rest)
  and parse_block_item_list tokens =
    if List.hd tokens == Token.RBrace
    then [], tokens
    else (
      let next_statement, rest = parse_block_item tokens in
      let statements, rest = parse_block_item_list rest in
      next_statement :: statements, rest)
  in
  parse_block toks
;;

let parse_next_param = function
  | Token.KInt :: Token.Ident name :: rest -> Ast.(Param (IntType, Ident name)), rest
  | Token.KChar :: Token.Ident name :: rest -> Ast.(Param (CharType, Ident name)), rest
  | _ -> failwith "Invalid function parameter"
;;

let rec parse_fun_params = function
  | Token.RParen :: rest -> [], rest
  | toks ->
    let param, rest = parse_next_param toks in
    let params, rest =
      match rest with
      | Token.Comma :: more_params -> parse_fun_params more_params
      | Token.RParen :: after_params -> [], after_params
      | _ -> failwith "Invalid list of parameters"
    in
    param :: params, rest
;;

let parse_fun fun_type name storage_class tokens =
  (* we've already parsed everything up to open paren *)
  let params, rest = parse_fun_params tokens in
  let opt_body, rest =
    match rest with
    | Token.LBrace :: _ ->
      let body, rest' = parse_function_body rest in
      Some body, rest'
    | Token.Semicolon :: rest' -> None, rest'
    | _ -> failwith "Unexpected token after function declaration"
  in
  let decl = Ast.Function { fun_type; name; storage_class; params; body = opt_body } in
  decl, rest
;;

let parse_top_level tokens =
  let storage_class, after_storage_class =
    match tokens with
    | Token.Static :: toks -> Ast.Static, toks
    | Token.Extern :: toks -> Ast.Extern, toks
    | _ -> Ast.Nothing, tokens
  in
  let tl_type, tl_name, after_id =
    match after_storage_class with
    | Token.(KInt :: Ident name :: rest) -> Ast.(IntType, Ident name, rest)
    | Token.(KChar :: Ident name :: rest) -> Ast.(CharType, Ident name, rest)
    | _ -> failwith "Parse error in parse_top_level: bad toplevel name or top"
  in
  let (Ast.Ident _) = tl_name in
  match after_id with
  | Token.LParen :: rest -> parse_fun tl_type tl_name storage_class rest
  | _ ->
    let decl, rest = parse_rest_of_declaration tl_name tl_type storage_class after_id in
    Ast.GlobalVar decl, rest
;;

let rec parse_top_levels = function
  | [] -> [] (* no functions left to parse *)
  | tokens ->
    let tl, rest = parse_top_level tokens in
    let tls = parse_top_levels rest in
    tl :: tls
;;

let parse tokens = Ast.Prog (parse_top_levels tokens)
