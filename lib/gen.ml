(* x64 Assembly Code Generation *)
let codegen prog =
  let chan = open_out "asm.s" in
  let _ = Printf.fprintf chan "    .globl main\n" in
  let gen_stmt = function
    | Ast.Return -> Printf.fprintf chan "ret"
    | Ast.ReturnVal (Ast.Const (Ast.Int i)) ->
      Printf.fprintf chan "    mov    $%d, %%rax\n" i;
      Printf.fprintf chan "    ret"
    | _ -> failwith "Expression not supported"
  in
  let gen_stmt statements = List.iter gen_stmt statements in
  let gen_fun f =
    match f with
    | Ast.FunDecl (_, Ast.Ident fun_name, _, Ast.Body statements) ->
      let _ = Printf.fprintf chan "%s:\n" fun_name in
      gen_stmt statements
  in
  match prog with
  | Ast.Prog f ->
    let _ = gen_fun f in
    close_out chan
;;
