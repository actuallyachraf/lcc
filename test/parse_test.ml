open Lcc_lib
open OUnit

let make_parse_constant_test name expected input =
  name >:: fun _ -> assert_equal expected (Parse.token_to_const input)
;;

let make_fun param = "int main(){ return " ^ param ^ "; }" |> Lex.lex |> Parse.parse
let make_decl param = "int var =" ^ param ^ ";" |> Lex.lex |> Parse.parse

let make_ast params block_items =
  let body = block_items in
  let name = Ast.Ident "main" in
  let f =
    Ast.Function
      { fun_type = Ast.IntType
      ; name
      ; params
      ; body = Some body
      ; storage_class = Ast.Nothing
      }
  in
  Ast.Prog [ f ]
;;

let make_parse_test name expected input =
  name >:: fun _ -> assert_equal expected (Parse.parse input)
;;

let make_for_ast =
  let const = Ast.Const (Ast.Int 1) in
  let for_loop =
    Ast.Statement
      (For
         { init = Some const
         ; cond = const
         ; post = Some const
         ; body = Block [ Ast.Statement (Expr (Some const)) ]
         })
  in
  make_ast [] [ for_loop ]
;;

let tests =
  "test suite for parser"
  >::: [ make_parse_constant_test "parse_constant" Ast.(Const (Int 5)) (Token.Int 5)
       ; make_parse_test
           "parse_statement"
           (make_fun "5")
           (Lex.lex "int main() { return 5; }")
       ; make_parse_test
           "parse_function"
           (make_fun "42")
           (Lex.lex "int main(){ return 42; }")
       ; make_parse_test
           "parse_int_declaration"
           (make_decl "99")
           (Lex.lex "int var = 99 ;")
       ; make_parse_test
           "parse_for_loop"
           make_for_ast
           (Lex.lex "int main() { for(1;1;1) {1;}}")
       ]
;;

let _ = run_test_tt_main tests
