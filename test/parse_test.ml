open Lcc_lib
open OUnit

let make_parse_constant_test name expected input =
  name >:: fun _ -> assert_equal expected (Parse.token_to_const input)
;;

let make_ret_statement =
  let value = Ast.(Const (Int 5)) in
  let ret = Ast.ReturnVal value in
  [ ret ], []
;;

let make_parse_statement_test name expected input =
  name >:: fun _ -> assert_equal expected (Parse.parse_statement_list input)
;;

let tests =
  "test suite for parser"
  >::: [ make_parse_constant_test "parse_constant" Ast.(Const (Int 5)) (Token.Int 5)
       ; make_parse_statement_test
           "parse_statement"
           make_ret_statement
           (Lex.lex "return 5")
       ]
;;

let _ = run_test_tt_main tests
