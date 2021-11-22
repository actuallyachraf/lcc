open Lcc_lib
open OUnit

let make_lex_is_digit_test name expected input =
  name >:: fun _ -> assert_equal expected (Lex.is_digit input) ~printer:string_of_bool
;;

let make_lex_is_alpha_test name expected input =
  name >:: fun _ -> assert_equal expected (Lex.is_alpha input) ~printer:string_of_bool
;;

let make_lex_test name expected input =
  name
  >:: fun _ -> assert_equal expected (Lex.lex input) ~printer:Helper.string_of_token_list
;;

let make_next_token_test name expected input =
  name
  >:: fun _ ->
  assert_equal expected (Lex.next_token input) ~printer:Helper.string_of_token_list
;;

let tests =
  "test suite for lexer"
  >::: [ make_lex_is_digit_test "digits" true '5'
       ; make_lex_is_alpha_test "alpha" true 'b'
       ; make_lex_is_digit_test "not digits" false 'c'
       ; make_lex_test "next_token" [ Token.LBrace; Token.RBrace ] "{}"
       ; make_lex_test "next_token_a" [ Token.Int 123 ] "123"
       ; make_lex_test "next_token_b" [ Token.Char 'A' ] "'A'"
       ; make_lex_test "next_token_c" [ Token.Return; Token.Semicolon ] "return ;"
       ; make_lex_test
           "next_token_d"
           [ Token.Return; Token.Int 42; Token.Semicolon ]
           "return 42;"
       ; make_lex_test
           "test lex keywords"
           [ Token.For
           ; Token.LParen
           ; Token.KInt
           ; Token.Ident "i"
           ; Token.Assign
           ; Token.Int 0
           ; Token.Semicolon
           ; Token.Ident "i"
           ; Token.Lt
           ; Token.Int 10
           ; Token.Semicolon
           ; Token.Ident "i"
           ; Token.Plus
           ; Token.Plus
           ; Token.RParen
           ]
           "for(int i = 0; i < 10;i++)"
       ]
;;

let _ = run_test_tt_main tests
