open Lcc_lib
open OUnit

let make_lex_is_digit_test name expected input =
  name >:: fun _ -> assert_equal expected (Lex.is_digit input) ~printer:string_of_bool
;;

let make_lex_is_alpha_test name expected input =
  name >:: fun _ -> assert_equal expected (Lex.is_alpha input) ~printer:string_of_bool
;;

let make_lex_test name expected input =
  name >:: fun _ -> assert_equal expected (Lex.lex input)
;;

let make_next_token_test name expected input =
  name
  >:: fun _ ->
  assert_equal expected (Lex.next_token input) ~printer:Helper.string_of_token_list
;;

let tests =
  "test suite for token"
  >::: [ make_lex_is_digit_test "digits" true '5'
       ; make_lex_is_alpha_test "alpha" true 'b'
       ; make_lex_is_digit_test "not digits" false 'c'
       ; make_lex_test "lex" [ 'i'; 'n'; 't' ] "int"
       ; make_next_token_test "next_token" [ Token.LBrace; Token.RBrace ] ("{}" |> Lex.lex)
       ; make_next_token_test "next_token_a" [ Token.Int 123 ] ("123" |> Lex.lex)
       ; make_next_token_test "next_token_b" [ Token.Char 'A' ] ("'A'" |> Lex.lex)
       ]
;;

let _ = run_test_tt_main tests
