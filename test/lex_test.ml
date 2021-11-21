open Lcc_lib
open OUnit

let make_lex_is_digit_test name expected input =
  name >:: fun _ -> assert_equal expected (Lex.is_digit input) ~printer:string_of_bool
;;

let make_lex_is_alpha_test name expected input =
  name >:: fun _ -> assert_equal expected (Lex.is_alpha input) ~printer:string_of_bool
;;

let tests =
  "test suite for token"
  >::: [ make_lex_is_digit_test "digits" true '5'
       ; make_lex_is_alpha_test "alpha" true 'b'
       ; make_lex_is_digit_test "not digits" false 'c'
       ]
;;
