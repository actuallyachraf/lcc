open Lcc_lib
open OUnit

let tests =
  "test suite for lcc_lib"
  >::: [ ("singleton"
         >:: fun _ ->
         assert_equal
           "IDENTIFIER<my_func>"
           (Token.token_of_string Token.(Ident "my_func")))
       ; ("not implemented"
         >:: fun _ ->
         assert_equal
           ~printer:Helper.string_of_string
           "ILLEGAL"
           (Token.token_of_string Token.Static))
       ]
;;

let _ = run_test_tt_main tests
