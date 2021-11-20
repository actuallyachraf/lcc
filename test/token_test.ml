open Lcc_lib
open OUnit

let tests =
  "test suite for token"
  >::: [ ("singleton"
         >:: fun _ ->
         assert_equal
           "IDENTIFIER<my_func>"
           (Token.token_of_string Token.(Ident "my_func")))
       ; ("not implemented"
         >:: fun _ -> assert_equal "Illegal" (Token.token_of_string Token.Comma))
       ]
;;

let _ = run_test_tt_main tests
