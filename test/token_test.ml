open Lcc_lib
open OUnit

let make_token_test name expected input =
  name
  >:: fun _ ->
  assert_equal expected (Token.token_of_string input) ~printer:Helper.string_of_string
;;

let tests =
  "test suite for token"
  >::: [ make_token_test "identifier" "IDENTIFIER<name>" (Token.Ident "name")
       ; make_token_test "return" "RETURN" Token.Return
       ; make_token_test "int" "INTEGER<5>" (Token.Int 5)
       ; make_token_test "char" "CHAR<A>" (Token.Char 'A')
       ; make_token_test "assign" "=" Token.Assign
       ; make_token_test "plus" "+" Token.Plus
       ; make_token_test "minus" "-" Token.Minus
       ; make_token_test "star" "*" Token.Star
       ; make_token_test "slash" "/" Token.Slash
       ; make_token_test "semicolon" ";" Token.Semicolon
       ; make_token_test "comma" "," Token.Comma
       ; make_token_test "lparen" "(" Token.LParen
       ; make_token_test "rparen" ")" Token.RParen
       ; make_token_test "lbrace" "{" Token.LBrace
       ; make_token_test "rbrace" "}" Token.RBrace
       ; make_token_test "illegal" "ILLEGAL" Token.Static
       ]
;;

let _ = run_test_tt_main tests
