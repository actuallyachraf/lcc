open Lcc_lib

let program = Lex.lex "int main() { return 42; }\n"
let tokens = Printf.printf "%s" (Pprint.pretty_print program)
let () = Gen.codegen (Parse.parse program)
