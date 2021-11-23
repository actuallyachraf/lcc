open Lcc_lib

let program = Lex.lex "int main() { return 42; }\n"
let _ = Printf.printf "%s" (Pprint.pretty_print program)
