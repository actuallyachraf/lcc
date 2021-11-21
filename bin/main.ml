open Lcc_lib

let program = Pprint.pretty_print (Lex.lex "return 42;")
let () = Printf.printf "%s\n" program
