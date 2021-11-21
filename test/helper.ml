open Base
open Lcc_lib

let string_of_string s : string = s
let string_of_token_list l = String.concat ~sep:" " (List.map l ~f:Token.token_of_string)
