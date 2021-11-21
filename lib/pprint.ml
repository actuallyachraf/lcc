open Base

let pretty_print token_list =
  String.concat ~sep:" " (List.map token_list ~f:Token.token_of_string)
;;
