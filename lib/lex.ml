let is_digit c =
  let code = Char.code c in
  code >= Char.code '0' && code <= Char.code '9'
;;

let is_alpha c =
  let code = Char.code c in
  (code >= Char.code 'a' && code <= Char.code 'z')
  || (code >= Char.code 'A' && code <= Char.code 'Z')
;;
