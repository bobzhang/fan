
include Char
let is_whitespace =
  function
    | ' ' | '\010' | '\013' | '\009' | '\026' | '\012' -> true
    | _ -> false

let is_newline = function
  | '\010' | '\013' -> true
  | _               -> false
        
let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_uppercase c = 'A' <= c && c <= 'Z'

let is_lowercase c = 'a' <= c && c <= 'z'


(* local variables: *)
(* compile-command: "pmake fchar.cmo" *)
(* end: *)
