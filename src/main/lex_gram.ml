(** lexing gram *)  


let  rec token = %lex_fan{
  | @whitespace %{token lexbuf}
  | @ocaml_lid("true"|"false")
  | @ocaml_uid
  | @ocaml_string
  | @ocaml_int
  | @ocaml_char
  | @ocaml_ant
  | @kwd_symbol("#" | "|" | "^" | "<" | "->" |"="  |"_" | "*" | "["
  |"]" | "*" | "?" | "+" | "(" | ")" | "-" | ":" | "@" |"{" | "}"
  |";" |"." | ",")
  | @ocaml_comment %{token lexbuf}
  | @ocaml_quotation
  | @ocaml_eof
  | @default}
    
let from_stream = Lexing_util.adapt_to_stream token 

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lex_gram.cmo" *)
(* end: *)




