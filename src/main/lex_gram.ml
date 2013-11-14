(** lexing gram *)  


let  rec token = %lex_fan{
  | @whitespace %{token lexbuf}
  | @ocaml_lid
  | @ocaml_uid
  | @ocaml_string
  | @ocaml_int
  | @ocaml_char
  | @ocaml_ant
  | "#" | "|" | "^" | "<" | "->" |"="  |"_" | "*" | "["
  |"]" | "*" | "?" | "+" | "(" | ")" | "-" | ":" | "@" |"{" | "}"
  |";" |"." | "," as txt %{
    `Sym {loc = Lexing_util.from_lexbuf lexbuf;txt}}
  | @ocaml_comment %{token lexbuf}
  | @ocaml_quotation
  | @ocaml_eof
  | @default}
    
let from_stream = Lexing_util.adapt_to_stream token 

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lex_gram.cmo" *)
(* end: *)




