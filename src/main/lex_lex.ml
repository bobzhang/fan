


(** get the location of current the lexeme *)
let  rec token = %lex_fan{
  | @whitespace %{token lexbuf}
  | @ocaml_lid("as"|"let")
  | @ocaml_char
  | @ocaml_string
  | @kwd_symbol(
      "#" | "|" | "^" | "<" | "->" |"=" | "*" | "["
    |"]" | "*" | "?" | "+" | "(" | ")" | "-" | "@")
  | @ocaml_comment %{token lexbuf}
  | @ocaml_quotation
  | @ocaml_eof
  | @default}
    

let from_lexbuf lb = Streamf.from (fun _ -> Some (token lb))
let from_stream = Lexing_util.adapt_to_stream token 

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lex_lex.cmo" *)
(* end: *)
