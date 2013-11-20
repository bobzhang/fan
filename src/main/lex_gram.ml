(** lexing gram *)  


let  rec token = %lex_fan{
  | @whitespace %{token lexbuf}
  | @ocaml_lid("true"|"false"|"as"|"_")
  | @ocaml_uid(
      "SEP"|"LEVEL"| "S"| "EOI"| "Lid"|"Uid"
  | "Ant"|"Quot"| "DirQuotation"| "Str"
  | "Label"| "Optlabel"| "Chr"| "Int"| "Int32"| "Int64"| "Nativeint" (* duplication check*)
  | "Flo"| "Pre"| "Inf"
  | "TRY"| "PEEK"
  | "L0"| "L1"| "First"| "Last"
  | "Before"| "After"| "Level"
  | "RA"|"Inline"|"Local")
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




