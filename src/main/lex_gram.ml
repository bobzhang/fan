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
    

let from_lexbuf lb = Streamf.from (fun _ -> Some (token lb))

let from_stream (loc:Locf.t) strm =
  let lb = Lexing.from_function (Lexing_util.lexing_store strm) in begin
    lb.lex_abs_pos <- loc.loc_start.pos_cnum;
    lb.lex_curr_p <- loc.loc_start;
    from_lexbuf  lb
  end

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lex_gram.cmo" *)
(* end: *)




