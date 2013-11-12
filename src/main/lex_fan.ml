
%regex{ (** FIXME remove duplication later see lexing_util.cmo *)
let not_star_symbolchar =
  [ '!' '%' '&' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '\\']
let symbolchar = '*'|not_star_symbolchar

};;

(** get the location of current the lexeme *)
let (!!)  = Lexing_util.from_lexbuf ;;

(** It could also import regex in the future
    {:import|
    Lexing_util:
    with_curr_loc
    update_loc ;
   Location_util:
    (--)
    from_lexbuf as  (!!)
    lex_antiquot : %{ a -> b -> c}  as xx ;
   Buffer:
    add_string -> (++)
    add_char -> (+>) ;
   |}  *)
let  rec token   = %lex_fan{
   | @whitespace %{ token lexbuf}
   | @ocaml_comment %{token lexbuf}
   | "~" (ocaml_lid as txt) ':' %{`Label {loc= !! lexbuf;txt}}

   | "?" (ocaml_lid as txt) ':' %{`Optlabel {loc= !!lexbuf;txt}}
   | ocaml_lid as txt  %{
     if  %p{"mod"|"land"|"lor"|"lxor"} txt  then
       `Inf{loc= !!lexbuf; txt ; level = 3}
     else if %p{"lsl"|"lsr" | "asr"} txt then
       `Inf{loc= !!lexbuf; txt ; level = 4}
     else 
       `Lid {loc= !!lexbuf;txt}}
   | @ocaml_uid
   | @ocaml_int_literal
   | @ocaml_float_literal       (** FIXME safety check *)
   | @ocaml_string
   | @ocaml_char
   | '(' (not_star_symbolchar symbolchar* as txt) ocaml_blank* ')' %{
     `Eident { loc = !! lexbuf ; txt}}
   | '(' ocaml_blank+ (symbolchar+ as txt) ocaml_blank* ')' %{
     `Eident {loc = !!lexbuf;txt}}
   | '(' ocaml_blank*
       ("or"
       | "mod"|"land"|"lor" |"lxor"
       |"lsl"|"lsr"|"asr" as txt) ocaml_blank* ')' %{
     `Eident {loc = !! lexbuf;txt}}

   | '!' symbolchar+ as txt %{ `Pre{loc= !!lexbuf; txt}}
   | ['~' '?'] symbolchar+ as txt  %{`Pre{loc = !!lexbuf; txt }}

   | "**" symbolchar* as txt %{ `Inf{loc = !!lexbuf; txt ; level = 4}}
   | ['*' '/' '%'] symbolchar* as txt  %{`Inf{loc = !!lexbuf; txt ; level = 3}}
   | ['+' '-'] symbolchar * as txt %{`Inf{loc = !!lexbuf; txt ; level = 2}}

   | ['@' '^'] symbolchar * as txt %{`Inf{loc = !!lexbuf; txt; level = 1}  }

   | ['=' '<' '>' '|' '&'] symbolchar *  as txt %{`Inf{loc = !!lexbuf; txt ; level = 0}}
       
   | ( "#"  | "`"  | "'"  | ","  | "."  | ".." | ":"  | "::"
   | ":=" | ":>" | ";"  | ";;" | "_" | "{"|"}"
   | "{<" |">}"
   |     (* At least a safe_delimchars *)
     ('(' | '[' ['|' ]? | '[' '<' | '[' '=' | '[' '>')
   | (')' | [ '|' ]? ']' | '>' ']')

   | ['!' '~' '?']    ) as txt  %{ `Sym {loc = !! lexbuf ;txt}}
           
   | "*)" %{
       begin
         Lexing_util.warn Comment_not_end (!! lexbuf) ;
         Lexing_util.move_curr_p (-1) lexbuf;
         let loc = !! lexbuf in
         `Sym {loc;txt="*"}
       end}
   | @ocaml_double_quotation
   | @line_directive %{token lexbuf}       
   | @ocaml_ant
   | @ocaml_eof
   | @default}

    
let from_lexbuf lb : Tokenf.stream =
  let next _ = Some (token lb)  in (* this requires the [lexeme_start_p] to be correct ...  *)
  Streamf.from next


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lex_fan.cmo" *)
(* end: *)

