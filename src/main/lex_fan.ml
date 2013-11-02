
%regex{ (** FIXME remove duplication later see lexing_util.cmo *)
let newline = ('\010' | '\013' | "\013\010")
let ocaml_blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let ident = (lowercase|uppercase) identchar*
    
let quotation_name = '.' ? (uppercase  identchar* '.') *
    (lowercase (identchar | '-') * )

let locname = ident

let lident = lowercase identchar *
let antifollowident =   identchar +   
let uident = uppercase identchar *


let hexa_char = ['0'-'9' 'A'-'F' 'a'-'f']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] hexa_char ['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
    ('.' ['0'-'9' '_']* )?
    (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
  

let not_star_symbolchar =
  [ '!' '%' '&' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '\\']

let symbolchar = '*'|not_star_symbolchar
let left_delimitor = (* At least a safe_delimchars *)
   '(' | '[' ['|' ]? | '[' '<' | '[' '=' | '[' '>'
let right_delimitor = ')' | [ '|' ]? ']' | '>' ']'
let ocaml_escaped_char =
  '\\'
  (['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']
  | ['0'-'9'] ['0'-'9'] ['0'-'9']
  |'x' hexa_char hexa_char)
  
let ocaml_char = ( [^ '\\' '\010' '\013'] | ocaml_escaped_char)
let ocaml_lid =  lowercase identchar *
let ocaml_uid =  uppercase identchar * 
};;



(*************************************)
(*    local operators                *)
(*************************************)
let (++) = Buffer.add_string
let (+>) = Buffer.add_char
(** get the location of current the lexeme *)
let (!!)  = Location_util.from_lexbuf ;;


%import{
Lexing_util:
  update_loc
  new_cxt
  push_loc_cont
  pop_loc
  lex_string
  lex_comment
  lex_quotation
  buff_contents
  err
  warn
  move_curr_p
  store
  ;
Location_util:
   (--)
   ;
};;
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
let  rec token : Lexing.lexbuf -> Tokenf.t  =
  %lex{
   | newline  %{
     begin
       update_loc  lexbuf;
       token lexbuf
       (* let loc = !! lexbuf in *)
       (* `Newline {loc;txt} *)
     end }
   | "~" (ocaml_lid as txt) ':' %{
     let loc = !! lexbuf in
     `Label {loc;txt}}

   | "?" (ocaml_lid as txt) ':' %{
     let loc = !!lexbuf in
     `Optlabel {loc;txt}}
         
   | ocaml_lid as txt  %{let loc =  !! lexbuf in `Lid {loc;txt}}
         
   | ocaml_uid as txt  %{let loc = !! lexbuf in `Uid {loc;txt}}
         
   | int_literal  (('l'|'L'|'n' as s ) ?) as txt %{
       (* FIXME - int_of_string ("-" ^ s) ??
          safety check *)
     let loc = !!lexbuf in
     match s with
     | Some 'l' -> `Int32 {loc;txt}
     | Some 'L' -> `Int64 {loc;txt}
     | Some 'n' -> `Nativeint {loc;txt}
     | _ -> `Int {loc;txt} }
   | float_literal as txt %{`Flo {loc = !!lexbuf;txt}}       (** FIXME safety check *)
   | '"' %{
       let c = new_cxt () in
       let old = lexbuf.lex_start_p in
       begin
         push_loc_cont c lexbuf lex_string;
         let loc = old --  lexbuf.lex_curr_p in
         `Str {loc; txt = buff_contents c}
       end}
   | "'" (newline as txt) "'" %{
       begin
         update_loc   lexbuf ~retract:1;
         let loc = !!lexbuf in
         `Chr {loc;txt}
       end}
         
   | "'" (ocaml_char as txt ) "'" %{
     let loc = !!lexbuf in `Chr {loc;txt}}
         
   | "'\\" (_ as c) %{err (Illegal_escape (String.make 1 c)) @@ !! lexbuf}
                                                   
   | '(' (not_star_symbolchar symbolchar* as txt) ocaml_blank* ')' %{
     `Eident { loc = !! lexbuf ; txt}}
   | '(' ocaml_blank+ (symbolchar+ as txt) ocaml_blank* ')' %{
     `Eident {loc = !!lexbuf;txt}}
   | '(' ocaml_blank*
       ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as txt) ocaml_blank* ')' %{
     `Eident {loc = !! lexbuf;txt}}
   | ( "#"  | "`"  | "'"  | ","  | "."  | ".." | ":"  | "::"
   | ":=" | ":>" | ";"  | ";;" | "_" | "{"|"}"
   | "{<" |">}"
   | left_delimitor | right_delimitor
   | ['~' '?' '!' '=' '<' '>' '|' '&' '@' '^' '+' '-' '*' '/' '%' '\\'] symbolchar * )
       as txt  %{ `Sym {loc = !! lexbuf ;txt}}
           
   | "*)" %{
       begin
         warn Comment_not_end (!! lexbuf) ;
         move_curr_p (-1) lexbuf;
         let loc = !! lexbuf in
         `Sym {loc;txt="*"}
       end}
   | ocaml_blank +  %{ token lexbuf }
         
         (* comment *)
   | "(*" (')' as x) ? %{
       let c = new_cxt () in
       (* let old = lexbuf.lex_start_p in *)
       begin
         if x <> None then warn Comment_start (!!lexbuf);
         store c lexbuf;
         push_loc_cont c lexbuf lex_comment;
         ignore (buff_contents c) ; (* Needed to clean the buffer *)
         (* let loc = old -- lexbuf.lex_curr_p in *)
         (* `Comment {loc;txt= buff_contents c} *)
         token lexbuf (* FIXME may bring it back later *)
       end}
   | ("%" as x) ? '%'  (quotation_name as name) ? ('@' (locname as meta))? "{" as shift %{
       let c = new_cxt () in
       let name =
         match name with
         | Some name -> Tokenf.name_of_string name
         | None -> Tokenf.empty_name  in
       begin
         let old = lexbuf.lex_start_p in
         let txt =
           begin
             store c lexbuf;
             push_loc_cont c lexbuf lex_quotation;
             buff_contents c
           end in
         let loc = old -- lexbuf.lex_curr_p in
         let shift = String.length shift in
         let retract = 1  in
         if x = None then
           `Quot{name;meta;shift;txt;loc;retract}
         else `DirQuotation {name;meta;shift;txt;loc;retract}
       end}
         
         
   | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
       ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
       [^'\010' '\013']* newline   %{
         let line = int_of_string num in begin
           update_loc  lexbuf ?file:name ~line ~absolute:true ;
           token lexbuf
         end}
           (* Antiquotation handling *)

       (******************)
       (* $x   *)
       (* $x{} *)
       (* $x:id *)
       (* ${}*)
       (******************)
   | '$' ( lident as name) ':'  antifollowident as txt %{
     `Ant{loc = !!lexbuf;
          kind = name;
          txt ;
          shift =  String.length name +  2;
          retract = 0;
          cxt = None}}
   | '$' lident as txt  %{
     `Ant{kind =""; txt ;loc = !!lexbuf; shift = 1; retract = 0; cxt = None}}
   | "$" ( lident as name)? "{"  as txt  %{
     let old = lexbuf.lex_start_p in
     let c = new_cxt () in
     begin
       store c lexbuf;
       push_loc_cont c lexbuf lex_quotation;
       `Ant{loc =
            {loc_start = old;
             loc_end = lexbuf.lex_curr_p;
             loc_ghost = false};
            kind = match name with | Some n -> n | None -> "";
            txt = buff_contents c;
            shift =  String.length txt ;
            retract =  1 ;
            cxt = None}
     end}
   | '$' (_ as c) %{err (Illegal_character c) (!! lexbuf)        }
   | eof %{
       let pos = lexbuf.lex_curr_p in (* FIXME *)
       (lexbuf.lex_curr_p <-
         { pos with pos_bol  = pos.pos_bol  + 1 ;
           pos_cnum = pos.pos_cnum + 1 };
        let loc = !!lexbuf in
        `EOI {loc;txt=""})}
   | _ as c %{ err (Illegal_character c) @@  !!lexbuf }}
    

    
let from_lexbuf lb : Tokenf.stream =
  let next _ = Some (token lb)  in (* this requires the [lexeme_start_p] to be correct ...  *)
  Streamf.from next


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lex_fan.cmo" *)
(* end: *)

