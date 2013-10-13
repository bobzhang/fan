
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
  lex_antiquot
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
let  token : Lexing.lexbuf -> (Ftoken.t * FLoc.t ) =
  %lex{
   | newline %{
     begin
       update_loc  lexbuf;
       (`NEWLINE, !! lexbuf )
     end }
   | "~" (ocaml_lid as x) ':' %{
     (`Label x, !! lexbuf )}

   | "?" (ocaml_lid as x) ':' %{ (`Optlabel x, !! lexbuf)}
         
   | ocaml_lid as x  %{(`Lid x, !! lexbuf )}
         
   | ocaml_uid as x  %{(`Uid x , !! lexbuf )}
         
   | int_literal  (('l'|'L'|'n' as s ) ?) as x %{
       (* FIXME - int_of_string ("-" ^ s) ??
          safety check *)
         let x = match s with
         | Some 'l' -> `Int32 x
         | Some 'L' -> `Int64 x
         | Some 'n' -> `Nativeint x
         | _ -> `Int x in
         ( x, !! lexbuf)}
   | float_literal as f %{ (`Flo f, !! lexbuf )}       (** FIXME safety check *)
   | '"' %{
       let c = new_cxt () in
       let old = lexbuf.lex_start_p in
       begin
         push_loc_cont c lexbuf lex_string;
         (`Str (buff_contents c), old --  lexbuf.lex_curr_p )
       end}
   | "'" (newline as x) "'" %{
       begin
         update_loc   lexbuf ~retract:1;
         (`Chr x, !! lexbuf)
       end}
         
   | "'" (ocaml_char as x ) "'" %{ (`Chr x , !! lexbuf )}
         
   | "'\\" (_ as c) %{err (Illegal_escape (String.make 1 c)) @@ !! lexbuf}
                                                   
   | '(' (not_star_symbolchar symbolchar* as op) ocaml_blank* ')' %{(`Eident op , !! lexbuf)}
   | '(' ocaml_blank+ (symbolchar+ as op) ocaml_blank* ')' %{(`Eident op, !! lexbuf)}
   | '(' ocaml_blank* ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as op) ocaml_blank* ')' 
       %{(`Eident op, !! lexbuf)}
   | ( "#"  | "`"  | "'"  | ","  | "."  | ".." | ":"  | "::"
   | ":=" | ":>" | ";"  | ";;" | "_" | "{"|"}"
   | "{<" |">}"
   | left_delimitor | right_delimitor
   | ['~' '?' '!' '=' '<' '>' '|' '&' '@' '^' '+' '-' '*' '/' '%' '\\'] symbolchar * )
       as x  %{(`Sym x  , !! lexbuf)}
           
   | "*)" %{
       begin
         warn Comment_not_end (!! lexbuf) ;
         move_curr_p (-1) lexbuf;
         (`Sym "*", !! lexbuf)
       end}
   | ocaml_blank + as x %{ (`Blank x, !! lexbuf)}
         
         (* comment *)
   | "(*" (')' as x) ? %{
       let c = new_cxt () in
       let old = lexbuf.lex_start_p in
       begin
         if x <> None then warn Comment_start (!!lexbuf);
         store c lexbuf;
         push_loc_cont c lexbuf lex_comment;
         (`Comment ( buff_contents c),
          old -- lexbuf.lex_curr_p)
       end}
   | ("%" as x) ? '%'  (quotation_name as name) ? ('@' (locname as meta))? "{"    as shift %{
       let c = new_cxt () in
       let name =
         match name with
         | Some name -> Ftoken.name_of_string name
         | None -> Ftoken.empty_name  in
       begin
         let old = lexbuf.lex_start_p in
         let content =
           begin
             store c lexbuf;
             push_loc_cont c lexbuf lex_quotation;
             buff_contents c
           end in
         let loc = old -- lexbuf.lex_curr_p in
         let shift = String.length shift in
         let retract = (* 2 *) 1  in
         (if x = None then
           `Quot{Ftoken.name;meta;shift;content;loc;retract}
         else `DirQuotation {Ftoken.name;meta;shift;content;loc;retract} ,loc)
       end}
         
         
   | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
       ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
       [^'\010' '\013']* newline %{
         let line = int_of_string num in begin
           update_loc  lexbuf ?file:name ~line ~absolute:true ;
           (`LINE_DIRECTIVE(line, name), !!lexbuf )
         end}
           (* Antiquotation handling *)
   | '$' %{
       let  dollar (c:Lexing_util.context) =
         %lex{
         (* FIXME *| does not work * | work *) (* $lid:x *)
         | ('`'? (identchar* |['.' '!']+) as name) ':' (antifollowident as x) %{
             begin
               let old = FLoc.move_pos (String.length name + 1) lexbuf.lex_start_p in
               (`Ant(name,x), old -- lexbuf.lex_curr_p)
             end}
         | lident as x  %{ (`Ant("",x), !!lexbuf)}  (* $lid *)
         | '(' ('`'? (identchar* |['.' '!']+) as name) ':' (* $(lid:ghohgosho)  )*) %{
             (* the first char is faked '(' to match the last ')', so we mvoe
                backwards one character *)
             let old = FLoc.move_pos (1+1+1+String.length name - 1) (List.hd  c.loc) in
             begin
               c.buffer +> '(';
               push_loc_cont c lexbuf lex_antiquot;
               (* lex_antiquot {c with loc = [old]} lexbuf ; *)
               (`Ant(name,buff_contents c),
                old -- Lexing.lexeme_end_p lexbuf)
             end}
         | '(' %{     (* $(xxxx)*)
             let old = FLoc.move_pos  (1+1-1) (List.hd c.loc) in
             begin
               c.buffer +> '(';
               push_loc_cont c lexbuf lex_antiquot;
               (* lex_antiquot   {c with loc = [old] } lexbuf ; *)
               (`Ant("", buff_contents c ), old -- Lexing.lexeme_end_p lexbuf)
             end}
         | _ as c %{err (Illegal_character c) (!! lexbuf) } }in
       let c = new_cxt () in
       if  !FConfig.antiquotations then  (* FIXME maybe always lex as antiquot?*)
         push_loc_cont c lexbuf  dollar
       else err Illegal_antiquote (!! lexbuf) }
           
   | eof %{
       let pos = lexbuf.lex_curr_p in (* FIXME *)
       (lexbuf.lex_curr_p <-
         { pos with pos_bol  = pos.pos_bol  + 1 ;
           pos_cnum = pos.pos_cnum + 1 };
        (`EOI, !!lexbuf ))}
         
   | _ as c %{ err (Illegal_character c) @@  !!lexbuf }}
    

    
let from_lexbuf lb : (Ftoken.t * FLoc.t ) Fstream.t =
  let next _ = Some (token lb)  in (* this requires the [lexeme_start_p] to be correct ...  *)
  Fstream.from next


(* local variables: *)
(* compile-command: "cd ../main_annot && pmake fan_lex.cmo" *)
(* end: *)

