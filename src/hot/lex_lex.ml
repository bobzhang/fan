{:regexp| (** FIXME remove duplication later see lexing_util.cmo *)
let newline = ('\010' | '\013' | "\013\010")
let ocaml_blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let ident = (lowercase|uppercase) identchar*
    
let quotation_name = '.' ? (uppercase  identchar* '.') *
    (lowercase (identchar | '-') * )

let locname = ident

let quotation_prefix =
  '{' (':' quotation_name)? ('@' locname)? '|'
    

let lident = lowercase identchar *
let antifollowident =   identchar +   
let uident = uppercase identchar *
let not_star_symbolchar =
  [(* '$' *) '!' '%' '&' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '\\']
let symbolchar = '*' | not_star_symbolchar
let quotchar =
  ['!' '%' '&' '+' '-' '.' '/' ':' '=' '?' '@' '^' '|' '~' '\\' '*']
let extra_quot =
  ['!' '%' '&' '+' '-' '.' '/' ':' '=' '?' '@' '^'  '~' '\\']
    (* FIX remove the '\' as extra quot*)
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
  
(* Delimitors are extended (from 3.09) in a conservative way *)

(* These chars that can't start an expession or a pattern: *)
let safe_delimchars = ['%' '&' '/' '@' '^']
    
(* These symbols are unsafe since "[<", "[|", etc. exsist. *)
let delimchars = safe_delimchars | ['|' '<' '>' ':' '=' '.']

let left_delims  = ['(' '[' ]
let right_delims = [')' ']' ]
    
let left_delimitor = (* At least a safe_delimchars *)
  left_delims delimchars* safe_delimchars (delimchars|left_delims)*
   (* A '(' or a new super '(' without "(<" *)
  | '(' (['|' ':'] delimchars* )?
  (* Old brackets, no new brackets starting with "[|" or "[:" *)
  | '[' ['|' ':']?
   (* Old "[<","{<" and new ones *)
  | ['[' ] delimchars* '<'
  | '[' '='
  | '[' '>' 
let right_delimitor =
  (* At least a safe_delimchars *)
  (delimchars|right_delims)* safe_delimchars (delimchars|right_delims)* right_delims
   | (delimchars* ['|' ':'])? ')'
   | ['|' ':']? ']'
   | '>' delimchars* [']' ]
let ocaml_escaped_char =
  '\\' (['\\' '"' 'n' 't' 'b' 'r' ' ' '\''] | ['0'-'9'] ['0'-'9'] ['0'-'9'] |'x' hexa_char hexa_char)
let ocaml_char =
  ( [! '\\' '\010' '\013'] | ocaml_escaped_char)
let ocaml_lid =
  lowercase identchar *
let ocaml_uid =
  uppercase identchar * 
|};;


(*************************************)
(*    local operators                *)    
(*************************************)        
let (++) = Buffer.add_string       
let (+>) = Buffer.add_char
(** get the location of current the lexeme *)
let (!!)  = Location_util.from_lexbuf ;;

(* let opt_char_len = Lexing_util;; *)
{:import|
Lexing_util:
  opt_char
  mk_quotation
  opt_char_len
  update_loc
  default_cxt
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
  clear_stack
  lexing_store
  with_store
  lex_simple_quotation
  ;
Location_util:
   (--)
   ;
|};;    


let  rec token : Lexing.lexbuf -> (Ftoken.t * FLoc.t ) = {:lexer|
| newline ->
    begin
      update_loc  lexbuf;
      (`NEWLINE, !! lexbuf )
    end
| ocaml_lid as x -> (`Lid x, !! lexbuf )
| '"' ->
    let c = default_cxt lexbuf in
    let old = lexbuf.lex_start_p in
    begin
      push_loc_cont c lexbuf lex_string;
      (`Str (buff_contents c), old --  lexbuf.lex_curr_p )
    end
| "'" (newline as x) "'" ->
    begin
      update_loc   lexbuf ~retract:1;
      (`Chr x, !! lexbuf)
    end
| "'" (ocaml_char as x ) "'" -> (`Chr x , !! lexbuf )
| "'\\" (_ as c) -> 
    err (Illegal_escape (String.make 1 c)) @@ !! lexbuf
| "#" | "|" | "^" | "<" | "->" |"="  |"_" | "*" | "["
|"]" | "*" | "?" | "+" | "(" | ")" | "-" as x ->
    (`Sym x, !! lexbuf)
| ocaml_blank + -> token lexbuf 
      (* comment *)      
| "(*" ->
    let c = default_cxt lexbuf in
    begin
      store c lexbuf;
      push_loc_cont c lexbuf lex_comment;
      token lexbuf 
    end
| "(*)" ->
    let c = default_cxt lexbuf in
    begin 
      warn Comment_start (!! lexbuf) ;
      lex_comment c lexbuf;
      token lexbuf 
    end
      (* quotation handling *)
| "{" -> (* border not included *)
    let old = lexbuf.lex_start_p in
    let c  = default_cxt lexbuf in
    begin
      lex_simple_quotation   c lexbuf;
      let loc=old--lexbuf.lex_curr_p in
      (`Quot {Ftoken.name=Ftoken.empty_name;
              meta=None;
              content = buff_contents c ;
              shift = 1;
              loc},loc)
    end
| eof ->
    let pos = lexbuf.lex_curr_p in (* FIXME *)
    (lexbuf.lex_curr_p <-
      { pos with pos_bol  = pos.pos_bol  + 1 ;
        pos_cnum = pos.pos_cnum + 1 };
     (`EOI, !!lexbuf ))
    
| _ as c ->  err (Illegal_character c) @@  !!lexbuf |}
  

let from_lexbuf lb = Fstream.from (fun _ -> Some (token lb))

let from_stream (loc:FLoc.t) strm =
  let () = Lexing_util.clear_stack () in
  let lb = Lexing.from_function (lexing_store strm) in begin
    lb.lex_abs_pos <- loc.loc_start.pos_cnum;
    lb.lex_curr_p <- loc.loc_start;
    from_lexbuf  lb
  end

(* local variables: *)
(* compile-command: "cd .. && pmake hot_annot/lex_lex.cmo" *)
(* end: *)
