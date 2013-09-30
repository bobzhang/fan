
{:regex2| (** FIXME remove duplication later see lexing_util.cmo *)
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
  ( [^ '\\' '\010' '\013'] | ocaml_escaped_char)
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
  with_curr_loc
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
|};;    
(** It could also import regex in the future
    {:import|
    Lexing_util:
    with_curr_loc
    default_cxt 
    update_loc ;
   Location_util:
    (--)
    from_lexbuf as  (!!)
    lex_antiquot : {| a -> b -> c|}  as xx ;
   Buffer:
    add_string -> (++)
    add_char -> (+>) ;
   |}  *)    
let  token : Lexing.lexbuf -> (Ftoken.t * FLoc.t ) = {:lex2|
| newline {begin
    update_loc  lexbuf;
    (`NEWLINE, !! lexbuf )
  end}
| "~" (ocaml_lid as x) ':' { (`Label x, !! lexbuf )}

| "?" (ocaml_lid as x) ':' { (`Optlabel x, !! lexbuf)}
      
| ocaml_lid as x  {(`Lid x, !! lexbuf )}

| ocaml_uid as x  {(`Uid x , !! lexbuf )}

| int_literal  (('l'|'L'|'n' as s ) ?) as x
    {
    begin 
      let x = match s with
      | Some 'l' -> `Int32 x
      | Some 'L' -> `Int64 x
      | Some 'n' -> `Nativeint x
      | _ -> `Int x in
      ( x, !! lexbuf)
    end
   }
| float_literal as f  {(`Flo f, !! lexbuf )}       (** FIXME safety check *)
| '"' {
  let c = default_cxt lexbuf in
  let old = lexbuf.lex_start_p in
  begin
    with_curr_loc lex_string c  lexbuf;
    (`Str (buff_contents c), old --  lexbuf.lex_curr_p )
  end}
| "'" (newline as x) "'"
    {begin
      update_loc   lexbuf ~retract:1; 
      (`Chr x, !! lexbuf)
    end}

| "'" (ocaml_char as x ) "'"  { (`Chr x , !! lexbuf )}

| "'\\" (_ as c) 
    {err (Illegal_escape (String.make 1 c)) @@ !! lexbuf}

| '(' (not_star_symbolchar symbolchar* as op) ocaml_blank* ')' 
    { (`Eident op , !! lexbuf)}
| '(' ocaml_blank+ (symbolchar+ as op) ocaml_blank* ')' 
    { (`Eident op, !! lexbuf)}
| '(' ocaml_blank* ("or"|"mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as op) ocaml_blank* ')' 
    { (`Eident op, !! lexbuf)}
| ( "#"  | "`"  | "'"  | ","  | "."  | ".." | ":"  | "::"
  | ":=" | ":>" | ";"  | ";;" | "_" | "{"|"}"
  | left_delimitor | right_delimitor | "{<" |">}"
  | ['~' '?' '!' '=' '<' '>' '|' '&' '@' '^' '+' '-' '*' '/' '%' '\\'] symbolchar * )
    as x  
      { (`Sym x  , !! lexbuf)}

| "*)" 
    {begin
      warn Comment_not_end (!! lexbuf) ;
      move_curr_p (-1) lexbuf;
      (`Sym "*", !! lexbuf)
    end}
| ocaml_blank + as x  {(`Blank x, !! lexbuf)}
      
      (* comment *)      
| "(*" 
    {let c = default_cxt lexbuf in
    let old = lexbuf.lex_start_p in
    begin
      store c lexbuf;
      with_curr_loc lex_comment c lexbuf;
      (`Comment ( buff_contents c),
       old -- lexbuf.lex_curr_p)
    end}
| "(*)" 
    {let c = default_cxt lexbuf in 
    let old = lexbuf.lex_start_p in
    begin 
      warn Comment_start (!! lexbuf) ;
      lex_comment c lexbuf;
      ( `Comment (buff_contents c),
        old -- lexbuf.lex_curr_p)
    end}
      (* quotation handling *)
| "{||}"
    {let loc  =     !! lexbuf in
    (`Quot { Ftoken.name=Ftoken.empty_name; meta=None; shift=2; content="";loc },loc)}
      
| "{" (":" (quotation_name as name))? ('@' (locname as meta))? '|' (extra_quot as p)? 
    {let c = default_cxt lexbuf in
    let (name,len) =
      match name with
      | Some name -> (Ftoken.name_of_string name,1 + String.length name)
      | None -> (Ftoken.empty_name,0)  in 
    let v = opt_char_len p in
    let shift = 2 + len + v  + (match meta with  | Some x -> String.length x + 1 | None -> 0) in
    let retract = 2 + v in
    begin
      Stack.push p opt_char;
      mk_quotation lex_quotation c lexbuf ~name ~meta ~shift ~retract 
    end}
| ("{:" | "{@" ) _ as c { err (Illegal_quotation c) @@  !!lexbuf}

|"#{:" (quotation_name as name) '|'  (extra_quot as p)? {
    let c  = default_cxt lexbuf in
    let len = String.length name in
    let () = Stack.push p opt_char in
    let retract = opt_char_len p + 2 in 
    let old = lexbuf.lex_start_p in
    let s =
      (with_curr_loc lex_quotation c lexbuf;
       buff_contents c) in

    let contents = String.sub s 0 (String.length s - retract) in
    (`DirQuotation(3+1 +len +(opt_char_len p), name,contents),
     old -- lexbuf.lex_curr_p)
  }
| "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
    ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
    [^ '\010' '\013'] * newline
    {
      let line = int_of_string num in begin 
        update_loc  lexbuf ?file:name ~line ~absolute:true ;
        (`LINE_DIRECTIVE(line, name), !!lexbuf )
      end
   }
        (* Antiquotation handling *)        
| '$'
    {
    let  dollar (c:Lexing_util.context) = {:lex2|
    | ('`'? (identchar* |['.' '!']+) as name) ':' (antifollowident as x)
        {begin
          let old = FLoc.move_pos (String.length name + 1) lexbuf.lex_start_p in
          (`Ant(name,x), old -- lexbuf.lex_curr_p)
        end}
    | lident as x  { (`Ant("",x), !!lexbuf)}
    | '(' ('`'? (identchar* |['.' '!']+) as name) ':'  (* $(lid:ghohgosho)  )*)
        (* the first char is faked '(' to match the last ')', so we mvoe
           backwards one character *)
        {
        let old = FLoc.move_pos (1+1+1+String.length name - 1) c.loc in
        begin
          c.buffer +> '(';
          lex_antiquot {c with loc = old} lexbuf ;
          (`Ant(name,buff_contents c),
           old -- Lexing.lexeme_end_p lexbuf)
        end
       }
    | '('      (* $(xxxx)*)
        {
         let old = FLoc.move_pos  (1+1-1) c.loc in
         begin
           c.buffer +> '(';
           lex_antiquot   {c with loc = old } lexbuf ;
           (`Ant("", buff_contents c ), old -- Lexing.lexeme_end_p lexbuf)
         end}
    | _ as c
        {err (Illegal_character c) (!! lexbuf) } |} in
    let c = default_cxt lexbuf in
    if  !FConfig.antiquotations then  (* FIXME maybe always lex as antiquot?*)
      with_curr_loc dollar c lexbuf
    else err Illegal_antiquote (!! lexbuf)
   }
| eof { 
  let pos = lexbuf.lex_curr_p in (* FIXME *)
  (lexbuf.lex_curr_p <-
    { pos with pos_bol  = pos.pos_bol  + 1 ;
      pos_cnum = pos.pos_cnum + 1 }; 
   (`EOI, !!lexbuf ))
    
| _ as c { 
  err (Illegal_character c) @@  !!lexbuf } |}


    
let from_lexbuf lb : (Ftoken.t * FLoc.t ) Fstream.t =
  let next _ = Some (token lb)  in (* this requires the [lexeme_start_p] to be correct ...  *)
  Fstream.from next


(* local variables: *)
(* compile-command: "fan_hot_cold -printer o test_lex2.ml" *)
(* end: *)
