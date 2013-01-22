(* open FanUtil; *)
open LibUtil;  
open Format;  
open Lexing;

type lex_error  =
  [ Illegal_character of char
  | Illegal_escape    of string
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_quotation
  | Unterminated_antiquot
  | Unterminated_string_in_comment
  | Unterminated_string_in_quotation
  | Unterminated_string_in_antiquot
  | Comment_start
  | Comment_not_end
  | Literal_overflow of string];

exception Lexing_error  of lex_error;

let print_lex_error ppf =  function
  [ Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment ->
      fprintf ppf "This comment contains an unterminated string literal"
  | Unterminated_string_in_quotation ->
      fprintf ppf "This quotation contains an unterminated string literal"
  | Unterminated_string_in_antiquot ->
      fprintf ppf "This antiquotaion contains an unterminated string literal"
  | Unterminated_quotation ->
      fprintf ppf "Quotation not terminated"
  | Unterminated_antiquot ->
      fprintf ppf "Antiquotation not terminated"
  | Literal_overflow ty ->
      fprintf ppf "Integer literal exceeds the range of representable integers of type %s" ty
  | Comment_start ->
      fprintf ppf "this is the start of a comment"
  | Comment_not_end ->
      fprintf ppf "this is not the end of a comment"];

            
let lex_error_to_string = to_string_of_printer print_lex_error;

let _ =
  Printexc.register_printer (function
    [ Lexing_error e -> Some (lex_error_to_string e)
    | _ -> None] );    


let debug = ref false;
let opt_char_len  = function
  [ Some _ -> 1
  | None -> 0];

let print_opt_char fmt = function
  [ Some c ->fprintf fmt "Some %c" c
  | None -> fprintf fmt "None"];

module Stack=struct   
  include Stack;
  let push v stk= begin 
    if!debug then Format.eprintf "Push %a@." print_opt_char v else ();
    push v stk
  end ;
  let pop stk = begin
    if !debug then Format.eprintf "Pop %a@." print_opt_char (top stk)
    else ();
    pop stk
  end; 
end;

(* the trailing char after "<<" *)    
let opt_char : Stack.t  (option char)  = Stack.create ();

let turn_on_quotation_debug () = debug:=true ;
  
let turn_off_quotation_debug () =
  debug:=false;
let clear_stack () = Stack.clear opt_char ;
let show_stack () = begin
  eprintf "stack expand to check the error message@.";
  Stack.iter (Format.eprintf "%a@." print_opt_char ) opt_char 
end;

(* To store some context information:
 *   loc       : position of the beginning of a string, quotation and comment
 *   in_comment: are we in a comment?
 *   quotations: shall we lex quotation?
 *               If quotations is false it's a SYMBOL token.
 *   antiquots : shall we lex antiquotations.
 *)

type context =
    { loc        :  FanLoc.position ; (* FanLoc.t  ; *)
     (* only record the start position when enter into a quotation or antiquotation*)
      in_comment : bool     ;
      quotations : bool     ;
      antiquots  : bool     ;
      lexbuf     : lexbuf   ;
      buffer     : Buffer.t };
      
let default_context lb =
  { loc        = FanLoc.dummy_pos ;
    in_comment = false     ;
    quotations = true      ;
    antiquots  = false     ;
    lexbuf     = lb        ;
    buffer     = Buffer.create 256 };
    

(* To buffer string literals, quotations and antiquotations *)

let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf);
  
let istore_char c i = Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i);
let buff_contents c =
  let contents = Buffer.contents c.buffer in begin
  Buffer.reset c.buffer; contents
  end;
    
let loc_merge c =
  FanLoc.of_positions c.loc (Lexing.lexeme_end_p c.lexbuf);
  (* FanLoc.merge c.loc (FanLoc.of_lexbuf c.lexbuf) *)
let quotations c = c.quotations;
let antiquots c = c.antiquots;
let is_in_comment c = c.in_comment;
let in_comment c = { (c) with in_comment = true };

(* update the lexing position to the loc
  combined with [with_curr_loc]  *)    
let set_start_p c = c.lexbuf.lex_start_p <- (* FanLoc.start_pos *) c.loc;

(* [unsafe] shift the lexing buffer, usually shift back *)    
let move_curr_p shift c =
  c.lexbuf.lex_curr_pos <- c.lexbuf.lex_curr_pos + shift;
let move_start_p shift c =
  c.lexbuf.lex_start_p <- FanLoc.move_pos shift c.lexbuf.lex_start_p;
      
(* create a new context with  the location of the context for the lexer
   the old context was kept *)      
let with_curr_loc lexer c =
  lexer ({(c) with loc = Lexing.lexeme_start_p c.lexbuf }) c.lexbuf;
    
let parse_nested ~lexer c = begin 
  with_curr_loc lexer c;
  set_start_p c;
  buff_contents c
end;


let store_parse f c =  begin
  store c ; f c c.lexbuf
end;

let parse f c =
  f c c.lexbuf;
let mk_quotation quotation c ~name ~loc ~shift ~retract =
  let s = parse_nested ~lexer:quotation ({c with loc = Lexing.lexeme_start_p  c.lexbuf}) in
  let contents = String.sub s 0 (String.length s - retract) in
  `QUOTATION {FanToken.q_name     = name     ;
              q_loc      = loc      ;
              q_shift    = shift    ;
              q_contents = contents };
    


(* Update the current location with file name and line number. *)

let update_loc   ?file ?(absolute=false) ?(retract=0) ?(line=1)  c  =
  let lexbuf = c.lexbuf in
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
  [ None -> pos.pos_fname
  | Some s -> s] in
  lexbuf.lex_curr_p <-
    { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - retract;} ;
	
let err (error:lex_error) (loc:FanLoc.t) =
  raise(FanLoc.Exc_located(loc, Lexing_error error));
  
let warn error loc =
  Format.eprintf "Warning: %a: %a@." FanLoc.print loc print_lex_error error;





{:.Fan.Lang.Lex.reg|

newline:
  ('\010' | '\013' | "\013\010");
blank:
   [' ' '\009' '\012'];
lowercase:
   ['a'-'z' '\223'-'\246' '\248'-'\255' '_'];
uppercase:
   ['A'-'Z' '\192'-'\214' '\216'-'\222'];
identchar:
   ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9'];
ident:
   (lowercase|uppercase) identchar*;
quotation_name:
   ident ('.'ident)*;
locname: ident;
lident: lowercase identchar *;
antifollowident:  identchar +   ;
uident: uppercase identchar *;
    
not_star_symbolchar:
   ['$' '!' '%' '&' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '\\'];
symbolchar:'*' | not_star_symbolchar;

quotchar:  ['!' '%' '&' '+' '-' '.' '/' ':' '=' '?' '@' '^' '|' '~' '\\' '*'];
 
extra_quot:  ['!' '%' '&' '+' '-' '.' '/' ':' '=' '?' '@' '^'  '~' '\\'];
    (* FIX remove the '\' as extra quot*)
hexa_char:['0'-'9' 'A'-'F' 'a'-'f'];
 
decimal_literal:  ['0'-'9'] ['0'-'9' '_']*;
 
hex_literal:  '0' ['x' 'X'] hexa_char ['0'-'9' 'A'-'F' 'a'-'f' '_']*;
 
oct_literal:  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*;

bin_literal:  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*;
 
int_literal:  decimal_literal | hex_literal | oct_literal | bin_literal;

float_literal:
  ['0'-'9'] ['0'-'9' '_']*
    ('.' ['0'-'9' '_']* )?
    (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?;
  
(* Delimitors are extended (from 3.09) in a conservative way *)

(* These chars that can't start an expression or a pattern: *)
safe_delimchars:['%' '&' '/' '@' '^'];
    
(* These symbols are unsafe since "[<", "[|", etc. exsist. *)
delimchars:safe_delimchars | ['|' '<' '>' ':' '=' '.'];

left_delims:['(' '[' '{'];
right_delims:[')' ']' '}'];
    
left_delimitor:
(* At least a safe_delimchars *)
  left_delims delimchars* safe_delimchars (delimchars|left_delims)*
   (* A '(' or a new super '(' without "(<" *)
  | '(' (['|' ':'] delimchars*)?
  (* Old brackets, no new brackets starting with "[|" or "[:" *)
  | '[' ['|' ':']?
   (* Old "[<","{<" and new ones *)
  | ['[' '{'] delimchars* '<'
   (* Old brace and new ones *)
   | '{' (['|' ':'] delimchars*)?;

right_delimitor:
  (* At least a safe_delimchars *)
  (delimchars|right_delims)* safe_delimchars (delimchars|right_delims)* right_delims
    (* A ')' or a new super ')' without ">)" *)
   | (delimchars* ['|' ':'])? ')'
    (* Old brackets, no new brackets ending with "|]" or ":]" *)
   | ['|' ':']? ']'
    (* Old ">]",">}" and new ones *)
   | '>' delimchars* [']' '}']
    (* Old brace and new ones *)
   | (delimchars* ['|' ':'])? '}'
|};

(* #default_quotation "lex";;
   FIXME the error message of [default_quotation]
 *)

(* let rec token c = {| *)
(*  |newline -> update_loc c ; `NEWLINE *)
(*  |blank+ -> *)
(*      let x = Ulexing.latin1_lexeme lexbuf in *)
(*      `BLANKS x *)
(*  |("~"|"?") (lowercase identchar *\)  ':' -> *)
(*      let x = Ulexing.latin1_lexeme lexbuf in *)
(*      if x.[0] = '~' then `LABEL x *)
(*      else `OPTLABEL x *)
(*  |(lowercase|uppercase) identchar* -> *)
(*      let x = Ulexing.latin1_lexeme lexbuf in *)
(*      if Char.is_uppercase x.[0] then `Uid x *)
(*      else `Lid x *)
(*  | int_literal ('l'|'L'|'n')? -> *)
(*      let x = Ulexing.latin1_lexeme lexbuf in *)
(*      try `INT(cvt_int_literal x,x) *)
(*      with  [Failure _ -> err (Literal_overflow "int") (FanLoc.of_lexbuf lexbuf)] *)
(* |}; *)












