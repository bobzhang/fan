


{

(** A lexical analyzer. *)


(* type context =
{ loc        : FanLoc.t    ;
  in_comment : bool     ;
   |+* FIXME When true, all lexers built by [Plexer.make ()] do not lex the
       quotation syntax any more. Default is false (quotations are
       lexed). +|
  quotations : bool     };

let default_context : context;

let mk : FanLoc.t -> Stream.t char -> Stream.t (Token.t * FanLoc.t);

let mk' : context -> Stream.t char -> Stream.t (Token.t * FanLoc.t);              *)
(* FIXME Beware the context argument must be given like that:
 * mk' { (default_context) with ... = ... } strm
 *)

open FanUtil
open LibUtil  
open Format  
open Lexing

type lex_error  =
  | Illegal_character of char
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
  | Literal_overflow of string

exception Lexing_error  of lex_error

let print_lex_error ppf =  function
  | Illegal_character c ->
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
      fprintf ppf "this is not the end of a comment"

            
let lex_error_to_string = to_string_of_printer print_lex_error
let _ =
  Printexc.register_printer (function
    | Lexing_error e -> Some (lex_error_to_string e)
    | _ -> None )    


let debug = ref false
let opt_char_len  = function
  | Some _ -> 1
  | None -> 0

let print_opt_char fmt = function
  | Some c ->fprintf fmt "Some %c" c
  | None -> fprintf fmt "None"
module Stack=struct   
  include Stack
  let push v stk= begin 
    if!debug then Format.eprintf "Push %a@." print_opt_char v else ();
    push v stk
  end 
  let pop stk = begin
    if !debug then Format.eprintf "Pop %a@." print_opt_char (top stk);
    pop stk
  end 
end

(* the trailing char after "<<" *)    
let opt_char : char option Stack.t = Stack.create ()
let turn_on_quotation_debug () = debug:=true
let turn_off_quotation_debug () = debug:=false
let clear_stack () = Stack.clear opt_char 
let show_stack () = begin
  eprintf "stack expand to check the error message@.";
  Stack.iter (Format.eprintf "%a@." print_opt_char ) opt_char 
end
    
    


(* To store some context information:
 *   loc       : position of the beginning of a string, quotation and comment
 *   in_comment: are we in a comment?
 *   quotations: shall we lex quotation?
 *               If quotations is false it's a SYMBOL token.
 *   antiquots : shall we lex antiquotations.
 *)

type context =
    { loc        : FanLoc.t    ;
      in_comment : bool     ;
      quotations : bool     ;
      antiquots  : bool     ;
      lexbuf     : lexbuf   ;
      buffer     : Buffer.t }
      
let default_context lb =
  { loc        = FanLoc.ghost ;
    in_comment = false     ;
    quotations = true      ;
    antiquots  = false     ;
    lexbuf     = lb        ;
    buffer     = Buffer.create 256 }
    

(* To buffer string literals, quotations and antiquotations *)

let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
let istore_char c i = Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)
let buff_contents c =
  let contents = Buffer.contents c.buffer in
  Buffer.reset c.buffer; contents
    
let loc c = FanLoc.merge c.loc (FanLoc.of_lexbuf c.lexbuf)
let quotations c = c.quotations
let antiquots c = c.antiquots
let is_in_comment c = c.in_comment
let in_comment c = { (c) with in_comment = true }

(* update the lexing position to the loc *)    
let set_start_p c = c.lexbuf.lex_start_p <- FanLoc.start_pos c.loc

(* [unsafe] shift the lexing buffer, usually shift back *)    
let move_start_p shift c =
  c.lexbuf.lex_curr_pos <- c.lexbuf.lex_curr_pos + shift
  (* let p = c.lexbuf.lex_start_p in *)
  (* c.lexbuf.lex_start_p <- { (p) with pos_cnum = p.pos_cnum + shift } *)
      
(* create a new context with  the location of the context for the lexer
   the old context was kept *)      
let with_curr_loc lexer c =
  lexer ({c with loc = FanLoc.of_lexbuf c.lexbuf}) c.lexbuf
    
let parse_nested ~lexer c = begin 
  with_curr_loc lexer c;
  set_start_p c;
  buff_contents c
end

let shift n c = { (c) with loc = FanLoc.move `both n c.loc }

let store_parse f c =  begin
  store c ; f c c.lexbuf
end

let parse f c =
  f c c.lexbuf
let mk_quotation quotation c ~name ~loc ~shift ~retract =
  let s = parse_nested ~lexer:quotation ({c with loc = FanLoc.of_lexbuf c.lexbuf}) in
  let contents = String.sub s 0 (String.length s - retract) in
  `QUOTATION {
  FanSig.q_name     = name     ;
  q_loc      = loc      ;
  q_shift    = shift    ;
  q_contents = contents }
    


(* Update the current location with file name and line number. *)

let update_loc   ?file ?(absolute=false) ?(retract=0) ?(line=1)  c  =
  let lexbuf = c.lexbuf in
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
  | None -> pos.pos_fname
  | Some s -> s in
  lexbuf.lex_curr_p <- { pos with
                         pos_fname = new_file;
                         pos_lnum = if absolute then line else pos.pos_lnum + line;
                         pos_bol = pos.pos_cnum - retract;
                       }
	

	


let err (error:lex_error) (loc:FanLoc.t) =
  raise(FanLoc.Exc_located(loc, Lexing_error error))
    
let warn error loc =
  Format.eprintf "Warning: %a: %a@." FanLoc.print loc print_lex_error error

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let ident = (lowercase|uppercase) identchar*
let locname = ident
let lident = lowercase identchar *
let uident = uppercase identchar *
    
let not_star_symbolchar =
  ['$' '!' '%' '&' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '\\']
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
    (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
  
(* Delimitors are extended (from 3.09) in a conservative way *)

(* These chars that can't start an expression or a pattern: *)
let safe_delimchars = ['%' '&' '/' '@' '^']
    
(* These symbols are unsafe since "[<", "[|", etc. exsist. *)
let delimchars = safe_delimchars | ['|' '<' '>' ':' '=' '.']

let left_delims  = ['(' '[' '{']
let right_delims = [')' ']' '}']
    
let left_delimitor =
(* At least a safe_delimchars *)
  left_delims delimchars* safe_delimchars (delimchars|left_delims)*
   (* A '(' or a new super '(' without "(<" *)
  | '(' (['|' ':'] delimchars*)?
  (* Old brackets, no new brackets starting with "[|" or "[:" *)
  | '[' ['|' ':']?
   (* Old "[<","{<" and new ones *)
  | ['[' '{'] delimchars* '<'
   (* Old brace and new ones *)
   | '{' (['|' ':'] delimchars*)?

let right_delimitor =
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

    
rule token c = parse
       | newline                            { update_loc c  ; `NEWLINE }
       | blank + as x                                                   { `BLANKS x }
       | "~" (lowercase identchar * as x) ':'                            { `LABEL x }
       | "?" (lowercase identchar * as x) ':'                         { `OPTLABEL x }
       | lowercase identchar * as x                                     { `LID x }
       | uppercase identchar * as x                                     { `UID x }
       | int_literal as i
           { try  `INT(cvt_int_literal i, i)
           with Failure _ -> err (Literal_overflow "int") (FanLoc.of_lexbuf lexbuf) }
       | float_literal as f
           { try  `FLOAT(float_of_string f, f)
           with Failure _ -> err (Literal_overflow "float") (FanLoc.of_lexbuf lexbuf) }
       | (int_literal as i) "l"
           { try `INT32(cvt_int32_literal i, i)
           with Failure _ -> err (Literal_overflow "int32") (FanLoc.of_lexbuf lexbuf) }
       | (int_literal as i) "L"
           { try  `INT64(cvt_int64_literal i, i)
           with Failure _ -> err (Literal_overflow "int64") (FanLoc.of_lexbuf lexbuf) }
       | (int_literal as i) "n"
           { try `NATIVEINT(cvt_nativeint_literal i, i)
           with Failure _ -> err (Literal_overflow "nativeint") (FanLoc.of_lexbuf lexbuf) }
       | '"'
           { with_curr_loc string c;
             let s = buff_contents c in `STR (TokenEval.string s, s)             }
       | "'" (newline as x) "'"
           { update_loc c  ~retract:1; `CHAR (TokenEval.char x, x)               }
       | "'" ( [^ '\\' '\010' '\013'] | '\\' (['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']
               |['0'-'9'] ['0'-'9'] ['0'-'9'] |'x' hexa_char hexa_char)  as x) "'"
           { `CHAR (TokenEval.char x, x) }
       | "'\\" (_ as c)
           { err (Illegal_escape (String.make 1 c)) (FanLoc.of_lexbuf lexbuf)         }
       | "(*"
           { store c; `COMMENT(parse_nested ~lexer:comment (in_comment c))                 }
       | "(*)"
           { warn Comment_start (FanLoc.of_lexbuf lexbuf)                             ;
             parse comment (in_comment c); `COMMENT (buff_contents c)               }
       | "*)"
           { warn Comment_not_end (FanLoc.of_lexbuf lexbuf)                           ;
             move_start_p (-1) c; `SYMBOL "*"                                       }
       (* | "<<" (extra_quot as p)? (quotchar* as beginning) *)
       | "{|" (extra_quot as p)? (quotchar* as beginning)
           { if quotations c  then
             (
              (* prerr_endline beginning; *)
              (* Format.eprintf "%s :%d@." beginning (-String.length beginning); *)
              move_start_p (-String.length beginning) c; (* FIX partial application*)
              Stack.push p opt_char;
              let len = 2 + opt_char_len p in 
              mk_quotation quotation c ~name:"" ~loc:"" ~shift:len ~retract:len)
           else parse
               (symbolchar_star
                  ((* "<<" ^ *)
                   "{|" ^
                   (match p with Some x -> String.make 1 x | None -> "")
                   ^ beginning))
               c                       }
       (* | "{||}" *)
       | "{||}"
           { if quotations c
           then `QUOTATION { FanSig.q_name = ""; q_loc = ""; q_shift = 2; q_contents = "" }
           else parse
               (symbolchar_star
                  (* "{||}" *)
                  "{||}"
               ) c                                   }
       (* | "<@" *)
       | "{@"
           { if quotations c then with_curr_loc maybe_quotation_at c
           else parse
               (symbolchar_star
                  "{@"
                         (* "<@" *)) c                                     }
       (* | "<:" *)
       | "{:"
           { if quotations c then with_curr_loc maybe_quotation_colon c
           else parse
               (symbolchar_star
                  (* "<:" *)
                  "{:"
               ) c                                     }
       | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
           ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
           [^ '\010' '\013'] * newline
           { let inum = int_of_string num in
           update_loc c ?file:name ~line:inum ~absolute:true ; `LINE_DIRECTIVE(inum, name)            }
       | '(' (not_star_symbolchar as op) ')'
           { `ESCAPED_IDENT (String.make 1 op) }
       | '(' (not_star_symbolchar symbolchar* not_star_symbolchar as op) ')'
           { `ESCAPED_IDENT op }
       | '(' (not_star_symbolchar symbolchar* as op) blank+ ')'
           { `ESCAPED_IDENT op }
       | '(' blank+ (symbolchar* not_star_symbolchar as op) ')'
           { `ESCAPED_IDENT op }
       | '(' blank+ (symbolchar+ as op) blank+ ')'
           { `ESCAPED_IDENT op }
       | ( "#"  | "`"  | "'"  | ","  | "."  | ".." | ":"  | "::"
           | ":=" | ":>" | ";"  | ";;" | "_"
           | left_delimitor | right_delimitor ) as x  { `SYMBOL x }
       | '$'
           {
            if antiquots c
            then with_curr_loc dollar (shift 1 c)
            else parse (symbolchar_star "$") c }
       | ['~' '?' '!' '=' '<' '>' '|' '&' '@' '^' '+' '-' '*' '/' '%' '\\'] symbolchar *
           as x { `SYMBOL x }
       | eof
           {
            let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <- { pos with pos_bol  = pos.pos_bol  + 1 ;
                                   pos_cnum = pos.pos_cnum + 1 };
            `EOI (* raise Stream.Failure*)      }
       | _ as c                 { err (Illegal_character c) (FanLoc.of_lexbuf lexbuf) }

and comment c = parse
    "(*"  { store c;
            with_curr_loc comment c;
            parse comment c
          }
     | "*)"   { store c }
     (* | '<' (':' ident)? ('@' locname)? '<' (extra_quot as p)? *)
     | '{' (':' ident)? ('@' locname)? '|' (extra_quot as p)?
         { store c;
           if quotations c then begin
             Stack.push p opt_char;
             with_curr_loc quotation c;
             parse comment c ;
           end}
     | ident                                             { store_parse comment c }
     | "\"" { store c;
           begin
             try with_curr_loc string c
             with FanLoc.Exc_located(_, Lexing_error Unterminated_string) ->
               err Unterminated_string_in_comment (loc c)
           end;
      Buffer.add_char c.buffer '"';
      parse comment c }
     | "''"                                              { store_parse comment c }
     | "'''"                                             { store_parse comment c }
     | "'" newline "'"
         { update_loc c ~retract:1; store_parse comment c                      }
     | "'" [^ '\\' '\'' '\010' '\013' ] "'"              { store_parse comment c }
     | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"     { store_parse comment c }
     | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"           { store_parse comment c }
     | "'\\" 'x' hexa_char hexa_char "'"                 { store_parse comment c }
     | eof
         { err Unterminated_comment (loc c)                                        }
     | newline
         { update_loc c ; store_parse comment c                      }
     | _                                                 { store_parse comment c }

and string c = parse
    '"'                                                       { set_start_p c }
    | '\\' newline ([' ' '\t'] * as space)
        { update_loc c  ~retract:(String.length space);
          store_parse string c                                                  }
    | '\\' ['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']           { store_parse string c }
    | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']                 { store_parse string c }
    | '\\' 'x' hexa_char hexa_char                       { store_parse string c }
    | '\\' (_ as x)
        {
         if is_in_comment c then
           store_parse string c
         else begin
           warn (Illegal_escape (String.make 1 x)) (FanLoc.of_lexbuf lexbuf);
           store_parse string c
         end }
    | newline
        { update_loc c ; store_parse string c                       }
    | eof                                     { err Unterminated_string (loc c) }
    | _                                                  { store_parse string c }

and symbolchar_star beginning c = parse
    | symbolchar* as tok            {
      (* move_start_p (-String.length beginning) c ; *)
      `SYMBOL(beginning ^ tok) }

(* <@loc< *)        
and maybe_quotation_at c = parse
    | (ident as loc) (* '<' *) '|' (extra_quot as p)?     {
         Stack.push p opt_char;
         mk_quotation quotation c ~name:"" ~loc
           ~shift:(2 + 1 + String.length loc + (opt_char_len p))
           ~retract:(2 + opt_char_len p)
       }
        (* { mk_quotation quotation c "" loc (1 + String.length loc)                 } *)
    | symbolchar* as tok
        { `SYMBOL((* "<@" *)"{@" ^ tok) }

(* <:name< *)        
and maybe_quotation_colon c = parse
    | (ident as name) (* '<' *) '|' (extra_quot as p)?  { begin 
        Stack.push p opt_char;
        mk_quotation quotation c
          ~name ~loc:""  ~shift:(2 + 1 + String.length name + (opt_char_len p))
          ~retract:(2 + opt_char_len p)
    end
    }
        (* { mk_quotation quotation c name "" (1 + String.length name)               } *)
    | (ident as name) '@' (locname as loc) (* '<' *) '|' (extra_quot as p)? { begin 
        Stack.push p opt_char;
        mk_quotation quotation c ~name ~loc
          ~shift:(2 + 2 + String.length loc + String.length name + opt_char_len p)
          ~retract:(2 + opt_char_len p)
      end}
   
        (* { mk_quotation quotation c name loc *)
        (* (2 + String.length loc + String.length name)               } *)
    | symbolchar* as tok                                   { `SYMBOL("<:" ^ tok) }

and quotation c = parse
    (* | '<' (':' ident)? ('@' locname)? '<' (extra_quot as p)? *)
    | '{' (':' ident)? ('@' locname)? '|' (extra_quot as p)?
        {
         begin
        store c ;
        Stack.push p opt_char; (* take care the order matters*)
        with_curr_loc quotation c ;
        parse quotation c
         end}
    | (extra_quot as p)? "|}"  {
      if not (Stack.is_empty opt_char) then
        let top = Stack.top opt_char in
        if p <> top then
          store_parse quotation c (*move on*)
        else begin
          ignore (Stack.pop opt_char);
          store c
        end
      else
        store_parse quotation c;
    }
    | "\"" {store c;
            begin
              try with_curr_loc string c
              with FanLoc.Exc_located(_,Lexing_error Unterminated_string) ->
                err Unterminated_string_in_quotation (loc c)
            end;
            Buffer.add_char c.buffer '"';
            parse quotation c
          }
    | eof {show_stack (); err Unterminated_quotation (loc c)}
    | newline                                     {
      update_loc c ;
      store_parse quotation c }
    | _
        {
         (* Format.eprintf "char in quotations %c@." (Lexing.lexeme_char  lexbuf 0); *)
         store_parse quotation c }
(*
  $lid:ident
  $ident
  $(lid:ghohgosho)
  $(....)
  $(....)
 *)
and dollar c = parse
    | ('`'? (identchar*|['.' '!']+) as name) ':' (lident as x)
        {set_start_p c; `ANTIQUOT(name,x)}
        (* { with_curr_loc (antiquot name) (shift (1 + String.length name) c)        } *)
    | lident as x 
        {set_start_p c; `ANTIQUOT("",x)}
    (* | '$' {store_parse (antiquot "") c} *)
    | '(' ('`'? (identchar*|['.' '!']+) as name) ':' {
      with_curr_loc (antiquot name 0) (shift (2 + String.length name) c)
      }
        
    | '(' {
      with_curr_loc (antiquot "" 0) (shift 1 c)
     }
    (* | '$'                                     { set_start_p c; `ANTIQUOT("", "") } *)
    (* | ('`'? (identchar*|['.' '!']+) as name) ':' *)
    (*     { with_curr_loc (antiquot name) (shift (1 + String.length name) c)        } *)
    | _ as c {
      err (Illegal_character c) (FanLoc.of_lexbuf lexbuf) (*unexpected char in antiquot*)
     }
        
      (* { store_parse (antiquot "") c } *)

(* depth makes sure the parentheses are balanced *)
and antiquot name depth c  = parse
    | ')'                      {
      if depth = 0 then
        let () = set_start_p c in
        `ANTIQUOT(name, buff_contents c)
      else store_parse (antiquot name (depth-1)) c }
    | '(' {
      store_parse (antiquot name (depth+1)) c
      }
        
    | eof                                   { err Unterminated_antiquot (loc c) }
    | newline
        { update_loc c ; store_parse (antiquot name depth) c              }
    (* | '<' (':' ident)? ('@' locname)? '<' (extra_quot as p)? *)
    | '{' (':' ident)? ('@' locname)? '|' (extra_quot as p)?
        {
      let () = Stack.push p opt_char in
      let () = store c in
      let () = with_curr_loc quotation c in
      parse (antiquot name depth) c 
      }
    | "\"" { store c ;
             begin
               try with_curr_loc string c
               with FanLoc.Exc_located (_,Lexing_error Unterminated_string) ->
                 err Unterminated_string_in_antiquot (loc c)
             end;
             Buffer.add_char c.buffer '"';
             parse (antiquot name depth) c 
      }
        
        (* { store c; with_curr_loc quotation c; *)
        (*   parse (antiquot name) c             } *)
    | _                                         { store_parse (antiquot name depth) c }





