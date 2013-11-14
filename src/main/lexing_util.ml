

(** FIXME, some should be pre-registered, and unused regex warnings are preferred  *)
%regex{

let locname = ident
let quotation_prefix =
  '%'? '%' quotation_name? ('@' locname )? "{"

let lident = lowercase identchar *
let antifollowident =   identchar +   
};;

%import{
Format:
  fprintf
  eprintf
  ;
};;



(** put elements from stream to string with offset 0 and [max] elements *)  
let lexing_store s buff max =
   let  self n s =
     if n >= max then n
     else
       match Streamf.peek s with
       | Some x -> (Streamf.junk s; buff.[n] <- x; n + 1)
       | _ -> n in 
   self 0 s
    
type lex_error  =
  | Illegal_character of char
  | Illegal_escape    of string
  | Illegal_quotation of string
  | Illegal_antiquote 
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_quotation
  | Unterminated_antiquot
  | Comment_start
  | Comment_not_end

exception Lexing_error  of lex_error

let print_lex_error ppf e =
  match e with
  | Illegal_antiquote ->
      fprintf ppf "Illegal_antiquote"
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_quotation s ->
      fprintf ppf "Illegal quotation (%s)" (String.escaped s)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_quotation ->
      fprintf ppf "Quotation not terminated"
  | Unterminated_antiquot ->
      fprintf ppf "Antiquotation not terminated"
  | Comment_start ->
      fprintf ppf "this is the start of a comment"
  | Comment_not_end ->
      fprintf ppf "this is not the end of a comment"
            
let lex_error_to_string = Formatf.to_string print_lex_error

    
let debug = ref false

let opt_char_len  = function
  | Some _ -> 1
  | None -> 0

let print_opt_char fmt = function
  | Some c ->fprintf fmt "Some %c" c
  | None -> fprintf fmt "None"
        
let turn_on_quotation_debug () = debug:=true
let turn_off_quotation_debug () = debug:=false


type context = { mutable loc : Locf.position list; buffer : Buffer.t; }

let new_cxt () = {
  loc = [];
  buffer = Buffer.create 256;
}      





(*************************************)
(*    local operators                *)    
(*************************************)        
let (++) = Buffer.add_string       
let (+>) = Buffer.add_char
(** get the location of current the lexeme *)
let from_lexbuf = Location_util.from_lexbuf    
let (!!)  = Location_util.from_lexbuf
    
let (--) = Location_util.(--)

    
(** To buffer string literals, quotations and antiquotations
    store the current lexeme *)
let store c lexbuf =
  c.buffer ++ Lexing.lexeme lexbuf

let with_store c lexbuf f =  
  begin
    store c  lexbuf;
    f c lexbuf
  end
                                  
let buff_contents c =
  let contents = Buffer.contents c.buffer in
  begin
    Buffer.reset c.buffer;
    contents
  end
    




(** [unsafe] shift the lexing buffer, usually shift back *)    
let move_curr_p shift  (lexbuf:Lexing.lexbuf)  =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + shift

      

let push_loc_cont c lexbuf lexer =
  begin
    c.loc <- Lexing.lexeme_start_p lexbuf :: c.loc;
    lexer c lexbuf
  end
(** unsafe, [@raise Failure] *)
let pop_loc c =
    c.loc <- List.tl c.loc
let null_loc c =
  c.loc = []
    


(** Update the current location with file name and line number.
   change [pos_fname] [pos_lnum] and [pos_bol],
   default behavior is adding a newline
    [retract] is only used for ocaml convention
    for example

    {[
    '
    '
    bol would require retract one chars  when parsing it as a whole
    ]}
 *)
let update_loc ?file ?(absolute=false) ?(retract=0) ?(line=1)   (lexbuf:Lexing.lexbuf)  =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
  | None -> pos.pos_fname
  | Some s -> s in
  lexbuf.lex_curr_p <-
    { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - retract;}
      
let err (error:lex_error) (loc:Locf.t) =
  raise (Locf.Exc_located(loc, Lexing_error error))

let warn error (loc:Locf.t) =
  Fan_warnings.emitf loc.loc_start "Warning: %s"  @@ lex_error_to_string error


(** return unit. All the comments are stored in the buffer
    Note that comment is isolate to quotation and antiquotation.
    The function itself already simulate its stack, and it will not distrub the stack
    since when comment token is lexed. The stack is returned back to normal
 *)
let rec lex_comment c = %lex{
  |"(*"  %{
      begin
      store c lexbuf ;
      push_loc_cont c lexbuf lex_comment;
      lex_comment c lexbuf
    end}
  | "*)"  %{
    begin
      store c lexbuf;
      pop_loc c ;
    end}
  | newline %{
    begin
      update_loc  lexbuf ;
      with_store  c lexbuf lex_comment;
    end}
  | eof %{
    err Unterminated_comment  @@ (* FIXME *)
    Location_util.of_positions (List.hd c.loc) lexbuf.lex_curr_p}
  | _ %{ with_store c lexbuf lex_comment}}


(** called by another lexer
    | '"' -> ( with_curr_loc  c string; let s = buff_contents c in `Str s )
    c.loc keeps the start position of "ghosgho"
    c.buffer keeps the lexed result
 *)    
let rec lex_string c = %lex{
  | '"' %{  pop_loc c}
  | '\\' newline ([' ' '\t'] * as space) %{
    (* Follow the ocaml convention, these characters does not take positions *)
    begin
      update_loc  lexbuf  ~retract:(String.length space);
      lex_string c lexbuf
    end}
  | ocaml_escaped_char %{ with_store  c lexbuf lex_string}
  | '\\' (_ as x) %{
    begin
      warn
        (Illegal_escape (String.make 1 x)) @@ Location_util.from_lexbuf lexbuf;
      with_store c lexbuf lex_string
    end}
  | newline %{
    begin
      update_loc  lexbuf;
      with_store  c lexbuf lex_string
    end}
  | eof %{  err Unterminated_string @@ Location_util.of_positions (List.hd c.loc) lexbuf.lex_curr_p}
  | _ %{  with_store  c lexbuf lex_string}}


let rec lex_quotation c = %lex{
  | quotation_prefix %{
    begin
      store c lexbuf ;
      push_loc_cont c lexbuf lex_quotation;
      lex_quotation c lexbuf
    end}
      
  | "}" %{
    begin
      store c lexbuf;
      pop_loc c
    end}
  | "{" %{
    begin
      store c lexbuf;
      push_loc_cont c lexbuf lex_quotation;
      lex_quotation c lexbuf
    end}
      
  | "(*" %{
    begin
      store c lexbuf;
      push_loc_cont c lexbuf lex_comment;
      lex_quotation c lexbuf
    end}
  | newline %{
    begin
      update_loc  lexbuf ;
      with_store c lexbuf lex_quotation 
    end   }   
  | "\"" %{ (* treat string specially, like %{ "{|"} should be accepted *)
    begin
      store c lexbuf;
      push_loc_cont c lexbuf lex_string;
      Buffer.add_char c.buffer '"';
      lex_quotation c lexbuf
    end}
  | eof %{
    begin
      err Unterminated_quotation @@
      Location_util.of_positions (List.hd c.loc) lexbuf.lex_curr_p
    end}
  | "'" ocaml_char "'" %{
    (* treat  char specially, otherwise '"' would not be parsed  *) 
    with_store c lexbuf lex_quotation }
  | _ %{ with_store c lexbuf  lex_quotation }}


let adapt_to_stream token =
  fun (loc:Locf.t) strm ->
    let lb = Lexing.from_function (lexing_store strm) in begin
    lb.lex_abs_pos <- loc.loc_start.pos_cnum;
    lb.lex_curr_p <- loc.loc_start;
    Streamf.from (fun _ -> Some (token  lb))
  end



let _ =
  Printexc.register_printer @@ function
    | Lexing_error e -> Some (lex_error_to_string e)
    | _ -> None   


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lexing_util.cmo" *)
(* end: *)
