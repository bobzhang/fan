open LibUtil
open Format
open Lexing
type lex_error =  
  | Illegal_character of char
  | Illegal_escape of string
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
exception Lexing_error of lex_error
let print_lex_error ppf =
  function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment  -> fprintf ppf "Comment not terminated"
  | Unterminated_string  -> fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment  ->
      fprintf ppf "This comment contains an unterminated string literal"
  | Unterminated_string_in_quotation  ->
      fprintf ppf "This quotation contains an unterminated string literal"
  | Unterminated_string_in_antiquot  ->
      fprintf ppf "This antiquotaion contains an unterminated string literal"
  | Unterminated_quotation  -> fprintf ppf "Quotation not terminated"
  | Unterminated_antiquot  -> fprintf ppf "Antiquotation not terminated"
  | Literal_overflow ty ->
      fprintf ppf
        "Integer literal exceeds the range of representable integers of type %s"
        ty
  | Comment_start  -> fprintf ppf "this is the start of a comment"
  | Comment_not_end  -> fprintf ppf "this is not the end of a comment"
let lex_error_to_string = to_string_of_printer print_lex_error
let _ =
  Printexc.register_printer
    (function | Lexing_error e -> Some (lex_error_to_string e) | _ -> None)
let debug = ref false
let opt_char_len = function | Some _ -> 1 | None  -> 0
let print_opt_char fmt =
  function | Some c -> fprintf fmt "Some %c" c | None  -> fprintf fmt "None"
module Stack =
  struct
    include Stack
    let push v stk =
      if debug.contents
      then Format.eprintf "Push %a@." print_opt_char v
      else ();
      push v stk
    let pop stk =
      if debug.contents
      then Format.eprintf "Pop %a@." print_opt_char (top stk)
      else ();
      pop stk
  end
let opt_char: char option Stack.t = Stack.create ()
let turn_on_quotation_debug () = debug := true
let turn_off_quotation_debug () = debug := false
let clear_stack () = Stack.clear opt_char
let show_stack () =
  eprintf "stack expand to check the error message@.";
  Stack.iter (Format.eprintf "%a@." print_opt_char) opt_char
type context = 
  {
  loc: FanLoc.position;
  in_comment: bool;
  quotations: bool;
  antiquots: bool;
  lexbuf: lexbuf;
  buffer: Buffer.t} 
let default_context lb =
  {
    loc = FanLoc.dummy_pos;
    in_comment = false;
    quotations = true;
    antiquots = false;
    lexbuf = lb;
    buffer = (Buffer.create 256)
  }
let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
let istore_char c i =
  Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)
let buff_contents c =
  let contents = Buffer.contents c.buffer in Buffer.reset c.buffer; contents
let loc_merge c = FanLoc.of_positions c.loc (Lexing.lexeme_end_p c.lexbuf)
let quotations c = c.quotations
let antiquots c = c.antiquots
let is_in_comment c = c.in_comment
let in_comment c = { c with in_comment = true }
let set_start_p c = (c.lexbuf).lex_start_p <- c.loc
let move_curr_p shift c =
  (c.lexbuf).lex_curr_pos <- (c.lexbuf).lex_curr_pos + shift
let move_start_p shift c =
  (c.lexbuf).lex_start_p <- FanLoc.move_pos shift (c.lexbuf).lex_start_p
let with_curr_loc lexer c =
  lexer { c with loc = (Lexing.lexeme_start_p c.lexbuf) } c.lexbuf
let parse_nested ~lexer  c =
  with_curr_loc lexer c; set_start_p c; buff_contents c
let store_parse f c = store c; f c c.lexbuf
let parse f c = f c c.lexbuf
let mk_quotation quotation c ~name  ~loc  ~shift  ~retract  =
  let s =
    parse_nested ~lexer:quotation
      { c with loc = (Lexing.lexeme_start_p c.lexbuf) } in
  let contents = String.sub s 0 ((String.length s) - retract) in
  `QUOTATION
    {
      FanToken.q_name = name;
      q_loc = loc;
      q_shift = shift;
      q_contents = contents
    }
let update_loc ?file  ?(absolute= false)  ?(retract= 0)  ?(line= 1)  c =
  let lexbuf = c.lexbuf in
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with | None  -> pos.pos_fname | Some s -> s in
  lexbuf.lex_curr_p <-
    {
      pos with
      pos_fname = new_file;
      pos_lnum = (if absolute then line else pos.pos_lnum + line);
      pos_bol = (pos.pos_cnum - retract)
    }
let err (error : lex_error) (loc : FanLoc.t) =
  raise (FanLoc.Exc_located (loc, (Lexing_error error)))
let warn error loc =
  Format.eprintf "Warning: %a: %a@." FanLoc.print loc print_lex_error error