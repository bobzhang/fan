open Format
open FLexer  
open LibUtil
open Lexing


(** put elements from stream to string with offset 0 and [max] elements *)  
let lexing_store s buff max =
   let  self n s =
     if n >= max then n
     else
       match XStream.peek s with
       | Some x -> (XStream.junk s; buff.[n] <- x; n + 1)
       | _ -> n in 
   self 0 s


(** In initial stage
    [Lexing.lexeme_start_p] returns
    {[ Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 ]}
    for a string input or a channel input (from_string, from_channel).

 *)    
let from_lexbuf lb =
  (** lexing entry *)
  let c = {
    loc = Lexing.lexeme_start_p lb;
    antiquots = !FConfig.antiquotations;
    lexbuf = lb;
    buffer = Buffer.create 256
  } in
  let next _ =
    let tok =  token {c with loc = Lexing.lexeme_start_p c.lexbuf } c.lexbuf in
    let loc = Location_util.from_lexbuf c.lexbuf in
    Some ((tok, loc)) in
  XStream.from next


let setup_loc lb loc =
  let start_pos = FLoc.start_pos loc in begin 
    lb.lex_abs_pos <- start_pos.pos_cnum;
    lb.lex_curr_p  <- start_pos
  end

(* the stack is cleared to clear the previous error message *)          
let from_string  loc str =
  let () = clear_stack () in 
  let lb = Lexing.from_string str in begin 
    setup_loc lb loc;
    from_lexbuf lb
  end

(* the stack is cleared to clear the previous error message *)    
let from_stream  loc strm =
  let () = clear_stack () in 
  let lb = Lexing.from_function (lexing_store strm) in begin 
    setup_loc lb loc;
    from_lexbuf  lb
  end

let mk () loc strm =
  from_stream  loc strm


(* remove trailing `EOI*)  
let rec clean  =  parser
  | (`EOI,loc)  -> {:stream| (`EOI,loc)|}
  |  x; 'xs  -> {:stream| x; 'clean xs|}
  |  -> {:stream||} 

let rec strict_clean = parser
  | (`EOI,_)  -> {:stream||}
  | x; 'xs  -> {:stream| x; 'strict_clean xs |}
  |  -> {:stream||} 
    
let debug_from_string  str =
  let loc = FLoc.string_loc  in
  let stream = from_string loc str  in
  stream |> clean |> XStream.iter
    (fun (t,loc) -> fprintf std_formatter "%a@;%a@\n" FToken.print t FLoc.print loc)

let debug_from_file  file =
  let loc = FLoc.mk file in
  let chan = open_in file in
  let stream = XStream.of_channel  chan in
  from_stream  loc stream |> clean |>
  XStream.iter
    (fun (t,loc) ->
      fprintf std_formatter "%a@;%a@\n" FToken.print t FLoc.print loc)

