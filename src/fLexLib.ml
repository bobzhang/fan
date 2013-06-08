open Format
open FLexer  
open LibUtil
open Lexing

let lexing_store s buff max =
   let  self n s =
     if n >= max then n
     else
       match XStream.peek s with
       | Some x -> (XStream.junk s; buff.[n] <- x; n + 1)
       | _ -> n in 
   self 0 s

let from_context c =
  let next _ =
    let tok = with_curr_loc token c in (* entry *)
    let loc = FLoc.of_lexbuf c.lexbuf in
    Some ((tok, loc))
  in XStream.from next

let from_lexbuf lb =
  let c = { (default_context lb) with
            loc        = Lexing.lexeme_start_p lb;
            antiquots  = !FConfig.antiquotations;
          }
  in from_context c

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
  from_stream  loc stream |> clean |> XStream.iter (
  fun (t,loc) -> fprintf std_formatter "%a@;%a@\n" FToken.print t FLoc.print loc
 )

