open Format;
open FanLexer;
open LibUtil;
open Lexing;

let lexing_store s buff max =
   let  self n s =
     if n >= max then n
     else
       match s with parser
       [ [< x >] -> begin buff.[n]<- x; succ n end
       | [< >] -> n ] in  
   self 0 s;

let from_context c =
  let next _ =
    let tok = with_curr_loc token c in
    let loc = FanLoc.of_lexbuf c.lexbuf in
    Some ((tok, loc))
  in Stream.from next;

let from_lexbuf ?(quotations = true) lb =
  let c = { (default_context lb) with
            loc        = FanLoc.of_lexbuf lb;
            antiquots  = !FanConfig.antiquotations;
            quotations = quotations      }
  in from_context c;

let setup_loc lb loc =
  let start_pos = FanLoc.start_pos loc in begin 
    lb.lex_abs_pos <- start_pos.pos_cnum;
    lb.lex_curr_p  <- start_pos
  end;

(* the stack is cleared to clear the previous error message *)          
let from_string ?quotations loc str =
  let () = clear_stack () in 
  let lb = Lexing.from_string str in begin 
    setup_loc lb loc;
    from_lexbuf ?quotations lb
  end;

(* the stack is cleared to clear the previous error message *)    
let from_stream ?quotations loc strm =
  let () = clear_stack () in 
  let lb = Lexing.from_function (lexing_store strm) in begin 
    setup_loc lb loc;
    from_lexbuf ?quotations lb
  end;

let mk () loc strm =
  from_stream ~quotations:!FanConfig.quotations loc strm;


(* remove trailing `EOI*)  
let rec clean  =  parser
  [ [< (`EOI,loc) >] -> [< (`EOI,loc) >]
  | [< x; 'xs>]  -> [< x ; 'clean xs >]
  | [< >] -> [< >] ] ;

let debug_from_string str =
  let loc = FanLoc.string_loc  in
  let stream = from_string loc str  in
  stream |> clean |> Stream.iter
    (fun (t,loc) -> fprintf std_formatter "%a@;%a@\n" FanToken.print t FanLoc.print loc);

let debug_from_file file =
  let loc = FanLoc.mk file in
  let chan = open_in file in
  let stream = Stream.of_channel  chan in
  from_stream loc stream |> clean |> Stream.iter (
  fun (t,loc) -> fprintf std_formatter "%a@;%a@\n" FanToken.print t FanLoc.print loc
 );

