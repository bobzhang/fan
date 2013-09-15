open Format
open Fan_lex
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


let from_string  {FLoc.loc_start;_} str =
  let () = clear_stack () in 
  let lb = Lexing.from_string str in begin 
    lb.lex_abs_pos <- loc_start.pos_cnum;
    lb.lex_curr_p <- loc_start;
    from_lexbuf lb
  end

let from_stream  {FLoc.loc_start;_} strm =
  let () = clear_stack () in 
  let lb = Lexing.from_function (lexing_store strm) in begin
    lb.lex_abs_pos <- loc_start.pos_cnum;
    lb.lex_curr_p <- loc_start;
    from_lexbuf  lb
  end


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
  stream
  |> clean
  |> XStream.iter
      (fun (t,loc) -> fprintf std_formatter "%a@;%a@\n" Ftoken.print t FLoc.print loc)

let list_of_string ?(verbose=true) str =
  let result = ref [] in
  let loc = FLoc.string_loc  in
  let stream = from_string loc str  in
  begin 
    stream
    |> clean
    |> XStream.iter
        (fun (t,loc) -> begin 
          result := (t,loc):: !result ;
          if verbose then 
            fprintf std_formatter "%a@;%a@\n" Ftoken.print t FLoc.print loc
        end) ;
   List.rev !result 
  end

let get_tokens s =
  List.map fst
    (list_of_string ~verbose:false s )
  
  
let debug_from_file  file =
  let loc = FLoc.mk file in
  let chan = open_in file in
  let stream = XStream.of_channel  chan in
  from_stream  loc stream |> clean |>
  XStream.iter
    (fun (t,loc) ->
      fprintf std_formatter "%a@;%a@\n" Ftoken.print t FLoc.print loc)

