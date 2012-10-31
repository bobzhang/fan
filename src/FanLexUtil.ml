open Format;
open FanLexer;
open LibUtil;


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

