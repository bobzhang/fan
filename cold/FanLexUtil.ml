open Format
open FanLexer
open LibUtil
open Lexing
let lexing_store s buff max =
  let self n s =
    if n >= max
    then n
    else
      (let (__strm :_ Stream.t)= s in
       match Stream.peek __strm with
       | Some x -> (Stream.junk __strm; buff.[n] <- x; succ n)
       | _ -> n) in
  self 0 s
let from_context c =
  let next _ =
    let tok = with_curr_loc token c in
    let loc = FanLoc.of_lexbuf c.lexbuf in Some (tok, loc) in
  Stream.from next
let from_lexbuf ?(quotations= true)  lb =
  let c =
    {
      (default_context lb) with
      loc = (FanLoc.of_lexbuf lb);
      antiquots = (FanConfig.antiquotations.contents);
      quotations
    } in
  from_context c
let setup_loc lb loc =
  let start_pos = FanLoc.start_pos loc in
  lb.lex_abs_pos <- start_pos.pos_cnum; lb.lex_curr_p <- start_pos
let from_string ?quotations  loc str =
  let () = clear_stack () in
  let lb = Lexing.from_string str in
  setup_loc lb loc; from_lexbuf ?quotations lb
let from_stream ?quotations  loc strm =
  let () = clear_stack () in
  let lb = Lexing.from_function (lexing_store strm) in
  setup_loc lb loc; from_lexbuf ?quotations lb
let mk () loc strm =
  from_stream ~quotations:(FanConfig.quotations.contents) loc strm
let rec clean (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (`EOI,loc) ->
      (Stream.junk __strm; Stream.lsing (fun _  -> (`EOI, loc)))
  | Some x ->
      (Stream.junk __strm;
       (let xs = __strm in Stream.icons x (Stream.slazy (fun _  -> clean xs))))
  | _ -> Stream.sempty
let rec strict_clean (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (`EOI,_) -> (Stream.junk __strm; Stream.sempty)
  | Some x ->
      (Stream.junk __strm;
       (let xs = __strm in
        Stream.icons x (Stream.slazy (fun _  -> strict_clean xs))))
  | _ -> Stream.sempty
let debug_from_string str =
  let loc = FanLoc.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (Stream.iter
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" FanToken.print t FanLoc.print loc))
let debug_from_file file =
  let loc = FanLoc.mk file in
  let chan = open_in file in
  let stream = Stream.of_channel chan in
  ((from_stream loc stream) |> clean) |>
    (Stream.iter
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" FanToken.print t FanLoc.print loc))