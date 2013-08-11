open Format
open FLexer
open LibUtil
open Lexing
let lexing_store s buff max =
  let self n s =
    if n >= max
    then n
    else
      (match XStream.peek s with
       | Some x -> (XStream.junk s; buff.[n] <- x; n + 1)
       | _ -> n) in
  self 0 s
let from_context c =
  let next _ =
    let tok = with_curr_loc token c in
    let loc = FLoc.of_lexbuf c.lexbuf in Some (tok, loc) in
  XStream.from next
let from_lexbuf lb =
  let c =
    {
      (default_context lb) with
      loc = (Lexing.lexeme_start_p lb);
      antiquots = (FConfig.antiquotations.contents)
    } in
  from_context c
let setup_loc lb loc =
  let start_pos = FLoc.start_pos loc in
  lb.lex_abs_pos <- start_pos.pos_cnum; lb.lex_curr_p <- start_pos
let from_string loc str =
  let () = clear_stack () in
  let lb = Lexing.from_string str in setup_loc lb loc; from_lexbuf lb
let from_stream loc strm =
  let () = clear_stack () in
  let lb = Lexing.from_function (lexing_store strm) in
  setup_loc lb loc; from_lexbuf lb
let mk () loc strm = from_stream loc strm
let rec clean (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some (`EOI,loc) ->
      (XStream.junk __strm; XStream.lsing (fun _  -> (`EOI, loc)))
  | Some x ->
      (XStream.junk __strm;
       (let xs = __strm in
        XStream.icons x (XStream.slazy (fun _  -> clean xs))))
  | _ -> XStream.sempty
let rec strict_clean (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some (`EOI,_) -> (XStream.junk __strm; XStream.sempty)
  | Some x ->
      (XStream.junk __strm;
       (let xs = __strm in
        XStream.icons x (XStream.slazy (fun _  -> strict_clean xs))))
  | _ -> XStream.sempty
let debug_from_string str =
  let loc = FLoc.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (XStream.iter
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" FToken.print t FLoc.print loc))
let debug_from_file file =
  let loc = FLoc.mk file in
  let chan = open_in file in
  let stream = XStream.of_channel chan in
  ((from_stream loc stream) |> clean) |>
    (XStream.iter
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" FToken.print t FLoc.print loc))