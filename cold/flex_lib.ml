open Format
open Fan_lex
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
let from_lexbuf lb =
  let c =
    {
      loc = (Lexing.lexeme_start_p lb);
      antiquots = (FConfig.antiquotations.contents);
      lexbuf = lb;
      buffer = (Buffer.create 256)
    } in
  let next _ =
    let tok =
      token { c with loc = (Lexing.lexeme_start_p c.lexbuf) } c.lexbuf in
    let loc = Location_util.from_lexbuf c.lexbuf in Some (tok, loc) in
  XStream.from next
let from_string { FLoc.loc_start = loc_start;_} str =
  let () = clear_stack () in
  let lb = Lexing.from_string str in
  lb.lex_abs_pos <- loc_start.pos_cnum;
  lb.lex_curr_p <- loc_start;
  from_lexbuf lb
let from_stream { FLoc.loc_start = loc_start;_} strm =
  let () = clear_stack () in
  let lb = Lexing.from_function (lexing_store strm) in
  lb.lex_abs_pos <- loc_start.pos_cnum;
  lb.lex_curr_p <- loc_start;
  from_lexbuf lb
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
let list_of_string ?(verbose= true)  str =
  let result = ref [] in
  let loc = FLoc.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (XStream.iter
       (fun (t,loc)  ->
          result := ((t, loc) :: (result.contents));
          if verbose
          then
            fprintf std_formatter "%a@;%a@\n" FToken.print t FLoc.print loc));
  List.rev result.contents
let debug_from_file file =
  let loc = FLoc.mk file in
  let chan = open_in file in
  let stream = XStream.of_channel chan in
  ((from_stream loc stream) |> clean) |>
    (XStream.iter
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" FToken.print t FLoc.print loc))