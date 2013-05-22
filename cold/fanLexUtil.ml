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
       | Some x -> begin XStream.junk s; buff.[n] <- x; n + 1 end
       | _ -> n) in
  self 0 s

let from_context c =
  let next _ =
    let tok = with_curr_loc token c in
    let loc = FanLoc.of_lexbuf c.lexbuf in Some (tok, loc) in
  XStream.from next

let from_lexbuf lb =
  let c =
    {
      (default_context lb) with
      loc = (Lexing.lexeme_start_p lb);
      antiquots = (FanConfig.antiquotations.contents)
    } in
  from_context c

let setup_loc lb loc =
  let start_pos = FanLoc.start_pos loc in
  begin lb.lex_abs_pos <- start_pos.pos_cnum; lb.lex_curr_p <- start_pos end

let from_string loc str =
  let () = clear_stack () in
  let lb = Lexing.from_string str in
  begin setup_loc lb loc; from_lexbuf lb end

let from_stream loc strm =
  let () = clear_stack () in
  let lb = Lexing.from_function (lexing_store strm) in
  begin setup_loc lb loc; from_lexbuf lb end

let mk () loc strm = from_stream loc strm

let rec clean (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some (`EOI,loc) ->
      begin XStream.junk __strm; XStream.lsing (fun _  -> (`EOI, loc)) end
  | Some x ->
      begin
        XStream.junk __strm;
        (let xs = __strm in
         XStream.icons x (XStream.slazy (fun _  -> clean xs)))
      end
  | _ -> XStream.sempty

let rec strict_clean (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some (`EOI,_) -> begin XStream.junk __strm; XStream.sempty end
  | Some x ->
      begin
        XStream.junk __strm;
        (let xs = __strm in
         XStream.icons x (XStream.slazy (fun _  -> strict_clean xs)))
      end
  | _ -> XStream.sempty

let debug_from_string str =
  let loc = FanLoc.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (XStream.iter
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" FanToken.print t FanLoc.print loc))

let debug_from_file file =
  let loc = FanLoc.mk file in
  let chan = open_in file in
  let stream = XStream.of_channel chan in
  ((from_stream loc stream) |> clean) |>
    (XStream.iter
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" FanToken.print t FanLoc.print loc))