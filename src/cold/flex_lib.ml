let lexing_store = Lexing_util.lexing_store
let fprintf = Format.fprintf
let std_formatter = Format.std_formatter
let from_string { Locf.loc_start = loc_start;_} str =
  let lb = Lexing.from_string str in
  lb.lex_abs_pos <- loc_start.pos_cnum;
  lb.lex_curr_p <- loc_start;
  Lex_fan.from_lexbuf lb
let from_stream { Locf.loc_start = loc_start;_} strm =
  let lb = Lexing.from_function (lexing_store strm) in
  lb.lex_abs_pos <- loc_start.pos_cnum;
  lb.lex_curr_p <- loc_start;
  Lex_fan.from_lexbuf lb
let rec clean (__strm : _ Fstream.t) =
  match Fstream.peek __strm with
  | Some (`EOI,loc) ->
      (Fstream.junk __strm; Fstream.lsing (fun _  -> (`EOI, loc)))
  | Some x ->
      (Fstream.junk __strm;
       (let xs = __strm in
        Fstream.icons x (Fstream.slazy (fun _  -> clean xs))))
  | _ -> Fstream.sempty
let rec strict_clean (__strm : _ Fstream.t) =
  match Fstream.peek __strm with
  | Some (`EOI,_) -> (Fstream.junk __strm; Fstream.sempty)
  | Some x ->
      (Fstream.junk __strm;
       (let xs = __strm in
        Fstream.icons x (Fstream.slazy (fun _  -> strict_clean xs))))
  | _ -> Fstream.sempty
let debug_from_string str =
  let loc = Locf.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (Fstream.iter
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" Ftoken.print t Locf.print loc))
let list_of_string ?(verbose= true)  str =
  let result = ref [] in
  let loc = Locf.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (Fstream.iter
       (fun (t,loc)  ->
          result := ((t, loc) :: (result.contents));
          if verbose
          then
            fprintf std_formatter "%a@;%a@\n" Ftoken.print t Locf.print loc));
  List.rev result.contents
let get_tokens s = List.map fst (list_of_string ~verbose:false s)
let debug_from_file file =
  let loc = Locf.mk file in
  let chan = open_in file in
  let stream = Fstream.of_channel chan in
  ((from_stream loc stream) |> clean) |>
    (Fstream.iter @@
       (fun (t,loc)  ->
          fprintf std_formatter "%a@;%a@\n" Ftoken.print t Locf.print loc))