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
let rec clean: Tokenf.stream -> Tokenf.stream =
  fun (__strm : _ Streamf.t)  ->
    match Streamf.peek __strm with
    | Some (`EOI _ as x) -> (Streamf.junk __strm; Streamf.ising x)
    | Some x ->
        (Streamf.junk __strm;
         (let xs = __strm in
          Streamf.icons x (Streamf.slazy (fun _  -> clean xs))))
    | _ -> Streamf.sempty
let rec strict_clean: Tokenf.stream -> Tokenf.stream =
  fun (__strm : _ Streamf.t)  ->
    match Streamf.peek __strm with
    | Some (`EOI _) -> (Streamf.junk __strm; Streamf.sempty)
    | Some x ->
        (Streamf.junk __strm;
         (let xs = __strm in
          Streamf.icons x (Streamf.slazy (fun _  -> strict_clean xs))))
    | _ -> Streamf.sempty
let debug_from_string str =
  let loc = Locf.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (Streamf.iter (fun t  -> fprintf std_formatter "%a@\n" Tokenf.print t))
let list_of_string ?(verbose= true)  str =
  let result = ref [] in
  let loc = Locf.string_loc in
  let stream = from_string loc str in
  (stream |> clean) |>
    (Streamf.iter
       (fun t  ->
          result := (t :: (result.contents));
          if verbose then fprintf std_formatter "%a@\n" Tokenf.print t));
  List.rev result.contents
let get_tokens s = list_of_string ~verbose:false s
let debug_from_file file =
  let loc = Locf.mk file in
  let chan = open_in file in
  let stream = Streamf.of_channel chan in
  ((from_stream loc stream) |> clean) |>
    (Streamf.iter @@ (fun t  -> fprintf std_formatter "%a@\n" Tokenf.print t))