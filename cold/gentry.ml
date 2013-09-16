let fprintf = Format.fprintf
let eprintf = Format.eprintf
type 'a t = Gstructure.entry 
let name (e : 'a t) = e.name
let print ppf e = fprintf ppf "%a@\n" Gprint.text#entry e
let dump ppf e = fprintf ppf "%a@\n" Gprint.dump#entry e
let trace_parser = ref false
let mk_dynamic g n =
  ({
     gram = g;
     name = n;
     start = (Gtools.empty_entry n);
     continue =
       (fun _  _  _  (__strm : _ Fstream.t)  -> raise Fstream.NotConsumed);
     desc = (Dlevels []);
     freezed = false
   } : 'a t )
let of_parser g n (p : Ftoken.stream -> 'a) =
  (let f ts = Gaction.mk (p ts) in
   {
     gram = g;
     name = n;
     start = (fun _  -> f);
     continue = (fun _  _  _  _  -> raise Fstream.NotConsumed);
     desc = (Dparser f);
     freezed = true
   } : 'a t )
let setup_parser (e : 'a t) (p : Ftoken.stream -> 'a) =
  let f ts = Gaction.mk (p ts) in
  e.start <- (fun _  -> f);
  e.continue <- (fun _  _  _  _  -> raise Fstream.NotConsumed);
  e.desc <- Dparser f
let clear (e : 'a t) =
  e.start <- (fun _  _  -> raise Fstream.NotConsumed);
  e.continue <- (fun _  _  _  _  -> raise Fstream.NotConsumed);
  e.desc <- Dlevels []
let obj x = x
let repr x = x
let gram_of_entry (e : 'a t) = e.gram
let action_parse (entry : 'a t) (ts : Ftoken.stream) =
  (try
     let p =
       if trace_parser.contents then Format.fprintf else Format.ifprintf in
     p Format.err_formatter "@[<4>%s@ " entry.name;
     (let res = entry.start 0 ts in
      let () = p Format.err_formatter "@]@." in res)
   with
   | Fstream.NotConsumed  ->
       FLoc.raise (Gtools.get_cur_loc ts)
         (Fstream.Error ("illegal begin of " ^ entry.name))
   | FLoc.Exc_located (_,_) as exc ->
       (eprintf "%s@." (Printexc.to_string exc); raise exc)
   | exc ->
       (eprintf "%s@." (Printexc.to_string exc);
        FLoc.raise (Gtools.get_cur_loc ts) exc) : Gaction.t )
let parse_origin_tokens entry stream =
  Gaction.get (action_parse entry stream)
let filter_and_parse_tokens (entry : 'a t) ts =
  parse_origin_tokens entry (FanTokenFilter.filter (entry.gram).gfilter ts)
let lex_string loc str = Flex_lib.from_stream loc (Fstream.of_string str)
let parse_string ?(lexer= Flex_lib.from_stream)  ?(loc= FLoc.string_loc) 
  (entry : 'a t) str =
  (((str |> Fstream.of_string) |> (lexer loc)) |>
     (FanTokenFilter.filter (entry.gram).gfilter))
    |> (parse_origin_tokens entry)
let parse (entry : 'a t) loc cs =
  parse_origin_tokens entry
    (FanTokenFilter.filter (entry.gram).gfilter (Flex_lib.from_stream loc cs))
let levels_of_entry = Ginsert.levels_of_entry
let extend = Ginsert.extend
let extend_single = Ginsert.extend_single
let copy = Ginsert.copy
let extend = Ginsert.extend
let unsafe_extend = Ginsert.unsafe_extend
let unsafe_extend_single = Ginsert.unsafe_extend_single
let delete_rule = Gdelete.delete_rule
let symb_failed = Gfailed.symb_failed
let symb_failed_txt = Gfailed.symb_failed_txt
let parser_of_symbol = Gparser.parser_of_symbol
let eoi_entry = Ginsert.eoi_entry