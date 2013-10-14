open LibUtil
open MkFan
let print_fan_error pp exn =
  Format.fprintf pp "@[<0>%s@]@." (Printexc.to_string exn)
let get_fan_error_message exn =
  let (loc,exn) =
    match exn with
    | Locf.Exc_located (loc,exn) ->
        (((Locf.start_off loc), (Locf.stop_off loc)), exn)
    | exn -> ((0, 0), exn) in
  let msg = UTop.get_message print_fan_error exn in
  let idx = ref ((String.length msg) - 1) in
  while (idx.contents > 0) && ((msg.[idx.contents]) = '\n') do decr idx done;
  if (idx.contents + 1) < (String.length msg)
  then (loc, (String.sub msg 0 (idx.contents + 1)))
  else (loc, msg)
let revise_parser str _bol =
  let eof = ref false in
  let lexbuf = UTop.lexbuf_of_string eof str in
  try
    let not_filtered_token_stream = Flex_lib.from_lexbuf lexbuf in
    let token_stream = Fgram.filter not_filtered_token_stream in
    match Fstream.peek token_stream with
    | Some (`EOI,_) -> (Fstream.junk token_stream; raise End_of_file)
    | _ -> UTop.Value (toplevel_phrase token_stream)
  with
  | End_of_file |Sys.Break |Locf.Exc_located (_,(End_of_file |Sys.Break )) as
      x -> raise x
  | Locf.Exc_located (_loc,y) ->
      UTop.Error ([(0, 0)], (Printexc.to_string y))
let normal () =
  UTop.parse_toplevel_phrase := UTop.parse_toplevel_phrase_default;
  Toploop.parse_use_file := Parse.use_file
let _ = Ast_parsers.use_parsers ["revise"; "stream"; "macro"]
let revise () = UTop.parse_toplevel_phrase := revise_parser
let _ = revise ()
let _ =
  Topdirs.dir_directory "+compiler-libs";
  Hashtbl.replace Toploop.directive_table "revise"
    (Toploop.Directive_none (fun ()  -> revise ()));
  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (fun ()  -> prerr_endline (Sys.getcwd ())));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun ()  -> normal ()))
let () = UTop_main.main ()
