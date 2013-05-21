open LibUtil

open Fan

let _ = Topdirs.dir_directory "+compiler-libs"

let wrap parse_fun lb =
  try
    let not_filtered_token_stream = FanLexUtil.from_lexbuf lb in
    let token_stream = Gram.filter not_filtered_token_stream in
    let (__strm :_ XStream.t)= token_stream in
    match XStream.peek __strm with
    | Some (`EOI,_) -> (XStream.junk __strm; raise End_of_file)
    | _ -> parse_fun token_stream
  with
  | End_of_file |Sys.Break |FanLoc.Exc_located (_,(End_of_file |Sys.Break ))
      as x -> raise x
  | FanLoc.Exc_located (loc,y) ->
      (Format.eprintf "@[<0>%a%s@]@." Toploop.print_location loc
         (Printexc.to_string y);
       raise Exit)
  | x -> (Format.eprintf "@[<0>%s@]@." (Printexc.to_string x); raise Exit)

let toplevel_phrase token_stream =
  match Gram.parse_origin_tokens Syntax.top_phrase token_stream with
  | Some stru ->
      let stru = AstFilters.apply_implem_filters stru in Ast2pt.phrase stru
  | None  -> raise End_of_file

let revise_parser str _bol =
  let eof = ref false in
  let lexbuf = UTop.lexbuf_of_string eof str in
  try
    let not_filtered_token_stream = FanLexUtil.from_lexbuf lexbuf in
    let token_stream = Gram.filter not_filtered_token_stream in
    match XStream.peek token_stream with
    | Some (`EOI,_) -> (XStream.junk token_stream; raise End_of_file)
    | _ -> UTop.Value (toplevel_phrase token_stream)
  with
  | End_of_file |Sys.Break |FanLoc.Exc_located (_,(End_of_file |Sys.Break ))
      as x -> raise x
  | FanLoc.Exc_located (_loc,y) ->
      UTop.Error ([(0, 0)], (Printexc.to_string y))

let normal () =
  UTop.parse_toplevel_phrase := UTop.parse_toplevel_phrase_default;
  Toploop.parse_use_file := Parse.use_file

let _ = AstParsers.use_parsers ["revise"; "stream"; "macro"]

let revise () = UTop.parse_toplevel_phrase := revise_parser

let _ = revise ()

let () = UTop_main.main ()

let _ =
  Hashtbl.replace Toploop.directive_table "revise"
    (Toploop.Directive_none (fun ()  -> revise ()));
  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (fun ()  -> prerr_endline (Sys.getcwd ())));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun ()  -> normal ()))