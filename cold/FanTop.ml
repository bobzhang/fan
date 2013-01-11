open Ast
open LibUtil
open Fan
let wrap parse_fun lb =
  let () = iter_and_take_callbacks (fun (_,f)  -> f ()) in
  let not_filtered_token_stream = FanLexUtil.from_lexbuf lb in
  let token_stream = Gram.filter not_filtered_token_stream in
  try
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
  match Gram.parse_origin_tokens
          (Syntax.top_phrase : str_item option Gram.t ) token_stream
  with
  | Some str_item ->
      let str_item = AstFilters.apply_topphrase_filters str_item in
      Ast2pt.phrase str_item
  | None  -> raise End_of_file
let fake token_stream =
  (try
     XStream.iter
       (fun (tok,_)  ->
          if tok = (`INT (3, "3"))
          then raise Not_found
          else
            Format.fprintf Format.std_formatter "@[%a@]@." FanToken.print tok)
       token_stream
   with | Not_found  -> ());
  prerr_endline "got it";
  Parsetree.Ptop_dir ("pwd", Parsetree.Pdir_none)
let use_file token_stream =
  let rec loop () =
    let (pl,stopped_at_directive) =
      Gram.parse_origin_tokens Syntax.implem token_stream in
    if stopped_at_directive <> None
    then
      match pl with
      | (`Directive (_loc,`Lid (_,"load"),`Str (_,s)))::[] ->
          (Topdirs.dir_load Format.std_formatter s; loop ())
      | (`Directive (_loc,`Lid (_,"directory"),`Str (_,s)))::[] ->
          (Topdirs.dir_directory s; loop ())
      | (`Directive (_loc,`Lid (_,"default_quotation"),`Str (_,s)))::[] ->
          (AstQuotation.set_default s; loop ())
      | _ -> (pl, false)
    else (pl, true) in
  let (pl0,eoi) = loop () in
  let pl =
    if eoi
    then []
    else
      (let rec loop () =
         let (pl,stopped_at_directive) =
           Gram.parse_origin_tokens Syntax.implem token_stream in
         if stopped_at_directive <> None then pl @ (loop ()) else pl in
       loop ()) in
  List.map Ast2pt.phrase (pl0 @ pl)
let revise_parser = wrap toplevel_phrase
let _ =
  Toploop.parse_toplevel_phrase := revise_parser;
  Toploop.parse_use_file := (wrap use_file);
  Syntax.current_warning :=
    ((fun loc  txt  ->
        Toploop.print_warning loc Format.err_formatter (Warnings.Camlp4 txt)));
  iter_and_take_callbacks (fun (_,f)  -> f ())
let _ =
  AstParsers.use_parsers
    ["revise"; "stream"; "debug"; "macro"; "ListComprehension"]
let normal () = Toploop.parse_toplevel_phrase := Parse.toplevel_phrase
let revise () = Toploop.parse_toplevel_phrase := revise_parser
let token () = Toploop.parse_toplevel_phrase := (wrap fake)
let _ =
  Hashtbl.replace Toploop.directive_table "revise"
    (Toploop.Directive_none (fun ()  -> revise ()))