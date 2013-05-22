open LibUtil

open MkFan

let _ =
  begin
    Syntax.current_warning :=
      ((fun loc  txt  ->
          Toploop.print_warning loc Format.err_formatter
            (Warnings.Camlp4 txt)));
    AstParsers.use_parsers ["revise"; "stream"; "macro"]
  end

let wrap parse_fun lb =
  try
    let not_filtered_token_stream = FanLexUtil.from_lexbuf lb in
    let token_stream = Gram.filter not_filtered_token_stream in
    let (__strm :_ XStream.t)= token_stream in
    match XStream.peek __strm with
    | Some (`EOI,_) -> begin XStream.junk __strm; raise End_of_file end
    | _ -> parse_fun token_stream
  with
  | End_of_file |Sys.Break |FanLoc.Exc_located (_,(End_of_file |Sys.Break ))
      as x -> raise x
  | FanLoc.Exc_located (loc,y) ->
      begin
        Format.eprintf "@[<0>%a%s@]@." Toploop.print_location loc
          (Printexc.to_string y);
        raise Exit
      end
  | x ->
      begin
        Format.eprintf "@[<0>%s@]@." (Printexc.to_string x); raise Exit
      end

let toplevel_phrase token_stream =
  match Gram.parse_origin_tokens Syntax.top_phrase token_stream with
  | Some stru ->
      let stru = AstFilters.apply_implem_filters stru in Ast2pt.phrase stru
  | None  -> raise End_of_file

let use_file token_stream =
  let rec loop () =
    let (pl,stopped_at_directive) =
      Gram.parse_origin_tokens Syntax.implem token_stream in
    if stopped_at_directive <> None
    then
      match pl with
      | (`Directive (_loc,`Lid (_,"load"),`Str (_,s)) : FAst.stru)::[] ->
          begin Topdirs.dir_load Format.std_formatter s; loop () end
      | (`Directive (_loc,`Lid (_,"directory"),`Str (_,s)) : FAst.stru)::[]
          -> begin Topdirs.dir_directory s; loop () end
      | (`Directive (_loc,`Lid (_,"default_quotation"),`Str (_,s)) :
          FAst.stru)::[] ->
          begin
            AstQuotation.set_default (FanToken.resolve_name ((`Sub []), s));
            loop ()
          end
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
  List.map (fun x  -> Ast2pt.phrase (AstFilters.apply_implem_filters x))
    (pl0 @ pl)

let revise_parser = wrap toplevel_phrase

let normal () =
  begin
    Toploop.parse_toplevel_phrase := Parse.toplevel_phrase;
    Toploop.parse_use_file := Parse.use_file
  end

let revise () =
  begin
    Toploop.parse_toplevel_phrase := revise_parser;
    Toploop.parse_use_file := (wrap use_file)
  end

let _ =
  begin
    Hashtbl.replace Toploop.directive_table "revise"
      (Toploop.Directive_none (fun ()  -> revise ()));
    Hashtbl.replace Toploop.directive_table "normal"
      (Toploop.Directive_none (fun ()  -> normal ()))
  end