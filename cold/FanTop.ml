module P = MakePreCast.Make(struct
  
  end)
open P
let wrap parse_fun lb =
  let () = iter_and_take_callbacks ( fun (_,f) -> f () ) in
  let not_filtered_token_stream = FanLexer.from_lexbuf lb in
  let token_stream = Gram.filter not_filtered_token_stream in
  try
    let (__strm : _ Stream.t ) = token_stream in
    match Stream.peek __strm with
    | Some (`EOI,_) -> ( Stream.junk __strm; raise End_of_file )
    | _ -> parse_fun token_stream
  with
  | End_of_file |Sys.Break |FanLoc.Exc_located
      (_,( End_of_file |Sys.Break  )) as x -> raise x
  | FanLoc.Exc_located (loc,y) -> (
      Format.eprintf "@[<0>%a%s@]@." Toploop.print_location loc (
        Printexc.to_string y );
      raise Exit )
  | x -> ( Format.eprintf "@[<0>%s@]@." ( Printexc.to_string x ); raise Exit
      )
let toplevel_phrase token_stream =
  match Gram.parse_origin_tokens (Syntax.top_phrase
          :Ast.str_item  option  Gram.t  ) token_stream
  with
  | Some str_item ->
      let str_item =
        Syntax.AstFilters.fold_topphrase_filters (
          fun t -> fun filter -> filter t ) str_item in
      Ast2pt.phrase str_item
  | None  -> raise End_of_file
let fake token_stream =
  (
  try
    Stream.iter (
      fun (tok,_) ->
        if tok = ( `INT (3, "3") )
        then raise Not_found
        else
          Format.fprintf Format.std_formatter "@[%a@]@." FanToken.print tok )
      token_stream
  with | Not_found  -> ()
  );
  prerr_endline "got it";
  Parsetree.Ptop_dir ("pwd", Parsetree.Pdir_none)
let use_file token_stream =
  let (pl0,eoi) =
    let rec loop () =
      let (pl,stopped_at_directive) =
        Gram.parse_origin_tokens Syntax.use_file token_stream in
      if stopped_at_directive <> None
      then (
        match pl with
        | Ast.StDir (_,"load",Ast.ExStr (_,s))::[] -> (
            Topdirs.dir_load Format.std_formatter s; loop () )
        | Ast.StDir (_,"directory",Ast.ExStr (_,s))::[] -> (
            Topdirs.dir_directory s; loop () )
        | _ -> (pl, false) )
      else (pl, true) in
    loop () in
  let pl =
    if eoi
    then []
    else
      let rec loop () =
        let (pl,stopped_at_directive) =
          Gram.parse_origin_tokens Syntax.use_file token_stream in
        if stopped_at_directive <> None then pl @ ( loop () ) else pl in
      loop () in
  List.map Ast2pt.phrase ( pl0 @ pl )
let revise_parser = wrap toplevel_phrase
let _ =
  Toploop.parse_toplevel_phrase := revise_parser;
  Toploop.parse_use_file := ( wrap use_file );
  Syntax.current_warning := (
    fun loc ->
      fun txt ->
        Toploop.print_warning loc Format.err_formatter ( Warnings.Camlp4 txt
          )
    );
  iter_and_take_callbacks ( fun (_,f) -> f () )
let _ =
  let open FanParsers in
    pa_r (module P);
    pa_rp (module P);
    pa_q (module P);
    pa_g (module P);
    pa_l (module P);
    pa_m (module P)
let normal () = Toploop.parse_toplevel_phrase := Parse.toplevel_phrase
let revise () = Toploop.parse_toplevel_phrase := revise_parser
let token () = Toploop.parse_toplevel_phrase := ( wrap fake )