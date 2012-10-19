open LibUtil

open Format

open Grammar

include Entry

include Structure

let _ = let gkeywords = (Hashtbl.create 301) in
        {gkeywords = gkeywords;
         gfilter = ( (FanToken.Filter.mk ~is_kwd:( (Hashtbl.mem gkeywords) ))
          ); glexer = ( (FanLexer.mk () ) );
         warning_verbose = ( (ref true ) );
         error_verbose = FanConfig.verbose}

let gram =
 let gkeywords = (Hashtbl.create 301) in
 {gkeywords = gkeywords;
  gfilter = ( (FanToken.Filter.mk ~is_kwd:( (Hashtbl.mem gkeywords) )) );
  glexer = ( (FanLexer.mk () ) ); warning_verbose = ( (ref true ) );
  error_verbose = FanConfig.verbose}

let mk = (mk gram)

let of_parser = fun name -> fun strm -> (of_parser gram name strm)

let get_filter = fun ()  -> gram.gfilter

let lex = fun loc -> fun cs -> ((gram.glexer) loc cs)

let lex_string = fun loc -> fun str -> (lex loc ( (Stream.of_string str) ))

let filter =
 fun ts ->
  (Tools.keep_prev_loc ( (FanToken.Filter.filter ( gram.gfilter ) ts) ))

let filter_and_parse_tokens =
 fun entry -> fun ts -> (parse_origin_tokens entry ( (filter ts) ))

let parse =
 fun entry ->
  fun loc -> fun cs -> (filter_and_parse_tokens entry ( (lex loc cs) ))

let parse_string =
 fun entry ->
  fun loc ->
   fun str -> (filter_and_parse_tokens entry ( (lex_string loc str) ))

let debug_origin_token_stream =
 fun (entry :
   'a t) ->
  fun tokens ->
   ((parse_origin_tokens entry (
      (Stream.map ( fun t -> (t, ghost_token_info) ) tokens) )) : 'a)

let debug_filtered_token_stream =
 fun entry ->
  fun tokens ->
   (filter_and_parse_tokens entry (
     (Stream.map ( fun t -> (t, FanLoc.ghost) ) tokens) ))

let parse_string_safe =
 fun entry ->
  fun loc ->
   fun s ->
    (try (parse_string entry loc s) with
     FanLoc.Exc_located (loc, e) ->
      (
      (eprintf "%s" ( (Printexc.to_string e) ))
      );
      (
      (FanLoc.error_report (loc, s))
      );
      (FanLoc.raise loc e))

let wrap_stream_parser =
 fun p ->
  fun loc ->
   fun s ->
    (try (p loc s) with
     FanLoc.Exc_located (loc, e) ->
      (
      (eprintf "error: %s@." ( (FanLoc.to_string loc) ))
      );
      (FanLoc.raise loc e))

let parse_file_with =
 fun ~rule ->
  fun file ->
   if (Sys.file_exists file) then
    (
    let ch = (open_in file) in
    let st = (Stream.of_channel ch) in (parse rule ( (FanLoc.mk file) ) st)
    )
   else (failwithf "@[file: %s not found@]@." file)

let delete_rule = Delete.delete_rule

let srules =
 fun e ->
  fun rl ->
   `Stree
    ((List.fold_left ( (flip ( (uncurry ( (Insert.insert_tree e) )) )) )
       DeadEnd  rl))

let sfold0 = Fold.sfold0

let sfold1 = Fold.sfold1

let sfold0sep = Fold.sfold0sep

let extend = Insert.extend

let eoi_entry =
 fun entry ->
  let entry_eoi = (mk ( (( (name entry) ) ^ "_eoi") )) in
  let () =
   (extend ( (entry_eoi : 'entry_eoi t) ) (
     ((fun ()
         ->
        (None , (
         [(None , None , (
           [((
             [`Snterm ((obj ( (entry : 'entry t) )));
              `Stoken
               ((( function | `EOI -> (true) | _ -> (false) ),
                 (`Normal, "`EOI")))] ), (
             (mk_action (
               fun __camlp4_0 ->
                fun (x :
                  'entry) ->
                 fun (_loc :
                   FanLoc.t) ->
                  (match __camlp4_0 with
                   | `EOI -> (x : 'entry_eoi)
                   | _ -> assert false) )) ))] ))] ))) () ) )) in
  entry_eoi