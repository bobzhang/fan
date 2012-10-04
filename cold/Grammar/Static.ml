open LibUtil

open Format

module Make =
                            functor (Lexer : Sig.Lexer) ->
                             struct
                              module Structure = (Structure.Make)(Lexer)

                              module Delete = (Delete.Make)(Structure)

                              module Insert = (Insert.Make)(Structure)

                              module Fold = (Fold.Make)(Structure)

                              module Tools = (Tools.Make)(Structure)

                              include Structure

                              let gram =
                               let gkeywords = (Hashtbl.create 301) in
                               {gkeywords = gkeywords;
                                gfilter = (
                                 (Token.Filter.mk ( (Hashtbl.mem gkeywords)
                                   )) ); glexer = ( (Lexer.mk () ) );
                                warning_verbose = ( (ref true ) );
                                error_verbose = FanConfig.verbose}

                              module Entry =
                               struct
                                module E = (Entry.Make)(Structure)

                                type 'a t = 'a E.t

                                let mk = (E.mk gram)

                                let of_parser =
                                 fun name ->
                                  fun strm -> (E.of_parser gram name strm)

                                let setup_parser = E.setup_parser

                                let name = E.name

                                let print = E.print

                                let clear = E.clear

                                let dump = E.dump

                                let obj = fun x -> x

                               end

                              let trace_parser = Entry.E.trace_parser

                              let get_filter = fun ()  -> gram.gfilter

                              let lex =
                               fun loc -> fun cs -> ((gram.glexer) loc cs)

                              let lex_string =
                               fun loc ->
                                fun str ->
                                 (lex loc ( (Stream.of_string str) ))

                              let filter =
                               fun ts ->
                                (Tools.keep_prev_loc (
                                  (Token.Filter.filter ( gram.gfilter ) ts)
                                  ))

                              let parse_origin_tokens =
                               fun (entry :
                                 'a Entry.t) ->
                                fun ts ->
                                 ((Entry.E.parse_origin_tokens entry ts) :
                                   'a)

                              let filter_and_parse_tokens =
                               fun entry ->
                                fun ts ->
                                 (parse_origin_tokens entry ( (filter ts) ))

                              let parse =
                               fun entry ->
                                fun loc ->
                                 fun cs ->
                                  (filter_and_parse_tokens entry (
                                    (lex loc cs) ))

                              let parse_string =
                               fun entry ->
                                fun loc ->
                                 fun str ->
                                  (filter_and_parse_tokens entry (
                                    (lex_string loc str) ))

                              let debug_origin_token_stream =
                               fun entry ->
                                fun tokens ->
                                 (parse_origin_tokens entry (
                                   (Stream.map (
                                     fun t -> (t, ghost_token_info) ) tokens)
                                   ))

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
                                    (eprintf "error: %s@." (
                                      (FanLoc.to_string loc) ))
                                    );
                                    (FanLoc.raise loc e))

                              let parse_file_with =
                               fun ~rule ->
                                fun file ->
                                 if (Sys.file_exists file) then
                                  (
                                  let ch = (open_in file) in
                                  let st = (Stream.of_channel ch) in
                                  (parse rule ( (FanLoc.mk file) ) st)
                                  )
                                 else
                                  (failwithf "@[file: %s not found@]@." file)

                              let delete_rule = Delete.delete_rule

                              let srules =
                               fun e ->
                                fun rl ->
                                 (Stree
                                   (List.fold_left (
                                     (flip (
                                       (uncurry ( (Insert.insert_tree e) ))
                                       )) ) DeadEnd  rl))

                              let sfold0 = Fold.sfold0

                              let sfold1 = Fold.sfold1

                              let sfold0sep = Fold.sfold0sep

                              let extend = Insert.extend

                             end
