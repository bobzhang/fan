open FanSig

module Id : Camlp4.Sig.Id =
              struct
               let name = "Camlp4OCamlParserParser"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     struct
                      open Camlp4.Sig

                      include Syntax

                      module M = (RStreamParser.Make)(Syntax)

                      open M

                      let _ = (Gram.Entry.clear stream_expr)

                      let _ = (Gram.Entry.clear stream_begin)

                      let _ = (Gram.Entry.clear stream_end)

                      let _ = (Gram.Entry.clear stream_quot)

                      let _ = (Gram.Entry.clear parser_case_list)

                      let _ = (
                      (Gram.extend (
                        (stream_expr : 'stream_expr Gram.Entry.t) ) (
                        ((fun ()
                            ->
                           (None , (
                            [(None , None , (
                              [((
                                [(
                                 (Gram.Snterml
                                   ((
                                    (Gram.Entry.obj (
                                      (expr : 'expr Gram.Entry.t) )) ),
                                    "top")) )] ), (
                                (Gram.Action.mk (
                                  fun (e :
                                    'expr) ->
                                   fun (_loc :
                                     Gram.Loc.t) ->
                                    (e : 'stream_expr) )) ))] ))] ))) () ) ))
                      );
                      (
                      (Gram.extend (
                        (stream_begin : 'stream_begin Gram.Entry.t) ) (
                        ((fun ()
                            ->
                           (None , (
                            [(None , None , (
                              [(( [( (Gram.Skeyword ("[<")) )] ), (
                                (Gram.Action.mk (
                                  fun _ ->
                                   fun (_loc :
                                     Gram.Loc.t) ->
                                    (() : 'stream_begin) )) ))] ))] ))) () )
                        ))
                      );
                      (
                      (Gram.extend ( (stream_end : 'stream_end Gram.Entry.t)
                        ) (
                        ((fun ()
                            ->
                           (None , (
                            [(None , None , (
                              [(( [( (Gram.Skeyword (">]")) )] ), (
                                (Gram.Action.mk (
                                  fun _ ->
                                   fun (_loc :
                                     Gram.Loc.t) ->
                                    (() : 'stream_end) )) ))] ))] ))) () ) ))
                      );
                      (
                      (Gram.extend (
                        (stream_quot : 'stream_quot Gram.Entry.t) ) (
                        ((fun ()
                            ->
                           (None , (
                            [(None , None , (
                              [(( [( (Gram.Skeyword ("'")) )] ), (
                                (Gram.Action.mk (
                                  fun _ ->
                                   fun (_loc :
                                     Gram.Loc.t) ->
                                    (() : 'stream_quot) )) ))] ))] ))) () )
                        ))
                      );
                      (Gram.extend (
                        (parser_case_list : 'parser_case_list Gram.Entry.t) )
                        (
                        ((fun ()
                            ->
                           (None , (
                            [(None , None , (
                              [((
                                [( (Gram.Sopt ((Gram.Skeyword ("|")))) ); (
                                 (Gram.Slist1sep
                                   ((
                                    (Gram.Snterm
                                      (Gram.Entry.obj (
                                        (parser_case :
                                          'parser_case Gram.Entry.t) ))) ), (
                                    (Gram.Skeyword ("|")) ))) )] ), (
                                (Gram.Action.mk (
                                  fun (pcl :
                                    'parser_case list) ->
                                   fun _ ->
                                    fun (_loc :
                                      Gram.Loc.t) ->
                                     (pcl : 'parser_case_list) )) ))] ))] )))
                          () ) ))

                     end
