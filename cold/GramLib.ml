open LibUtil

open FanUtil

let test_patt_lessminus =
                             (Gram.of_parser "test_patt_lessminus" (
                               fun strm ->
                                let rec skip_patt =
                                 fun n ->
                                  (match (stream_peek_nth n strm) with
                                   | Some ((`KEYWORD "<-")) -> n
                                   | Some ((`KEYWORD ("[" | "[<"))) ->
                                      (skip_patt (
                                        (( (ignore_upto "]" ( (n + 1) )) ) +
                                          1) ))
                                   | Some ((`KEYWORD "(")) ->
                                      (skip_patt (
                                        (( (ignore_upto ")" ( (n + 1) )) ) +
                                          1) ))
                                   | Some ((`KEYWORD "{")) ->
                                      (skip_patt (
                                        (( (ignore_upto "}" ( (n + 1) )) ) +
                                          1) ))
                                   | (Some
                                       ((`KEYWORD
                                         ((("as" | "::") | ",") | "_")))
                                      | Some ((`LIDENT _) | (`UIDENT _))) ->
                                      (skip_patt ( (n + 1) ))
                                   | (Some (_) | None) ->
                                      (raise Stream.Failure ))
                                and ignore_upto =
                                 fun end_kwd ->
                                  fun n ->
                                   (match (stream_peek_nth n strm) with
                                    | Some ((`KEYWORD prm)) when
                                       (prm = end_kwd) ->
                                       n
                                    | Some ((`KEYWORD ("[" | "[<"))) ->
                                       (ignore_upto end_kwd (
                                         (( (ignore_upto "]" ( (n + 1) )) ) +
                                           1) ))
                                    | Some ((`KEYWORD "(")) ->
                                       (ignore_upto end_kwd (
                                         (( (ignore_upto ")" ( (n + 1) )) ) +
                                           1) ))
                                    | Some ((`KEYWORD "{")) ->
                                       (ignore_upto end_kwd (
                                         (( (ignore_upto "}" ( (n + 1) )) ) +
                                           1) ))
                                    | Some (_) ->
                                       (ignore_upto end_kwd ( (n + 1) ))
                                    | None -> (raise Stream.Failure )) in
                                (skip_patt 1) ))

let is_revised =
                                                   fun ~expr ->
                                                    fun ~sem_expr_for_list ->
                                                     (try
                                                       (
                                                      (Gram.delete_rule expr
                                                        (
                                                        [`Skeyword ("[");
                                                         `Snterm
                                                          ((Gram.obj (
                                                             (sem_expr_for_list :
                                                               'sem_expr_for_list Gram.t)
                                                             )));
                                                         `Skeyword ("::");
                                                         `Snterm
                                                          ((Gram.obj (
                                                             (expr :
                                                               'expr Gram.t)
                                                             )));
                                                         `Skeyword ("]")] ))
                                                      );
                                                       (true)
                                                      with
                                                      Not_found -> (false))
