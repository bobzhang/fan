module Make =
 functor (Structure : Structure.S) ->
  struct
   module Dump = (Print.MakeDump)(Structure)

   module Print = (Print.Make)(Structure)

   module Tools = (Tools.Make)(Structure)

   open Format

   open Structure

   open Tools

   type 'a t = internal_entry

   let name = fun e -> e.ename

   let print = fun ppf -> fun e -> (fprintf ppf "%a@\n" Print.entry e)

   let dump = fun ppf -> fun e -> (fprintf ppf "%a@\n" Dump.entry e)

   let trace_parser = (ref false )

   let mk =
    fun g ->
     fun n ->
      {egram = g; ename = n; estart = ( (empty_entry n) );
       econtinue = (
        fun _ ->
         fun _ ->
          fun _ -> fun (__strm : _ Stream.t) -> (raise Stream.Failure ) );
       edesc = ( (Dlevels (([]))) )}

   let action_parse =
    fun entry ->
     fun ts ->
      ((try
         let p = if !trace_parser then Format.fprintf else Format.ifprintf in
         let () = (p Format.err_formatter "@[<4>%s@ " ( entry.ename )) in
         let res = ((entry.estart) 0 ts) in
         let () = (p Format.err_formatter "@]@.") in res
        with
        | Stream.Failure ->
           (FanLoc.raise ( (get_prev_loc ts) ) (
             (Stream.Error ("illegal begin of " ^ ( entry.ename ))) ))
        | (FanLoc.Exc_located (_, _) as exc) -> (raise exc)
        | exc -> (FanLoc.raise ( (get_prev_loc ts) ) exc)) : Action.t)

   let lex =
    fun entry -> fun loc -> fun cs -> (((entry.egram).glexer) loc cs)

   let lex_string =
    fun entry ->
     fun loc -> fun str -> (lex entry loc ( (Stream.of_string str) ))

   let filter =
    fun entry ->
     fun ts ->
      (keep_prev_loc (
        (Token.Filter.filter ( (get_filter ( entry.egram )) ) ts) ))

   let parse_tokens_after_filter =
    fun entry -> fun ts -> (Action.get ( (action_parse entry ts) ))

   let parse_tokens_before_filter =
    fun entry ->
     fun ts -> (parse_tokens_after_filter entry ( (filter entry ts) ))

   let parse =
    fun entry ->
     fun loc ->
      fun cs -> (parse_tokens_before_filter entry ( (lex entry loc cs) ))

   let parse_string =
    fun entry ->
     fun loc ->
      fun str ->
       (parse_tokens_before_filter entry ( (lex_string entry loc str) ))

   let of_parser =
    fun g ->
     fun n ->
      fun (p :
        ((Token.t * token_info) Stream.t -> 'a)) ->
       (let f = fun ts -> (Action.mk ( (p ts) )) in
        {egram = g; ename = n; estart = ( fun _ -> f );
         econtinue = (
          fun _ ->
           fun _ ->
            fun _ -> fun (__strm : _ Stream.t) -> (raise Stream.Failure ) );
         edesc = ( (Dparser (f)) )} : 'a t)

   let setup_parser =
    fun e ->
     fun (p :
       ((Token.t * token_info) Stream.t -> 'a)) ->
      let f = fun ts -> (Action.mk ( (p ts) )) in
      (
      e.estart <- fun _ -> f
      );
      (
      e.econtinue <-
       fun _ ->
        fun _ ->
         fun _ -> fun (__strm : _ Stream.t) -> (raise Stream.Failure )
      );
      e.edesc <- (Dparser (f))

   let clear =
    fun e ->
     (
     e.estart <-
      fun _ -> fun (__strm : _ Stream.t) -> (raise Stream.Failure )
     );
     (
     e.econtinue <-
      fun _ ->
       fun _ -> fun _ -> fun (__strm : _ Stream.t) -> (raise Stream.Failure )
     );
     e.edesc <- (Dlevels (([])))

   let obj = fun x -> x

  end
