open FanSig
open Format
open Structure
open Tools
type 'a t =  internal_entry  
let name (e) = e.ename
let print (ppf) (e) = (fprintf ppf "%a@\n" ( Print.text#entry ) e)
let dump (ppf) (e) = (fprintf ppf "%a@\n" ( Print.dump#entry ) e)
let trace_parser = (ref false )
let mk (g) (n) =
  {egram = g;ename = n;estart = ( (empty_entry n) );econtinue = (
                                                      (fun (_) ->
                                                        (fun (_) ->
                                                          (fun (_) ->
                                                            (fun
                                                              ((__strm : _
                                                                 Stream.t ))
                                                              ->
                                                              (raise
                                                                Stream.Failure
                                                                ))))) );
    edesc = ( Dlevels ([]) )}
let action_parse (entry) (ts) =
  (begin try
    let p =
      if trace_parser.contents then begin Format.fprintf
      end else begin Format.ifprintf
      end in
    let ()  = (p Format.err_formatter "@[<4>%s@ " ( entry.ename )) in
    let res = ((entry.estart) 0 ts) in
    let ()  = (p Format.err_formatter "@]@.") in res with
    | Stream.Failure  ->
        (FanLoc.raise ( (get_prev_loc ts) ) (
          Stream.Error (("illegal begin of " ^ ( entry.ename ))) ))
    | (FanLoc.Exc_located(_,_) as exc) ->   (raise exc)
    | exc ->   (FanLoc.raise ( (get_prev_loc ts) ) exc) end : Action.t  )
let lex (entry) (loc) (cs) = (((entry.egram).glexer) loc cs)
let lex_string (entry) (loc) (str) =
  (lex entry loc ( (Stream.of_string str) ))
let filter (entry) (ts) =
  (keep_prev_loc (
    (FanToken.Filter.filter ( (get_filter ( entry.egram )) ) ts) ))
let parse_origin_tokens (entry) (ts) =
  (Action.get ( (action_parse entry ts) ))
let filter_and_parse_tokens (entry) (ts) =
  (parse_origin_tokens entry ( (filter entry ts) ))
let parse (entry) (loc) (cs) =
  (filter_and_parse_tokens entry ( (lex entry loc cs) ))
let parse_string (entry) (loc) (str) =
  (filter_and_parse_tokens entry ( (lex_string entry loc str) ))
let of_parser (g) (n) ((p : (( token * token_info ) Stream.t  -> 'a) )) =
  let f (ts) = (Action.mk ( (p ts) )) in
  {egram = g;ename = n;estart = ( (fun (_) -> f) );econtinue = (
                                                     (fun (_) ->
                                                       (fun (_) ->
                                                         (fun (_) ->
                                                           (fun
                                                             ((__strm : _
                                                                Stream.t ))
                                                             ->
                                                             (raise
                                                               Stream.Failure
                                                               ))))) );
    edesc = ( Dparser (f) )}
let setup_parser (e) ((p : (( token * token_info ) Stream.t  -> 'a) )) =
  let f (ts) = (Action.mk ( (p ts) )) in
  begin
    e.estart <- (fun (_) -> f);
    e.econtinue <-
      (fun (_) ->
        (fun (_) ->
          (fun (_) ->
            (fun ((__strm : _ Stream.t )) -> (raise Stream.Failure )))));
    e.edesc <- Dparser (f)
    end
let clear (e) =
  begin
  e.estart <-
    (fun (_) -> (fun ((__strm : _ Stream.t )) -> (raise Stream.Failure )));
  e.econtinue <-
    (fun (_) ->
      (fun (_) ->
        (fun (_) ->
          (fun ((__strm : _ Stream.t )) -> (raise Stream.Failure )))));
  e.edesc <- Dlevels ([])
  end
let obj (x) = x