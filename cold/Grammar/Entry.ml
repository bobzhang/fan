open LibUtil
open Format
open Structure
open Tools
open FanToken
type 'a t = entry 
let name e = e.ename
let print ppf e = fprintf ppf "%a@\n" Print.text#entry e
let dump ppf e = fprintf ppf "%a@\n" Print.dump#entry e
let trace_parser = ref false
let mk_dynamic g n =
  {
    egram = g;
    ename = n;
    estart = (empty_entry n);
    econtinue =
      (fun _  _  _  (__strm : _ XStream.t)  -> raise XStream.Failure);
    edesc = (Dlevels []);
    freezed = false
  }
let action_parse entry (ts : stream) =
  (try
     let p =
       if trace_parser.contents then Format.fprintf else Format.ifprintf in
     p Format.err_formatter "@[<4>%s@ " entry.ename;
     (let res = entry.estart 0 ts in p Format.err_formatter "@]@."; res)
   with
   | XStream.Failure  ->
       FanLoc.raise (get_prev_loc ts)
         (XStream.Error ("illegal begin of " ^ entry.ename))
   | FanLoc.Exc_located (_,_) as exc ->
       (eprintf "%s@." (Printexc.to_string exc); raise exc)
   | exc ->
       (eprintf "%s@." (Printexc.to_string exc);
        FanLoc.raise (get_prev_loc ts) exc) : Action.t )
let lex entry loc cs = (entry.egram).glexer loc cs
let lex_string entry loc str = lex entry loc (XStream.of_string str)
let parse_origin_tokens entry ts = Action.get (action_parse entry ts)
let filter entry ts = FanTokenFilter.filter (get_filter entry.egram) ts
let filter_and_parse_tokens entry ts =
  parse_origin_tokens entry (filter entry ts)
let parse entry loc cs = filter_and_parse_tokens entry (lex entry loc cs)
let parse_string entry loc str =
  filter_and_parse_tokens entry (lex_string entry loc str)
let of_parser g n (p : stream -> 'a) =
  let f ts = Action.mk (p ts) in
  {
    egram = g;
    ename = n;
    estart = (fun _  -> f);
    econtinue =
      (fun _  _  _  (__strm : _ XStream.t)  -> raise XStream.Failure);
    edesc = (Dparser f);
    freezed = true
  }
let setup_parser e (p : stream -> 'a) =
  let f ts = Action.mk (p ts) in
  e.estart <- (fun _  -> f);
  e.econtinue <-
    (fun _  _  _  (__strm : _ XStream.t)  -> raise XStream.Failure);
  e.edesc <- Dparser f
let clear e =
  e.estart <- (fun _  (__strm : _ XStream.t)  -> raise XStream.Failure);
  e.econtinue <-
    (fun _  _  _  (__strm : _ XStream.t)  -> raise XStream.Failure);
  e.edesc <- Dlevels []
let obj x = x
let repr x = x