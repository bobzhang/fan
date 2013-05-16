
open LibUtil
open Format
open Gstructure
open Gtools
open FanToken

  
type 'a t  =  entry

let name e = e.ename
let print ppf e = fprintf ppf "%a@\n" Gprint.text#entry e
let dump ppf e = fprintf ppf "%a@\n" Gprint.dump#entry e
let trace_parser = ref false


let mk_dynamic g n ={
  egram = g;
  ename = n;
  estart = empty_entry n;
  econtinue _ _ _ = parser | ;
  edesc = Dlevels [] ;
  freezed = false;     
}

(* [estart] The main entrance to consume the parser  *)  
let action_parse entry (ts: stream) : Gaction.t =
  try 
    let p = if !trace_parser then Format.fprintf else Format.ifprintf in
    (p Format.err_formatter "@[<4>%s@ " entry.ename ;
    let res = entry.estart 0 ts ;
    p Format.err_formatter "@]@." ;
    res)
  with
  | XStream.Failure ->
      FanLoc.raise (get_cur_loc ts)
        (XStream.Error ("illegal begin of " ^ entry.ename))
  | FanLoc.Exc_located (_, _) as exc -> 
      (eprintf "%s@." (Printexc.to_string exc); raise exc)
  | exc -> 
      (eprintf "%s@." (Printexc.to_string exc);
      FanLoc.raise (get_prev_loc ts) exc)


(* stream parser is not extensible *)  
let of_parser g n (p : stream -> 'a)   =
  let f ts = Gaction.mk (p ts) in {
  egram = g;
  ename = n;
  estart _ = f;
  econtinue _ _ _ = parser |;
  edesc = Dparser f;
  freezed = true (* false *);    
}

let setup_parser e (p : stream -> 'a) =
  let f ts = Gaction.mk (p ts) in begin
    e.estart <- fun _ -> f;
    e.econtinue <- fun _ _ _ -> fun _ -> raise XStream.Failure;
    e.edesc <- Dparser f
  end

let clear e = begin 
  e.estart <- fun _ -> fun _ -> raise XStream.Failure;
  e.econtinue <- fun _ _ _ -> fun _-> raise XStream.Failure;
  e.edesc <- Dlevels []
end

let obj x = x
let repr x = x


let name_of_entry {ename;_} = ename
let gram_of_entry {egram;_} = egram
let parse_origin_tokens entry ts = Gaction.get (action_parse entry ts)
let filter_and_parse_tokens entry ts =
  parse_origin_tokens entry (FanTokenFilter.filter entry.egram.gfilter  ts)
    
let glexer = FanLexUtil.mk ()

(* UNfiltered token stream *)
let lex  loc cs =  glexer loc cs

let lex_string loc str = lex  loc (XStream.of_string str)

let parse_string ?(loc=FanLoc.string_loc) entry  str =
  parse_origin_tokens entry
    (FanTokenFilter.filter entry.egram.gfilter
       (glexer loc (XStream.of_string str)))
    
let parse entry loc cs =
  parse_origin_tokens entry
    (FanTokenFilter.filter entry.egram.gfilter
       (glexer loc cs))

  
    
let delete_rule = Gdelete.delete_rule
let symb_failed = Gfailed.symb_failed
let symb_failed_txt = Gfailed.symb_failed_txt

let parser_of_symbol = Gparser.parser_of_symbol


let levels_of_entry = Ginsert.levels_of_entry
let extend = Ginsert.extend
let extend_single = Ginsert.extend_single
let copy = Ginsert.copy    

(* buggy*)
(* let eoi_entry = Ginsert.eoi_entry *)
