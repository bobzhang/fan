
open LibUtil
open Format
open Gstructure
open Gtools
open Ftoken

  
type 'a t  =  entry

let name (e:'a t) = e.name
let print ppf e = fprintf ppf "%a@\n" Gprint.text#entry e
let dump ppf e = fprintf ppf "%a@\n" Gprint.dump#entry e
let trace_parser = ref false


let mk_dynamic g n : 'a t ={
  gram = g;
  name = n;
  start = empty_entry n;
  continue _ _ _ = parser | ;
  desc = Dlevels [] ;
  freezed = false;     
}

(* [estart] The main entrance to consume the parser  *)  
let action_parse (entry:'a t) (ts: stream) : Gaction.t =
  try 
    let p = if !trace_parser then Format.fprintf else Format.ifprintf in
    (p Format.err_formatter "@[<4>%s@ " entry.name ;
    let res = entry.start 0 ts in
    let () = p Format.err_formatter "@]@." in
    res)
  with
  | XStream.NotConsumed ->
      FLoc.raise (get_cur_loc ts)
        (XStream.Error ("illegal begin of " ^ entry.name))
  | FLoc.Exc_located (_, _) as exc -> 
      (eprintf "%s@." (Printexc.to_string exc); raise exc)
  | exc -> 
      (eprintf "%s@." (Printexc.to_string exc);
      FLoc.raise (get_cur_loc ts) exc)


(* stream parser is not extensible *)  
let of_parser g n (p : stream -> 'a) : 'a t   =
  let f ts = Gaction.mk (p ts) in {
  gram = g;
  name = n;
  start _ = f;
  continue _ _ _ = parser |;
  desc = Dparser f;
  freezed = true (* false *);    
}

let setup_parser (e:'a t) (p : stream -> 'a) =
  let f ts = Gaction.mk (p ts) in begin
    e.start <- fun _ -> f;
    e.continue <- fun _ _ _ -> fun _ -> raise XStream.NotConsumed;
    e.desc <- Dparser f
  end

let clear (e:'a t) = begin 
  e.start <- fun _ -> fun _ -> raise XStream.NotConsumed;
  e.continue <- fun _ _ _ -> fun _-> raise XStream.NotConsumed;
  e.desc <- Dlevels []
end

let obj x = x
let repr x = x


let name_of_entry (e:'a t) = e.name
let gram_of_entry (e:'a t) = e.gram
let parse_origin_tokens entry ts = Gaction.get (action_parse entry ts)
let filter_and_parse_tokens (entry:'a t) ts =
  parse_origin_tokens entry (FanTokenFilter.filter entry.gram.gfilter  ts)
    
(** set the default lexer *)
let glexer = Flex_lib.from_stream 

(* UNfiltered token stream *)
let lex  loc cs =  glexer loc cs

let lex_string loc str = lex  loc (XStream.of_string str)

let parse_string ?(loc=FLoc.string_loc) (entry:'a t)  str =
  parse_origin_tokens entry
    (FanTokenFilter.filter entry.gram.gfilter
       (glexer loc (XStream.of_string str)))
    
let parse (entry:'a t) loc cs =
  parse_origin_tokens entry
    (FanTokenFilter.filter entry.gram.gfilter
       (glexer loc cs))
;;

{:import|
Ginsert:
  levels_of_entry
  extend
  extend_single
  copy extend
  unsafe_extend
  unsafe_extend_single;
Gdelete:
  delete_rule;
Gfailed: 
  symb_failed
  symb_failed_txt;
Gparser:
   parser_of_symbol;
Ginsert:
   (* buggy *)
   eoi_entry;
|};;


