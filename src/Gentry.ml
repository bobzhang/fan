
open LibUtil
open Format
open Gstructure
open Gtools
open FanToken
open Ginsert
  
type 'a t  =  entry

let name e = e.ename

let print ppf e = fprintf ppf "%a@\n" Gprint.text#entry e
let dump ppf e = fprintf ppf "%a@\n" Gprint.dump#entry e
let trace_parser = ref false




let extend entry (position, levels) =
  let levels =  scan_olevels entry levels in (* for side effect *)
  let elev = insert_olevels_in_levels entry position levels in
  (entry.edesc <- Dlevels elev;
  entry.estart <-Gparser.start_parser_of_entry entry;
  entry.econtinue <- Gparser.continue_parser_of_entry entry)


    
let extend_single entry (position,level) = 
  let level = scan_olevel entry level in
  let elev = insert_olevel entry position level in
  (entry.edesc <-Dlevels elev;
  entry.estart <-Gparser.start_parser_of_entry entry;
  entry.econtinue <- Gparser.continue_parser_of_entry entry)

    
let mk_dynamic g n ={
  egram = g;
  ename = n;
  estart = empty_entry n;
  econtinue _ _ _ = (fun _ -> raise XStream.Failure);
  edesc = Dlevels [] ;
  freezed = false;     
}

(* [estart] The main entrance to consume the parser  *)  
let action_parse entry (ts: stream) : Gaction.t =
  try begin
    let p =
      if !trace_parser then
        Format.fprintf
      else Format.ifprintf ;
    p Format.err_formatter "@[<4>%s@ " entry.ename ;
    let res = entry.estart 0 ts ;
    p Format.err_formatter "@]@." ;
    res
  end
  with
  | XStream.Failure ->
        FanLoc.raise (get_cur_loc ts)
          (XStream.Error ("illegal begin of " ^ entry.ename))
  | FanLoc.Exc_located (_, _) as exc -> begin
      eprintf "%s@." (Printexc.to_string exc);
      raise exc
  end
  | exc -> begin
      eprintf "%s@." (Printexc.to_string exc);
      FanLoc.raise (get_prev_loc ts) exc
  end

(* stream parser is not extensible *)  
let of_parser g n (p : stream -> 'a)   =
  let f ts = Gaction.mk (p ts) in {
  egram = g;
  ename = n;
  estart _ = f;
  econtinue _ _ _ = fun _ -> raise XStream.Failure;
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

  
(* [eoi_entry] could be improved *)    
(* let eoi_entry entry = *)
(*   let open Gstru in *)
(*   let g = gram_of_entry entry in  *)
(*   let entry_eoi = (mk_dynamic g (name entry ^ "_eoi")) in *)
(*   begin *)
(*     {:extend| entry_eoi: [  entry{x}; `EOI -> x ] |} ; *)
(*     entry_eoi *)
(*   end *)

let delete_rule = Gdelete.delete_rule
let symb_failed = Gfailed.symb_failed
let symb_failed_txt = Gfailed.symb_failed_txt
let parser_of_symbol = Gparser.parser_of_symbol
let levels_of_entry = Ginsert.levels_of_entry    
