type 'a t  =  Gdefs.entry

let name (e:'a t) = e.name

let map ~name (f : 'a -> 'b) (e:'a t) : 'b t =
  (Obj.magic {
   e with
   start =  (fun lev str -> (Obj.magic (f (Obj.magic (e.start lev str ) : 'a) ) : Gaction.t));
   name ; } : 'b t)
    
let print ppf e = Format.fprintf ppf "%a@\n" Gprint.text#entry e

let dump ppf e = Format.fprintf ppf "%a@\n" Gprint.dump#entry e

let trace_parser = ref false


let mk_dynamic  n : 'a t ={
  name = n;
  start = Gtools.empty_entry n;
  continue  = (fun _ _ _ _ -> raise Streamf.NotConsumed);
  levels =  [] ;
  freezed = false;     
}

let clear (e:'a t) = begin 
  e.start <- (fun _ -> fun _ -> raise Streamf.NotConsumed);
  e.continue <- (fun _ _ _ -> fun _-> raise Streamf.NotConsumed);
  e.levels <- []
end

let obj x = x
let repr x = x



(** driver of the parse, it would call [start] *)
let action_parse (entry:'a t) (ts: Tokenf.stream) : Gaction.t =
  try 
    entry.start 0 ts 
  with
  | Streamf.NotConsumed ->
      Locf.raise (Token_stream.cur_loc ts) (Streamf.Error ("illegal begin of " ^ entry.name))
  | Locf.Exc_located (_, _) as exc -> raise exc
  | exc -> 
      Locf.raise (Token_stream.cur_loc ts) exc

let eoi_action_parse (entry:'a t) (ts: Tokenf.stream) : Gaction.t =
  try 
    let res = entry.start 0 ts in
    match Streamf.peek ts with
    | Some (`EOI _) -> res
    | Some x  -> Locf.failf (Tokenf.get_loc x)  "EOI expected"
    | None -> res
  with
  | Streamf.NotConsumed ->
      Locf.raise (Token_stream.cur_loc ts) (Streamf.Error ("illegal begin of " ^ entry.name))
  | Locf.Exc_located (_, _) as exc -> raise exc
  | exc -> 
      Locf.raise (Token_stream.cur_loc ts) exc

let parse_tokens_eoi entry stream =         
  Gaction.get (eoi_action_parse entry stream)
    
let parse_origin_tokens entry stream =
  Gaction.get (action_parse entry stream)


let extend_single = Ginsert.extend_single ;;

let protect = Ginsert.protect
    
let copy = Ginsert.copy;;
let unsafe_extend_single = Ginsert.unsafe_extend_single;;
let entry_first = Gtools.entry_first
let delete_rule = Gdelete.delete_rule
let symb_failed = Gfailed.symb_failed
let symb_failed_txt = Gfailed.symb_failed_txt
let parser_of_symbol = Gparser.parser_of_symbol    

    
(* local variables: *)
(* compile-command: "pmake gentry.cmo" *)
(* end: *)
