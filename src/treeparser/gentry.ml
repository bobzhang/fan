type 'a t  =  Gdefs.entry

let name (e:'a t) = e.name

let map ~name (f : 'a -> 'b) (e:'a t) : 'b t =
  (Obj.magic
     {
   e with
   start =  (fun lev str -> (Obj.magic (f (Obj.magic (e.start lev str ) : 'a) ) : Gaction.t));
   name ;
 } : 'b t)
    
let print ppf e = Format.fprintf ppf "%a@\n" Gprint.text#entry e

let dump ppf e = Format.fprintf ppf "%a@\n" Gprint.dump#entry e

let trace_parser = ref false

(* let filter_of_gram (x :'a t) = x.gram.gfilter *)

let mk_dynamic (* g *) n : 'a t ={
  (* gram = g; *)
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



(* let gram_of_entry (e:'a t) = e.gram *)

(** driver of the parse, it would call [start]
   
 *)
let action_parse (entry:'a t) (ts: Tokenf.stream) : Gaction.t =
  try 
    let p = if !trace_parser then Format.fprintf else Format.ifprintf in
    (p Format.err_formatter "@[<4>%s@ " entry.name ;
    let res = entry.start 0 ts in
    let () = p Format.err_formatter "@]@." in
    res)
  with
  | Streamf.NotConsumed ->
      Locf.raise (Token_stream.cur_loc ts) (Streamf.Error ("illegal begin of " ^ entry.name))
  | Locf.Exc_located (_, _) as exc -> raise exc
  | exc -> 
      Locf.raise (Token_stream.cur_loc ts) exc
    
let parse_origin_tokens entry stream =
  Gaction.get (action_parse entry stream)

(* let filter_and_parse_tokens (entry:'a t) ts = *)
(*   parse_origin_tokens entry (Tokenf.filter entry.gram.gfilter  ts) *)
       

let extend_single = Ginsert.extend_single ;;
let copy = Ginsert.copy;;
let unsafe_extend_single = Ginsert.unsafe_extend_single;;
let entry_first = Gtools.entry_first
let delete_rule = Gdelete.delete_rule
let symb_failed = Gfailed.symb_failed
let symb_failed_txt = Gfailed.symb_failed_txt
let parser_of_symbol = Gparser.parser_of_symbol    

    
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gentry.cmo" *)
(* end: *)
