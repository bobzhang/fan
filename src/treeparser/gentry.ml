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

let get_levels (x:'a t)=  x.levels

let fresh_with_levels (x:'a t) levels =
  begin 
    x.levels <- levels;
    x.start <- Gparser.start_parser_of_entry x ;
    x.continue <- Gparser.continue_parser_of_entry x
  end

    
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

type 'a single_extend_statement = {
    entry : 'a t ;
    olevel : Gdefs.olevel
  }
      
let extend_single {entry; olevel= (lb  : Gdefs.olevel)} =
  let olevel = Ginsert.scan_olevel entry lb in
  let elev = Ginsert.insert_olevel entry lb.label olevel in
  (entry.levels <-  elev;
   entry.start <-Gparser.start_parser_of_entry entry;
   entry.continue <- Gparser.continue_parser_of_entry entry)

(* let protect (entry:Gdefs.entry) lb action = *)
(*   let old = entry.levels in *)
(*   try  *)
(*     let olevel = Ginsert.scan_olevel entry lb in *)
(*     let elev = Ginsert.insert_olevel entry lb.label olevel in *)
(*     (entry.levels <-  elev; *)
(*      entry.start <-Gparser.start_parser_of_entry entry; *)
(*      entry.continue <- Gparser.continue_parser_of_entry entry); *)
(*     action entry *)
(*   with *)
(*     x -> *)
(*       begin *)
(*         entry.levels <- old; *)
(*         entry.start <- Gparser.start_parser_of_entry entry; *)
(*         entry.continue <- Gparser.continue_parser_of_entry entry; *)
(*         raise x *)
(*       end *)


let protects  (es : 'a single_extend_statement list ) action =
  let olds =
    List.map (fun (x:'a single_extend_statement) -> x.entry.levels) es in
  try begin 
    List.iter (fun e -> extend_single e ) es;
    let res = action () in
    begin
      List.iter2 (fun e t ->
        begin
          e.entry.levels <- t;
          e.entry.start <- Gparser.start_parser_of_entry e.entry;
          e.entry.continue <- Gparser.continue_parser_of_entry e.entry;
        end) es olds ;
    res
    end
  end
  with
    x ->
      begin
        List.iter2
          (fun e t ->
            begin
              e.entry.levels <- t;
              e.entry.start <- Gparser.start_parser_of_entry e.entry;
              e.entry.continue <- Gparser.continue_parser_of_entry e.entry;
            end) es olds;
        raise x 
      end

  
let copy (e:Gdefs.entry) : Gdefs.entry =
  let result =
    {e with start =  (fun _ -> assert false );
     continue= fun _ -> assert false;
   } in
  (result.start <- Gparser.start_parser_of_entry result;
   result.continue <- Gparser.continue_parser_of_entry result;
   result)

let refresh_level ~f (x:Gdefs.level)  =
  Ginsert.level_of_olevel
    {label = Some x.level;
     lassoc =  x.lassoc;
     productions = f x.productions 
   }
(* let unsafe_extend_single entry *)
(*     (lb : Gdefs.olevel) *)
(*     = *)
(*   let olevel = Ginsert.unsafe_scan_olevel entry lb in *)
(*   let elev = Ginsert.insert_olevel entry lb.label olevel in *)
(*   (entry.levels <- elev; *)
(*    entry.start <-Gparser.start_parser_of_entry entry; *)
(*    entry.continue <- Gparser.continue_parser_of_entry entry) *)

    
(* let extend_single = Ginsert.extend_single ;; *)

(* let protect = Ginsert.protect *)
    
(* let copy = Ginsert.copy;; *)
(* let unsafe_extend_single = Ginsert.unsafe_extend_single;; *)
let entry_first = Gtools.entry_first
let delete_rule = Gdelete.delete_rule
let symb_failed = Gfailed.symb_failed
let symb_failed_txt = Gfailed.symb_failed_txt
let parser_of_symbol = Gparser.parser_of_symbol    

    
(* local variables: *)
(* compile-command: "pmake gentry.cmo" *)
(* end: *)
