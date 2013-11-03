
%import{
Format:
  fprintf
  eprintf
  ;
 
};;




type 'a t  =  Gdefs.entry

let name (e:'a t) = e.name

let map ~name (f : 'a -> 'b) (e:'a t) : 'b t =
  (Obj.magic
     {
   e with
   start =  (fun lev str -> (Obj.magic (f (Obj.magic (e.start lev str ) : 'a) ) : Gaction.t));
   name ;
 } : 'b t)
    
let print ppf e = fprintf ppf "%a@\n" Gprint.text#entry e

let dump ppf e = fprintf ppf "%a@\n" Gprint.dump#entry e

let trace_parser = ref false


let mk_dynamic g n : 'a t ={
  gram = g;
  name = n;
  start = Gtools.empty_entry n;
  continue _ _ _ = %parser{ | };
  levels =  [] ;
  freezed = false;     
}

let clear (e:'a t) = begin 
  e.start <- fun _ -> fun _ -> raise Streamf.NotConsumed;
  e.continue <- fun _ _ _ -> fun _-> raise Streamf.NotConsumed;
  e.levels <- []
end

let obj x = x
let repr x = x



let gram_of_entry (e:'a t) = e.gram

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

let filter_and_parse_tokens (entry:'a t) ts =
  parse_origin_tokens entry (Tokenf.filter entry.gram.gfilter  ts)
    



let lex_string loc str = Flex_lib.from_stream  loc (Streamf.of_string str)

(* Raw lexing follwed by a
   filter 
 *)    
let parse_string ?(lexer=Flex_lib.from_stream) ?(loc=Locf.string_loc) (entry:'a t)  str =
  str
   |> Streamf.of_string
   |> lexer loc
   |> Tokenf.filter entry.gram.gfilter
   |> parse_origin_tokens entry

       
let parse (entry:'a t) loc cs =
  parse_origin_tokens entry
    (Tokenf.filter entry.gram.gfilter
       (Flex_lib.from_stream loc cs))
;;

%import{
Ginsert:
  extend
  extend_single
  copy extend
  unsafe_extend
  unsafe_extend_single;
Gtools:
  entry_first;
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
};;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gentry.cmo" *)
(* end: *)
