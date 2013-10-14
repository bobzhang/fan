
%import{
Format:
  fprintf
  eprintf
  ;
 
};;




type 'a t  =  Gstructure.entry

let name (e:'a t) = e.name

let print ppf e = fprintf ppf "%a@\n" Gprint.text#entry e

let dump ppf e = fprintf ppf "%a@\n" Gprint.dump#entry e

let trace_parser = ref false


let mk_dynamic g n : 'a t ={
  gram = g;
  name = n;
  start = Gtools.empty_entry n;
  continue _ _ _ = parser | ;
  desc = Dlevels [] ;
  freezed = false;     
}






let of_parser g n (p : Ftoken.stream -> 'a) : 'a t   =
  let f ts = Gaction.mk (p ts) in {
  gram = g;
  name = n;
  start  = fun _ -> f;
  continue =  fun _ _ _  _  -> raise Fstream.NotConsumed;
  desc = Dparser f;
  freezed = true (* false *);    
}

let setup_parser (e:'a t) (p : Ftoken.stream -> 'a) =
  let f ts = Gaction.mk (p ts) in begin
    e.start <- fun _ -> f;
    e.continue <- fun _ _ _ -> fun _ -> raise Fstream.NotConsumed;
    e.desc <- Dparser f
  end

let clear (e:'a t) = begin 
  e.start <- fun _ -> fun _ -> raise Fstream.NotConsumed;
  e.continue <- fun _ _ _ -> fun _-> raise Fstream.NotConsumed;
  e.desc <- Dlevels []
end

let obj x = x
let repr x = x



let gram_of_entry (e:'a t) = e.gram


let action_parse (entry:'a t) (ts: Ftoken.stream) : Gaction.t =
  try 
    let p = if !trace_parser then Format.fprintf else Format.ifprintf in
    (p Format.err_formatter "@[<4>%s@ " entry.name ;
    let res = entry.start 0 ts in
    let () = p Format.err_formatter "@]@." in
    res)
  with
  | Fstream.NotConsumed ->
      Locf.raise (Gtools.get_cur_loc ts) (Fstream.Error ("illegal begin of " ^ entry.name))
  | Locf.Exc_located (_, _) as exc -> raise exc
  | exc -> 
      Locf.raise (Gtools.get_cur_loc ts) exc
    
let parse_origin_tokens entry stream =
  Gaction.get (action_parse entry stream)

let filter_and_parse_tokens (entry:'a t) ts =
  parse_origin_tokens entry (FanTokenFilter.filter entry.gram.gfilter  ts)
    



let lex_string loc str = Flex_lib.from_stream  loc (Fstream.of_string str)

let parse_string ?(lexer=Flex_lib.from_stream) ?(loc=Locf.string_loc) (entry:'a t)  str =
  str
   |> Fstream.of_string |> lexer loc
   |> FanTokenFilter.filter entry.gram.gfilter
   |> parse_origin_tokens entry

       
let parse (entry:'a t) loc cs =
  parse_origin_tokens entry
    (FanTokenFilter.filter entry.gram.gfilter
       (Flex_lib.from_stream loc cs))
;;

%import{
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
};;
