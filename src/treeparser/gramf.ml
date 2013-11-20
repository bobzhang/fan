

open Format
include Gdefs
  
include Gentry


let mk_action = Gaction.mk
type action = Gaction.t 


(* FIXME duplicate some code from Entry *)
let mk f = mk_dynamic (* gram *) f


let debug_origin_token_stream (entry:'a t ) tokens : 'a =
  parse_origin_tokens entry tokens
  
    
let wrap_stream_parser ?(loc=Locf.mk "<stream>") p s =
  try p ~loc s
  with Locf.Exc_located(loc,e) -> begin
    eprintf "error: %s" (Locf.to_string loc) ;
    Locf.raise loc e;
  end 











(* local variables: *)
(* compile-command: "cd .. && pmake treeparser/gramf.cmo" *)
(* end: *)
