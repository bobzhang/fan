open Gstructure


    
val with_loc: 'b Tokenf.parse -> ('b*Locf.t) Tokenf.parse

val level_number: entry -> string -> int

val parser_of_tree :
    entry -> int * assoc -> (Gaction.t * Locf.t) Stack.t ->  tree ->
      (Gaction.t * Locf.t) Tokenf.parse

val parser_of_terminals :
    terminal list -> (Gaction.t * Locf.t) list  Tokenf.parse


(**
  {[
  let a : Tokenf.t = Obj.magic & Gparser.parser_of_terminals
  [`Skeyword "a";`Skeyword "b"; `Skeyword "c"]
  (fun _ v _  -> Gaction.mk (fun  c b a ->  v))
  [< (`Key "a",_loc) ; (`Key "b", _loc); (`Key "c",_loc) >];
  val a : Tokenf.t = `Key "c"
  ]}
 *)            
val parser_of_symbol :
    entry ->  symbol -> (Gaction.t * Locf.t) Tokenf.parse
    

val start_parser_of_levels : entry -> level list -> int -> Gaction.t Tokenf.parse 

(** [start] would call [continue] *)
val start_parser_of_entry :  entry ->  int -> Gaction.t Tokenf.parse 

val continue_parser_of_levels : entry -> int -> level list -> int -> Gaction.t cont_parse 
val continue_parser_of_entry :  entry -> int -> Gaction.t cont_parse
