open Gstructure


    
val with_loc: 'b Ftoken.parse -> ('b*Locf.t) Ftoken.parse

val level_number: entry -> string -> int

val parser_of_tree :
    entry -> int * assoc -> (Gaction.t * Locf.t) Stack.t ->  tree ->
      (Gaction.t * Locf.t) Ftoken.parse

val parser_of_terminals :
    terminal list -> (Gaction.t * Locf.t) list  Ftoken.parse


(**
  {[
  let a : Ftoken.t = Obj.magic & Gparser.parser_of_terminals
  [`Skeyword "a";`Skeyword "b"; `Skeyword "c"]
  (fun _ v _  -> Gaction.mk (fun  c b a ->  v))
  [< (`Key "a",_loc) ; (`Key "b", _loc); (`Key "c",_loc) >];
  val a : Ftoken.t = `Key "c"
  ]}
 *)            
val parser_of_symbol :
    entry ->  symbol -> (Gaction.t * Locf.t) Ftoken.parse
    

val start_parser_of_levels : entry -> level list -> int -> Gaction.t Ftoken.parse 

(** [start] would call [continue] *)
val start_parser_of_entry :  entry ->  int -> Gaction.t Ftoken.parse 

val continue_parser_of_levels : entry -> int -> level list -> int -> Gaction.t cont_parse 
val continue_parser_of_entry :  entry -> int -> Gaction.t cont_parse
