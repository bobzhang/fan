(** Gparser module *)
open Gstructure


(** apply the [parse_fun] and
   get the result and the covered location of consumed areas *)    
val with_loc: 'b Tokenf.parse -> ('b*Locf.t) Tokenf.parse

(** given a level string, return a number from 0
   {[
   Gparser.level_number (Obj.magic expr) "top";
   - : int = 0
   Gparser.level_number (Obj.magic expr) "simple";
   - : int = 16
   ]} *)  
val level_number: entry -> string -> int


(** It outputs a stateful parser, but its generation process is functional *)    
val parser_of_tree :
    entry -> int * assoc -> Gaction.t Stack.t ->  tree ->
      (Gaction.t * Locf.t) Tokenf.parse


(** pure, no exception thrown out  *)
val parser_of_terminals :
    terminal list -> Tokenf.t list option  Tokenf.parse


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
