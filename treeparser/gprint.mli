
(** Pretty printer for Fan's grammar *)
open Gstructure
open Format

(**  signature for printing grammar *)
class type grammar_print  = object

  (* method description : formatter -> description -> unit *)
  method entry : formatter -> entry -> unit
  method level : formatter -> level -> unit
  method levels : formatter -> level list -> unit

  method set_action : bool -> unit
      
  method production : formatter -> production -> unit
  method productions : formatter -> production list -> unit

  method rule : formatter -> symbol list -> unit      
  method rules : formatter -> symbol list list -> unit

  method symbol : formatter -> symbol -> unit
  method symbol1 : formatter -> symbol -> unit
  method tree : formatter -> tree -> unit
end
      
      
val pp_assoc : Format.formatter -> assoc -> unit
      
class text_grammar : grammar_print

(** inherit from [text_grammar] with customized behavior for
    [tree] and [level]
 *)    
class dump_grammar : grammar_print 
    
val text : text_grammar
val string_of_symbol : symbol -> string
val dump : dump_grammar
