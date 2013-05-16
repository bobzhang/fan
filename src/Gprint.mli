open Gstructure
open Format


class type grammar_print  = object
  method assoc : formatter -> assoc -> unit
  method description : formatter -> description -> unit
  method entry : formatter -> entry -> unit
  method level : formatter -> level -> unit
  method levels : formatter -> level list -> unit
  method list :
      ?sep:space_formatter ->
        ?first:space_formatter ->
          ?last:space_formatter ->
            (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  method meta :
      string list -> formatter -> symbol list -> unit
  method option :
      ?first:space_formatter ->
        ?last:space_formatter ->
          (formatter -> 'a -> unit) ->
            formatter -> 'a option -> unit
  method rule : formatter -> symbol list -> unit
  method production : formatter -> production -> unit
  method productions : formatter -> production list -> unit            
  method rules : formatter -> symbol list list -> unit
  method symbol : formatter -> symbol -> unit
  method symbol1 : formatter -> symbol -> unit
  method tree : formatter -> tree -> unit
end

      
class text_grammar : grammar_print

(** inherit from [text_grammar] with customized behavior for
    [tree] and [level]
 *)    
class dump_grammar : grammar_print 
    
val text : text_grammar
val string_of_symbol : symbol -> string
val dump : dump_grammar
