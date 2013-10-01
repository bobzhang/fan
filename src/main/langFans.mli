

(** [fans] DDSL compiler *)

  
open FAst
(* val g : Fgram.gram
   grammar used by two entries [fan_quot] and [fan_quots]
   they should not be mixed with other entries which has
   diffierent gram
 *)

val fan_quots : exp Fgram.t
