

(** [fans] DDSL compiler *)

  
open Astf
(* val g : Gramf.gram
   grammar used by two entries [fan_quot] and [fan_quots]
   they should not be mixed with other entries which has
   diffierent gram
 *)

val fan_quots : exp Gramf.t
