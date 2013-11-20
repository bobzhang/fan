

(** [fans] DDSL compiler *)

  

(* val g : Gramf.gram
   grammar used by two entries [fan_quot] and [fan_quots]
   they should not be mixed with other entries which has
   diffierent gram
 *)

val fan_quots : Astf.exp Gramf.t
val fan_quot : unit Gramf.t
