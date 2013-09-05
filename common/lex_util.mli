val nullable : Automata_def.regexp -> bool
val tag_compare : 'a -> 'a -> int
type t_transition = OnChars of int | ToAction of int
module Tags :Set.S with type elt = Automata_def.tag_info

module TagMap : Map.S with type key = Automata_def.tag_info

type transition = t_transition * Tags.t
val trans_compare : 'a * Tags.t -> 'a * Tags.t -> int
module TransSet : Set.S with type elt = transition

val emptymatch : Automata_def.regexp -> Tags.t
val addtags : TransSet.t -> Tags.t -> TransSet.t
val firstpos : Automata_def.regexp -> TransSet.t
val followpos :
  int -> (Automata_def.lexer_entry * 'a) list -> TransSet.t array
