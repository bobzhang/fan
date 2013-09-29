open Tag_regexp





type t_transition =
  | OnChars of int
  | ToAction of int

module Tags :Set.S with type elt = tag_info
      
type transition = t_transition * Tags.t
      


module TagMap : Map.S with type key = tag_info

module TransSet : Set.S with type elt = transition

val firstpos : regexp -> TransSet.t

val followpos : int ->
  ('a Translate_lex.lexer_entry * 'b) list -> TransSet.t array
