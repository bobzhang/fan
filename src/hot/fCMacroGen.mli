
(** Deprecated *)
open FAst

    
type 'a item_or_def =
  | Str of 'a
  | Def of string * (string list * exp) option
  | Und of string

    
val define :
  exp:exp Fgram.t ->
  pat:pat Fgram.t -> (string list * exp) option -> string -> unit
      
val undef : exp:'a Fgram.t -> pat:'b Fgram.t -> string -> unit
    
val execute_macro :
  exp:exp Fgram.t ->
  pat:pat Fgram.t -> 'a ->  'a item_or_def -> 'a
