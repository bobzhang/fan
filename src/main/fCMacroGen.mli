
(** Deprecated *)
open FAst

    
type 'a item_or_def =
  | Str of 'a
  | Def of string * (string list * exp) option
  | Und of string

    
val define :
  exp:exp Gramf.t ->
  pat:pat Gramf.t -> (string list * exp) option -> string -> unit
      
val undef : exp:'a Gramf.t -> pat:'b Gramf.t -> string -> unit
    
val execute_macro :
  exp:exp Gramf.t ->
  pat:pat Gramf.t -> 'a ->  'a item_or_def -> 'a
