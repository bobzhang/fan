
(** Deprecated *)
open FAst
  
type 'a item_or_def =
  | Str of 'a
  | Def of string * (string list * exp) option
  | Und of string
  | ITE of bool * 'a item_or_def list * 'a item_or_def list
  | Lazy of 'a Lazy.t
val defined : (string * (string list * exp) option) list ref
val is_defined : string -> bool
val incorrect_number : FLoc.t -> 'a list -> 'b list -> 'c
val define :
  exp:exp Fgram.t ->
  pat:pat Fgram.t -> (string list * exp) option -> string -> unit
val undef : exp:'a Fgram.t -> pat:'b Fgram.t -> string -> unit
val parse_def : exp:exp Fgram.t -> pat:pat Fgram.t -> string -> unit
val execute_macro :
  exp:exp Fgram.t ->
  pat:pat Fgram.t -> 'a -> ('a -> 'a -> 'a) -> 'a item_or_def -> 'a
val execute_macro_list :
  exp:exp Fgram.t ->
  pat:pat Fgram.t -> 'a -> ('a -> 'a -> 'a) -> 'a item_or_def list -> 'a
val stack : bool Stack.t
val make_ITE_result :
  'a item_or_def list -> 'a item_or_def list -> 'a item_or_def
type branch = Then | Else
val execute_macro_if_active_branch :
  exp:exp Fgram.t ->
  pat:pat Fgram.t ->
  'a -> 'b -> ('b -> 'b -> 'b) -> branch -> 'b item_or_def -> 'b item_or_def
