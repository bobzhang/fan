
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
  exp:exp Gram.t ->
  pat:pat Gram.t -> (string list * exp) option -> string -> unit
val undef : exp:'a Gram.t -> pat:'b Gram.t -> string -> unit
val parse_def : exp:exp Gram.t -> pat:pat Gram.t -> string -> unit
val execute_macro :
  exp:exp Gram.t ->
  pat:pat Gram.t -> 'a -> ('a -> 'a -> 'a) -> 'a item_or_def -> 'a
val execute_macro_list :
  exp:exp Gram.t ->
  pat:pat Gram.t -> 'a -> ('a -> 'a -> 'a) -> 'a item_or_def list -> 'a
val stack : bool Stack.t
val make_ITE_result :
  'a item_or_def list -> 'a item_or_def list -> 'a item_or_def
type branch = Then | Else
val execute_macro_if_active_branch :
  exp:exp Gram.t ->
  pat:pat Gram.t ->
  'a -> 'b -> ('b -> 'b -> 'b) -> branch -> 'b item_or_def -> 'b item_or_def
