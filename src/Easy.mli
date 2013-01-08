open Ast 

val gen_str_item :
  ?module_name:string ->
  ?arity:int ->
  ?trail:expr ->
  ?cons_transform:(string -> string) ->
  id:FSig.basic_id_transform ->
  names:string list ->
  mk_tuple:(FSig.ty_info list -> expr) ->
  mk_record:(FSig.record_col list -> expr) ->
  mk_variant:(string -> FSig.ty_info list -> expr) -> unit -> 
  FSig.module_types -> str_item
val gen_object :
  ?module_name:string ->
  ?arity:int ->
  ?trail:expr ->
  ?cons_transform:(string -> string) ->
  kind:FSig.k ->
  base:string ->
  class_name:string ->
  names:string list ->
  mk_tuple:(FSig.ty_info list -> expr) ->
  mk_record:(FSig.record_col list -> expr) ->
  mk_variant:(string -> FSig.ty_info list -> expr) -> unit -> 
  FSig.module_types -> str_item
