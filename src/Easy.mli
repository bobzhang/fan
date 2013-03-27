open Ast 

val gen_stru :
  ?module_name:string ->
  ?arity:int ->
  ?default:exp ->
  ?cons_transform:(string -> string) ->
  id:FSig.basic_id_transform ->
  ?names:string list ->
  mk_tuple:(FSig.ty_info list -> exp) ->
  mk_record:(FSig.record_col list -> exp) ->
  mk_variant:(string -> FSig.ty_info list -> exp) -> unit -> 
  FSig.module_types -> stru
val gen_object :
  ?module_name:string ->
  ?arity:int ->
  ?default:exp ->
  ?cons_transform:(string -> string) ->
  kind:FSig.kind ->
  base:string ->
  class_name:string ->
  ?names:string list ->
  mk_tuple:(FSig.ty_info list -> exp) ->
  mk_record:(FSig.record_col list -> exp) ->
  mk_variant:(string -> FSig.ty_info list -> exp) -> unit -> 
  FSig.module_types -> stru
