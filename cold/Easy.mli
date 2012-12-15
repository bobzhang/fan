open Ast
  
val gen_str_item :
  ?module_name:string ->
  ?arity:int ->
  ?trail:expr ->
  id:FSig.basic_id_transform ->
  names:string list ->
  mk_tuple:(FSig.ty_info list -> expr) ->
  mk_record:(FSig.record_info -> expr) ->
  (string -> FSig.ty_info list -> expr) ->
  FSig.module_types -> str_item
      
val gen_object :
  ?module_name:string ->
  ?arity:int ->
  ?trail:expr ->
  kind:FSig.k ->
  base:string ->
  class_name:string ->
  names:string list ->
  mk_tuple:(FSig.ty_info list -> expr) ->
  mk_record:(FSig.record_info -> expr) ->
  (string -> FSig.ty_info list -> expr) ->
  FSig.module_types -> str_item
