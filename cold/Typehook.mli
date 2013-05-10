open Ast
open FSig  

val print_collect_mtyps: bool ref


val show_code : bool ref

val register :
    ?filter:(string->bool) -> ?position:string ->
      plugin_name * (FSig.mtyps -> stru option) -> unit
val show_modules : unit -> unit
val plugin_add : plugin_name -> unit
val plugin_remove : plugin_name -> unit


class type traversal = object
  inherit Objs.map
  method get_cur_mtyps: FSig.mtyps
  method get_cur_and_types: FSig.and_types
  (* method in_and_types: *)
  method update_cur_and_types:
      (FSig.and_types -> FSig.and_types) -> unit
  method update_cur_mtyps:
      (FSig.mtyps -> FSig.mtyps) -> unit

end
val traversal: unit -> traversal    

(* val g : Gram.gram
   grammar used by two entries [fan_quot] and [fan_quots]
   they should not be mixed with other entries which has
   diffierent gram
 *)
val fan_quot : exp Gram.t
val fan_quots : exp Gram.t
val include_quot: stru Gram.t
val save_quot: exp Gram.t
