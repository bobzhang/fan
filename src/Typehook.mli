open Ast
open FSig  

val print_collect_module_types: bool ref

(* type plugin = { *)
(*   transform : FSig.module_types -> str_item; *)
(*   mutable activate : bool; *)
(*   position: string option; *)
(*   filter: (string -> bool) option; *)
(* } *)
(* type plugin_name = string *)
(* val filters : (plugin_name, plugin) LibUtil.Hashtbl.t *)
val show_code : bool ref

val register :
    ?filter:(string->bool) -> ?position:string ->
      plugin_name * (FSig.module_types -> str_item) -> unit
val show_modules : unit -> unit
val plugin_add : plugin_name -> unit
val plugin_remove : plugin_name -> unit


class type traversal = object
  inherit FanAst.map
  method get_cur_module_types: FSig.module_types
  method get_cur_and_types: FSig.and_types
  (* method in_and_types: *)
  method update_cur_and_types:
      (FSig.and_types -> FSig.and_types) -> unit
  method update_cur_module_types:
      (FSig.module_types -> FSig.module_types) -> unit

end
val traversal: unit -> traversal    

(* val g : Gram.gram
   grammar used by two entries [fan_quot] and [fan_quots]
   they should not be mixed with other entries which has
   diffierent gram
 *)
val fan_quot : expr Gram.t
val fan_quots : expr Gram.t
val include_quot: str_item Gram.t
val save_quot: expr Gram.t
