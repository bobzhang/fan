open Ast
  
val keep : bool ref
type plugin = {
  plugin_transform : FSig.module_types -> str_item;
  mutable plugin_activate : bool;
}
type plugin_name = string
val filters : (plugin_name, plugin) LibUtil.Hashtbl.t
val show_code : bool ref
val register : plugin_name * (FSig.module_types -> str_item) -> unit
val show_modules : unit -> unit
val plugin_add : plugin_name -> unit
val plugin_remove : plugin_name -> unit


class type traversal = object
  inherit Camlp4Ast.map
  method get_cur_module_types: FSig.module_types
  method get_cur_and_types: FSig.and_types
  (* method in_and_types: *)
  method update_cur_and_types:
      (FSig.and_types -> FSig.and_types) -> unit
  method update_cur_module_types:
      (FSig.module_types -> FSig.module_types) -> unit

end
val traversal: unit -> traversal    
(* val g : Gram.gram *)
val fan_quot : expr Gram.t
val fan_quots : expr Gram.t
