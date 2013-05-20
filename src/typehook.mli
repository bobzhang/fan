
open FSigUtil  

val print_collect_mtyps: bool ref


val show_code : bool ref

val filters : (plugin_name, plugin) Hashtbl.t
(** [register] to filters *)
val register :
    ?filter:(string->bool) -> ?position:string ->
      plugin_name * (mtyps -> AstN.stru option) -> unit
          
val show_modules : unit -> unit

(** Register the plugin to [FanState.current_filters], this is controlled by [fans] DDSL *)
val plugin_add : plugin_name -> unit


(** Remove the plugin from  [FanState.current_filters], see [plugin_add] *)
val plugin_remove : plugin_name -> unit

(** Entrance is  [mexp]
    Choose [mexp] as an entrace point to make the traversal
    more modular.
    Usage {[
    let v =  {:mexp| struct $s end |} in
    let mexp = (Typehook.traversal ())#mexp v 
    ]}
    This function will *apply all the plugins* after finishing the traversal
    to generate the code *)  
class type traversal = object
  inherit Objs.map
  method get_cur_mtyps: mtyps
  method get_cur_and_types: and_types
  (* method in_and_types: *)
  method update_cur_and_types:
      (and_types -> and_types) -> unit
  method update_cur_mtyps:
      (mtyps -> mtyps) -> unit

end
val traversal: unit -> traversal    

