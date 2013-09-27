
(** Experimental: state management for deriving *)
open FSigUtil

(* when you do the iteration, you should do it in reverse order *)  
val current_filters : (plugin_name * plugin) list ref

    
val reset_current_filters : unit -> unit
val keep : bool ref     
val reset : unit -> unit
val gensym : ?pkg:string -> string -> string
  
