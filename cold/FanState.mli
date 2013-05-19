open FSigUtil


val current_filters : (plugin_name * plugin) list ref
val reset_current_filters : unit -> unit
val keep : bool ref     
val reset : unit -> unit
val gensym : ?pkg:string -> string -> string
  
