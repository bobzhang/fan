open FSig
let current_filters: (plugin_name* plugin) list ref = ref []
let keep = ref true
let id = ref 0
let reset () = keep := true; current_filters := []
let gensym ?(pkg= "")  prefix =
  let res =
    "fan_" ^ (prefix ^ ("_" ^ (pkg ^ ("_" ^ (string_of_int id.contents))))) in
  incr id; res