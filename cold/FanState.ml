open FSig

let current_filters: (plugin_name * plugin) list ref = ref []

let reset_current_filters () = current_filters.contents <- []

let keep = ref true

let id = ref 0

let reset () = keep.contents <- true; current_filters.contents <- []

let gensym ?(pkg= "")  prefix =
  let res =
    "fan_" ^ (prefix ^ ("_" ^ (pkg ^ ("_" ^ (string_of_int id.contents))))) in
  incr id; res