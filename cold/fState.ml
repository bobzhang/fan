open FSigUtil

let current_filters: (plugin_name * plugin) list ref = ref []

let reset_current_filters () = current_filters := []

let keep = ref true

let id = ref 0

let reset () = begin keep := true; current_filters := [] end

let gensym ?(pkg= "")  prefix =
  let res =
    "fan_" ^ (prefix ^ ("_" ^ (pkg ^ ("_" ^ (string_of_int id.contents))))) in
  begin incr id; res end