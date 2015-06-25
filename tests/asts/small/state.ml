open Sigs_util
let current_filters: (plugin_name* plugin) list ref = ref []
let reset_current_filters = function | () -> current_filters := []
let keep = ref true
let id = ref 0
let reset = function | () -> (keep := true; current_filters := [])
let gensym ?(pkg= "")  =
  function
  | prefix ->
      let res =
        "fan_" ^ (prefix ^ ("_" ^ (pkg ^ ("_" ^ (string_of_int (!id)))))) in
      (incr id; res)
