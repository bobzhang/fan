open FAst 

type 'a filter = 'a -> 'a

type key = string

val interf_filters : (key, sigi filter) Hashtbl.t

val implem_filters : (key, stru filter) Hashtbl.t

val topphrase_filters : (key, stru filter) Hashtbl.t

val applied_interf_filters : (key * sigi filter) Queue.t

val applied_implem_filters : (key * stru filter) Queue.t

val applied_topphrase_filters : (key * stru filter) Queue.t

    
val apply_interf_filters : sigi -> sigi

val apply_implem_filters : stru -> stru

val apply_topphrase_filters : stru -> stru

    
val use_interf_filter : key -> unit

val use_implem_filter : key -> unit

val use_topphrase_filter : key -> unit

val register_sigi_filter : key * sigi filter -> unit

val register_stru_filter : key * stru filter -> unit

val register_topphrase_filter : key * stru filter -> unit
