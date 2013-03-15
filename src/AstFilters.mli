open Ast 
type 'a filter = 'a -> 'a
type key = string
val interf_filters : (key, sig_item filter) LibUtil.Hashtbl.t
val implem_filters : (key, stru filter) LibUtil.Hashtbl.t
val topphrase_filters : (key, stru filter) LibUtil.Hashtbl.t
val applied_interf_filters : (key * sig_item filter) Queue.t
val applied_implem_filters : (key * stru filter) Queue.t
val applied_topphrase_filters : (key * stru filter) Queue.t

val apply_interf_filters : sig_item -> sig_item
val apply_implem_filters : stru -> stru
val apply_topphrase_filters : stru -> stru

val use_interf_filter : key -> unit
val use_implem_filter : key -> unit
val use_topphrase_filter : key -> unit

val register_sig_item_filter : key * sig_item filter -> unit
val register_stru_filter : key * stru filter -> unit
val register_topphrase_filter : key * stru filter -> unit
