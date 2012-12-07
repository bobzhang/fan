
type 'a filter = 'a -> 'a
type key = string
val interf_filters : (key, Ast.sig_item filter) LibUtil.Hashtbl.t
val implem_filters : (key, Ast.str_item filter) LibUtil.Hashtbl.t
val topphrase_filters : (key, Ast.str_item filter) LibUtil.Hashtbl.t
val applied_interf_filters : (key * Ast.sig_item filter) Queue.t
val applied_implem_filters : (key * Ast.str_item filter) Queue.t
val applied_topphrase_filters : (key * Ast.str_item filter) Queue.t

val apply_interf_filters : Ast.sig_item -> Ast.sig_item
val apply_implem_filters : Ast.str_item -> Ast.str_item
val apply_topphrase_filters : Ast.str_item -> Ast.str_item

val use_interf_filter : key -> unit
val use_implem_filter : key -> unit
val use_topphrase_filter : key -> unit

val register_sig_item_filter : key * Ast.sig_item filter -> unit
val register_str_item_filter : key * Ast.str_item filter -> unit
val register_topphrase_filter : key * Ast.str_item filter -> unit
