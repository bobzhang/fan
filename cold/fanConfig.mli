val ocaml_standard_library : string

val version : string

val unsafe : bool ref
val verbose : bool ref
val antiquotations : bool ref

val inter_phrases : string option ref
val impl_magic_number : string
val intf_magic_number : string
val ocaml_ast_intf_magic_number : string
val ocaml_ast_impl_magic_number : string
val current_input_file : string ref
val bug_main_address : string
val fan_debug : bool ref
val conversion_table : (string, string) Hashtbl.t
val gram_warning_verbose : bool ref
val compilation_unit : string option ref

(** This is a list of directories to search for INCLUDE statements. *)
val include_dirs : string list ref

(** a list of files for dynamic loading *)
val dynload_dirs : string list ref

val fan_standard_library : string
    
(* val current_filters:  (plugin_name * plugin) list ref *)



    
