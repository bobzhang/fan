
let ocaml_standard_library = Config.standard_library;;


let version = Sys.ocaml_version;;
let unsafe             = ref false;;
let verbose            = ref false;;
let antiquotations     = ref false;;
let quotations         = ref true;;
let inter_phrases: string option ref
    = ref None;;
let ast_impl_magic_number = "FAN2013M002";;
let camlp4_ast_intf_magic_number = "Camlp42006N002";;
let ocaml_ast_intf_magic_number = Config.ast_intf_magic_number;;
let ocaml_ast_impl_magic_number = Config.ast_impl_magic_number;;
let current_input_file = ref "";;

(* new config *)
let bug_main_address = "hongboz@seas.upenn.edu";;

let fan_debug = ref false;;
let conversion_table : (string, string) Hashtbl.t = Hashtbl.create 50


let gram_warning_verbose = ref true

let compilation_unit = ref None

(* This is a list of directories to search for INCLUDE statements. *)
let include_dirs = ref [];;
    
(* when you do the iteration, you should do it in reverse order *)  
(* let current_filters:  ref (list (plugin_name * plugin)) = ref [];; *)
