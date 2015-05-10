
let ocaml_standard_library = "/Users/hongbozhang/.opam/4.02.1/lib/ocaml"

(* WARNING, WARNING, WARNING
   make sure to update for each compiler release *)
let cmi_magic_number = "Caml1999I017"

let ocaml_ast_impl_magic_number = "Caml1999M016"

let ocaml_ast_intf_magic_number = "Caml1999N015"

let impl_magic_number = "FAN2013M002"

let intf_magic_number = "FAN2013N002"

let version = "4.02.1"

let unsafe             = ref false;;

let antiquotations     = ref false;;

let inter_phrases : string option ref = ref None;;


(* let ocaml_ast_intf_magic_number = Oconfig.ast_intf_magic_number;; *)
(* let ocaml_ast_impl_magic_number = Oconfig.ast_impl_magic_number;; *)


let current_input_file = ref "";;

(* new config *)
let bug_main_address = "hongbo.bob.zhang@gmail.com";;

let fan_debug = ref false;;




let gram_warning_verbose = ref true

let compilation_unit = ref None


let include_dirs = ref []

let dynload_dirs = ref []

let fan_standard_library =
  try Sys.getenv "FAN_DIR"
  with Not_found -> 
    Filename.concat ocaml_standard_library "fan"

      
let fan_plugins_library =
  try
    Sys.getenv "FAN_LIB_DIR"
  with Not_found ->
    Filename.concat ocaml_standard_library "fanplugin"
      
    
