
(** This module handles command line argument processing
    
    *)


  
val initial_spec_list : (string * Arg.spec * string) list

(** The first argument is file name
    we dispatch different functions based on the filename extension
 *)
val anon_fun : string -> unit
