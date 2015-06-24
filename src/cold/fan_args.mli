
(** This module handles command line argument processing
    
    *)


  





type file_kind =
  | Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string 

val input_file : file_kind -> unit

val output_file : string option ref
