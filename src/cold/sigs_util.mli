
(** Utilities for deriving mechanism *)
  
open Astfn
  
type named_type = (string* decl)
and and_types = named_type list
and types =
    [ `Mutual of and_types
    | `Single of named_type ]
and mtyps =  types list


type plugin_name = string 

type plugin = {
    transform:(mtyps -> stru option);
    position: string option ;
    filter: (string->bool) option ;
}
      
val stru_from_mtyps : f:(named_type -> decl) -> mtyps ->stru option

val stru_from_ty : f:(string -> stru) -> mtyps -> stru

val apply_filter : (string -> bool) -> mtyps -> mtyps        

val pp_print_types : Format.formatter -> types -> unit
val pp_print_mtyps : Format.formatter -> mtyps -> unit     

val transform_mtyps : mtyps ->
  (string * ident * int) list * mtyps    
