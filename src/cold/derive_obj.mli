
open Astfn
open Sigs_util
open Ctyp

val mk :
  ?arity:int ->
  ?default:exp ->
  ?cons_transform:(string -> string) ->
  kind:kind ->
  base:string ->
  class_name:string ->
  ?names:string list ->
  mk_record:(record_col list -> exp) ->
  mk_variant:(string option -> ty_info list -> exp) -> unit -> 
  mtyps -> stru
