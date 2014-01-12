
open Astfn
open Sigs_util
open Ctyp

type param = {
    arity: int;
    names: string list;
    plugin_name:  string;
    mk_record: (Ctyp.record_col list -> exp) option;
    mk_variant: (string option -> Ctyp.ty_info list -> exp) option ;
    excludes: string list;
  }


module type S =
    sig
      val p : param
    end

module Make (U:S) : sig 
  val mk :
  ?default:exp ->
  (* ?cons_transform:(string -> string) -> *)
  kind:kind ->
  base:string ->
  class_name:string ->
    unit -> 
      mtyps -> stru
end
