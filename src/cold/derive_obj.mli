
open Astfn





type default =
  | Atom of exp
  | Invalid_argument

type param = {
    arity: int;
    names: string list;
    plugin_name:  string;
    mk_record: (Ctyp.record_col list -> exp) option;
    mk_variant: (string option -> Ctyp.ty_info list -> exp) option ;
    default : default option; 
    excludes: string list;
    kind : Ctyp.kind;
    base : string;
    class_name : string;
  }


module type S =
  sig
    val p : param
  end



module Make (U:S) : sig 
 
  val exp_of_ctyp : ctyp -> exp

  val mk_prefix : opt_decl_params -> exp -> exp

  val exp_of_poly_variant :
            result:ctyp -> row_field -> exp

  val exp_of_or_ctyp : or_ctyp -> exp

  val fun_of_tydcl :
      result:ctyp -> decl -> exp

  val obj_of_mtyps :
         Sigs_util.mtyps -> stru option

end

val register :   param -> unit    
