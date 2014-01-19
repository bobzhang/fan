
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
 
  val obj_simple_exp_of_ctyp : Astfn.ctyp -> Astfn.exp

  val mk_prefix : Astfn.opt_decl_params -> Astfn.exp -> Astfn.exp

  val exp_of_poly_variant :
            result:Astfn.ctyp -> Astfn.row_field -> Astfn.exp

  val exp_of_or_ctyp : or_ctyp -> exp

  val fun_of_tydcl :
      result:Astfn.ctyp -> decl -> exp

  val obj_of_mtyps :
         Sigs_util.mtyps -> stru option

end

val register :   param -> unit    
