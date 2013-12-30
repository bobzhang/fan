open Astf

type 'a t

val to_string : 'a t -> string
val of_string : string -> 'a t
val literal : literal t
val flag : flag t
val position_flag : position_flag t
val strings : strings t
val lident : lident t
val alident : alident t
val auident : auident t
val aident : aident t
val astring : astring t
val uident : uident t
val ident : ident t
val ident' : ident' t
val vid : vid t
val vid' : vid' t
val dupath : dupath t
val dlpath : dlpath t
val any : any t
val ctyp : ctyp t
val type_parameters : type_parameters t
val row_field : row_field t
val tag_names : tag_names t
val decl : decl t
val type_constr : type_constr t
val opt_type_constr : opt_type_constr t
val decl_param : decl_param t
val decl_params : decl_params t
val opt_decl_params : opt_decl_params t
val type_info : type_info t
val type_repr : type_repr t
val name_ctyp : name_ctyp t
val or_ctyp : or_ctyp t
val of_ctyp : of_ctyp t
val pat : pat t
val rec_pat : rec_pat t
val exp : exp t
val rec_exp : rec_exp t
val mtyp : mtyp t
val sigi : sigi t
val mbind : mbind t
val constr : constr t
val bind : bind t
val case : case t
val mexp : mexp t
val stru : stru t
val cltdecl : cltdecl t
val cltyp : cltyp t
val clsigi : clsigi t
val cldecl : cldecl t
val clexp : clexp t
val clfield : clfield t
val ep : ep t
val rec_bind : rec_bind t

type dyn

external dyn_tag : 'a t -> dyn t = "%identity"

module Pack :
  functor (X : sig type 'a t end) ->
    sig
      type pack = dyn t * Obj.t
      exception Pack_error
      val pack : 'b t -> 'a X.t -> dyn t * Obj.t
      val unpack : 'a t -> pack -> 'a X.t
      val print_tag : Format.formatter -> pack -> unit
    end
