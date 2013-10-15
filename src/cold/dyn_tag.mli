open FAst
  
type 'a tag
      
val string_of_tag : 'a tag -> string
val literal_tag : literal tag
val flag_tag : flag tag
val position_flag_tag : position_flag tag
val strings_tag : strings tag
val lident_tag : lident tag
val alident_tag : alident tag
val auident_tag : auident tag
val aident_tag : aident tag
val astring_tag : astring tag
val uident_tag : uident tag
val ident_tag : ident tag
val ident'_tag : ident' tag
val vid_tag : vid tag
val vid'_tag : vid' tag
val dupath_tag : dupath tag
val dlpath_tag : dlpath tag
val any_tag : any tag
val ctyp_tag : ctyp tag
val type_parameters_tag : type_parameters tag
val row_field_tag : row_field tag
val tag_names_tag : tag_names tag
val typedecl_tag : typedecl tag
val type_constr_tag : type_constr tag
val opt_type_constr_tag : opt_type_constr tag
val decl_param_tag : decl_param tag
val decl_params_tag : decl_params tag
val opt_decl_params_tag : opt_decl_params tag
val type_info_tag : type_info tag
val type_repr_tag : type_repr tag
val name_ctyp_tag : name_ctyp tag
val or_ctyp_tag : or_ctyp tag
val of_ctyp_tag : of_ctyp tag
val pat_tag : pat tag
val rec_pat_tag : rec_pat tag
val exp_tag : exp tag
val rec_exp_tag : rec_exp tag
val mtyp_tag : mtyp tag
val sigi_tag : sigi tag
val mbind_tag : mbind tag
val constr_tag : constr tag
val bind_tag : bind tag
val case_tag : case tag
val mexp_tag : mexp tag
val stru_tag : stru tag
val cltdecl_tag : cltdecl tag
val cltyp_tag : cltyp tag
val clsigi_tag : clsigi tag
val cldecl_tag : cldecl tag
val clexp_tag : clexp tag
val clfield_tag : clfield tag
val ep_tag : ep tag
val rec_bind_tag : rec_bind tag

type dyn
external dyn_tag : 'a tag -> dyn tag = "%identity"
    
module Pack :
  functor (X : sig type 'a t end) ->
    sig
      type pack = dyn tag * Obj.t
      exception Pack_error
      val pack : 'b tag -> 'a X.t -> dyn tag * Obj.t
      val unpack : 'a tag -> pack -> 'a X.t
      val print_tag : Format.formatter -> pack -> unit
    end
