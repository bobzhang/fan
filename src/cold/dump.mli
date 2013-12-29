




val pp_exp : Astf.exp Formatf.t 
val pp_pat : Astf.pat Formatf.t 
val pp_stru : Astf.stru Formatf.t 
val pp_ctyp : Astf.ctyp Formatf.t 
    
val exp_to_string : Astf.exp -> string
module N :
  sig
    val pp_exp : Astfn.exp Formatf.t 
    val pp_pat : Astfn.pat Formatf.t 
    val pp_stru : Astfn.stru Formatf.t 
    val pp_ctyp : Astfn.ctyp Formatf.t 
  end
