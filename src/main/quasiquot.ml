%import{
Ast_quotation:
  add_quotation
  ;
Parsef:
  exp_filter
  pat_filter
  exp_filter_n
  pat_filter_n
  ;
FanAstN:
  m
  ;
Ast_gen:
  loc_of
  ;
};;
open Syntaxf

let efilter str e =
    let e = exp_filter e in
    let _loc = loc_of e in
    %exp{($e : Astf.$lid:str)} (* BOOTSTRAPPING, assocaited with module [Astf] *)
      

let pfilter str e =
  let p = pat_filter e in
  let _loc = loc_of p in
  %pat{($p : Astf.$lid:str)} (* BOOTSTRAPPING, associated with module [Astf] *);;
  
let domain = `Absolute ["Fan"; "Lang"; "Meta"]

let me = object
  inherit Metaf.meta;
  method! loc _loc loc =
    match !Ast_quotation.current_loc_name with
    | None -> `Lid (_loc, !Locf.name)
    | Some "here" ->
        Ast_gen.meta_here _loc loc
    | Some x ->  `Lid(_loc,x) 
end
let mp = object
  inherit Metaf.meta
  method! loc _loc _ = %pat'{ _ } (* we use [subst_first_loc] *)    
end
    
let _ = begin 
  add_quotation {domain; name =  "sigi'"} sigi_quot ~mexp:me#sigi
    ~mpat:mp#sigi ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "stru'"} stru_quot ~mexp:(me#stru)
    ~mpat:(mp#stru) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "ctyp'"} ctyp_quot ~mexp:(me#ctyp)
    ~mpat:(mp#ctyp) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "pat'"} pat_quot ~mexp:(me#pat)
    ~mpat:(mp#pat) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "exp'"} exp_quot ~mexp:(me#exp)
    ~mpat:(mp#exp) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "mtyp'"} mtyp_quot ~mexp:(me#mtyp)
    ~mpat:(mp#mtyp) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "mexp'"} mexp_quot ~mexp:(me#mexp)
    ~mpat:(mp#mexp) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "cltyp'"} cltyp_quot ~mexp:(me#cltyp)
    ~mpat:(mp#cltyp) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "clexp'"} clexp_quot ~mexp:(me#clexp)
    ~mpat:(mp#clexp) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "clsigi'"} clsigi_quot ~mexp:(me#clsigi)
    ~mpat:(mp#clsigi) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "clfield'"} clfield_quot ~mexp:(me#clfield)
    ~mpat:(mp#clfield) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "constr'"} constr_quot ~mexp:(me#constr)
    ~mpat:(mp#constr) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "bind'"} bind_quot ~mexp:(me#bind)
    ~mpat:(mp#bind) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "rec_exp'"} rec_exp_quot ~mexp:(me#rec_exp)
    ~mpat:(mp#rec_exp) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "case'"} case_quot ~mexp:(me#case)
    ~mpat:(mp#case) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "mbind'"} mbind_quot ~mexp:(me#mbind)
    ~mpat:(mp#mbind) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "ident'"} ident_quot ~mexp:(me#ident)
    ~mpat:(mp#ident) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "rec_flag'"} rec_flag_quot ~mexp:(me#flag)
    ~mpat:(mp#flag) ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "private_flag'"} private_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag)
    ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "row_var_flag'"} row_var_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag)
    ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "mutable_flag'"} mutable_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag)
    ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "virtual_flag'"} virtual_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag)
    ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "override_flag'"} override_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag)
    ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "direction_flag'"} direction_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag)
    ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "or_ctyp'"} constructor_declarations
    ~mexp:(me#or_ctyp) ~mpat:(me#or_ctyp) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "row_field'"} row_field ~mexp:(me#row_field)
    ~mpat:(mp#row_field) ~exp_filter ~pat_filter
end

let _ = begin
  add_quotation {domain; name =  "sigi"} sigi_quot ~mexp:(me#sigi)
    ~mpat:(mp#sigi) ~exp_filter:(efilter "sigi")
    ~pat_filter:(pfilter "sigi");
  add_quotation {domain; name =  "stru"} stru_quot ~mexp:(me#stru)
    ~mpat:(mp#stru) ~exp_filter:(efilter "stru")
    ~pat_filter:(pfilter "stru");
  add_quotation {domain; name =  "ctyp"} ctyp_quot ~mexp:(me#ctyp)
    ~mpat:(mp#ctyp) ~exp_filter:(efilter "ctyp")
    ~pat_filter:(pfilter "ctyp");
  add_quotation {domain; name =  "pat"} pat_quot ~mexp:(me#pat)
    ~mpat:(mp#pat) ~exp_filter:(efilter "pat")
    ~pat_filter:(pfilter "pat");
  add_quotation {domain; name =  "ep"} exp_quot ~mexp:(me#exp)
    ~mpat:(mp#exp) ~exp_filter:(efilter "ep")
    ~pat_filter:(pfilter "ep");
  add_quotation {domain; name =  "exp"} exp_quot ~mexp:(me#exp)
    ~mpat:(mp#exp) ~exp_filter:(efilter "exp")
    ~pat_filter:(pfilter "exp");
  add_quotation {domain; name =  "mtyp"} mtyp_quot ~mexp:(me#mtyp)
    ~mpat:(mp#mtyp) ~exp_filter:(efilter "mtyp")
    ~pat_filter:(pfilter "mtyp");
  add_quotation {domain; name =  "mexp"} mexp_quot ~mexp:(me#mexp)
    ~mpat:(mp#mexp) ~exp_filter:(efilter "mexp")
    ~pat_filter:(pfilter "mexp");
  add_quotation {domain; name =  "cltyp"} cltyp_quot ~mexp:(me#cltyp)
    ~mpat:(mp#cltyp) ~exp_filter:(efilter "cltyp")
    ~pat_filter:(pfilter "cltyp");
  add_quotation {domain; name =  "clexp"} clexp_quot ~mexp:(me#clexp)
    ~mpat:(mp#clexp) ~exp_filter:(efilter "clexp")
    ~pat_filter:(pfilter "clexp");
  add_quotation {domain; name =  "clsigi"} clsigi_quot ~mexp:(me#clsigi)
    ~mpat:(mp#clsigi) ~exp_filter:(efilter "clsigi")
    ~pat_filter:(pfilter "clsigi");
  add_quotation {domain; name =  "clfield"} clfield_quot ~mexp:(me#clfield)
    ~mpat:(mp#clfield) ~exp_filter:(efilter "clfield")
    ~pat_filter:(pfilter "clfield");
  add_quotation {domain; name =  "constr"} constr_quot ~mexp:(me#constr)
    ~mpat:(mp#constr) ~exp_filter:(efilter "constr")
    ~pat_filter:(pfilter "constr");
  add_quotation {domain; name =  "bind"} bind_quot ~mexp:(me#bind)
    ~mpat:(mp#bind) ~exp_filter:(efilter "bind")
    ~pat_filter:(pfilter "bind");
  add_quotation {domain; name =  "rec_exp"} rec_exp_quot ~mexp:(me#rec_exp)
    ~mpat:(mp#rec_exp) ~exp_filter:(efilter "rec_exp")
    ~pat_filter:(pfilter "rec_exp");
  add_quotation {domain; name =  "case"} case_quot ~mexp:(me#case)
    ~mpat:(mp#case) ~exp_filter:(efilter "case")
    ~pat_filter:(pfilter "case");
  add_quotation {domain; name =  "mbind"} mbind_quot ~mexp:(me#mbind)
    ~mpat:(mp#mbind) ~exp_filter:(efilter "mbind")
    ~pat_filter:(pfilter "mbind");
  add_quotation {domain; name =  "ident"} ident_quot ~mexp:(me#ident)
    ~mpat:(mp#ident) ~exp_filter:(efilter "ident")
    ~pat_filter:(pfilter "ident");
  add_quotation {domain; name =  "or_ctyp"} constructor_declarations
    ~mexp:(me#or_ctyp) ~mpat:(me#or_ctyp)
    ~exp_filter:(efilter "or_ctyp") ~pat_filter:(pfilter "or_ctyp");
  add_quotation {domain; name =  "row_field"} row_field ~mexp:(me#row_field)
    ~mpat:(mp#row_field) ~exp_filter:(efilter "row_field")
    ~pat_filter:(pfilter "row_field");
end
;;

(****************************************)
(* side effect                          *)
(****************************************)




(*************************************************************************)
(** begin quotation for Astf without locations *)


let efilter str e =
    let e = exp_filter_n e in
    let _loc = loc_of e in
    %exp{($e : Astfn.$lid:str)} (* BOOTSTRAPPING, associated with module [Astfn] *)
let pfilter str e =
  let p = pat_filter_n e in
  let _loc = loc_of p in
  %pat{($p : Astfn.$lid:str)};; (* BOOTSTRAPPING, associated with module [Astfn] *)


begin
    add_quotation {domain; name =  "sigi-"} sigi_quot ~mexp:(fun loc p -> m#sigi loc (Objs.strip_sigi p))
    ~mpat:(fun loc p -> m#sigi loc (Objs.strip_sigi p))
     ~exp_filter:(efilter "sigi")
    ~pat_filter:(pfilter "sigi");
  add_quotation {domain; name =  "stru-"} stru_quot ~mexp:(fun loc p -> m#stru loc (Objs.strip_stru p))
    ~mpat:(fun loc p -> m#stru loc (Objs.strip_stru p)) ~exp_filter:(efilter "stru")
    ~pat_filter:(pfilter "stru");
  add_quotation {domain; name =  "ctyp-"} ctyp_quot ~mexp:(fun loc p -> m#ctyp loc (Objs.strip_ctyp p))
    ~mpat:(fun loc p -> m#ctyp loc (Objs.strip_ctyp p)) ~exp_filter:(efilter "ctyp")
    ~pat_filter:(pfilter "ctyp");
  add_quotation {domain; name =  "pat-"} pat_quot ~mexp:(fun loc p -> m#pat loc (Objs.strip_pat p))
    ~mpat:(fun loc p -> m#pat loc (Objs.strip_pat p)) ~exp_filter:(efilter "pat")
    ~pat_filter:(pfilter "pat");
  add_quotation {domain; name =  "ep-"} exp_quot ~mexp:(fun loc p -> m#exp loc (Objs.strip_exp p))
    ~mpat:(fun loc p -> m#exp loc (Objs.strip_exp p)) ~exp_filter:(efilter "ep")
    ~pat_filter:(pfilter "ep");
  add_quotation {domain; name =  "exp-"} exp_quot
    ~mexp:(fun loc p -> m#exp loc (Objs.strip_exp p))
    ~mpat:(fun loc p -> m#exp loc (Objs.strip_exp p))
    ~exp_filter:(efilter "exp")
    ~pat_filter:(pfilter "exp");
  add_quotation {domain; name =  "mtyp-"} mtyp_quot ~mexp:(fun loc p -> m#mtyp loc (Objs.strip_mtyp p))
    ~mpat:(fun loc p -> m#mtyp loc (Objs.strip_mtyp p)) ~exp_filter:(efilter "mtyp")
    ~pat_filter:(pfilter "mtyp");
  add_quotation {domain; name =  "mexp-"} mexp_quot ~mexp:(fun loc p -> m#mexp loc (Objs.strip_mexp p))
    ~mpat:(fun loc p -> m#mexp loc (Objs.strip_mexp p)) ~exp_filter:(efilter "mexp")
    ~pat_filter:(pfilter "mexp");
  add_quotation {domain; name =  "cltyp-"} cltyp_quot ~mexp:(fun loc p -> m#cltyp loc (Objs.strip_cltyp p))
    ~mpat:(fun loc p -> m#cltyp loc (Objs.strip_cltyp p)) ~exp_filter:(efilter "cltyp")
    ~pat_filter:(pfilter "cltyp");
  add_quotation {domain; name =  "clexp-"} clexp_quot ~mexp:(fun loc p -> m#clexp loc (Objs.strip_clexp p))
    ~mpat:(fun loc p -> m#clexp loc (Objs.strip_clexp p)) ~exp_filter:(efilter "clexp")
    ~pat_filter:(pfilter "clexp");
  add_quotation {domain; name =  "clsigi-"} clsigi_quot ~mexp:(fun loc p -> m#clsigi loc (Objs.strip_clsigi p))
    ~mpat:(fun loc p -> m#clsigi loc (Objs.strip_clsigi p)) ~exp_filter:(efilter "clsigi")
    ~pat_filter:(pfilter "clsigi");
  add_quotation {domain; name =  "clfield-"} clfield_quot ~mexp:(fun loc p -> m#clfield loc (Objs.strip_clfield p))
    ~mpat:(fun loc p -> m#clfield loc (Objs.strip_clfield p)) ~exp_filter:(efilter "clfield")
    ~pat_filter:(pfilter "clfield");
  add_quotation {domain; name =  "constr-"} constr_quot ~mexp:(fun loc p -> m#constr loc (Objs.strip_constr p))
    ~mpat:(fun loc p -> m#constr loc (Objs.strip_constr p)) ~exp_filter:(efilter "constr")
    ~pat_filter:(pfilter "constr");
  add_quotation {domain; name =  "bind-"} bind_quot ~mexp:(fun loc p -> m#bind loc (Objs.strip_bind p))
    ~mpat:(fun loc p -> m#bind loc (Objs.strip_bind p)) ~exp_filter:(efilter "bind")
    ~pat_filter:(pfilter "bind");
  add_quotation {domain; name =  "rec_exp-"} rec_exp_quot ~mexp:(fun loc p -> m#rec_exp loc (Objs.strip_rec_exp p))
    ~mpat:(fun loc p -> m#rec_exp loc (Objs.strip_rec_exp p)) ~exp_filter:(efilter "rec_exp")
    ~pat_filter:(pfilter "rec_exp");
  add_quotation {domain; name =  "case-"} case_quot ~mexp:(fun loc p -> m#case loc (Objs.strip_case p))
    ~mpat:(fun loc p -> m#case loc (Objs.strip_case p)) ~exp_filter:(efilter "case")
    ~pat_filter:(pfilter "case");
  add_quotation {domain; name =  "mbind-"} mbind_quot ~mexp:(fun loc p -> m#mbind loc (Objs.strip_mbind p))
    ~mpat:(fun loc p -> m#mbind loc (Objs.strip_mbind p)) ~exp_filter:(efilter "mbind")
    ~pat_filter:(pfilter "mbind");
  add_quotation {domain; name =  "ident-"} ident_quot ~mexp:(fun loc p -> m#ident loc (Objs.strip_ident p))
    ~mpat:(fun loc p -> m#ident loc (Objs.strip_ident p)) ~exp_filter:(efilter "ident")
    ~pat_filter:(pfilter "ident");
  add_quotation {domain; name =  "or_ctyp-"} constructor_declarations
    ~mexp:(fun loc p -> m#or_ctyp loc (Objs.strip_or_ctyp p)) ~mpat:(fun loc p -> m#or_ctyp loc (Objs.strip_or_ctyp p))
    ~exp_filter:(efilter "or_ctyp") ~pat_filter:(pfilter "or_ctyp");
  add_quotation {domain; name =  "row_field-"} row_field ~mexp:(fun loc p -> m#row_field loc (Objs.strip_row_field p))
    ~mpat:(fun loc p -> m#row_field loc (Objs.strip_row_field p)) ~exp_filter:(efilter "row_field")
    ~pat_filter:(pfilter "row_field");
end;;


let exp_filter = exp_filter_n in
let pat_filter = pat_filter_n in
begin
    add_quotation {domain; name =  "sigi-'"} sigi_quot ~mexp:(fun loc p -> m#sigi loc (Objs.strip_sigi p))
    ~mpat:(fun loc p -> m#sigi loc (Objs.strip_sigi p))
     ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "stru-'"} stru_quot ~mexp:(fun loc p -> m#stru loc (Objs.strip_stru p))
    ~mpat:(fun loc p -> m#stru loc (Objs.strip_stru p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "ctyp-'"} ctyp_quot ~mexp:(fun loc p -> m#ctyp loc (Objs.strip_ctyp p))
    ~mpat:(fun loc p -> m#ctyp loc (Objs.strip_ctyp p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "pat-'"} pat_quot ~mexp:(fun loc p -> m#pat loc (Objs.strip_pat p))
    ~mpat:(fun loc p -> m#pat loc (Objs.strip_pat p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "ep-'"} exp_quot ~mexp:(fun loc p -> m#exp loc (Objs.strip_exp p))
    ~mpat:(fun loc p -> m#exp loc (Objs.strip_exp p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "exp-'"} exp_quot
    ~mexp:(fun loc p -> m#exp loc (Objs.strip_exp p))
    ~mpat:(fun loc p -> m#exp loc (Objs.strip_exp p))
    ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "mtyp-'"} mtyp_quot ~mexp:(fun loc p -> m#mtyp loc (Objs.strip_mtyp p))
    ~mpat:(fun loc p -> m#mtyp loc (Objs.strip_mtyp p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "mexp-'"} mexp_quot ~mexp:(fun loc p -> m#mexp loc (Objs.strip_mexp p))
    ~mpat:(fun loc p -> m#mexp loc (Objs.strip_mexp p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "cltyp-'"} cltyp_quot ~mexp:(fun loc p -> m#cltyp loc (Objs.strip_cltyp p))
    ~mpat:(fun loc p -> m#cltyp loc (Objs.strip_cltyp p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "clexp-'"} clexp_quot ~mexp:(fun loc p -> m#clexp loc (Objs.strip_clexp p))
    ~mpat:(fun loc p -> m#clexp loc (Objs.strip_clexp p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "clsigi-'"} clsigi_quot ~mexp:(fun loc p -> m#clsigi loc (Objs.strip_clsigi p))
    ~mpat:(fun loc p -> m#clsigi loc (Objs.strip_clsigi p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "clfield-'"} clfield_quot ~mexp:(fun loc p -> m#clfield loc (Objs.strip_clfield p))
    ~mpat:(fun loc p -> m#clfield loc (Objs.strip_clfield p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "constr-'"} constr_quot ~mexp:(fun loc p -> m#constr loc (Objs.strip_constr p))
    ~mpat:(fun loc p -> m#constr loc (Objs.strip_constr p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "bind-'"} bind_quot ~mexp:(fun loc p -> m#bind loc (Objs.strip_bind p))
    ~mpat:(fun loc p -> m#bind loc (Objs.strip_bind p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "rec_exp-'"} rec_exp_quot ~mexp:(fun loc p -> m#rec_exp loc (Objs.strip_rec_exp p))
    ~mpat:(fun loc p -> m#rec_exp loc (Objs.strip_rec_exp p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "case-'"} case_quot ~mexp:(fun loc p -> m#case loc (Objs.strip_case p))
    ~mpat:(fun loc p -> m#case loc (Objs.strip_case p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "mbind-'"} mbind_quot ~mexp:(fun loc p -> m#mbind loc (Objs.strip_mbind p))
    ~mpat:(fun loc p -> m#mbind loc (Objs.strip_mbind p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "ident-'"} ident_quot ~mexp:(fun loc p -> m#ident loc (Objs.strip_ident p))
    ~mpat:(fun loc p -> m#ident loc (Objs.strip_ident p)) ~exp_filter
    ~pat_filter;
  add_quotation {domain; name =  "or_ctyp-'"} constructor_declarations
    ~mexp:(fun loc p -> m#or_ctyp loc (Objs.strip_or_ctyp p)) ~mpat:(fun loc p -> m#or_ctyp loc (Objs.strip_or_ctyp p))
    ~exp_filter ~pat_filter;
  add_quotation {domain; name =  "row_field-'"} row_field ~mexp:(fun loc p -> m#row_field loc (Objs.strip_row_field p))
    ~mpat:(fun loc p -> m#row_field loc (Objs.strip_row_field p)) ~exp_filter
    ~pat_filter
end
;;
(* local variables: *)
(* compile-command: "cd .. && pmake  main_annot/quasiquot.cmo" *)
(* end: *)
