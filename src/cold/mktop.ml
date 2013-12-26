let of_exp = Ast_quotation.of_exp
let of_stru_with_filter = Ast_quotation.of_stru_with_filter
let of_stru = Ast_quotation.of_stru
let of_exp_with_filter = Ast_quotation.of_exp_with_filter
let of_clfield_with_filter = Ast_quotation.of_clfield_with_filter
let add_quotation = Ast_quotation.add_quotation
let add = Ast_quotation.add
let loc_of = Ast_gen.loc_of
let m = FanAstN.m
let pat_filter = Parsef.pat_filter
let exp_filter = Parsef.exp_filter
let exp_filter_n = Parsef.exp_filter_n
let pat_filter_n = Parsef.pat_filter_n
open! Syntaxf
include Prelude
let efilter str e =
  let e = Parsef.exp_filter e in
  let _loc = loc_of e in
  (`Constraint
     (_loc, e, (`Dot (_loc, (`Uid (_loc, "Astf")), (`Lid (_loc, str))))) : 
    Astf.exp )
let pfilter str e =
  let p = Parsef.pat_filter e in
  let _loc = loc_of p in
  (`Constraint
     (_loc, p, (`Dot (_loc, (`Uid (_loc, "Astf")), (`Lid (_loc, str))))) : 
    Astf.pat )
let domain = Ns.lang
let _ =
  of_stru_with_filter ~name:{ domain; name = "ocaml" } ~entry:strus
    ~filter:(fun s  ->
               let _loc = loc_of s in
               let v: Astf.mexp = `Struct (_loc, s) in
               let mexp = (Typehook.traversal ())#mexp v in
               let code =
                 match mexp with
                 | (`Struct (_loc,s) : Astf.mexp) -> s
                 | _ -> failwith "can not find items back " in
               if !Typehook.show_code
               then
                 (try Ast2pt.print_stru Format.std_formatter code
                  with
                  | _ ->
                      Util.prerr_endlinef
                        "There is a printer bugOur code generator may still work when Printer is brokenPlz send bug report to %s"
                        Configf.bug_main_address);
               code) ()
let domain = Ns.macro
let _ =
  of_exp_with_filter ~name:{ domain; name = "exp" } ~entry:exp
    ~filter:(Ast_macros.macro_expander#exp) ();
  of_clfield_with_filter ~name:{ domain; name = "clfield" } ~entry:clfield
    ~filter:(Ast_macros.macro_expander#clfield) ();
  of_stru_with_filter ~name:{ domain; name = "stru" } ~entry:stru
    ~filter:(Ast_macros.macro_expander#stru)
let domain = `Absolute ["Fan"; "Lang"; "Meta"]
let _ =
  add_quotation { domain; name = "sigi'" } sigi_quot ~mexp:(Filters.me#sigi)
    ~mpat:(Filters.mp#sigi) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "stru'" } stru_quot ~mexp:(Filters.me#stru)
    ~mpat:(Filters.mp#stru) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "ctyp'" } ctyp_quot ~mexp:(Filters.me#ctyp)
    ~mpat:(Filters.mp#ctyp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "pat'" } pat_quot ~mexp:(Filters.me#pat)
    ~mpat:(Filters.mp#pat) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "exp'" } exp_quot ~mexp:(Filters.me#exp)
    ~mpat:(Filters.mp#exp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mtyp'" } mtyp_quot ~mexp:(Filters.me#mtyp)
    ~mpat:(Filters.mp#mtyp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mexp'" } mexp_quot ~mexp:(Filters.me#mexp)
    ~mpat:(Filters.mp#mexp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "cltyp'" } cltyp_quot
    ~mexp:(Filters.me#cltyp) ~mpat:(Filters.mp#cltyp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "clexp'" } clexp_quot
    ~mexp:(Filters.me#clexp) ~mpat:(Filters.mp#clexp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "clsigi'" } clsigi_quot
    ~mexp:(Filters.me#clsigi) ~mpat:(Filters.mp#clsigi) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "clfield'" } clfield_quot
    ~mexp:(Filters.me#clfield) ~mpat:(Filters.mp#clfield) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "constr'" } constr_quot
    ~mexp:(Filters.me#constr) ~mpat:(Filters.mp#constr) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "bind'" } bind_quot ~mexp:(Filters.me#bind)
    ~mpat:(Filters.mp#bind) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "rec_exp'" } rec_exp_quot
    ~mexp:(Filters.me#rec_exp) ~mpat:(Filters.mp#rec_exp) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "case'" } case_quot ~mexp:(Filters.me#case)
    ~mpat:(Filters.mp#case) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mbind'" } mbind_quot
    ~mexp:(Filters.me#mbind) ~mpat:(Filters.mp#mbind) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "ident'" } ident_quot
    ~mexp:(Filters.me#ident) ~mpat:(Filters.mp#ident) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "rec_flag'" } rec_flag_quot
    ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "private_flag'" } private_flag_quot
    ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "row_var_flag'" } row_var_flag_quot
    ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mutable_flag'" } mutable_flag_quot
    ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "virtual_flag'" } virtual_flag_quot
    ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "override_flag'" } override_flag_quot
    ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "direction_flag'" } direction_flag_quot
    ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "or_ctyp'" } constructor_declarations
    ~mexp:(Filters.me#or_ctyp) ~mpat:(Filters.me#or_ctyp) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "row_field'" } row_field
    ~mexp:(Filters.me#row_field) ~mpat:(Filters.mp#row_field) ~exp_filter
    ~pat_filter
let _ =
  add_quotation { domain; name = "sigi" } sigi_quot ~mexp:(Filters.me#sigi)
    ~mpat:(Filters.mp#sigi) ~exp_filter:(efilter "sigi")
    ~pat_filter:(pfilter "sigi");
  add_quotation { domain; name = "stru" } stru_quot ~mexp:(Filters.me#stru)
    ~mpat:(Filters.mp#stru) ~exp_filter:(efilter "stru")
    ~pat_filter:(pfilter "stru");
  add_quotation { domain; name = "ctyp" } ctyp_quot ~mexp:(Filters.me#ctyp)
    ~mpat:(Filters.mp#ctyp) ~exp_filter:(efilter "ctyp")
    ~pat_filter:(pfilter "ctyp");
  add_quotation { domain; name = "pat" } pat_quot ~mexp:(Filters.me#pat)
    ~mpat:(Filters.mp#pat) ~exp_filter:(efilter "pat")
    ~pat_filter:(pfilter "pat");
  add_quotation { domain; name = "ep" } exp_quot ~mexp:(Filters.me#exp)
    ~mpat:(Filters.mp#exp) ~exp_filter:(efilter "ep")
    ~pat_filter:(pfilter "ep");
  add_quotation { domain; name = "exp" } exp_quot ~mexp:(Filters.me#exp)
    ~mpat:(Filters.mp#exp) ~exp_filter:(efilter "exp")
    ~pat_filter:(pfilter "exp");
  add_quotation { domain; name = "mtyp" } mtyp_quot ~mexp:(Filters.me#mtyp)
    ~mpat:(Filters.mp#mtyp) ~exp_filter:(efilter "mtyp")
    ~pat_filter:(pfilter "mtyp");
  add_quotation { domain; name = "mexp" } mexp_quot ~mexp:(Filters.me#mexp)
    ~mpat:(Filters.mp#mexp) ~exp_filter:(efilter "mexp")
    ~pat_filter:(pfilter "mexp");
  add_quotation { domain; name = "cltyp" } cltyp_quot
    ~mexp:(Filters.me#cltyp) ~mpat:(Filters.mp#cltyp)
    ~exp_filter:(efilter "cltyp") ~pat_filter:(pfilter "cltyp");
  add_quotation { domain; name = "clexp" } clexp_quot
    ~mexp:(Filters.me#clexp) ~mpat:(Filters.mp#clexp)
    ~exp_filter:(efilter "clexp") ~pat_filter:(pfilter "clexp");
  add_quotation { domain; name = "clsigi" } clsigi_quot
    ~mexp:(Filters.me#clsigi) ~mpat:(Filters.mp#clsigi)
    ~exp_filter:(efilter "clsigi") ~pat_filter:(pfilter "clsigi");
  add_quotation { domain; name = "clfield" } clfield_quot
    ~mexp:(Filters.me#clfield) ~mpat:(Filters.mp#clfield)
    ~exp_filter:(efilter "clfield") ~pat_filter:(pfilter "clfield");
  add_quotation { domain; name = "constr" } constr_quot
    ~mexp:(Filters.me#constr) ~mpat:(Filters.mp#constr)
    ~exp_filter:(efilter "constr") ~pat_filter:(pfilter "constr");
  add_quotation { domain; name = "bind" } bind_quot ~mexp:(Filters.me#bind)
    ~mpat:(Filters.mp#bind) ~exp_filter:(efilter "bind")
    ~pat_filter:(pfilter "bind");
  add_quotation { domain; name = "rec_exp" } rec_exp_quot
    ~mexp:(Filters.me#rec_exp) ~mpat:(Filters.mp#rec_exp)
    ~exp_filter:(efilter "rec_exp") ~pat_filter:(pfilter "rec_exp");
  add_quotation { domain; name = "case" } case_quot ~mexp:(Filters.me#case)
    ~mpat:(Filters.mp#case) ~exp_filter:(efilter "case")
    ~pat_filter:(pfilter "case");
  add_quotation { domain; name = "mbind" } mbind_quot
    ~mexp:(Filters.me#mbind) ~mpat:(Filters.mp#mbind)
    ~exp_filter:(efilter "mbind") ~pat_filter:(pfilter "mbind");
  add_quotation { domain; name = "ident" } ident_quot
    ~mexp:(Filters.me#ident) ~mpat:(Filters.mp#ident)
    ~exp_filter:(efilter "ident") ~pat_filter:(pfilter "ident");
  add_quotation { domain; name = "or_ctyp" } constructor_declarations
    ~mexp:(Filters.me#or_ctyp) ~mpat:(Filters.me#or_ctyp)
    ~exp_filter:(efilter "or_ctyp") ~pat_filter:(pfilter "or_ctyp");
  add_quotation { domain; name = "row_field" } row_field
    ~mexp:(Filters.me#row_field) ~mpat:(Filters.mp#row_field)
    ~exp_filter:(efilter "row_field") ~pat_filter:(pfilter "row_field");
  of_exp ~name:{ domain; name = "with_exp" } ~entry:with_exp_lang ();
  of_stru ~name:{ domain; name = "with_stru" } ~entry:with_stru_lang ();
  add { domain; name = "str" } Dyn_tag.exp
    (fun _loc  _loc_option  s  -> `Str (_loc, s));
  add { domain; name = "str" } Dyn_tag.stru
    (fun _loc  _loc_option  s  -> `StExp (_loc, (`Str (_loc, s))))
let efilter str e =
  let e = exp_filter_n e in
  let _loc = loc_of e in
  (`Constraint
     (_loc, e, (`Dot (_loc, (`Uid (_loc, "Astfn")), (`Lid (_loc, str))))) : 
    Astf.exp )
let pfilter str e =
  let p = pat_filter_n e in
  let _loc = loc_of p in
  (`Constraint
     (_loc, p, (`Dot (_loc, (`Uid (_loc, "Astfn")), (`Lid (_loc, str))))) : 
    Astf.pat )
let _ =
  add_quotation { domain; name = "sigi-" } sigi_quot
    ~mexp:(fun loc  p  -> m#sigi loc (Objs.strip_sigi p))
    ~mpat:(fun loc  p  -> m#sigi loc (Objs.strip_sigi p))
    ~exp_filter:(efilter "sigi") ~pat_filter:(pfilter "sigi");
  add_quotation { domain; name = "stru-" } stru_quot
    ~mexp:(fun loc  p  -> m#stru loc (Objs.strip_stru p))
    ~mpat:(fun loc  p  -> m#stru loc (Objs.strip_stru p))
    ~exp_filter:(efilter "stru") ~pat_filter:(pfilter "stru");
  add_quotation { domain; name = "ctyp-" } ctyp_quot
    ~mexp:(fun loc  p  -> m#ctyp loc (Objs.strip_ctyp p))
    ~mpat:(fun loc  p  -> m#ctyp loc (Objs.strip_ctyp p))
    ~exp_filter:(efilter "ctyp") ~pat_filter:(pfilter "ctyp");
  add_quotation { domain; name = "pat-" } pat_quot
    ~mexp:(fun loc  p  -> m#pat loc (Objs.strip_pat p))
    ~mpat:(fun loc  p  -> m#pat loc (Objs.strip_pat p))
    ~exp_filter:(efilter "pat") ~pat_filter:(pfilter "pat");
  add_quotation { domain; name = "ep-" } exp_quot
    ~mexp:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
    ~mpat:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
    ~exp_filter:(efilter "ep") ~pat_filter:(pfilter "ep");
  add_quotation { domain; name = "exp-" } exp_quot
    ~mexp:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
    ~mpat:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
    ~exp_filter:(efilter "exp") ~pat_filter:(pfilter "exp");
  add_quotation { domain; name = "mtyp-" } mtyp_quot
    ~mexp:(fun loc  p  -> m#mtyp loc (Objs.strip_mtyp p))
    ~mpat:(fun loc  p  -> m#mtyp loc (Objs.strip_mtyp p))
    ~exp_filter:(efilter "mtyp") ~pat_filter:(pfilter "mtyp");
  add_quotation { domain; name = "mexp-" } mexp_quot
    ~mexp:(fun loc  p  -> m#mexp loc (Objs.strip_mexp p))
    ~mpat:(fun loc  p  -> m#mexp loc (Objs.strip_mexp p))
    ~exp_filter:(efilter "mexp") ~pat_filter:(pfilter "mexp");
  add_quotation { domain; name = "cltyp-" } cltyp_quot
    ~mexp:(fun loc  p  -> m#cltyp loc (Objs.strip_cltyp p))
    ~mpat:(fun loc  p  -> m#cltyp loc (Objs.strip_cltyp p))
    ~exp_filter:(efilter "cltyp") ~pat_filter:(pfilter "cltyp");
  add_quotation { domain; name = "clexp-" } clexp_quot
    ~mexp:(fun loc  p  -> m#clexp loc (Objs.strip_clexp p))
    ~mpat:(fun loc  p  -> m#clexp loc (Objs.strip_clexp p))
    ~exp_filter:(efilter "clexp") ~pat_filter:(pfilter "clexp");
  add_quotation { domain; name = "clsigi-" } clsigi_quot
    ~mexp:(fun loc  p  -> m#clsigi loc (Objs.strip_clsigi p))
    ~mpat:(fun loc  p  -> m#clsigi loc (Objs.strip_clsigi p))
    ~exp_filter:(efilter "clsigi") ~pat_filter:(pfilter "clsigi");
  add_quotation { domain; name = "clfield-" } clfield_quot
    ~mexp:(fun loc  p  -> m#clfield loc (Objs.strip_clfield p))
    ~mpat:(fun loc  p  -> m#clfield loc (Objs.strip_clfield p))
    ~exp_filter:(efilter "clfield") ~pat_filter:(pfilter "clfield");
  add_quotation { domain; name = "constr-" } constr_quot
    ~mexp:(fun loc  p  -> m#constr loc (Objs.strip_constr p))
    ~mpat:(fun loc  p  -> m#constr loc (Objs.strip_constr p))
    ~exp_filter:(efilter "constr") ~pat_filter:(pfilter "constr");
  add_quotation { domain; name = "bind-" } bind_quot
    ~mexp:(fun loc  p  -> m#bind loc (Objs.strip_bind p))
    ~mpat:(fun loc  p  -> m#bind loc (Objs.strip_bind p))
    ~exp_filter:(efilter "bind") ~pat_filter:(pfilter "bind");
  add_quotation { domain; name = "rec_exp-" } rec_exp_quot
    ~mexp:(fun loc  p  -> m#rec_exp loc (Objs.strip_rec_exp p))
    ~mpat:(fun loc  p  -> m#rec_exp loc (Objs.strip_rec_exp p))
    ~exp_filter:(efilter "rec_exp") ~pat_filter:(pfilter "rec_exp");
  add_quotation { domain; name = "case-" } case_quot
    ~mexp:(fun loc  p  -> m#case loc (Objs.strip_case p))
    ~mpat:(fun loc  p  -> m#case loc (Objs.strip_case p))
    ~exp_filter:(efilter "case") ~pat_filter:(pfilter "case");
  add_quotation { domain; name = "mbind-" } mbind_quot
    ~mexp:(fun loc  p  -> m#mbind loc (Objs.strip_mbind p))
    ~mpat:(fun loc  p  -> m#mbind loc (Objs.strip_mbind p))
    ~exp_filter:(efilter "mbind") ~pat_filter:(pfilter "mbind");
  add_quotation { domain; name = "ident-" } ident_quot
    ~mexp:(fun loc  p  -> m#ident loc (Objs.strip_ident p))
    ~mpat:(fun loc  p  -> m#ident loc (Objs.strip_ident p))
    ~exp_filter:(efilter "ident") ~pat_filter:(pfilter "ident");
  add_quotation { domain; name = "or_ctyp-" } constructor_declarations
    ~mexp:(fun loc  p  -> m#or_ctyp loc (Objs.strip_or_ctyp p))
    ~mpat:(fun loc  p  -> m#or_ctyp loc (Objs.strip_or_ctyp p))
    ~exp_filter:(efilter "or_ctyp") ~pat_filter:(pfilter "or_ctyp");
  add_quotation { domain; name = "row_field-" } row_field
    ~mexp:(fun loc  p  -> m#row_field loc (Objs.strip_row_field p))
    ~mpat:(fun loc  p  -> m#row_field loc (Objs.strip_row_field p))
    ~exp_filter:(efilter "row_field") ~pat_filter:(pfilter "row_field")
let _ =
  let exp_filter = exp_filter_n in
  let pat_filter = pat_filter_n in
  add_quotation { domain; name = "sigi-'" } sigi_quot
    ~mexp:(fun loc  p  -> m#sigi loc (Objs.strip_sigi p))
    ~mpat:(fun loc  p  -> m#sigi loc (Objs.strip_sigi p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "stru-'" } stru_quot
    ~mexp:(fun loc  p  -> m#stru loc (Objs.strip_stru p))
    ~mpat:(fun loc  p  -> m#stru loc (Objs.strip_stru p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "ctyp-'" } ctyp_quot
    ~mexp:(fun loc  p  -> m#ctyp loc (Objs.strip_ctyp p))
    ~mpat:(fun loc  p  -> m#ctyp loc (Objs.strip_ctyp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "pat-'" } pat_quot
    ~mexp:(fun loc  p  -> m#pat loc (Objs.strip_pat p))
    ~mpat:(fun loc  p  -> m#pat loc (Objs.strip_pat p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "ep-'" } exp_quot
    ~mexp:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
    ~mpat:(fun loc  p  -> m#exp loc (Objs.strip_exp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "exp-'" } exp_quot
    ~mexp:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
    ~mpat:(fun loc  p  -> m#exp loc (Objs.strip_exp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "mtyp-'" } mtyp_quot
    ~mexp:(fun loc  p  -> m#mtyp loc (Objs.strip_mtyp p))
    ~mpat:(fun loc  p  -> m#mtyp loc (Objs.strip_mtyp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "mexp-'" } mexp_quot
    ~mexp:(fun loc  p  -> m#mexp loc (Objs.strip_mexp p))
    ~mpat:(fun loc  p  -> m#mexp loc (Objs.strip_mexp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "cltyp-'" } cltyp_quot
    ~mexp:(fun loc  p  -> m#cltyp loc (Objs.strip_cltyp p))
    ~mpat:(fun loc  p  -> m#cltyp loc (Objs.strip_cltyp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "clexp-'" } clexp_quot
    ~mexp:(fun loc  p  -> m#clexp loc (Objs.strip_clexp p))
    ~mpat:(fun loc  p  -> m#clexp loc (Objs.strip_clexp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "clsigi-'" } clsigi_quot
    ~mexp:(fun loc  p  -> m#clsigi loc (Objs.strip_clsigi p))
    ~mpat:(fun loc  p  -> m#clsigi loc (Objs.strip_clsigi p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "clfield-'" } clfield_quot
    ~mexp:(fun loc  p  -> m#clfield loc (Objs.strip_clfield p))
    ~mpat:(fun loc  p  -> m#clfield loc (Objs.strip_clfield p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "constr-'" } constr_quot
    ~mexp:(fun loc  p  -> m#constr loc (Objs.strip_constr p))
    ~mpat:(fun loc  p  -> m#constr loc (Objs.strip_constr p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "bind-'" } bind_quot
    ~mexp:(fun loc  p  -> m#bind loc (Objs.strip_bind p))
    ~mpat:(fun loc  p  -> m#bind loc (Objs.strip_bind p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "rec_exp-'" } rec_exp_quot
    ~mexp:(fun loc  p  -> m#rec_exp loc (Objs.strip_rec_exp p))
    ~mpat:(fun loc  p  -> m#rec_exp loc (Objs.strip_rec_exp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "case-'" } case_quot
    ~mexp:(fun loc  p  -> m#case loc (Objs.strip_case p))
    ~mpat:(fun loc  p  -> m#case loc (Objs.strip_case p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "mbind-'" } mbind_quot
    ~mexp:(fun loc  p  -> m#mbind loc (Objs.strip_mbind p))
    ~mpat:(fun loc  p  -> m#mbind loc (Objs.strip_mbind p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "ident-'" } ident_quot
    ~mexp:(fun loc  p  -> m#ident loc (Objs.strip_ident p))
    ~mpat:(fun loc  p  -> m#ident loc (Objs.strip_ident p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "or_ctyp-'" } constructor_declarations
    ~mexp:(fun loc  p  -> m#or_ctyp loc (Objs.strip_or_ctyp p))
    ~mpat:(fun loc  p  -> m#or_ctyp loc (Objs.strip_or_ctyp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "row_field-'" } row_field
    ~mexp:(fun loc  p  -> m#row_field loc (Objs.strip_row_field p))
    ~mpat:(fun loc  p  -> m#row_field loc (Objs.strip_row_field p))
    ~exp_filter ~pat_filter
let p = Gramf.mk "p"
let _ =
  Gramf.extend_single
    ({
       entry = (p : 'p Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "when"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(`Fun\n   (_loc,\n     (`Bar\n        (_loc, (`CaseWhen (_loc, p, e, (`Bool (_loc, true)))),\n          (`Case (_loc, (`Any _loc), (`Bool (_loc, false))))))) : Astf.exp )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                         ((`Fun
                             (_loc,
                               (`Bar
                                  (_loc,
                                    (`CaseWhen
                                       (_loc, p, e, (`Bool (_loc, true)))),
                                    (`Case
                                       (_loc, (`Any _loc),
                                         (`Bool (_loc, false))))))) : 
                         Astf.exp ) : 'p ) : 'exp ->
                                               Tokenf.txt ->
                                                 'pat -> Locf.t -> 'p ))
               };
              {
                symbols = [Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
                annot =
                  "`Fun\n  (_loc,\n    (`Bar\n       (_loc, (`Case (_loc, p, (`Bool (_loc, true)))),\n         (`Case (_loc, (`Any _loc), (`Bool (_loc, false)))))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  (_loc : Locf.t)  ->
                        (`Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case (_loc, p, (`Bool (_loc, true)))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Bool (_loc, false))))))) : 'p ) : 
                     'pat -> Locf.t -> 'p ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let () = of_exp ~name:{ domain; name = "p" } ~entry:p ()
let import = Gramf.mk "import"
let _ =
  let a: 'a Gramf.t = Gramf.mk "a" and n: 'n Gramf.t = Gramf.mk "n" in
  Gramf.extend_single
    ({
       entry = (a : 'a Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   List1 (Nterm (Gramf.obj (n : 'n Gramf.t )));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "Ast_gen.sem_of_list\n  (List.map\n     (fun ((l : Tokenf.txt),r)  ->\n        let xloc = l.loc in\n        let pr = `Lid (xloc, (l.txt)) in\n        let pl =\n          match r with\n          | None  -> pr\n          | Some (y : Tokenf.txt) -> let yloc = y.loc in `Lid (yloc, (y.txt)) in\n        (`Value\n           (_loc, (`Negative _loc),\n             (`Bind (_loc, pl, (`Dot (_loc, (`Uid (_loc, m)), pr))))) : \n          Astf.stru )) ns)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (ns : 'n list)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let m = __fan_0.txt in
                         (Ast_gen.sem_of_list
                            (List.map
                               (fun ((l : Tokenf.txt),r)  ->
                                  let xloc = l.loc in
                                  let pr = `Lid (xloc, (l.txt)) in
                                  let pl =
                                    match r with
                                    | None  -> pr
                                    | Some (y : Tokenf.txt) ->
                                        let yloc = y.loc in
                                        `Lid (yloc, (y.txt)) in
                                  (`Value
                                     (_loc, (`Negative _loc),
                                       (`Bind
                                          (_loc, pl,
                                            (`Dot
                                               (_loc, (`Uid (_loc, m)), pr))))) : 
                                    Astf.stru )) ns) : 'a ) : Tokenf.txt ->
                                                                'n list ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'a ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (n : 'n Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "(x, None)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : Tokenf.txt)  (_loc : Locf.t)  ->
                         ((x, None) : 'n ) : Tokenf.txt -> Locf.t -> 'n ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "as"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                    Tokenf.pattern )];
                annot = "(x, (Some y))\n";
                fn =
                  (Gramf.mk_action
                     (fun (y : Tokenf.txt)  _  (x : Tokenf.txt) 
                        (_loc : Locf.t)  -> ((x, (Some y)) : 'n ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'n ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (import : 'import Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols = [List1 (Nterm (Gramf.obj (a : 'a Gramf.t )))];
                 annot = "Ast_gen.sem_of_list xs\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'a list)  (_loc : Locf.t)  ->
                         (Ast_gen.sem_of_list xs : 'import ) : 'a list ->
                                                                 Locf.t ->
                                                                   'import ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let () = of_stru ~name:{ domain; name = "import" } ~entry:import ()
let () =
  let f (loc : Locf.t) _meta _content =
    let s = Locf.to_string loc in (`Str (loc, s) : Astf.exp ) in
  let f2 (loc : Locf.t) _meta _content =
    let s = Locf.to_string loc in
    (`StExp (loc, (`Str (loc, s))) : Astf.stru ) in
  Ast_quotation.add { domain; name = "here" } Dyn_tag.exp f;
  Ast_quotation.add { domain; name = "here" } Dyn_tag.stru f2
let () =
  Printexc.register_printer @@
    (function
     | Out_of_memory  -> Some "Out of memory"
     | Assert_failure (file,line,char) ->
         Some
           (Format.sprintf "Assertion failed, file %S, line %d, char %d" file
              line char)
     | Match_failure (file,line,char) ->
         Some
           (Format.sprintf
              "Pattern matching failed, file %S, line %d, char %d" file line
              char)
     | Failure str -> Some (Format.sprintf "Failure: %S" str)
     | Invalid_argument str ->
         Some (Format.sprintf "Invalid argument: %S" str)
     | Sys_error str -> Some (Format.sprintf "I/O error: %S" str)
     | Streamf.NotConsumed  ->
         Some (Format.sprintf "Parse failure(NotConsumed)")
     | Streamf.Error str -> Some (Format.sprintf "Streamf.Error %s" str)
     | _ -> None)
