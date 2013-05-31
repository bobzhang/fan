open Format

open FanOps

open AstLib

open Filters

include PreCast

open AstQuotation

open Fsyntax

open LibUtil

open AstQuotation

open FControl

let efilter str e =
  let e = exp_filter e in
  let _loc = loc_of e in
  (`Constraint
     (_loc, e, (`Dot (_loc, (`Uid (_loc, "FAst")), (`Lid (_loc, str))))) : 
    FAst.exp )

let pfilter str e =
  let p = pat_filter e in
  let _loc = loc_of p in
  (`Constraint
     (_loc, p, (`Dot (_loc, (`Uid (_loc, "FAst")), (`Lid (_loc, str))))) : 
    FAst.pat )

let d = `Absolute ["Fan"; "Lang"]

let _ =
  begin
    of_stru_with_filter ~name:(d, "ocaml") ~entry:strus
      ~filter:LangOcaml.filter;
    of_exp ~name:(d, "fans") ~entry:LangFans.fan_quots;
    of_exp ~name:(d, "save") ~entry:LangSave.save_quot;
    of_stru ~name:(d, "include") ~entry:LangInclude.include_quot
  end

let d = `Absolute ["Fan"; "Lang"; "Macro"]

let _ =
  begin
    of_exp_with_filter ~name:(d, "exp") ~entry:exp
      ~filter:(AstMacros.macro_expander#exp);
    of_clfield_with_filter ~name:(d, "clfield") ~entry:clfield
      ~filter:(AstMacros.macro_expander#clfield);
    of_stru_with_filter ~name:(d, "stru") ~entry:stru
      ~filter:(AstMacros.macro_expander#stru)
  end

let d = `Absolute ["Fan"; "Lang"; "Meta"]

let _ =
  begin
    add_quotation (d, "sigi'") sigi_quot ~mexp:(Filters.me#sigi)
      ~mpat:(Filters.mp#sigi) ~exp_filter ~pat_filter;
    add_quotation (d, "stru'") stru_quot ~mexp:(Filters.me#stru)
      ~mpat:(Filters.mp#stru) ~exp_filter ~pat_filter;
    add_quotation (d, "ctyp'") ctyp_quot ~mexp:(Filters.me#ctyp)
      ~mpat:(Filters.mp#ctyp) ~exp_filter ~pat_filter;
    add_quotation (d, "pat'") pat_quot ~mexp:(Filters.me#pat)
      ~mpat:(Filters.mp#pat) ~exp_filter ~pat_filter;
    add_quotation (d, "exp'") exp_quot ~mexp:(Filters.me#exp)
      ~mpat:(Filters.mp#exp) ~exp_filter ~pat_filter;
    add_quotation (d, "mtyp'") mtyp_quot ~mexp:(Filters.me#mtyp)
      ~mpat:(Filters.mp#mtyp) ~exp_filter ~pat_filter;
    add_quotation (d, "mexp'") mexp_quot ~mexp:(Filters.me#mexp)
      ~mpat:(Filters.mp#mexp) ~exp_filter ~pat_filter;
    add_quotation (d, "cltyp'") cltyp_quot ~mexp:(Filters.me#cltyp)
      ~mpat:(Filters.mp#cltyp) ~exp_filter ~pat_filter;
    add_quotation (d, "clexp'") clexp_quot ~mexp:(Filters.me#clexp)
      ~mpat:(Filters.mp#clexp) ~exp_filter ~pat_filter;
    add_quotation (d, "clsigi'") clsigi_quot ~mexp:(Filters.me#clsigi)
      ~mpat:(Filters.mp#clsigi) ~exp_filter ~pat_filter;
    add_quotation (d, "clfield'") clfield_quot ~mexp:(Filters.me#clfield)
      ~mpat:(Filters.mp#clfield) ~exp_filter ~pat_filter;
    add_quotation (d, "constr'") constr_quot ~mexp:(Filters.me#constr)
      ~mpat:(Filters.mp#constr) ~exp_filter ~pat_filter;
    add_quotation (d, "bind'") bind_quot ~mexp:(Filters.me#bind)
      ~mpat:(Filters.mp#bind) ~exp_filter ~pat_filter;
    add_quotation (d, "rec_exp'") rec_exp_quot ~mexp:(Filters.me#rec_exp)
      ~mpat:(Filters.mp#rec_exp) ~exp_filter ~pat_filter;
    add_quotation (d, "case'") case_quot ~mexp:(Filters.me#case)
      ~mpat:(Filters.mp#case) ~exp_filter ~pat_filter;
    add_quotation (d, "mbind'") mbind_quot ~mexp:(Filters.me#mbind)
      ~mpat:(Filters.mp#mbind) ~exp_filter ~pat_filter;
    add_quotation (d, "ident'") ident_quot ~mexp:(Filters.me#ident)
      ~mpat:(Filters.mp#ident) ~exp_filter ~pat_filter;
    add_quotation (d, "rec_flag'") rec_flag_quot ~mexp:(Filters.me#flag)
      ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
    add_quotation (d, "private_flag'") private_flag_quot
      ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
    add_quotation (d, "row_var_flag'") row_var_flag_quot
      ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
    add_quotation (d, "mutable_flag'") mutable_flag_quot
      ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
    add_quotation (d, "virtual_flag'") virtual_flag_quot
      ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
    add_quotation (d, "override_flag'") override_flag_quot
      ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
    add_quotation (d, "direction_flag'") direction_flag_quot
      ~mexp:(Filters.me#flag) ~mpat:(Filters.mp#flag) ~exp_filter ~pat_filter;
    add_quotation (d, "or_ctyp'") constructor_declarations
      ~mexp:(Filters.me#or_ctyp) ~mpat:(Filters.me#or_ctyp) ~exp_filter
      ~pat_filter;
    add_quotation (d, "row_field'") row_field ~mexp:(Filters.me#row_field)
      ~mpat:(Filters.mp#row_field) ~exp_filter ~pat_filter
  end

let _ =
  begin
    add_quotation (d, "sigi") sigi_quot ~mexp:(Filters.me#sigi)
      ~mpat:(Filters.mp#sigi) ~exp_filter:(efilter "sigi")
      ~pat_filter:(pfilter "sigi");
    add_quotation (d, "stru") stru_quot ~mexp:(Filters.me#stru)
      ~mpat:(Filters.mp#stru) ~exp_filter:(efilter "stru")
      ~pat_filter:(pfilter "stru");
    add_quotation (d, "ctyp") ctyp_quot ~mexp:(Filters.me#ctyp)
      ~mpat:(Filters.mp#ctyp) ~exp_filter:(efilter "ctyp")
      ~pat_filter:(pfilter "ctyp");
    add_quotation (d, "pat") pat_quot ~mexp:(Filters.me#pat)
      ~mpat:(Filters.mp#pat) ~exp_filter:(efilter "pat")
      ~pat_filter:(pfilter "pat");
    add_quotation (d, "ep") exp_quot ~mexp:(Filters.me#exp)
      ~mpat:(Filters.mp#exp) ~exp_filter:(efilter "ep")
      ~pat_filter:(pfilter "ep");
    add_quotation (d, "exp") exp_quot ~mexp:(Filters.me#exp)
      ~mpat:(Filters.mp#exp) ~exp_filter:(efilter "exp")
      ~pat_filter:(pfilter "exp");
    add_quotation (d, "mtyp") mtyp_quot ~mexp:(Filters.me#mtyp)
      ~mpat:(Filters.mp#mtyp) ~exp_filter:(efilter "mtyp")
      ~pat_filter:(pfilter "mtyp");
    add_quotation (d, "mexp") mexp_quot ~mexp:(Filters.me#mexp)
      ~mpat:(Filters.mp#mexp) ~exp_filter:(efilter "mexp")
      ~pat_filter:(pfilter "mexp");
    add_quotation (d, "cltyp") cltyp_quot ~mexp:(Filters.me#cltyp)
      ~mpat:(Filters.mp#cltyp) ~exp_filter:(efilter "cltyp")
      ~pat_filter:(pfilter "cltyp");
    add_quotation (d, "clexp") clexp_quot ~mexp:(Filters.me#clexp)
      ~mpat:(Filters.mp#clexp) ~exp_filter:(efilter "clexp")
      ~pat_filter:(pfilter "clexp");
    add_quotation (d, "clsigi") clsigi_quot ~mexp:(Filters.me#clsigi)
      ~mpat:(Filters.mp#clsigi) ~exp_filter:(efilter "clsigi")
      ~pat_filter:(pfilter "clsigi");
    add_quotation (d, "clfield") clfield_quot ~mexp:(Filters.me#clfield)
      ~mpat:(Filters.mp#clfield) ~exp_filter:(efilter "clfield")
      ~pat_filter:(pfilter "clfield");
    add_quotation (d, "constr") constr_quot ~mexp:(Filters.me#constr)
      ~mpat:(Filters.mp#constr) ~exp_filter:(efilter "constr")
      ~pat_filter:(pfilter "constr");
    add_quotation (d, "bind") bind_quot ~mexp:(Filters.me#bind)
      ~mpat:(Filters.mp#bind) ~exp_filter:(efilter "bind")
      ~pat_filter:(pfilter "bind");
    add_quotation (d, "rec_exp") rec_exp_quot ~mexp:(Filters.me#rec_exp)
      ~mpat:(Filters.mp#rec_exp) ~exp_filter:(efilter "rec_exp")
      ~pat_filter:(pfilter "rec_exp");
    add_quotation (d, "case") case_quot ~mexp:(Filters.me#case)
      ~mpat:(Filters.mp#case) ~exp_filter:(efilter "case")
      ~pat_filter:(pfilter "case");
    add_quotation (d, "mbind") mbind_quot ~mexp:(Filters.me#mbind)
      ~mpat:(Filters.mp#mbind) ~exp_filter:(efilter "mbind")
      ~pat_filter:(pfilter "mbind");
    add_quotation (d, "ident") ident_quot ~mexp:(Filters.me#ident)
      ~mpat:(Filters.mp#ident) ~exp_filter:(efilter "ident")
      ~pat_filter:(pfilter "ident");
    add_quotation (d, "or_ctyp") constructor_declarations
      ~mexp:(Filters.me#or_ctyp) ~mpat:(Filters.me#or_ctyp)
      ~exp_filter:(efilter "or_ctyp") ~pat_filter:(pfilter "or_ctyp");
    add_quotation (d, "row_field") row_field ~mexp:(Filters.me#row_field)
      ~mpat:(Filters.mp#row_field) ~exp_filter:(efilter "row_field")
      ~pat_filter:(pfilter "row_field");
    of_exp ~name:(d, "with_exp") ~entry:with_exp_lang;
    of_stru ~name:(d, "with_stru") ~entry:with_stru_lang;
    add ((`Absolute ["Fan"; "Lang"]), "str") FDyn.exp_tag
      (fun _loc  _loc_option  s  -> `Str (_loc, s));
    add ((`Absolute ["Fan"; "Lang"]), "str") FDyn.stru_tag
      (fun _loc  _loc_option  s  -> `StExp (_loc, (`Str (_loc, s))))
  end

let p = Fgram.mk "p"

let _ =
  Fgram.extend_single (p : 'p Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword "when";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("Fgram.mk_action\n  (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->\n     ((`Fun\n         (_loc,\n           (`Bar\n              (_loc, (`CaseWhen (_loc, p, e, (`Lid (_loc, \"true\")))),\n                (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) : \n     FAst.exp ) : 'p ))\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->
                   ((`Fun
                       (_loc,
                         (`Bar
                            (_loc,
                              (`CaseWhen (_loc, p, e, (`Lid (_loc, "true")))),
                              (`Case
                                 (_loc, (`Any _loc), (`Lid (_loc, "false"))))))) : 
                   FAst.exp ) : 'p )))));
        ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
          ("Fgram.mk_action\n  (fun (p : 'pat)  (_loc : FLoc.t)  ->\n     (`Fun\n        (_loc,\n          (`Bar\n             (_loc, (`Case (_loc, p, (`Lid (_loc, \"true\")))),\n               (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) : \n     'p ))\n",
            (Fgram.mk_action
               (fun (p : 'pat)  (_loc : FLoc.t)  ->
                  (`Fun
                     (_loc,
                       (`Bar
                          (_loc, (`Case (_loc, p, (`Lid (_loc, "true")))),
                            (`Case
                               (_loc, (`Any _loc), (`Lid (_loc, "false"))))))) : 
                  'p )))))]))

let _ = of_exp ~name:(d, "p") ~entry:p

open PFan

open PMacro

open PGrammar

open PStream

let _ = of_exp ~name:(d, "stream") ~entry:PStream.stream_exp

open AstInjection

open PluginsN

open CodeTemplate

open FLexGen

let m = new FanAstN.meta

let efilter str e =
  let e = exp_filter_n e in
  let _loc = loc_of e in
  (`Constraint
     (_loc, e, (`Dot (_loc, (`Uid (_loc, "FAstN")), (`Lid (_loc, str))))) : 
    FAst.exp )

let pfilter str e =
  let p = pat_filter_n e in
  let _loc = loc_of p in
  (`Constraint
     (_loc, p, (`Dot (_loc, (`Uid (_loc, "FAstN")), (`Lid (_loc, str))))) : 
    FAst.pat )

let _ =
  begin
    add_quotation (d, "sigi-") sigi_quot
      ~mexp:(fun loc  p  -> m#sigi loc (Objs.strip_sigi p))
      ~mpat:(fun loc  p  -> m#sigi loc (Objs.strip_sigi p))
      ~exp_filter:(efilter "sigi") ~pat_filter:(pfilter "sigi");
    add_quotation (d, "stru-") stru_quot
      ~mexp:(fun loc  p  -> m#stru loc (Objs.strip_stru p))
      ~mpat:(fun loc  p  -> m#stru loc (Objs.strip_stru p))
      ~exp_filter:(efilter "stru") ~pat_filter:(pfilter "stru");
    add_quotation (d, "ctyp-") ctyp_quot
      ~mexp:(fun loc  p  -> m#ctyp loc (Objs.strip_ctyp p))
      ~mpat:(fun loc  p  -> m#ctyp loc (Objs.strip_ctyp p))
      ~exp_filter:(efilter "ctyp") ~pat_filter:(pfilter "ctyp");
    add_quotation (d, "pat-") pat_quot
      ~mexp:(fun loc  p  -> m#pat loc (Objs.strip_pat p))
      ~mpat:(fun loc  p  -> m#pat loc (Objs.strip_pat p))
      ~exp_filter:(efilter "pat") ~pat_filter:(pfilter "pat");
    add_quotation (d, "ep-") exp_quot
      ~mexp:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
      ~mpat:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
      ~exp_filter:(efilter "ep") ~pat_filter:(pfilter "ep");
    add_quotation (d, "exp-") exp_quot
      ~mexp:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
      ~mpat:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
      ~exp_filter:(efilter "exp") ~pat_filter:(pfilter "exp");
    add_quotation (d, "mtyp-") mtyp_quot
      ~mexp:(fun loc  p  -> m#mtyp loc (Objs.strip_mtyp p))
      ~mpat:(fun loc  p  -> m#mtyp loc (Objs.strip_mtyp p))
      ~exp_filter:(efilter "mtyp") ~pat_filter:(pfilter "mtyp");
    add_quotation (d, "mexp-") mexp_quot
      ~mexp:(fun loc  p  -> m#mexp loc (Objs.strip_mexp p))
      ~mpat:(fun loc  p  -> m#mexp loc (Objs.strip_mexp p))
      ~exp_filter:(efilter "mexp") ~pat_filter:(pfilter "mexp");
    add_quotation (d, "cltyp-") cltyp_quot
      ~mexp:(fun loc  p  -> m#cltyp loc (Objs.strip_cltyp p))
      ~mpat:(fun loc  p  -> m#cltyp loc (Objs.strip_cltyp p))
      ~exp_filter:(efilter "cltyp") ~pat_filter:(pfilter "cltyp");
    add_quotation (d, "clexp-") clexp_quot
      ~mexp:(fun loc  p  -> m#clexp loc (Objs.strip_clexp p))
      ~mpat:(fun loc  p  -> m#clexp loc (Objs.strip_clexp p))
      ~exp_filter:(efilter "clexp") ~pat_filter:(pfilter "clexp");
    add_quotation (d, "clsigi-") clsigi_quot
      ~mexp:(fun loc  p  -> m#clsigi loc (Objs.strip_clsigi p))
      ~mpat:(fun loc  p  -> m#clsigi loc (Objs.strip_clsigi p))
      ~exp_filter:(efilter "clsigi") ~pat_filter:(pfilter "clsigi");
    add_quotation (d, "clfield-") clfield_quot
      ~mexp:(fun loc  p  -> m#clfield loc (Objs.strip_clfield p))
      ~mpat:(fun loc  p  -> m#clfield loc (Objs.strip_clfield p))
      ~exp_filter:(efilter "clfield") ~pat_filter:(pfilter "clfield");
    add_quotation (d, "constr-") constr_quot
      ~mexp:(fun loc  p  -> m#constr loc (Objs.strip_constr p))
      ~mpat:(fun loc  p  -> m#constr loc (Objs.strip_constr p))
      ~exp_filter:(efilter "constr") ~pat_filter:(pfilter "constr");
    add_quotation (d, "bind-") bind_quot
      ~mexp:(fun loc  p  -> m#bind loc (Objs.strip_bind p))
      ~mpat:(fun loc  p  -> m#bind loc (Objs.strip_bind p))
      ~exp_filter:(efilter "bind") ~pat_filter:(pfilter "bind");
    add_quotation (d, "rec_exp-") rec_exp_quot
      ~mexp:(fun loc  p  -> m#rec_exp loc (Objs.strip_rec_exp p))
      ~mpat:(fun loc  p  -> m#rec_exp loc (Objs.strip_rec_exp p))
      ~exp_filter:(efilter "rec_exp") ~pat_filter:(pfilter "rec_exp");
    add_quotation (d, "case-") case_quot
      ~mexp:(fun loc  p  -> m#case loc (Objs.strip_case p))
      ~mpat:(fun loc  p  -> m#case loc (Objs.strip_case p))
      ~exp_filter:(efilter "case") ~pat_filter:(pfilter "case");
    add_quotation (d, "mbind-") mbind_quot
      ~mexp:(fun loc  p  -> m#mbind loc (Objs.strip_mbind p))
      ~mpat:(fun loc  p  -> m#mbind loc (Objs.strip_mbind p))
      ~exp_filter:(efilter "mbind") ~pat_filter:(pfilter "mbind");
    add_quotation (d, "ident-") ident_quot
      ~mexp:(fun loc  p  -> m#ident loc (Objs.strip_ident p))
      ~mpat:(fun loc  p  -> m#ident loc (Objs.strip_ident p))
      ~exp_filter:(efilter "ident") ~pat_filter:(pfilter "ident");
    add_quotation (d, "or_ctyp-") constructor_declarations
      ~mexp:(fun loc  p  -> m#or_ctyp loc (Objs.strip_or_ctyp p))
      ~mpat:(fun loc  p  -> m#or_ctyp loc (Objs.strip_or_ctyp p))
      ~exp_filter:(efilter "or_ctyp") ~pat_filter:(pfilter "or_ctyp");
    add_quotation (d, "row_field-") row_field
      ~mexp:(fun loc  p  -> m#row_field loc (Objs.strip_row_field p))
      ~mpat:(fun loc  p  -> m#row_field loc (Objs.strip_row_field p))
      ~exp_filter:(efilter "row_field") ~pat_filter:(pfilter "row_field")
  end

let _ =
  let exp_filter = exp_filter_n in
  let pat_filter = pat_filter_n in
  begin
    add_quotation (d, "sigi-'") sigi_quot
      ~mexp:(fun loc  p  -> m#sigi loc (Objs.strip_sigi p))
      ~mpat:(fun loc  p  -> m#sigi loc (Objs.strip_sigi p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "stru-'") stru_quot
      ~mexp:(fun loc  p  -> m#stru loc (Objs.strip_stru p))
      ~mpat:(fun loc  p  -> m#stru loc (Objs.strip_stru p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "ctyp-'") ctyp_quot
      ~mexp:(fun loc  p  -> m#ctyp loc (Objs.strip_ctyp p))
      ~mpat:(fun loc  p  -> m#ctyp loc (Objs.strip_ctyp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "pat-'") pat_quot
      ~mexp:(fun loc  p  -> m#pat loc (Objs.strip_pat p))
      ~mpat:(fun loc  p  -> m#pat loc (Objs.strip_pat p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "ep-'") exp_quot
      ~mexp:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
      ~mpat:(fun loc  p  -> m#exp loc (Objs.strip_exp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "exp-'") exp_quot
      ~mexp:(fun loc  p  -> m#exp loc (Objs.strip_exp p))
      ~mpat:(fun loc  p  -> m#exp loc (Objs.strip_exp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "mtyp-'") mtyp_quot
      ~mexp:(fun loc  p  -> m#mtyp loc (Objs.strip_mtyp p))
      ~mpat:(fun loc  p  -> m#mtyp loc (Objs.strip_mtyp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "mexp-'") mexp_quot
      ~mexp:(fun loc  p  -> m#mexp loc (Objs.strip_mexp p))
      ~mpat:(fun loc  p  -> m#mexp loc (Objs.strip_mexp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "cltyp-'") cltyp_quot
      ~mexp:(fun loc  p  -> m#cltyp loc (Objs.strip_cltyp p))
      ~mpat:(fun loc  p  -> m#cltyp loc (Objs.strip_cltyp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "clexp-'") clexp_quot
      ~mexp:(fun loc  p  -> m#clexp loc (Objs.strip_clexp p))
      ~mpat:(fun loc  p  -> m#clexp loc (Objs.strip_clexp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "clsigi-'") clsigi_quot
      ~mexp:(fun loc  p  -> m#clsigi loc (Objs.strip_clsigi p))
      ~mpat:(fun loc  p  -> m#clsigi loc (Objs.strip_clsigi p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "clfield-'") clfield_quot
      ~mexp:(fun loc  p  -> m#clfield loc (Objs.strip_clfield p))
      ~mpat:(fun loc  p  -> m#clfield loc (Objs.strip_clfield p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "constr-'") constr_quot
      ~mexp:(fun loc  p  -> m#constr loc (Objs.strip_constr p))
      ~mpat:(fun loc  p  -> m#constr loc (Objs.strip_constr p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "bind-'") bind_quot
      ~mexp:(fun loc  p  -> m#bind loc (Objs.strip_bind p))
      ~mpat:(fun loc  p  -> m#bind loc (Objs.strip_bind p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "rec_exp-'") rec_exp_quot
      ~mexp:(fun loc  p  -> m#rec_exp loc (Objs.strip_rec_exp p))
      ~mpat:(fun loc  p  -> m#rec_exp loc (Objs.strip_rec_exp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "case-'") case_quot
      ~mexp:(fun loc  p  -> m#case loc (Objs.strip_case p))
      ~mpat:(fun loc  p  -> m#case loc (Objs.strip_case p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "mbind-'") mbind_quot
      ~mexp:(fun loc  p  -> m#mbind loc (Objs.strip_mbind p))
      ~mpat:(fun loc  p  -> m#mbind loc (Objs.strip_mbind p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "ident-'") ident_quot
      ~mexp:(fun loc  p  -> m#ident loc (Objs.strip_ident p))
      ~mpat:(fun loc  p  -> m#ident loc (Objs.strip_ident p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "or_ctyp-'") constructor_declarations
      ~mexp:(fun loc  p  -> m#or_ctyp loc (Objs.strip_or_ctyp p))
      ~mpat:(fun loc  p  -> m#or_ctyp loc (Objs.strip_or_ctyp p)) ~exp_filter
      ~pat_filter;
    add_quotation (d, "row_field-'") row_field
      ~mexp:(fun loc  p  -> m#row_field loc (Objs.strip_row_field p))
      ~mpat:(fun loc  p  -> m#row_field loc (Objs.strip_row_field p))
      ~exp_filter ~pat_filter
  end

let normal_handler =
  function
  | Out_of_memory  -> Some "Out of memory"
  | Assert_failure (file,line,char) ->
      Some
        (Format.sprintf "Assertion failed, file %S, line %d, char %d" file
           line char)
  | Match_failure (file,line,char) ->
      Some
        (Format.sprintf "Pattern matching failed, file %S, line %d, char %d"
           file line char)
  | Failure str -> Some (Format.sprintf "Failure: %S" str)
  | Invalid_argument str -> Some (Format.sprintf "Invalid argument: %S" str)
  | Sys_error str -> Some (Format.sprintf "I/O error: %S" str)
  | XStream.Failure  -> Some (Format.sprintf "Parse failure")
  | XStream.Error str -> Some (Format.sprintf "XStream.Error %s" str)
  | _ -> None

let just_print_filters () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k  _v  -> fprintf f "%s@;" k) tbl in
  begin
    pp "@[for interface:@[<hv2>%a@]@]@." p_tbl AstFilters.interf_filters;
    pp "@[for phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.implem_filters;
    pp "@[for top_phrase:@[<hv2>%a@]@]@." p_tbl AstFilters.topphrase_filters
  end

let just_print_parsers () =
  let pp = eprintf in
  let p_tbl f tbl = Hashtbl.iter (fun k  _v  -> fprintf f "%s@;" k) tbl in
  pp "@[Loaded Parsers:@;@[<hv2>%a@]@]@." p_tbl AstParsers.registered_parsers

let just_print_applied_parsers () =
  let pp = eprintf in
  pp "@[Applied Parsers:@;@[<hv2>%a@]@]@."
    (fun f  q  -> Queue.iter (fun (k,_)  -> fprintf f "%s@;" k) q)
    AstParsers.applied_parsers

type file_kind =  
  | Intf of string
  | Impl of string
  | Str of string
  | ModuleImpl of string
  | IncludeDir of string 

let print_loaded_modules = ref false

let loaded_modules = ref SSet.empty

let add_to_loaded_modules name =
  loaded_modules := (SSet.add name loaded_modules.contents)

let (objext,libext) =
  if Dynlink.is_native then (".cmxs", ".cmxs") else (".cmo", ".cma")

let require name =
  if not (SSet.mem name loaded_modules.contents)
  then begin add_to_loaded_modules name; DynLoader.load (name ^ libext) end

let _ =
  let open FControl in
    Fgram.unsafe_extend_single (item : 'item Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "require";
            `Stoken
              (((function | `STR (_,_) -> true | _ -> false)),
                (`Normal, "`STR (_,_)"))],
             ("Fgram.mk_action\n  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `STR (_,s) -> (require s : 'item )\n     | _ -> failwith \"require s\n\")\n",
               (Fgram.mk_action
                  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                     match __fan_1 with
                     | `STR (_,s) -> (require s : 'item )
                     | _ -> failwith "require s\n"))))]))

let output_file = ref None

let process_intf name =
  let v =
    match PreCast.parse_file name PreCast.parse_interf with
    | None  -> None
    | Some x -> let x = AstFilters.apply_interf_filters x in Some x in
  PreCast.CurrentPrinter.print_interf ?input_file:(Some name)
    ?output_file:(output_file.contents) v

let process_impl name =
  let v =
    match PreCast.parse_file name PreCast.parse_implem with
    | None  -> None
    | Some x -> let x = AstFilters.apply_implem_filters x in Some x in
  PreCast.CurrentPrinter.print_implem ?input_file:(Some name)
    ?output_file:(output_file.contents) v

let input_file x =
  match x with
  | Intf file_name ->
      begin
        FConfig.compilation_unit :=
          (Some
             (String.capitalize
                (let open Filename in chop_extension (basename file_name))));
        FConfig.current_input_file := file_name; process_intf file_name
      end
  | Impl file_name ->
      begin
        FConfig.compilation_unit :=
          (Some
             (String.capitalize
                (let open Filename in chop_extension (basename file_name))));
        FConfig.current_input_file := file_name; process_impl file_name
      end
  | Str s ->
      let (f,o) = Filename.open_temp_file "from_string" ".ml" in
      begin
        output_string o s; close_out o; FConfig.current_input_file := f;
        process_impl f; at_exit (fun ()  -> Sys.remove f)
      end
  | ModuleImpl file_name -> require file_name
  | IncludeDir dir -> Ref.modify FConfig.dynload_dirs (cons dir)

let initial_spec_list =
  [("-I", (FArg.String ((fun x  -> input_file (IncludeDir x)))),
     "<directory>  Add directory in search patch for object files.");
  ("-intf", (FArg.String ((fun x  -> input_file (Intf x)))),
    "<file>  Parse <file> as an interface, whatever its extension.");
  ("-impl", (FArg.String ((fun x  -> input_file (Impl x)))),
    "<file>  Parse <file> as an implementation, whatever its extension.");
  ("-str", (FArg.String ((fun x  -> input_file (Str x)))),
    "<string>  Parse <string> as an implementation.");
  ("-o", (FArg.String ((fun x  -> output_file := (Some x)))),
    "<file> Output on <file> instead of standard output.");
  ("-unsafe", (FArg.Set FConfig.unsafe),
    "Generate unsafe accesses to array and strings.");
  ("-verbose", (FArg.Set FConfig.verbose), "More verbose in parsing errors.");
  ("-where",
    (FArg.Unit
       ((fun ()  ->
           begin print_endline FConfig.fan_standard_library; exit 0 end))),
    " Print location of standard library and exit");
  ("-loc", (FArg.Set_string FLoc.name),
    ("<name>   Name of the location variable (default: " ^
       (FLoc.name.contents ^ ").")));
  ("-v",
    (FArg.Unit
       ((fun ()  ->
           begin eprintf "Fan version %s@." FConfig.version; exit 0 end))),
    "Print Fan version and exit.");
  ("-compilation-unit",
    (FArg.Unit
       ((fun ()  ->
           begin
             (match FConfig.compilation_unit.contents with
              | Some v -> printf "%s@." v
              | None  -> printf "null");
             exit 0
           end))), "Print the current compilation unit");
  ("-plugin", (FArg.String require), "load plugin cma or cmxs files");
  ("-loaded-modules", (FArg.Set print_loaded_modules),
    "Print the list of loaded modules.");
  ("-loaded-filters", (FArg.Unit just_print_filters),
    "Print the registered filters.");
  ("-loaded-parsers", (FArg.Unit just_print_parsers),
    "Print the loaded parsers.");
  ("-used-parsers", (FArg.Unit just_print_applied_parsers),
    "Print the applied parsers.");
  ("-dlang",
    (FArg.String
       ((fun s  ->
           AstQuotation.default :=
             (FToken.resolve_name FLoc.ghost ((`Sub []), s))))),
    " Set the default language");
  ("-printer",
    (FArg.Symbol
       (["p"; "o"],
         ((fun x  ->
             if x = "o"
             then PreCast.register_text_printer ()
             else PreCast.register_bin_printer ())))),
    "p  for binary and o  for text ")]

let anon_fun name =
  input_file
    (if Filename.check_suffix name ".mli"
     then Intf name
     else
       if Filename.check_suffix name ".ml"
       then Impl name
       else
         if Filename.check_suffix name objext
         then ModuleImpl name
         else
           if Filename.check_suffix name libext
           then ModuleImpl name
           else raise (FArg.Bad ("don't know what to do with " ^ name)))