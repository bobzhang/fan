let add_quotation = Ast_quotation.add_quotation
let loc_of = Ast_gen.loc_of
let stringnize =
  [("nativeint'",
     (Some (`Dot ((`Uid "Nativeint"), (`Lid "to_string")) :>Astfn.exp)));
  ("int'", (Some (`Lid "string_of_int" :>Astfn.exp)));
  ("int32'", (Some (`Dot ((`Uid "Int32"), (`Lid "to_string")) :>Astfn.exp)));
  ("int64'", (Some (`Dot ((`Uid "Int64"), (`Lid "to_string")) :>Astfn.exp)));
  ("chr'", (Some (`Dot ((`Uid "Char"), (`Lid "escaped")) :>Astfn.exp)));
  ("str'", (Some (`Dot ((`Uid "String"), (`Lid "escaped")) :>Astfn.exp)));
  ("flo'", (Some (`Lid "string_of_float" :>Astfn.exp)));
  ("bool'", None)]
let expander ant_annot =
  object (_this__007_ : 'this_type__008_)
    inherit  Astf_map.map as super
    method! pat (x : Astf.pat) =
      match x with
      | `Ant (_loc,x) ->
          let meta_loc_pat _loc _ = (`Any _loc :>Astf.pat) in
          let mloc _loc = meta_loc_pat _loc _loc in
          let e = Tokenf.ant_expand Parsef.pat x in
          (match ((x.kind), (x.cxt)) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_)
             |(("vrn" as x),Some ("exp"|"pat")) ->
               let x = String.capitalize x in
               (`App
                  (_loc, (`Vrn (_loc, x)),
                    (`Par
                       (_loc,
                         (`Com (_loc, (mloc _loc :>Astf.pat), (e :>Astf.pat)))))) :>
                 Astf.pat)
           | _ -> super#pat e)
      | e -> super#pat e
    method! exp (x : Astf.exp) =
      match x with
      | `Ant (_loc,x) ->
          let meta_loc_exp _loc loc =
            match !Ast_quotation.current_loc_name with
            | Some "here" ->
                ((Ast_gen.meta_here _loc loc :>Astf.exp) :>Astf.exp)
            | x ->
                let x = Option.default (!Locf.name) x in
                (`Lid (_loc, x) :>Astf.exp) in
          let mloc _loc = meta_loc_exp _loc _loc in
          let e = Tokenf.ant_expand Parsef.exp x in
          (match ((x.kind), (x.cxt)) with
           | (("uid"|"lid"|"flo"|"int"|"int32"|"int64"|"nativeint"|"chr"
               |"str"|"par"|"seq" as x),_) ->
               (`App
                  (_loc, (`Vrn (_loc, (String.capitalize x))),
                    (`Par
                       (_loc,
                         (`Com (_loc, (mloc _loc :>Astf.exp), (e :>Astf.exp)))))) :>
               Astf.exp)
           | (("vrn" as x),Some ("exp"|"pat"|"ep")) ->
               (`App
                  (_loc, (`Vrn (_loc, (String.capitalize x))),
                    (`Par
                       (_loc,
                         (`Com (_loc, (mloc _loc :>Astf.exp), (e :>Astf.exp)))))) :>
               Astf.exp)
           | (("nativeint'"|"int'"|"int32'"|"int64'"|"chr'"|"str'"|"flo'"
               |"bool'" as x),_) ->
               let v =
                 match List.assoc x stringnize with
                 | Some x ->
                     let x = Fill.exp _loc x in
                     (`App (_loc, (x :>Astf.exp), (e :>Astf.exp)) :>Astf.exp)
                 | None  -> e in
               let s =
                 (String.sub x 0 ((String.length x) - 1)) |>
                   String.capitalize in
               (`App
                  (_loc, (`Vrn (_loc, s)),
                    (`Par
                       (_loc,
                         (`Com (_loc, (mloc _loc :>Astf.exp), (v :>Astf.exp)))))) :>
                 Astf.exp)
           | (_,ty) ->
               let e =
                 match (ty, ant_annot) with
                 | (Some ty,true ) ->
                     (`Subtype
                        (_loc, (e :>Astf.exp),
                          (`Dot
                             (_loc, (`Uid (_loc, "Astf")), (`Lid (_loc, ty))))) :>
                     Astf.exp)
                 | _ -> e in
               super#exp e)
      | e -> super#exp e
  end
let expandern ant_annot =
  object (_this__005_ : 'this_type__006_)
    inherit  Astf_map.map as super
    method! pat (x : Astf.pat) =
      match x with
      | `Ant (_loc,x) ->
          let e = Tokenf.ant_expand Parsef.pat x in
          (match ((x.kind), (x.cxt)) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_)
             |(("vrn" as x),Some ("exp"|"pat")) ->
               let x = String.capitalize x in
               (`App (_loc, (`Vrn (_loc, x)), (e :>Astf.pat)) :>Astf.pat)
           | _ -> super#pat e)
      | e -> super#pat e
    method! exp (x : Astf.exp) =
      match x with
      | `Ant (_loc,x) ->
          let e = Tokenf.ant_expand Parsef.exp x in
          (match ((x.kind), (x.cxt)) with
           | (("uid"|"lid"|"par"|"seq"|"flo"|"int"|"int32"|"int64"
               |"nativeint"|"chr"|"str" as x),_)
             |(("vrn" as x),Some ("exp"|"pat"|"ep")) ->
               (`App
                  (_loc, (`Vrn (_loc, (String.capitalize x))),
                    (e :>Astf.exp)) :>Astf.exp)
           | (("nativeint'"|"int'"|"int32'"|"int64'"|"chr'"|"str'"|"flo'"
               |"bool'" as x),_) ->
               let v =
                 match List.assoc x stringnize with
                 | Some x ->
                     let x = Fill.exp _loc x in
                     (`App (_loc, (x :>Astf.exp), (e :>Astf.exp)) :>Astf.exp)
                 | None  -> e in
               let s =
                 (String.sub x 0 ((String.length x) - 1)) |>
                   String.capitalize in
               (`App (_loc, (`Vrn (_loc, s)), (v :>Astf.exp)) :>Astf.exp)
           | (_,ty) ->
               let e =
                 match (ty, ant_annot) with
                 | (Some ty,true ) ->
                     (`Subtype
                        (_loc, (e :>Astf.exp),
                          (`Dot
                             (_loc, (`Uid (_loc, "Astfn")),
                               (`Lid (_loc, ty))))) :>Astf.exp)
                 | _ -> e in
               super#exp e)
      | e -> super#exp e
  end
open Syntaxf
open Astf
let v = expander false
let u = expander true
let exp_filter (x : ep) = v#exp (x :>exp)
let pat_filter (x : ep) = v#pat (x :>pat)
let efilter str (e : ep) =
  let e = u#exp (e :>exp) in
  let _loc = loc_of e in
  (`Subtype
     (_loc, (e :>Astf.exp),
       (`Dot (_loc, (`Uid (_loc, "Astf")), (`Lid (_loc, str))))) :>Astf.exp)
let pfilter str (e : ep) =
  let p = u#pat (e :>pat) in
  let _loc = loc_of p in
  (`Constraint
     (_loc, (p :>Astf.pat),
       (`Dot (_loc, (`Uid (_loc, "Astf")), (`Lid (_loc, str))))) :>Astf.pat)
let domain = `Absolute ["Fan"; "Lang"; "Meta"]
let me =
  object (_this__003_ : 'this_type__004_)
    inherit  Metaf.meta
    method! loc _loc loc =
      match !Ast_quotation.current_loc_name with
      | None  -> `Lid (_loc, (!Locf.name))
      | Some "here" -> Ast_gen.meta_here _loc loc
      | Some x -> `Lid (_loc, x)
  end
let mp =
  object (_this__001_ : 'this_type__002_)
    inherit  Metaf.meta
    method! loc _loc _ = `Any _loc
  end
let m = new Metafn.meta
let _ =
  add_quotation { domain; name = "sigi'" } sigi_quot ~mexp:(me#sigi)
    ~mpat:(mp#sigi) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "stru'" } stru_quot ~mexp:(me#stru)
    ~mpat:(mp#stru) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "decl'" } decl_quot ~mexp:(me#decl)
    ~mpat:(mp#decl) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "ctyp'" } ctyp_quot ~mexp:(me#ctyp)
    ~mpat:(mp#ctyp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "pat'" } pat_quot ~mexp:(me#pat)
    ~mpat:(mp#pat) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "exp'" } exp_quot ~mexp:(me#exp)
    ~mpat:(mp#exp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "ep'" } ep ~mexp:(me#ep) ~mpat:(mp#ep)
    ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mtyp'" } mtyp_quot ~mexp:(me#mtyp)
    ~mpat:(mp#mtyp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mexp'" } mexp_quot ~mexp:(me#mexp)
    ~mpat:(mp#mexp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "cltyp'" } cltyp_quot ~mexp:(me#cltyp)
    ~mpat:(mp#cltyp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "clexp'" } clexp_quot ~mexp:(me#clexp)
    ~mpat:(mp#clexp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "clsigi'" } clsigi_quot ~mexp:(me#clsigi)
    ~mpat:(mp#clsigi) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "clfield'" } clfield_quot ~mexp:(me#clfield)
    ~mpat:(mp#clfield) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "constr'" } constr_quot ~mexp:(me#constr)
    ~mpat:(mp#constr) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "bind'" } bind_quot ~mexp:(me#bind)
    ~mpat:(mp#bind) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "rec_exp'" } rec_exp_quot ~mexp:(me#rec_exp)
    ~mpat:(mp#rec_exp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "case'" } case_quot ~mexp:(me#case)
    ~mpat:(mp#case) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mbind'" } mbind_quot ~mexp:(me#mbind)
    ~mpat:(mp#mbind) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "ident'" } ident_quot ~mexp:(me#ident)
    ~mpat:(mp#ident) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "rec_flag'" } rec_flag_quot ~mexp:(me#flag)
    ~mpat:(mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "private_flag'" } private_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "row_var_flag'" } row_var_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mutable_flag'" } mutable_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "virtual_flag'" } virtual_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "override_flag'" } override_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "direction_flag'" } direction_flag_quot
    ~mexp:(me#flag) ~mpat:(mp#flag) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "or_ctyp'" } constructor_declarations
    ~mexp:(me#or_ctyp) ~mpat:(me#or_ctyp) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "row_field'" } row_field
    ~mexp:(me#row_field) ~mpat:(mp#row_field) ~exp_filter ~pat_filter
let _ =
  add_quotation { domain; name = "sigi" } sigi_quot ~mexp:(me#sigi)
    ~mpat:(mp#sigi) ~exp_filter:(efilter "sigi") ~pat_filter:(pfilter "sigi");
  add_quotation { domain; name = "stru" } stru_quot ~mexp:(me#stru)
    ~mpat:(mp#stru) ~exp_filter:(efilter "stru") ~pat_filter:(pfilter "stru");
  add_quotation { domain; name = "decl" } decl_quot ~mexp:(me#decl)
    ~mpat:(mp#decl) ~exp_filter:(efilter "decl") ~pat_filter:(pfilter "decl");
  add_quotation { domain; name = "ctyp" } ctyp_quot ~mexp:(me#ctyp)
    ~mpat:(mp#ctyp) ~exp_filter:(efilter "ctyp") ~pat_filter:(pfilter "ctyp");
  add_quotation { domain; name = "pat" } pat_quot ~mexp:(me#pat)
    ~mpat:(mp#pat) ~exp_filter:(efilter "pat") ~pat_filter:(pfilter "pat");
  add_quotation { domain; name = "ep" } ep ~mexp:(me#ep) ~mpat:(mp#ep)
    ~exp_filter:(efilter "ep") ~pat_filter:(pfilter "ep");
  add_quotation { domain; name = "exp" } exp_quot ~mexp:(me#exp)
    ~mpat:(mp#exp) ~exp_filter:(efilter "exp") ~pat_filter:(pfilter "exp");
  add_quotation { domain; name = "mtyp" } mtyp_quot ~mexp:(me#mtyp)
    ~mpat:(mp#mtyp) ~exp_filter:(efilter "mtyp") ~pat_filter:(pfilter "mtyp");
  add_quotation { domain; name = "mexp" } mexp_quot ~mexp:(me#mexp)
    ~mpat:(mp#mexp) ~exp_filter:(efilter "mexp") ~pat_filter:(pfilter "mexp");
  add_quotation { domain; name = "cltyp" } cltyp_quot ~mexp:(me#cltyp)
    ~mpat:(mp#cltyp) ~exp_filter:(efilter "cltyp")
    ~pat_filter:(pfilter "cltyp");
  add_quotation { domain; name = "clexp" } clexp_quot ~mexp:(me#clexp)
    ~mpat:(mp#clexp) ~exp_filter:(efilter "clexp")
    ~pat_filter:(pfilter "clexp");
  add_quotation { domain; name = "clsigi" } clsigi_quot ~mexp:(me#clsigi)
    ~mpat:(mp#clsigi) ~exp_filter:(efilter "clsigi")
    ~pat_filter:(pfilter "clsigi");
  add_quotation { domain; name = "clfield" } clfield_quot ~mexp:(me#clfield)
    ~mpat:(mp#clfield) ~exp_filter:(efilter "clfield")
    ~pat_filter:(pfilter "clfield");
  add_quotation { domain; name = "constr" } constr_quot ~mexp:(me#constr)
    ~mpat:(mp#constr) ~exp_filter:(efilter "constr")
    ~pat_filter:(pfilter "constr");
  add_quotation { domain; name = "bind" } bind_quot ~mexp:(me#bind)
    ~mpat:(mp#bind) ~exp_filter:(efilter "bind") ~pat_filter:(pfilter "bind");
  add_quotation { domain; name = "rec_exp" } rec_exp_quot ~mexp:(me#rec_exp)
    ~mpat:(mp#rec_exp) ~exp_filter:(efilter "rec_exp")
    ~pat_filter:(pfilter "rec_exp");
  add_quotation { domain; name = "case" } case_quot ~mexp:(me#case)
    ~mpat:(mp#case) ~exp_filter:(efilter "case") ~pat_filter:(pfilter "case");
  add_quotation { domain; name = "mbind" } mbind_quot ~mexp:(me#mbind)
    ~mpat:(mp#mbind) ~exp_filter:(efilter "mbind")
    ~pat_filter:(pfilter "mbind");
  add_quotation { domain; name = "ident" } ident_quot ~mexp:(me#ident)
    ~mpat:(mp#ident) ~exp_filter:(efilter "ident")
    ~pat_filter:(pfilter "ident");
  add_quotation { domain; name = "or_ctyp" } constructor_declarations
    ~mexp:(me#or_ctyp) ~mpat:(me#or_ctyp) ~exp_filter:(efilter "or_ctyp")
    ~pat_filter:(pfilter "or_ctyp");
  add_quotation { domain; name = "row_field" } row_field ~mexp:(me#row_field)
    ~mpat:(mp#row_field) ~exp_filter:(efilter "row_field")
    ~pat_filter:(pfilter "row_field")
let v = expandern false
let u = expandern true
let exp_filter_n (x : ep) = v#exp (x :>exp)
let pat_filter_n (x : ep) = v#pat (x :>pat)
let efilter str (e : ep) =
  let e = u#exp (e :>exp) in
  let _loc = loc_of e in
  (`Subtype
     (_loc, (e :>Astf.exp),
       (`Dot (_loc, (`Uid (_loc, "Astfn")), (`Lid (_loc, str))))) :>Astf.exp)
let pfilter str (e : ep) =
  let p = u#pat (e :>pat) in
  let _loc = loc_of p in
  (`Constraint
     (_loc, (p :>Astf.pat),
       (`Dot (_loc, (`Uid (_loc, "Astfn")), (`Lid (_loc, str))))) :>Astf.pat)
let _ =
  add_quotation { domain; name = "sigi-" } sigi_quot
    ~mexp:(fun loc  p  -> m#sigi loc (Strip.sigi p))
    ~mpat:(fun loc  p  -> m#sigi loc (Strip.sigi p))
    ~exp_filter:(efilter "sigi") ~pat_filter:(pfilter "sigi");
  add_quotation { domain; name = "stru-" } stru_quot
    ~mexp:(fun loc  p  -> m#stru loc (Strip.stru p))
    ~mpat:(fun loc  p  -> m#stru loc (Strip.stru p))
    ~exp_filter:(efilter "stru") ~pat_filter:(pfilter "stru");
  add_quotation { domain; name = "decl-" } decl_quot
    ~mexp:(fun loc  p  -> m#decl loc (Strip.decl p))
    ~mpat:(fun loc  p  -> m#decl loc (Strip.decl p))
    ~exp_filter:(efilter "decl") ~pat_filter:(pfilter "decl");
  add_quotation { domain; name = "ctyp-" } ctyp_quot
    ~mexp:(fun loc  p  -> m#ctyp loc (Strip.ctyp p))
    ~mpat:(fun loc  p  -> m#ctyp loc (Strip.ctyp p))
    ~exp_filter:(efilter "ctyp") ~pat_filter:(pfilter "ctyp");
  add_quotation { domain; name = "pat-" } pat_quot
    ~mexp:(fun loc  p  -> m#pat loc (Strip.pat p))
    ~mpat:(fun loc  p  -> m#pat loc (Strip.pat p))
    ~exp_filter:(efilter "pat") ~pat_filter:(pfilter "pat");
  add_quotation { domain; name = "ep-" } ep
    ~mexp:(fun loc  p  -> m#ep loc (Strip.ep p))
    ~mpat:(fun loc  p  -> m#ep loc (Strip.ep p)) ~exp_filter:(efilter "ep")
    ~pat_filter:(pfilter "ep");
  add_quotation { domain; name = "exp-" } exp_quot
    ~mexp:(fun loc  p  -> m#exp loc (Strip.exp p))
    ~mpat:(fun loc  p  -> m#exp loc (Strip.exp p))
    ~exp_filter:(efilter "exp") ~pat_filter:(pfilter "exp");
  add_quotation { domain; name = "mtyp-" } mtyp_quot
    ~mexp:(fun loc  p  -> m#mtyp loc (Strip.mtyp p))
    ~mpat:(fun loc  p  -> m#mtyp loc (Strip.mtyp p))
    ~exp_filter:(efilter "mtyp") ~pat_filter:(pfilter "mtyp");
  add_quotation { domain; name = "mexp-" } mexp_quot
    ~mexp:(fun loc  p  -> m#mexp loc (Strip.mexp p))
    ~mpat:(fun loc  p  -> m#mexp loc (Strip.mexp p))
    ~exp_filter:(efilter "mexp") ~pat_filter:(pfilter "mexp");
  add_quotation { domain; name = "cltyp-" } cltyp_quot
    ~mexp:(fun loc  p  -> m#cltyp loc (Strip.cltyp p))
    ~mpat:(fun loc  p  -> m#cltyp loc (Strip.cltyp p))
    ~exp_filter:(efilter "cltyp") ~pat_filter:(pfilter "cltyp");
  add_quotation { domain; name = "clexp-" } clexp_quot
    ~mexp:(fun loc  p  -> m#clexp loc (Strip.clexp p))
    ~mpat:(fun loc  p  -> m#clexp loc (Strip.clexp p))
    ~exp_filter:(efilter "clexp") ~pat_filter:(pfilter "clexp");
  add_quotation { domain; name = "clsigi-" } clsigi_quot
    ~mexp:(fun loc  p  -> m#clsigi loc (Strip.clsigi p))
    ~mpat:(fun loc  p  -> m#clsigi loc (Strip.clsigi p))
    ~exp_filter:(efilter "clsigi") ~pat_filter:(pfilter "clsigi");
  add_quotation { domain; name = "clfield-" } clfield_quot
    ~mexp:(fun loc  p  -> m#clfield loc (Strip.clfield p))
    ~mpat:(fun loc  p  -> m#clfield loc (Strip.clfield p))
    ~exp_filter:(efilter "clfield") ~pat_filter:(pfilter "clfield");
  add_quotation { domain; name = "constr-" } constr_quot
    ~mexp:(fun loc  p  -> m#constr loc (Strip.constr p))
    ~mpat:(fun loc  p  -> m#constr loc (Strip.constr p))
    ~exp_filter:(efilter "constr") ~pat_filter:(pfilter "constr");
  add_quotation { domain; name = "bind-" } bind_quot
    ~mexp:(fun loc  p  -> m#bind loc (Strip.bind p))
    ~mpat:(fun loc  p  -> m#bind loc (Strip.bind p))
    ~exp_filter:(efilter "bind") ~pat_filter:(pfilter "bind");
  add_quotation { domain; name = "rec_exp-" } rec_exp_quot
    ~mexp:(fun loc  p  -> m#rec_exp loc (Strip.rec_exp p))
    ~mpat:(fun loc  p  -> m#rec_exp loc (Strip.rec_exp p))
    ~exp_filter:(efilter "rec_exp") ~pat_filter:(pfilter "rec_exp");
  add_quotation { domain; name = "case-" } case_quot
    ~mexp:(fun loc  p  -> m#case loc (Strip.case p))
    ~mpat:(fun loc  p  -> m#case loc (Strip.case p))
    ~exp_filter:(efilter "case") ~pat_filter:(pfilter "case");
  add_quotation { domain; name = "mbind-" } mbind_quot
    ~mexp:(fun loc  p  -> m#mbind loc (Strip.mbind p))
    ~mpat:(fun loc  p  -> m#mbind loc (Strip.mbind p))
    ~exp_filter:(efilter "mbind") ~pat_filter:(pfilter "mbind");
  add_quotation { domain; name = "ident-" } ident_quot
    ~mexp:(fun loc  p  -> m#ident loc (Strip.ident p))
    ~mpat:(fun loc  p  -> m#ident loc (Strip.ident p))
    ~exp_filter:(efilter "ident") ~pat_filter:(pfilter "ident");
  add_quotation { domain; name = "or_ctyp-" } constructor_declarations
    ~mexp:(fun loc  p  -> m#or_ctyp loc (Strip.or_ctyp p))
    ~mpat:(fun loc  p  -> m#or_ctyp loc (Strip.or_ctyp p))
    ~exp_filter:(efilter "or_ctyp") ~pat_filter:(pfilter "or_ctyp");
  add_quotation { domain; name = "row_field-" } row_field
    ~mexp:(fun loc  p  -> m#row_field loc (Strip.row_field p))
    ~mpat:(fun loc  p  -> m#row_field loc (Strip.row_field p))
    ~exp_filter:(efilter "row_field") ~pat_filter:(pfilter "row_field")
let _ =
  let exp_filter = exp_filter_n in
  let pat_filter = pat_filter_n in
  add_quotation { domain; name = "sigi-'" } sigi_quot
    ~mexp:(fun loc  p  -> m#sigi loc (Strip.sigi p))
    ~mpat:(fun loc  p  -> m#sigi loc (Strip.sigi p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "stru-'" } stru_quot
    ~mexp:(fun loc  p  -> m#stru loc (Strip.stru p))
    ~mpat:(fun loc  p  -> m#stru loc (Strip.stru p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "decl-'" } decl_quot
    ~mexp:(fun loc  p  -> m#decl loc (Strip.decl p))
    ~mpat:(fun loc  p  -> m#decl loc (Strip.decl p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "ctyp-'" } ctyp_quot
    ~mexp:(fun loc  p  -> m#ctyp loc (Strip.ctyp p))
    ~mpat:(fun loc  p  -> m#ctyp loc (Strip.ctyp p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "pat-'" } pat_quot
    ~mexp:(fun loc  p  -> m#pat loc (Strip.pat p))
    ~mpat:(fun loc  p  -> m#pat loc (Strip.pat p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "ep-'" } ep
    ~mexp:(fun loc  p  -> m#ep loc (Strip.ep p))
    ~mpat:(fun loc  p  -> m#ep loc (Strip.ep p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "exp-'" } exp_quot
    ~mexp:(fun loc  p  -> m#exp loc (Strip.exp p))
    ~mpat:(fun loc  p  -> m#exp loc (Strip.exp p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mtyp-'" } mtyp_quot
    ~mexp:(fun loc  p  -> m#mtyp loc (Strip.mtyp p))
    ~mpat:(fun loc  p  -> m#mtyp loc (Strip.mtyp p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mexp-'" } mexp_quot
    ~mexp:(fun loc  p  -> m#mexp loc (Strip.mexp p))
    ~mpat:(fun loc  p  -> m#mexp loc (Strip.mexp p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "cltyp-'" } cltyp_quot
    ~mexp:(fun loc  p  -> m#cltyp loc (Strip.cltyp p))
    ~mpat:(fun loc  p  -> m#cltyp loc (Strip.cltyp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "clexp-'" } clexp_quot
    ~mexp:(fun loc  p  -> m#clexp loc (Strip.clexp p))
    ~mpat:(fun loc  p  -> m#clexp loc (Strip.clexp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "clsigi-'" } clsigi_quot
    ~mexp:(fun loc  p  -> m#clsigi loc (Strip.clsigi p))
    ~mpat:(fun loc  p  -> m#clsigi loc (Strip.clsigi p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "clfield-'" } clfield_quot
    ~mexp:(fun loc  p  -> m#clfield loc (Strip.clfield p))
    ~mpat:(fun loc  p  -> m#clfield loc (Strip.clfield p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "constr-'" } constr_quot
    ~mexp:(fun loc  p  -> m#constr loc (Strip.constr p))
    ~mpat:(fun loc  p  -> m#constr loc (Strip.constr p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "bind-'" } bind_quot
    ~mexp:(fun loc  p  -> m#bind loc (Strip.bind p))
    ~mpat:(fun loc  p  -> m#bind loc (Strip.bind p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "rec_exp-'" } rec_exp_quot
    ~mexp:(fun loc  p  -> m#rec_exp loc (Strip.rec_exp p))
    ~mpat:(fun loc  p  -> m#rec_exp loc (Strip.rec_exp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "case-'" } case_quot
    ~mexp:(fun loc  p  -> m#case loc (Strip.case p))
    ~mpat:(fun loc  p  -> m#case loc (Strip.case p)) ~exp_filter ~pat_filter;
  add_quotation { domain; name = "mbind-'" } mbind_quot
    ~mexp:(fun loc  p  -> m#mbind loc (Strip.mbind p))
    ~mpat:(fun loc  p  -> m#mbind loc (Strip.mbind p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "ident-'" } ident_quot
    ~mexp:(fun loc  p  -> m#ident loc (Strip.ident p))
    ~mpat:(fun loc  p  -> m#ident loc (Strip.ident p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "or_ctyp-'" } constructor_declarations
    ~mexp:(fun loc  p  -> m#or_ctyp loc (Strip.or_ctyp p))
    ~mpat:(fun loc  p  -> m#or_ctyp loc (Strip.or_ctyp p)) ~exp_filter
    ~pat_filter;
  add_quotation { domain; name = "row_field-'" } row_field
    ~mexp:(fun loc  p  -> m#row_field loc (Strip.row_field p))
    ~mpat:(fun loc  p  -> m#row_field loc (Strip.row_field p)) ~exp_filter
    ~pat_filter
