open Parsetree
open Longident
open Asttypes
open Lib
open FanUtil
open FanAst
open ParsetreeHelper
let mkvirtual =
  function | `ViVirtual -> Virtual | `ViNil -> Concrete | _ -> assert false
let mkdirection =
  function | `DiTo -> Upto | `DiDownto -> Downto | _ -> assert false
let mkrf =
  function
  | `ReRecursive -> Recursive
  | `ReNil -> Nonrecursive
  | _ -> assert false
let ident_tag i =
  let rec self i acc =
    match i with
    | `IdAcc (_loc,`Lid (_,"*predef*"),`Lid (_,"option")) ->
        Some ((ldot (lident "*predef*") "option"), `lident)
    | `IdAcc (_loc,i1,i2) -> self i2 (self i1 acc)
    | `IdApp (_loc,i1,i2) ->
        (match ((self i1 None), (self i2 None), acc) with
         | (Some (l,_),Some (r,_),None ) -> Some ((Lapply (l, r)), `app)
         | _ -> error (FanAst.loc_of_ident i) "invalid long identifer")
    | `Uid (_loc,s) ->
        (match (acc, s) with
         | (None ,"") -> None
         | (None ,s) -> Some ((lident s), `uident)
         | (Some (_,(`uident|`app)),"") -> acc
         | (Some (x,(`uident|`app)),s) -> Some ((ldot x s), `uident)
         | _ -> error (FanAst.loc_of_ident i) "invalid long identifier")
    | `Lid (_loc,s) ->
        let x =
          match acc with
          | None  -> lident s
          | Some (acc,(`uident|`app)) -> ldot acc s
          | _ -> error (loc_of_ident i) "invalid long identifier" in
        Some (x, `lident)
    | _ -> error (loc_of_ident i) "invalid long identifier" in
  match self i None with
  | Some x -> x
  | None  -> error (loc_of_ident i) "invalid long identifier "
let ident_noloc i = fst (ident_tag i)
let ident i = with_loc (ident_noloc i) (loc_of_ident i)
let long_lident msg id =
  match ident_tag id with
  | (i,`lident) -> with_loc i (loc_of_ident id)
  | _ -> error (loc_of_ident id) msg
let long_type_ident = long_lident "invalid long identifier type"
let long_class_ident = long_lident "invalid class name"
let long_uident_noloc i =
  match ident_tag i with
  | (Ldot (i,s),`uident) -> ldot i s
  | (Lident s,`uident) -> lident s
  | (i,`app) -> i
  | _ -> error (loc_of_ident i) "uppercase identifier expected"
let long_uident i = with_loc (long_uident_noloc i) (loc_of_ident i)
let rec ctyp_long_id_prefix t =
  match t with
  | `TyId (_loc,i) -> ident_noloc i
  | `TyApp (_loc,m1,m2) ->
      let li1 = ctyp_long_id_prefix m1 in
      let li2 = ctyp_long_id_prefix m2 in Lapply (li1, li2)
  | t -> error (loc_of_ctyp t) "invalid module expression"
let ctyp_long_id t =
  match t with
  | `TyId (_loc,i) -> (false, (long_type_ident i))
  | `TyApp (loc,_,_) -> error loc "invalid type name"
  | `TyCls (_,i) -> (true, (ident i))
  | t -> error (loc_of_ctyp t) "invalid type"
let predef_option loc =
  `TyId
    (loc, (`IdAcc (loc, (`Lid (loc, "*predef*")), (`Lid (loc, "option")))))
let rec ctyp =
  function
  | `TyId (loc,i) ->
      let li = long_type_ident i in mktyp loc (Ptyp_constr (li, []))
  | `TyAli (loc,t1,t2) ->
      let (t,i) =
        match (t1, t2) with
        | (t,`TyQuo (_,s)) -> (t, s)
        | (`TyQuo (_,s),t) -> (t, s)
        | _ -> error loc "invalid alias type" in
      mktyp loc (Ptyp_alias ((ctyp t), i))
  | `TyAny loc -> mktyp loc Ptyp_any
  | `TyApp (loc,_,_) as f ->
      let (f,al) = Ctyp.view_app [] f in
      let (is_cls,li) = ctyp_long_id f in
      if is_cls
      then mktyp loc (Ptyp_class (li, (List.map ctyp al), []))
      else mktyp loc (Ptyp_constr (li, (List.map ctyp al)))
  | `TyArr (loc,`TyLab (_,lab,t1),t2) ->
      mktyp loc (Ptyp_arrow (lab, (ctyp t1), (ctyp t2)))
  | `TyArr (loc,`TyOlb (loc1,lab,t1),t2) ->
      let t1 = `TyApp (loc1, (predef_option loc1), t1) in
      mktyp loc (Ptyp_arrow (("?" ^ lab), (ctyp t1), (ctyp t2)))
  | `TyArr (loc,t1,t2) -> mktyp loc (Ptyp_arrow ("", (ctyp t1), (ctyp t2)))
  | `TyObj (loc,fl,`RvNil) -> mktyp loc (Ptyp_object (meth_list fl []))
  | `TyObj (loc,fl,`RvRowVar) ->
      mktyp loc (Ptyp_object (meth_list fl [mkfield loc Pfield_var]))
  | `TyCls (loc,id) -> mktyp loc (Ptyp_class ((ident id), [], []))
  | `TyPkg (loc,pt) ->
      let (i,cs) = package_type pt in mktyp loc (Ptyp_package (i, cs))
  | `TyPol (loc,t1,t2) ->
      mktyp loc (Ptyp_poly ((Ctyp.to_var_list t1), (ctyp t2)))
  | `TyQuo (loc,s) -> mktyp loc (Ptyp_var s)
  | `TyTup (loc,`TySta (_,t1,t2)) ->
      mktyp loc
        (Ptyp_tuple (List.map ctyp (list_of_ctyp t1 (list_of_ctyp t2 []))))
  | `TyVrnEq (loc,t) -> mktyp loc (Ptyp_variant ((row_field t), true, None))
  | `TyVrnSup (loc,t) ->
      mktyp loc (Ptyp_variant ((row_field t), false, None))
  | `TyVrnInf (loc,t) ->
      mktyp loc (Ptyp_variant ((row_field t), true, (Some [])))
  | `TyVrnInfSup (loc,t,t') ->
      mktyp loc
        (Ptyp_variant ((row_field t), true, (Some (Ctyp.name_tags t'))))
  | `TyLab (loc,_,_) -> error loc "labelled type not allowed here"
  | `TyMan (loc,_,_) -> error loc "manifest type not allowed here"
  | `TyOlb (loc,_,_) -> error loc "labelled type not allowed here"
  | `TyRec (loc,_) -> error loc "record type not allowed here"
  | `TySum (loc,_) -> error loc "sum type not allowed here"
  | `TyPrv (loc,_) -> error loc "private type not allowed here"
  | `TyMut (loc,_) -> error loc "mutable type not allowed here"
  | `TyOr (loc,_,_) -> error loc "type1 | type2 not allowed here"
  | `TyAnd (loc,_,_) -> error loc "type1 and type2 not allowed here"
  | `TyOf (loc,_,_) -> error loc "type1 of type2 not allowed here"
  | `TyCol (loc,_,_) -> error loc "type1 : type2 not allowed here"
  | `TySem (loc,_,_) -> error loc "type1 ; type2 not allowed here"
  | `Ant (loc,_) -> error loc "antiquotation not allowed here"
  | `TyOfAmp (_,_,_)|`TyAmp (_,_,_)|`TySta (_,_,_)|`TyCom (_,_,_)|`TyVrn
                                                                    (_,_)|
      `TyQuM (_,_)|`TyQuP (_,_)|`TyDcl (_,_,_,_,_)|`TyAnP _|`TyAnM _|
      `TyTypePol (_,_,_)|`TyObj (_,_,`Ant _)|`TyNil _|`TyTup (_,_) ->
      assert false
and row_field =
  function
  | `TyNil _loc -> []
  | `TyVrn (_loc,i) -> [Rtag (i, true, [])]
  | `TyOfAmp (_loc,`TyVrn (_,i),t) ->
      [Rtag (i, true, (List.map ctyp (list_of_ctyp t [])))]
  | `TyOf (_loc,`TyVrn (_,i),t) ->
      [Rtag (i, false, (List.map ctyp (list_of_ctyp t [])))]
  | `TyOr (_loc,t1,t2) -> (row_field t1) @ (row_field t2)
  | t -> [Rinherit (ctyp t)]
and meth_list fl acc =
  match fl with
  | `TyNil _loc -> acc
  | `TySem (_loc,t1,t2) -> meth_list t1 (meth_list t2 acc)
  | `TyCol (loc,`TyId (_,`Lid (_,lab)),t) ->
      (mkfield loc (Pfield (lab, (mkpolytype (ctyp t))))) :: acc
  | _ -> assert false
and package_type_constraints wc acc =
  match wc with
  | `WcNil _loc -> acc
  | `WcTyp (_loc,`TyId (_,id),ct) -> ((ident id), (ctyp ct)) :: acc
  | `WcAnd (_loc,wc1,wc2) ->
      package_type_constraints wc1 (package_type_constraints wc2 acc)
  | _ ->
      error (loc_of_with_constr wc)
        "unexpected `with constraint' for a package type"
and package_type: module_type -> package_type =
  function
  | `MtWit (_loc,`MtId (_,i),wc) ->
      ((long_uident i), (package_type_constraints wc []))
  | `MtId (_loc,i) -> ((long_uident i), [])
  | mt -> error (loc_of_module_type mt) "unexpected package type"
let mktype loc tl cl tk tp tm =
  let (params,variance) = List.split tl in
  {
    ptype_params = params;
    ptype_cstrs = cl;
    ptype_kind = tk;
    ptype_private = tp;
    ptype_manifest = tm;
    ptype_loc = loc;
    ptype_variance = variance
  }
let mkprivate' m = if m then Private else Public
let mkprivate =
  function | `PrPrivate -> Private | `PrNil -> Public | _ -> assert false
let mktrecord =
  function
  | `TyCol (loc,`TyId (_,`Lid (sloc,s)),`TyMut (_,t)) ->
      ((with_loc s sloc), Mutable, (mkpolytype (ctyp t)), loc)
  | `TyCol (loc,`TyId (_,`Lid (sloc,s)),t) ->
      ((with_loc s sloc), Immutable, (mkpolytype (ctyp t)), loc)
  | _ -> assert false
let mkvariant =
  function
  | `TyId (loc,`Uid (sloc,s)) -> ((with_loc s sloc), [], None, loc)
  | `TyOf (loc,`TyId (_,`Uid (sloc,s)),t) ->
      ((with_loc s sloc), (List.map ctyp (list_of_ctyp t [])), None, loc)
  | `TyCol (loc,`TyId (_,`Uid (sloc,s)),`TyArr (_,t,u)) ->
      ((with_loc s sloc), (List.map ctyp (list_of_ctyp t [])),
        (Some (ctyp u)), loc)
  | `TyCol (loc,`TyId (_,`Uid (sloc,s)),t) ->
      ((with_loc s sloc), [], (Some (ctyp t)), loc)
  | _ -> assert false
let rec type_decl tl cl loc m pflag =
  function
  | `TyMan (_loc,t1,t2) -> type_decl tl cl loc (Some (ctyp t1)) pflag t2
  | `TyPrv (_loc,t) ->
      if pflag
      then error _loc "multiple private keyword used, use only one instead"
      else type_decl tl cl loc m true t
  | `TyRec (_loc,t) ->
      mktype loc tl cl
        (Ptype_record (List.map mktrecord (list_of_ctyp t [])))
        (mkprivate' pflag) m
  | `TySum (_loc,t) ->
      mktype loc tl cl
        (Ptype_variant (List.map mkvariant (list_of_ctyp t [])))
        (mkprivate' pflag) m
  | t ->
      if m <> None
      then error loc "only one manifest type allowed by definition"
      else
        (let m = match t with | `TyNil _loc -> None | _ -> Some (ctyp t) in
         mktype loc tl cl Ptype_abstract (mkprivate' pflag) m)
let type_decl tl cl t loc = type_decl tl cl loc None false t
let mkvalue_desc loc t p =
  { pval_type = (ctyp t); pval_prim = p; pval_loc = loc }
let rec list_of_meta_list =
  function
  | `LNil -> []
  | `LCons (x,xs) -> x :: (list_of_meta_list xs)
  | `Ant _ -> assert false
let mkmutable =
  function | `MuMutable -> Mutable | `MuNil -> Immutable | _ -> assert false
let paolab lab p =
  match (lab, p) with
  | ("",(`PaId (_loc,`Lid (_,i))|`PaTyc (_loc,`PaId (_,`Lid (_,i)),_))) -> i
  | ("",p) -> error (loc_of_patt p) "bad ast in label"
  | _ -> lab
let opt_private_ctyp =
  function
  | `TyPrv (_loc,t) -> (Ptype_abstract, Private, (ctyp t))
  | t -> (Ptype_abstract, Public, (ctyp t))
let rec type_parameters t acc =
  match t with
  | `TyApp (_loc,t1,t2) -> type_parameters t1 (type_parameters t2 acc)
  | `TyQuP (_loc,s) -> (s, (true, false)) :: acc
  | `TyQuM (_loc,s) -> (s, (false, true)) :: acc
  | `TyQuo (_loc,s) -> (s, (false, false)) :: acc
  | _ -> assert false
let rec optional_type_parameters t acc =
  match t with
  | `TyApp (_loc,t1,t2) ->
      optional_type_parameters t1 (optional_type_parameters t2 acc)
  | `TyQuP (loc,s) -> ((Some (with_loc s loc)), (true, false)) :: acc
  | `TyAnP _loc -> (None, (true, false)) :: acc
  | `TyQuM (loc,s) -> ((Some (with_loc s loc)), (false, true)) :: acc
  | `TyAnM _loc -> (None, (false, true)) :: acc
  | `TyQuo (loc,s) -> ((Some (with_loc s loc)), (false, false)) :: acc
  | `TyAny _loc -> (None, (false, false)) :: acc
  | _ -> assert false
let rec class_parameters t acc =
  match t with
  | `TyCom (_loc,t1,t2) -> class_parameters t1 (class_parameters t2 acc)
  | `TyQuP (loc,s) -> ((with_loc s loc), (true, false)) :: acc
  | `TyQuM (loc,s) -> ((with_loc s loc), (false, true)) :: acc
  | `TyQuo (loc,s) -> ((with_loc s loc), (false, false)) :: acc
  | _ -> assert false
let rec type_parameters_and_type_name t acc =
  match t with
  | `TyApp (_loc,t1,t2) ->
      type_parameters_and_type_name t1 (optional_type_parameters t2 acc)
  | `TyId (_loc,i) -> ((ident i), acc)
  | _ -> assert false
let mkwithtyp pwith_type loc id_tpl ct =
  let (id,tpl) = type_parameters_and_type_name id_tpl [] in
  let (params,variance) = List.split tpl in
  let (kind,priv,ct) = opt_private_ctyp ct in
  (id,
    (pwith_type
       {
         ptype_params = params;
         ptype_cstrs = [];
         ptype_kind = kind;
         ptype_private = priv;
         ptype_manifest = (Some ct);
         ptype_loc = loc;
         ptype_variance = variance
       }))
let rec mkwithc wc acc =
  match wc with
  | `WcNil _loc -> acc
  | `WcTyp (loc,id_tpl,ct) ->
      (mkwithtyp (fun x  -> Pwith_type x) loc id_tpl ct) :: acc
  | `WcMod (_loc,i1,i2) ->
      ((long_uident i1), (Pwith_module (long_uident i2))) :: acc
  | `WcTyS (loc,id_tpl,ct) ->
      (mkwithtyp (fun x  -> Pwith_typesubst x) loc id_tpl ct) :: acc
  | `WcMoS (_loc,i1,i2) ->
      ((long_uident i1), (Pwith_modsubst (long_uident i2))) :: acc
  | `WcAnd (_loc,wc1,wc2) -> mkwithc wc1 (mkwithc wc2 acc)
  | `Ant (loc,_) -> error loc "bad with constraint (antiquotation)"
let rec patt_fa al =
  function | `PaApp (_,f,a) -> patt_fa (a :: al) f | f -> (f, al)
let rec deep_mkrangepat loc c1 c2 =
  if c1 = c2
  then mkghpat loc (Ppat_constant (Const_char c1))
  else
    mkghpat loc
      (Ppat_or
         ((mkghpat loc (Ppat_constant (Const_char c1))),
           (deep_mkrangepat loc (Char.chr ((Char.code c1) + 1)) c2)))
let rec mkrangepat loc c1 c2 =
  if c1 > c2
  then mkrangepat loc c2 c1
  else
    if c1 = c2
    then mkpat loc (Ppat_constant (Const_char c1))
    else
      mkpat loc
        (Ppat_or
           ((mkghpat loc (Ppat_constant (Const_char c1))),
             (deep_mkrangepat loc (Char.chr ((Char.code c1) + 1)) c2)))
let rec patt =
  function
  | `PaId (loc,`Lid (_,("true"|"false" as txt))) ->
      let p = Ppat_construct ({ txt = (Lident txt); loc }, None, false) in
      mkpat loc p
  | `PaId (loc,`Lid (sloc,s)) -> mkpat loc (Ppat_var (with_loc s sloc))
  | `PaId (loc,i) ->
      let p = Ppat_construct ((long_uident i), None, false) in mkpat loc p
  | `PaAli (loc,p1,p2) ->
      let (p,i) =
        match (p1, p2) with
        | (p,`PaId (_loc,`Lid (sloc,s))) -> (p, (with_loc s sloc))
        | (`PaId (_loc,`Lid (sloc,s)),p) -> (p, (with_loc s sloc))
        | _ -> error loc "invalid alias pattern" in
      mkpat loc (Ppat_alias ((patt p), i))
  | `Ant (loc,_) -> error loc "antiquotation not allowed here"
  | `PaAny loc -> mkpat loc Ppat_any
  | `PaApp (loc,`PaId (_,`Uid (sloc,s)),`PaTup (_,`PaAny loc_any)) ->
      mkpat loc
        (Ppat_construct
           ((lident_with_loc s sloc), (Some (mkpat loc_any Ppat_any)), false))
  | `PaApp (loc,_,_) as f ->
      let (f,al) = patt_fa [] f in
      let al = List.map patt al in
      (match (patt f).ppat_desc with
       | Ppat_construct (li,None ,_) ->
           let a =
             match al with | a::[] -> a | _ -> mkpat loc (Ppat_tuple al) in
           mkpat loc (Ppat_construct (li, (Some a), false))
       | Ppat_variant (s,None ) ->
           let a =
             match al with | a::[] -> a | _ -> mkpat loc (Ppat_tuple al) in
           mkpat loc (Ppat_variant (s, (Some a)))
       | _ ->
           error (loc_of_patt f)
             "this is not a constructor, it cannot be applied in a pattern")
  | `PaArr (loc,p) ->
      mkpat loc (Ppat_array (List.map patt (list_of_patt p [])))
  | `PaChr (loc,s) ->
      mkpat loc (Ppat_constant (Const_char (char_of_char_token loc s)))
  | `PaInt (loc,s) ->
      let i =
        try int_of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int" in
      mkpat loc (Ppat_constant (Const_int i))
  | `PaInt32 (loc,s) ->
      let i32 =
        try Int32.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int32" in
      mkpat loc (Ppat_constant (Const_int32 i32))
  | `PaInt64 (loc,s) ->
      let i64 =
        try Int64.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int64" in
      mkpat loc (Ppat_constant (Const_int64 i64))
  | `PaNativeInt (loc,s) ->
      let nati =
        try Nativeint.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type nativeint" in
      mkpat loc (Ppat_constant (Const_nativeint nati))
  | `PaFlo (loc,s) ->
      mkpat loc (Ppat_constant (Const_float (remove_underscores s)))
  | `PaLab (loc,_,_) -> error loc "labeled pattern not allowed here"
  | `PaOlb (loc,_,_)|`PaOlbi (loc,_,_,_) ->
      error loc "labeled pattern not allowed here"
  | `PaOrp (loc,p1,p2) -> mkpat loc (Ppat_or ((patt p1), (patt p2)))
  | `PaRng (loc,p1,p2) ->
      (match (p1, p2) with
       | (`PaChr (loc1,c1),`PaChr (loc2,c2)) ->
           let c1 = char_of_char_token loc1 c1 in
           let c2 = char_of_char_token loc2 c2 in mkrangepat loc c1 c2
       | _ -> error loc "range pattern allowed only for characters")
  | `PaRec (loc,p) ->
      let ps = list_of_patt p [] in
      let is_wildcard = function | `PaAny _loc -> true | _ -> false in
      let (wildcards,ps) = List.partition is_wildcard ps in
      let is_closed = if wildcards = [] then Closed else Open in
      mkpat loc (Ppat_record ((List.map mklabpat ps), is_closed))
  | `PaStr (loc,s) ->
      mkpat loc (Ppat_constant (Const_string (string_of_string_token loc s)))
  | `PaTup (loc,`PaCom (_,p1,p2)) ->
      mkpat loc
        (Ppat_tuple (List.map patt (list_of_patt p1 (list_of_patt p2 []))))
  | `PaTup (loc,_) -> error loc "singleton tuple pattern"
  | `PaTyc (loc,p,t) -> mkpat loc (Ppat_constraint ((patt p), (ctyp t)))
  | `PaTyp (loc,i) -> mkpat loc (Ppat_type (long_type_ident i))
  | `PaVrn (loc,s) -> mkpat loc (Ppat_variant (s, None))
  | `PaLaz (loc,p) -> mkpat loc (Ppat_lazy (patt p))
  | `PaMod (loc,m) -> mkpat loc (Ppat_unpack (with_loc m loc))
  | `PaEq (_,_,_)|`PaSem (_,_,_)|`PaCom (_,_,_)|`PaNil _ as p ->
      error (loc_of_patt p) "invalid pattern"
and mklabpat =
  function
  | `PaEq (_loc,i,p) -> ((ident i), (patt p))
  | p -> error (loc_of_patt p) "invalid pattern"
let override_flag loc =
  function
  | `OvOverride _ -> Override
  | `OvNil _  -> Fresh
  | _ -> error loc "antiquotation not allowed here"
let rec expr =
  function
  | `ExAcc (_loc,_,_)|`ExId (_loc,`IdAcc (_,_,_)) as e ->
      let (e,l) =
        match Expr.sep_dot_expr [] e with
        | (loc,ml,`ExId (sloc,`Uid (_,s)))::l ->
            ((mkexp loc (Pexp_construct ((mkli sloc s ml), None, false))), l)
        | (loc,ml,`ExId (sloc,`Lid (_,s)))::l ->
            ((mkexp loc (Pexp_ident (mkli sloc s ml))), l)
        | (_,[],e)::l -> ((expr e), l)
        | _ -> error _loc "bad ast in expression" in
      let (_,e) =
        List.fold_left
          (fun (loc_bp,e1)  (loc_ep,ml,e2)  ->
             match e2 with
             | `ExId (sloc,`Lid (_,s)) ->
                 let loc = FanLoc.merge loc_bp loc_ep in
                 (loc, (mkexp loc (Pexp_field (e1, (mkli sloc s ml)))))
             | _ -> error (loc_of_expr e2) "lowercase identifier expected")
          (_loc, e) l in
      e
  | `Ant (loc,_) -> error loc "antiquotation not allowed here"
  | `ExApp (loc,_,_) as f ->
      let (f,al) = Expr.view_app [] f in
      let al = List.map label_expr al in
      (match (expr f).pexp_desc with
       | Pexp_construct (li,None ,_) ->
           let al = List.map snd al in
           let a =
             match al with | a::[] -> a | _ -> mkexp loc (Pexp_tuple al) in
           mkexp loc (Pexp_construct (li, (Some a), false))
       | Pexp_variant (s,None ) ->
           let al = List.map snd al in
           let a =
             match al with | a::[] -> a | _ -> mkexp loc (Pexp_tuple al) in
           mkexp loc (Pexp_variant (s, (Some a)))
       | _ -> mkexp loc (Pexp_apply ((expr f), al)))
  | `ExAre (loc,e1,e2) ->
      mkexp loc
        (Pexp_apply
           ((mkexp loc (Pexp_ident (array_function loc "Array" "get"))),
             [("", (expr e1)); ("", (expr e2))]))
  | `ExArr (loc,e) ->
      mkexp loc (Pexp_array (List.map expr (list_of_expr e [])))
  | `ExAsf loc -> mkexp loc Pexp_assertfalse
  | `ExAss (loc,e,v) ->
      let e =
        match e with
        | `ExAcc (loc,x,`ExId (_,`Lid (_,"contents"))) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (lident_with_loc ":=" loc))),
                [("", (expr x)); ("", (expr v))])
        | `ExAcc (loc,_,_) ->
            (match (expr e).pexp_desc with
             | Pexp_field (e,lab) -> Pexp_setfield (e, lab, (expr v))
             | _ -> error loc "bad record access")
        | `ExAre (loc,e1,e2) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (array_function loc "Array" "set"))),
                [("", (expr e1)); ("", (expr e2)); ("", (expr v))])
        | `ExId (lloc,`Lid (_,lab)) ->
            Pexp_setinstvar ((with_loc lab lloc), (expr v))
        | `ExSte (loc,e1,e2) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (array_function loc "String" "set"))),
                [("", (expr e1)); ("", (expr e2)); ("", (expr v))])
        | _ -> error loc "bad left part of assignment" in
      mkexp loc e
  | `ExAsr (loc,e) -> mkexp loc (Pexp_assert (expr e))
  | `ExChr (loc,s) ->
      mkexp loc (Pexp_constant (Const_char (char_of_char_token loc s)))
  | `ExCoe (loc,e,t1,t2) ->
      let t1 = match t1 with | `TyNil _loc -> None | t -> Some (ctyp t) in
      mkexp loc (Pexp_constraint ((expr e), t1, (Some (ctyp t2))))
  | `ExFlo (loc,s) ->
      mkexp loc (Pexp_constant (Const_float (remove_underscores s)))
  | `ExFor (loc,i,e1,e2,df,el) ->
      let e3 = `ExSeq (loc, el) in
      mkexp loc
        (Pexp_for
           ((with_loc i loc), (expr e1), (expr e2), (mkdirection df),
             (expr e3)))
  | `ExFun (loc,`McArr (_,`PaLab (_,lab,po),w,e)) ->
      mkexp loc
        (Pexp_function
           (lab, None, [((patt_of_lab loc lab po), (when_expr e w))]))
  | `ExFun (loc,`McArr (_,`PaOlbi (_,lab,p,e1),w,e2)) ->
      let lab = paolab lab p in
      mkexp loc
        (Pexp_function
           (("?" ^ lab), (Some (expr e1)), [((patt p), (when_expr e2 w))]))
  | `ExFun (loc,`McArr (_,`PaOlb (_,lab,p),w,e)) ->
      let lab = paolab lab p in
      mkexp loc
        (Pexp_function
           (("?" ^ lab), None, [((patt_of_lab loc lab p), (when_expr e w))]))
  | `ExFun (loc,a) -> mkexp loc (Pexp_function ("", None, (match_case a [])))
  | `ExIfe (loc,e1,e2,e3) ->
      mkexp loc (Pexp_ifthenelse ((expr e1), (expr e2), (Some (expr e3))))
  | `ExInt (loc,s) ->
      let i =
        try int_of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int" in
      mkexp loc (Pexp_constant (Const_int i))
  | `ExInt32 (loc,s) ->
      let i32 =
        try Int32.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int32" in
      mkexp loc (Pexp_constant (Const_int32 i32))
  | `ExInt64 (loc,s) ->
      let i64 =
        try Int64.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int64" in
      mkexp loc (Pexp_constant (Const_int64 i64))
  | `ExNativeInt (loc,s) ->
      let nati =
        try Nativeint.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type nativeint" in
      mkexp loc (Pexp_constant (Const_nativeint nati))
  | `ExLab (loc,_,_) -> error loc "labeled expression not allowed here"
  | `ExLaz (loc,e) -> mkexp loc (Pexp_lazy (expr e))
  | `ExLet (loc,rf,bi,e) ->
      mkexp loc (Pexp_let ((mkrf rf), (binding bi []), (expr e)))
  | `ExLmd (loc,i,me,e) ->
      mkexp loc
        (Pexp_letmodule ((with_loc i loc), (module_expr me), (expr e)))
  | `ExMat (loc,e,a) -> mkexp loc (Pexp_match ((expr e), (match_case a [])))
  | `ExNew (loc,id) -> mkexp loc (Pexp_new (long_type_ident id))
  | `ExObj (loc,po,cfl) ->
      let p = match po with | `PaNil _loc -> `PaAny loc | p -> p in
      let cil = class_str_item cfl [] in
      mkexp loc (Pexp_object { pcstr_pat = (patt p); pcstr_fields = cil })
  | `ExOlb (loc,_,_) -> error loc "labeled expression not allowed here"
  | `ExOvr (loc,iel) -> mkexp loc (Pexp_override (mkideexp iel []))
  | `ExRec (loc,lel,eo) ->
      (match lel with
       | `RbNil _loc -> error loc "empty record"
       | _ ->
           let eo = match eo with | `ExNil _loc -> None | e -> Some (expr e) in
           mkexp loc (Pexp_record ((mklabexp lel []), eo)))
  | `ExSeq (_loc,e) ->
      let rec loop =
        function
        | [] -> expr (`ExId (_loc, (`Uid (_loc, "()"))))
        | e::[] -> expr e
        | e::el ->
            let _loc = FanLoc.merge (loc_of_expr e) _loc in
            mkexp _loc (Pexp_sequence ((expr e), (loop el))) in
      loop (list_of_expr e [])
  | `ExSnd (loc,e,s) -> mkexp loc (Pexp_send ((expr e), s))
  | `ExSte (loc,e1,e2) ->
      mkexp loc
        (Pexp_apply
           ((mkexp loc (Pexp_ident (array_function loc "String" "get"))),
             [("", (expr e1)); ("", (expr e2))]))
  | `ExStr (loc,s) ->
      mkexp loc (Pexp_constant (Const_string (string_of_string_token loc s)))
  | `ExTry (loc,e,a) -> mkexp loc (Pexp_try ((expr e), (match_case a [])))
  | `ExTup (loc,`ExCom (_,e1,e2)) ->
      mkexp loc
        (Pexp_tuple (List.map expr (list_of_expr e1 (list_of_expr e2 []))))
  | `ExTup (loc,_) -> error loc "singleton tuple"
  | `ExTyc (loc,e,t) ->
      mkexp loc (Pexp_constraint ((expr e), (Some (ctyp t)), None))
  | `ExId (loc,`Uid (_,"()")) ->
      mkexp loc (Pexp_construct ((lident_with_loc "()" loc), None, true))
  | `ExId (loc,`Lid (_,("true"|"false" as s))) ->
      mkexp loc (Pexp_construct ((lident_with_loc s loc), None, true))
  | `ExId (loc,`Lid (_,s)) -> mkexp loc (Pexp_ident (lident_with_loc s loc))
  | `ExId (loc,`Uid (_,s)) ->
      mkexp loc (Pexp_construct ((lident_with_loc s loc), None, true))
  | `ExVrn (loc,s) -> mkexp loc (Pexp_variant (s, None))
  | `ExWhi (loc,e1,el) ->
      let e2 = `ExSeq (loc, el) in
      mkexp loc (Pexp_while ((expr e1), (expr e2)))
  | `ExOpI (loc,i,e) -> mkexp loc (Pexp_open ((long_uident i), (expr e)))
  | `ExPkg (loc,`MeTyc (_,me,pt)) ->
      mkexp loc
        (Pexp_constraint
           ((mkexp loc (Pexp_pack (module_expr me))),
             (Some (mktyp loc (Ptyp_package (package_type pt)))), None))
  | `ExPkg (loc,me) -> mkexp loc (Pexp_pack (module_expr me))
  | `ExFUN (loc,i,e) -> mkexp loc (Pexp_newtype (i, (expr e)))
  | `ExCom (loc,_,_) -> error loc "expr, expr: not allowed here"
  | `ExSem (loc,_,_) ->
      error loc
        "expr; expr: not allowed here, use begin ... end or [|...|] to surround them"
  | `ExId (_,_)|`ExNil _ as e -> error (loc_of_expr e) "invalid expr"
and patt_of_lab _loc lab =
  function
  | `PaNil _loc -> patt (`PaId (_loc, (`Lid (_loc, lab))))
  | p -> patt p
and expr_of_lab _loc lab =
  function
  | `ExNil _loc -> expr (`ExId (_loc, (`Lid (_loc, lab))))
  | e -> expr e
and label_expr =
  function
  | `ExLab (loc,lab,eo) -> (lab, (expr_of_lab loc lab eo))
  | `ExOlb (loc,lab,eo) -> (("?" ^ lab), (expr_of_lab loc lab eo))
  | e -> ("", (expr e))
and binding x acc =
  match x with
  | `BiAnd (_loc,x,y) -> binding x (binding y acc)
  | `BiEq
      (_loc,`PaId (sloc,`Lid (_,bind_name)),`ExTyc (_,e,`TyTypePol (_,vs,ty)))
      ->
      let rec id_to_string x =
        match x with
        | `TyId (_loc,`Lid (_,x)) -> [x]
        | `TyApp (_loc,x,y) -> (id_to_string x) @ (id_to_string y)
        | _ -> assert false in
      let vars = id_to_string vs in
      let ampersand_vars = List.map (fun x  -> "&" ^ x) vars in
      let ty' = varify_constructors vars (ctyp ty) in
      let mkexp = mkexp _loc in
      let mkpat = mkpat _loc in
      let e = mkexp (Pexp_constraint ((expr e), (Some (ctyp ty)), None)) in
      let rec mk_newtypes x =
        match x with
        | newtype::[] -> mkexp (Pexp_newtype (newtype, e))
        | newtype::newtypes ->
            mkexp (Pexp_newtype (newtype, (mk_newtypes newtypes)))
        | [] -> assert false in
      let pat =
        mkpat
          (Ppat_constraint
             ((mkpat (Ppat_var (with_loc bind_name sloc))),
               (mktyp _loc (Ptyp_poly (ampersand_vars, ty'))))) in
      let e = mk_newtypes vars in (pat, e) :: acc
  | `BiEq (_loc,p,`ExTyc (_,e,`TyPol (_,vs,ty))) ->
      ((patt (`PaTyc (_loc, p, (`TyPol (_loc, vs, ty))))), (expr e)) :: acc
  | `BiEq (_loc,p,e) -> ((patt p), (expr e)) :: acc
  | `BiNil _loc -> acc
  | _ -> assert false
and match_case x acc =
  match x with
  | `McOr (_loc,x,y) -> match_case x (match_case y acc)
  | `McArr (_loc,p,w,e) -> ((patt p), (when_expr e w)) :: acc
  | `McNil _loc -> acc
  | _ -> assert false
and when_expr e w =
  match w with
  | `ExNil _loc -> expr e
  | w -> mkexp (loc_of_expr w) (Pexp_when ((expr w), (expr e)))
and mklabexp x acc =
  match x with
  | `RbSem (_loc,x,y) -> mklabexp x (mklabexp y acc)
  | `RbEq (_loc,i,e) -> ((ident i), (expr e)) :: acc
  | _ -> assert false
and mkideexp x acc =
  match x with
  | `RbNil _loc -> acc
  | `RbSem (_loc,x,y) -> mkideexp x (mkideexp y acc)
  | `RbEq (_loc,`Lid (sloc,s),e) -> ((with_loc s sloc), (expr e)) :: acc
  | _ -> assert false
and mktype_decl x acc =
  match x with
  | `TyAnd (_loc,x,y) -> mktype_decl x (mktype_decl y acc)
  | `TyDcl (cloc,c,tl,td,cl) ->
      let cl =
        List.map
          (fun (t1,t2)  ->
             let loc = FanLoc.merge (loc_of_ctyp t1) (loc_of_ctyp t2) in
             ((ctyp t1), (ctyp t2), loc)) cl in
      ((with_loc c cloc),
        (type_decl (List.fold_right optional_type_parameters tl []) cl td
           cloc))
        :: acc
  | _ -> assert false
and module_type =
  function
  | `MtNil loc -> error loc "abstract/nil module type not allowed here"
  | `MtId (loc,i) -> mkmty loc (Pmty_ident (long_uident i))
  | `MtFun (loc,n,nt,mt) ->
      mkmty loc
        (Pmty_functor ((with_loc n loc), (module_type nt), (module_type mt)))
  | `MtQuo (loc,_) -> error loc "module type variable not allowed here"
  | `MtSig (loc,sl) -> mkmty loc (Pmty_signature (sig_item sl []))
  | `MtWit (loc,mt,wc) ->
      mkmty loc (Pmty_with ((module_type mt), (mkwithc wc [])))
  | `MtOf (loc,me) -> mkmty loc (Pmty_typeof (module_expr me))
  | `Ant (_loc,_) -> assert false
and sig_item s l =
  match s with
  | `SgNil _loc -> l
  | `SgCls (loc,cd) ->
      (mksig loc
         (Psig_class
            (List.map class_info_class_type (list_of_class_type cd []))))
      :: l
  | `SgClt (loc,ctd) ->
      (mksig loc
         (Psig_class_type
            (List.map class_info_class_type (list_of_class_type ctd []))))
      :: l
  | `SgSem (_loc,sg1,sg2) -> sig_item sg1 (sig_item sg2 l)
  | `SgDir (_,_,_) -> l
  | `SgExc (loc,`TyId (_,`Uid (_,s))) ->
      (mksig loc (Psig_exception ((with_loc s loc), []))) :: l
  | `SgExc (loc,`TyOf (_,`TyId (_,`Uid (_,s)),t)) ->
      (mksig loc
         (Psig_exception
            ((with_loc s loc), (List.map ctyp (list_of_ctyp t [])))))
      :: l
  | `SgExc (_,_) -> assert false
  | `SgExt (loc,n,t,sl) ->
      (mksig loc
         (Psig_value
            ((with_loc n loc), (mkvalue_desc loc t (list_of_meta_list sl)))))
      :: l
  | `SgInc (loc,mt) -> (mksig loc (Psig_include (module_type mt))) :: l
  | `SgMod (loc,n,mt) ->
      (mksig loc (Psig_module ((with_loc n loc), (module_type mt)))) :: l
  | `SgRecMod (loc,mb) ->
      (mksig loc (Psig_recmodule (module_sig_binding mb []))) :: l
  | `SgMty (loc,n,mt) ->
      let si =
        match mt with
        | `MtQuo (_,_) -> Pmodtype_abstract
        | _ -> Pmodtype_manifest (module_type mt) in
      (mksig loc (Psig_modtype ((with_loc n loc), si))) :: l
  | `SgOpn (loc,id) -> (mksig loc (Psig_open (long_uident id))) :: l
  | `SgTyp (loc,tdl) -> (mksig loc (Psig_type (mktype_decl tdl []))) :: l
  | `SgVal (loc,n,t) ->
      (mksig loc (Psig_value ((with_loc n loc), (mkvalue_desc loc t [])))) ::
      l
  | `Ant (loc,_) -> error loc "antiquotation in sig_item"
and module_sig_binding x acc =
  match x with
  | `MbAnd (_loc,x,y) -> module_sig_binding x (module_sig_binding y acc)
  | `MbCol (loc,s,mt) -> ((with_loc s loc), (module_type mt)) :: acc
  | _ -> assert false
and module_str_binding x acc =
  match x with
  | `MbAnd (_loc,x,y) -> module_str_binding x (module_str_binding y acc)
  | `MbColEq (loc,s,mt,me) ->
      ((with_loc s loc), (module_type mt), (module_expr me)) :: acc
  | _ -> assert false
and module_expr =
  function
  | `MeNil loc -> error loc "nil module expression"
  | `MeId (loc,i) -> mkmod loc (Pmod_ident (long_uident i))
  | `MeApp (loc,me1,me2) ->
      mkmod loc (Pmod_apply ((module_expr me1), (module_expr me2)))
  | `MeFun (loc,n,mt,me) ->
      mkmod loc
        (Pmod_functor ((with_loc n loc), (module_type mt), (module_expr me)))
  | `MeStr (loc,sl) -> mkmod loc (Pmod_structure (str_item sl []))
  | `MeTyc (loc,me,mt) ->
      mkmod loc (Pmod_constraint ((module_expr me), (module_type mt)))
  | `MePkg (loc,`ExTyc (_,e,`TyPkg (_,pt))) ->
      mkmod loc
        (Pmod_unpack
           (mkexp loc
              (Pexp_constraint
                 ((expr e),
                   (Some (mktyp loc (Ptyp_package (package_type pt)))), None))))
  | `MePkg (loc,e) -> mkmod loc (Pmod_unpack (expr e))
  | `Ant (loc,_) -> error loc "antiquotation in module_expr"
and str_item s l =
  match s with
  | `StNil _loc -> l
  | `StCls (loc,cd) ->
      (mkstr loc
         (Pstr_class
            (List.map class_info_class_expr (list_of_class_expr cd []))))
      :: l
  | `StClt (loc,ctd) ->
      (mkstr loc
         (Pstr_class_type
            (List.map class_info_class_type (list_of_class_type ctd []))))
      :: l
  | `StSem (_loc,st1,st2) -> str_item st1 (str_item st2 l)
  | `StDir (_,_,_) -> l
  | `StExc (loc,`TyId (_,`Uid (_,s)),`ONone) ->
      (mkstr loc (Pstr_exception ((with_loc s loc), []))) :: l
  | `StExc (loc,`TyOf (_,`TyId (_,`Uid (_,s)),t),`ONone) ->
      (mkstr loc
         (Pstr_exception
            ((with_loc s loc), (List.map ctyp (list_of_ctyp t [])))))
      :: l
  | `StExc (loc,`TyId (_,`Uid (_,s)),`OSome i) ->
      (mkstr loc (Pstr_exn_rebind ((with_loc s loc), (ident i)))) :: l
  | `StExc (loc,`TyOf (_,`TyId (_,`Uid (_,_)),_),`OSome _) ->
      error loc "type in exception alias"
  | `StExc (_,_,_) -> assert false
  | `StExp (loc,e) -> (mkstr loc (Pstr_eval (expr e))) :: l
  | `StExt (loc,n,t,sl) ->
      (mkstr loc
         (Pstr_primitive
            ((with_loc n loc), (mkvalue_desc loc t (list_of_meta_list sl)))))
      :: l
  | `StInc (loc,me) -> (mkstr loc (Pstr_include (module_expr me))) :: l
  | `StMod (loc,n,me) ->
      (mkstr loc (Pstr_module ((with_loc n loc), (module_expr me)))) :: l
  | `StRecMod (loc,mb) ->
      (mkstr loc (Pstr_recmodule (module_str_binding mb []))) :: l
  | `StMty (loc,n,mt) ->
      (mkstr loc (Pstr_modtype ((with_loc n loc), (module_type mt)))) :: l
  | `StOpn (loc,id) -> (mkstr loc (Pstr_open (long_uident id))) :: l
  | `StTyp (loc,tdl) -> (mkstr loc (Pstr_type (mktype_decl tdl []))) :: l
  | `StVal (loc,rf,bi) ->
      (mkstr loc (Pstr_value ((mkrf rf), (binding bi [])))) :: l
  | `Ant (loc,_) -> error loc "antiquotation in str_item"
and class_type =
  function
  | `CtCon (loc,`ViNil,id,tl) ->
      mkcty loc
        (Pcty_constr
           ((long_class_ident id), (List.map ctyp (Ctyp.list_of_opt tl []))))
  | `CtFun (loc,`TyLab (_,lab,t),ct) ->
      mkcty loc (Pcty_fun (lab, (ctyp t), (class_type ct)))
  | `CtFun (loc,`TyOlb (loc1,lab,t),ct) ->
      let t = `TyApp (loc1, (predef_option loc1), t) in
      mkcty loc (Pcty_fun (("?" ^ lab), (ctyp t), (class_type ct)))
  | `CtFun (loc,t,ct) -> mkcty loc (Pcty_fun ("", (ctyp t), (class_type ct)))
  | `CtSig (loc,t_o,ctfl) ->
      let t = match t_o with | `TyNil _loc -> `TyAny loc | t -> t in
      let cil = class_sig_item ctfl [] in
      mkcty loc
        (Pcty_signature
           { pcsig_self = (ctyp t); pcsig_fields = cil; pcsig_loc = loc })
  | `CtCon (loc,_,_,_) ->
      error loc "invalid virtual class inside a class type"
  | `Ant (_,_)|`CtEq (_,_,_)|`CtCol (_,_,_)|`CtAnd (_,_,_)|`CtNil _ ->
      assert false
and class_info_class_expr ci =
  match ci with
  | `CeEq (_,`CeCon (loc,vir,`Lid (nloc,name),params),ce) ->
      let (loc_params,(params,variance)) =
        match params with
        | `TyNil _loc -> (loc, ([], []))
        | t -> ((loc_of_ctyp t), (List.split (class_parameters t []))) in
      {
        pci_virt = (mkvirtual vir);
        pci_params = (params, loc_params);
        pci_name = (with_loc name nloc);
        pci_expr = (class_expr ce);
        pci_loc = loc;
        pci_variance = variance
      }
  | ce -> error (loc_of_class_expr ce) "bad class definition"
and class_info_class_type ci =
  match ci with
  | `CtEq (_,`CtCon (loc,vir,`Lid (nloc,name),params),ct)|`CtCol
                                                            (_,`CtCon
                                                                 (loc,vir,
                                                                  `Lid
                                                                    (nloc,name),params),ct)
      ->
      let (loc_params,(params,variance)) =
        match params with
        | `TyNil _loc -> (loc, ([], []))
        | t -> ((loc_of_ctyp t), (List.split (class_parameters t []))) in
      {
        pci_virt = (mkvirtual vir);
        pci_params = (params, loc_params);
        pci_name = (with_loc name nloc);
        pci_expr = (class_type ct);
        pci_loc = loc;
        pci_variance = variance
      }
  | ct ->
      error (loc_of_class_type ct)
        "bad class/class type declaration/definition"
and class_sig_item c l =
  match c with
  | `CgNil _loc -> l
  | `CgCtr (loc,t1,t2) -> (mkctf loc (Pctf_cstr ((ctyp t1), (ctyp t2)))) :: l
  | `CgSem (_loc,csg1,csg2) -> class_sig_item csg1 (class_sig_item csg2 l)
  | `CgInh (loc,ct) -> (mkctf loc (Pctf_inher (class_type ct))) :: l
  | `CgMth (loc,s,pf,t) ->
      (mkctf loc (Pctf_meth (s, (mkprivate pf), (mkpolytype (ctyp t))))) :: l
  | `CgVal (loc,s,b,v,t) ->
      (mkctf loc (Pctf_val (s, (mkmutable b), (mkvirtual v), (ctyp t)))) :: l
  | `CgVir (loc,s,b,t) ->
      (mkctf loc (Pctf_virt (s, (mkprivate b), (mkpolytype (ctyp t))))) :: l
  | `Ant (_,_) -> assert false
and class_expr =
  function
  | `CeApp (loc,_,_) as c ->
      let (ce,el) = ClassExpr.view_app [] c in
      let el = List.map label_expr el in
      mkcl loc (Pcl_apply ((class_expr ce), el))
  | `CeCon (loc,`ViNil,id,tl) ->
      mkcl loc
        (Pcl_constr
           ((long_class_ident id), (List.map ctyp (Ctyp.list_of_opt tl []))))
  | `CeFun (loc,`PaLab (_,lab,po),ce) ->
      mkcl loc
        (Pcl_fun (lab, None, (patt_of_lab loc lab po), (class_expr ce)))
  | `CeFun (loc,`PaOlbi (_,lab,p,e),ce) ->
      let lab = paolab lab p in
      mkcl loc
        (Pcl_fun (("?" ^ lab), (Some (expr e)), (patt p), (class_expr ce)))
  | `CeFun (loc,`PaOlb (_,lab,p),ce) ->
      let lab = paolab lab p in
      mkcl loc
        (Pcl_fun
           (("?" ^ lab), None, (patt_of_lab loc lab p), (class_expr ce)))
  | `CeFun (loc,p,ce) ->
      mkcl loc (Pcl_fun ("", None, (patt p), (class_expr ce)))
  | `CeLet (loc,rf,bi,ce) ->
      mkcl loc (Pcl_let ((mkrf rf), (binding bi []), (class_expr ce)))
  | `CeStr (loc,po,cfl) ->
      let p = match po with | `PaNil _loc -> `PaAny loc | p -> p in
      let cil = class_str_item cfl [] in
      mkcl loc (Pcl_structure { pcstr_pat = (patt p); pcstr_fields = cil })
  | `CeTyc (loc,ce,ct) ->
      mkcl loc (Pcl_constraint ((class_expr ce), (class_type ct)))
  | `CeCon (loc,_,_,_) ->
      error loc "invalid virtual class inside a class expression"
  | `Ant (_,_)|`CeEq (_,_,_)|`CeAnd (_,_,_)|`CeNil _ -> assert false
and class_str_item c l =
  match c with
  | `CrNil _ -> l
  | `CrCtr (loc,t1,t2) -> (mkcf loc (Pcf_constr ((ctyp t1), (ctyp t2)))) :: l
  | `CrSem (_loc,cst1,cst2) -> class_str_item cst1 (class_str_item cst2 l)
  | `CrInh (loc,ov,ce,pb) ->
      let opb = if pb = "" then None else Some pb in
      (mkcf loc (Pcf_inher ((override_flag loc ov), (class_expr ce), opb)))
        :: l
  | `CrIni (loc,e) -> (mkcf loc (Pcf_init (expr e))) :: l
  | `CrMth (loc,s,ov,pf,e,t) ->
      let t =
        match t with | `TyNil _loc -> None | t -> Some (mkpolytype (ctyp t)) in
      let e = mkexp loc (Pexp_poly ((expr e), t)) in
      (mkcf loc
         (Pcf_meth
            ((with_loc s loc), (mkprivate pf), (override_flag loc ov), e)))
        :: l
  | `CrVal (loc,s,ov,mf,e) ->
      (mkcf loc
         (Pcf_val
            ((with_loc s loc), (mkmutable mf), (override_flag loc ov),
              (expr e))))
      :: l
  | `CrVir (loc,s,pf,t) ->
      (mkcf loc
         (Pcf_virt ((with_loc s loc), (mkprivate pf), (mkpolytype (ctyp t)))))
      :: l
  | `CrVvr (loc,s,mf,t) ->
      (mkcf loc (Pcf_valvirt ((with_loc s loc), (mkmutable mf), (ctyp t))))
      :: l
  | `Ant (_,_) -> assert false
let sig_item ast = sig_item ast []
let str_item ast = str_item ast []
let directive =
  function
  | `ExNil _loc -> Pdir_none
  | `ExStr (_,s) -> Pdir_string s
  | `ExInt (_,i) -> Pdir_int (int_of_string i)
  | `ExId (_loc,`Lid (_,"true")) -> Pdir_bool true
  | `ExId (_loc,`Lid (_,"false")) -> Pdir_bool false
  | e -> Pdir_ident (ident_noloc (ident_of_expr e))
let phrase =
  function
  | `StDir (_,d,dp) -> Ptop_dir (d, (directive dp))
  | si -> Ptop_def (str_item si)
open Format
let pp = fprintf
let print_expr f e = pp f "@[%a@]@." AstPrint.expression (expr e)
let print_patt f e = pp f "@[%a@]@." AstPrint.pattern (patt e)
let print_str_item f e = pp f "@[%a@]@." AstPrint.structure (str_item e)
let print_ctyp f e = pp f "@[%a@]@." AstPrint.core_type (ctyp e)
let _ = Ctyp.to_string := (LibUtil.to_string_of_printer print_ctyp)
