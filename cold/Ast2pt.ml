open Parsetree
open Longident
open Asttypes
open Lib
open LibUtil
open FanUtil
open FanAst
open ParsetreeHelper
open FanLoc
let mkvirtual: virtual_flag -> Asttypes.virtual_flag =
  function
  | `Virtual _ -> Virtual
  | `ViNil _ -> Concrete
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"
let mkdirection: direction_flag -> Asttypes.direction_flag =
  function
  | `To _ -> Upto
  | `Downto _ -> Downto
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"
let mkrf: rec_flag -> Asttypes.rec_flag =
  function
  | `Recursive _ -> Recursive
  | `ReNil _ -> Nonrecursive
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"
let ident_tag (i : ident) =
  let rec self i acc =
    match i with
    | `Dot (_loc,`Lid (_,"*predef*"),`Lid (_,"option")) ->
        Some ((ldot (lident "*predef*") "option"), `lident)
    | `Dot (_loc,i1,i2) -> self i2 (self i1 acc)
    | `App (_loc,i1,i2) ->
        (match ((self i1 None), (self i2 None), acc) with
         | (Some (l,_),Some (r,_),None ) -> Some ((Lapply (l, r)), `app)
         | _ -> errorf (loc_of i) "invalid long identifer %s" (dump_ident i))
    | `Uid (_loc,s) ->
        (match (acc, s) with
         | (None ,"") -> None
         | (None ,s) -> Some ((lident s), `uident)
         | (Some (_,(`uident|`app)),"") -> acc
         | (Some (x,(`uident|`app)),s) -> Some ((ldot x s), `uident)
         | _ ->
             errorf (FanAst.loc_of i) "invalid long identifier %s"
               (dump_ident i))
    | `Lid (_loc,s) ->
        let x =
          match acc with
          | None  -> lident s
          | Some (acc,(`uident|`app)) -> ldot acc s
          | _ ->
              errorf (loc_of i) "invalid long identifier %s" (dump_ident i) in
        Some (x, `lident)
    | `Ant (_,_) -> error (loc_of i) "invalid long identifier" in
  match self i None with
  | Some x -> x
  | None  -> error (loc_of i) "invalid long identifier "
let ident_noloc i = fst (ident_tag i)
let ident (i : ident) =
  (with_loc (ident_noloc i) (loc_of i) : Longident.t Location.loc )
let long_lident ~err  id =
  match ident_tag id with
  | (i,`lident) -> i +> (loc_of id)
  | _ -> error (loc_of id) err
let long_type_ident: ident -> Longident.t Location.loc =
  long_lident ~err:"invalid long identifier type"
let long_class_ident = long_lident ~err:"invalid class name"
let long_uident_noloc i =
  match ident_tag i with
  | (Ldot (i,s),`uident) -> ldot i s
  | (Lident s,`uident) -> lident s
  | (i,`app) -> i
  | _ -> error (loc_of i) "uppercase identifier expected"
let long_uident i = (long_uident_noloc i) +> (loc_of i)
let rec ctyp_long_id_prefix (t : ctyp) =
  (match t with
   | `Id (_loc,i) -> ident_noloc i
   | `App (_loc,m1,m2) ->
       let li1 = ctyp_long_id_prefix m1 in
       let li2 = ctyp_long_id_prefix m2 in Lapply (li1, li2)
   | t -> errorf (loc_of t) "invalid module expression %s" (dump_ctyp t) : 
  Longident.t )
let ctyp_long_id (t : ctyp) =
  (match t with
   | `Id (_loc,i) -> (false, (long_type_ident i))
   | `ClassPath (_,i) -> (true, (ident i))
   | t -> errorf (loc_of t) "invalid type %s" (dump_ctyp t) : (bool*
                                                                Longident.t
                                                                Location.loc) )
let predef_option loc =
  `Id (loc, (`Dot (loc, (`Lid (loc, "*predef*")), (`Lid (loc, "option")))))
let rec ctyp (x : ctyp) =
  match x with
  | `Id (_loc,i) ->
      let li = long_type_ident i in mktyp _loc (Ptyp_constr (li, []))
  | `Alias (_loc,t1,`Quote (_,_,`Some `Lid (_,s))) ->
      mktyp _loc (Ptyp_alias ((ctyp t1), s))
  | `Any _loc -> mktyp _loc Ptyp_any
  | `App (_loc,_,_) as f ->
      let (f,al) = view_app [] f in
      let (is_cls,li) = ctyp_long_id f in
      if is_cls
      then mktyp _loc (Ptyp_class (li, (List.map ctyp al), []))
      else mktyp _loc (Ptyp_constr (li, (List.map ctyp al)))
  | `Arrow (loc,`Label (_,`Lid (_,lab),t1),t2) ->
      mktyp loc (Ptyp_arrow (lab, (ctyp t1), (ctyp t2)))
  | `Arrow (loc,`TyOlb (loc1,`Lid (_,lab),t1),t2) ->
      let t1 = `App (loc1, (predef_option loc1), t1) in
      mktyp loc (Ptyp_arrow (("?" ^ lab), (ctyp t1), (ctyp t2)))
  | `Arrow (loc,t1,t2) -> mktyp loc (Ptyp_arrow ("", (ctyp t1), (ctyp t2)))
  | `TyObj (_loc,fl,`RvNil _) -> mktyp _loc (Ptyp_object (meth_list fl []))
  | `TyObj (_loc,fl,`RowVar _) ->
      mktyp _loc (Ptyp_object (meth_list fl [mkfield _loc Pfield_var]))
  | `ClassPath (loc,id) -> mktyp loc (Ptyp_class ((ident id), [], []))
  | `Package (_loc,pt) ->
      let (i,cs) = package_type pt in mktyp _loc (Ptyp_package (i, cs))
  | `TyPol (loc,t1,t2) ->
      mktyp loc (Ptyp_poly ((Ctyp.to_var_list t1), (ctyp t2)))
  | `Quote (_loc,`Normal _,`Some `Lid (_,s)) -> mktyp _loc (Ptyp_var s)
  | `Tup (loc,`Sta (_,t1,t2)) ->
      mktyp loc
        (Ptyp_tuple (List.map ctyp (list_of_star' t1 (list_of_star' t2 []))))
  | `TyVrnEq (_loc,t) ->
      mktyp _loc (Ptyp_variant ((row_field t []), true, None))
  | `TyVrnSup (_loc,t) ->
      mktyp _loc (Ptyp_variant ((row_field t []), false, None))
  | `TyVrnInf (_loc,t) ->
      mktyp _loc (Ptyp_variant ((row_field t []), true, (Some [])))
  | `TyVrnInfSup (_loc,t,t') ->
      mktyp _loc
        (Ptyp_variant ((row_field t []), true, (Some (Ctyp.name_tags t'))))
  | x -> errorf (loc_of x) "ctyp: %s" (dump_ctyp x)
and row_field (x : ctyp) acc =
  match x with
  | `Nil _loc -> []
  | `TyVrn (_loc,`C (_,i)) -> (Rtag (i, true, [])) :: acc
  | `TyOfAmp (_loc,`TyVrn (_,`C (_,i)),t) ->
      (Rtag (i, true, (List.map ctyp (list_of_amp' t [])))) :: acc
  | `Of (_loc,`TyVrn (_,`C (_,i)),t) ->
      (Rtag (i, false, (List.map ctyp (list_of_amp' t [])))) :: acc
  | `Or (_loc,t1,t2) -> row_field t1 (row_field t2 acc)
  | t -> (Rinherit (ctyp t)) :: acc
and meth_list (fl : ctyp) (acc : core_field_type list) =
  (match fl with
   | `Nil _ -> acc
   | `Sem (_loc,t1,t2) -> meth_list t1 (meth_list t2 acc)
   | `TyCol (_loc,`Id (_,`Lid (_,lab)),t) ->
       (mkfield _loc (Pfield (lab, (mkpolytype (ctyp t))))) :: acc
   | x -> errorf (loc_of x) "meth_list: %s" (dump_ctyp x) : core_field_type
                                                              list )
and package_type_constraints (wc : with_constr)
  (acc : (Longident.t Asttypes.loc* core_type) list) =
  (match wc with
   | `Nil _ -> acc
   | `TypeEq (_loc,`Id (_,id),ct) -> ((ident id), (ctyp ct)) :: acc
   | `And (_loc,wc1,wc2) ->
       package_type_constraints wc1 (package_type_constraints wc2 acc)
   | x ->
       errorf (loc_of x) "unexpected `with constraint:%s' for a package type"
         (dump_with_constr x) : (Longident.t Asttypes.loc* core_type) list )
and package_type (x : module_type) =
  match x with
  | `With (_loc,`Id (_,i),wc) ->
      ((long_uident i), (package_type_constraints wc []))
  | `Id (_loc,i) -> ((long_uident i), [])
  | mt ->
      errorf (loc_of mt) "unexpected package type: %s" (dump_module_type mt)
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
  function
  | `Private _ -> Private
  | `PrNil _ -> Public
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"
let mktrecord (x : ctyp) =
  match x with
  | `TyCol (_loc,`Id (_,`Lid (sloc,s)),`Mut (_,t)) ->
      ((with_loc s sloc), Mutable, (mkpolytype (ctyp t)), _loc)
  | `TyCol (_loc,`Id (_,`Lid (sloc,s)),t) ->
      ((with_loc s sloc), Immutable, (mkpolytype (ctyp t)), _loc)
  | t -> errorf (loc_of t) "mktrecord %s " (dump_ctyp t)
let mkvariant (x : ctyp) =
  match x with
  | `Id (_loc,`Uid (sloc,s)) -> ((with_loc s sloc), [], None, _loc)
  | `Of (_loc,`Id (_,`Uid (sloc,s)),t) ->
      ((with_loc s sloc), (List.map ctyp (list_of_and' t [])), None, _loc)
  | `TyCol (_loc,`Id (_,`Uid (sloc,s)),`Arrow (_,t,u)) ->
      ((with_loc s sloc), (List.map ctyp (list_of_and' t [])),
        (Some (ctyp u)), _loc)
  | `TyCol (_loc,`Id (_,`Uid (sloc,s)),t) ->
      ((with_loc s sloc), [], (Some (ctyp t)), _loc)
  | t -> errorf (loc_of t) "mkvariant %s " (dump_ctyp t)
let rec type_decl (tl : (string Asttypes.loc option* (bool* bool)) list)
  (cl : (core_type* core_type* Location.t) list) loc m pflag (x : ctyp) =
  match x with
  | `TyMan (_loc,t1,t2) -> type_decl tl cl loc (Some (ctyp t1)) pflag t2
  | `Priv (_loc,t) ->
      if pflag
      then error _loc "multiple private keyword used, use only one instead"
      else type_decl tl cl loc m true t
  | `TyRec (_loc,t) ->
      mktype loc tl cl
        (Ptype_record (List.map mktrecord (list_of_sem' t [])))
        (mkprivate' pflag) m
  | `Sum (_loc,t) ->
      mktype loc tl cl
        (Ptype_variant (List.map mkvariant (list_of_or' t [])))
        (mkprivate' pflag) m
  | t ->
      if m <> None
      then
        errorf loc "only one manifest type allowed by definition %s"
          (dump_ctyp t)
      else
        (let m = match t with | `Nil _loc -> None | _ -> Some (ctyp t) in
         mktype loc tl cl Ptype_abstract (mkprivate' pflag) m)
let type_decl tl cl t loc = type_decl tl cl loc None false t
let mkvalue_desc loc t p =
  { pval_type = (ctyp t); pval_prim = p; pval_loc = loc }
let rec list_of_meta_list =
  function
  | `LNil -> []
  | `LCons (x,xs) -> x :: (list_of_meta_list xs)
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"
let mkmutable =
  function
  | `Mutable _ -> Mutable
  | `MuNil _ -> Immutable
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"
let paolab (lab : string) (p : patt) =
  (match (lab, p) with
   | ("",(`Id (_loc,`Lid (_,i))|`Constraint (_loc,`Id (_,`Lid (_,i)),_))) ->
       i
   | ("",p) -> errorf (loc_of p) "paolab %s" (dump_patt p)
   | _ -> lab : string )
let quote_map (x : ctyp) =
  match x with
  | `Quote (_loc,p,s) ->
      let tuple =
        match p with
        | `Positive _ -> (true, false)
        | `Negative _ -> (false, true)
        | `Normal _ -> (false, false)
        | `Ant (_loc,_) -> error _loc "antiquotation not expected here" in
      let s =
        match s with
        | `None -> None
        | `Some `Lid (sloc,s) -> Some (s +> sloc)
        | `Some `Ant (_loc,_)|`Ant (_loc,_) ->
            error _loc "antiquotation not expected here" in
      (s, tuple)
  | t -> errorf (loc_of x) "quote_map %s" (dump_ctyp t)
let optional_type_parameters (t : ctyp) =
  List.map quote_map (list_of_app' t [])
let class_parameters (t : ctyp) =
  List.filter_map
    (function
     | `Nil _ -> None
     | x ->
         (match quote_map x with
          | (Some x,v) -> Some (x, v)
          | (None ,_) ->
              errorf (loc_of t) "class_parameters %s" (dump_ctyp t)))
    (list_of_com t [])
let type_parameters_and_type_name t =
  let rec aux t acc =
    match t with
    | `App (_loc,t1,t2) -> aux t1 ((optional_type_parameters t2) @ acc)
    | `Id (_loc,i) -> ((ident i), acc)
    | x -> errorf (loc_of x) "type_parameters_and_type_name %s" (dump_ctyp x) in
  aux t []
let rec patt_fa al =
  function | `App (_,f,a) -> patt_fa (a :: al) f | f -> (f, al)
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
let rec patt (x : patt) =
  match x with
  | `Id (_loc,`Lid (_,("true"|"false" as txt))) ->
      let p =
        Ppat_construct ({ txt = (Lident txt); loc = _loc }, None, false) in
      mkpat _loc p
  | `Id (_loc,`Lid (sloc,s)) -> mkpat _loc (Ppat_var (with_loc s sloc))
  | `Id (_loc,i) ->
      let p = Ppat_construct ((long_uident i), None, false) in mkpat _loc p
  | `Alias (_loc,p1,x) ->
      (match x with
       | `Lid (sloc,s) ->
           mkpat _loc (Ppat_alias ((patt p1), (with_loc s sloc)))
       | `Ant (_loc,_) -> error _loc "invalid antiquotations")
  | `Ant (loc,_) -> error loc "antiquotation not allowed here"
  | `Any _loc -> mkpat _loc Ppat_any
  | `App (_loc,`Id (_,`Uid (sloc,s)),`Tup (_,`Any loc_any)) ->
      mkpat _loc
        (Ppat_construct
           ((lident_with_loc s sloc), (Some (mkpat loc_any Ppat_any)), false))
  | `App (loc,_,_) as f ->
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
           error (loc_of f)
             "this is not a constructor, it cannot be applied in a pattern")
  | `Array (loc,p) ->
      mkpat loc (Ppat_array (List.map patt (list_of_sem' p [])))
  | `Chr (loc,s) ->
      mkpat loc (Ppat_constant (Const_char (char_of_char_token loc s)))
  | `Int (loc,s) ->
      let i =
        try int_of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int" in
      mkpat loc (Ppat_constant (Const_int i))
  | `Int32 (loc,s) ->
      let i32 =
        try Int32.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int32" in
      mkpat loc (Ppat_constant (Const_int32 i32))
  | `Int64 (loc,s) ->
      let i64 =
        try Int64.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int64" in
      mkpat loc (Ppat_constant (Const_int64 i64))
  | `NativeInt (loc,s) ->
      let nati =
        try Nativeint.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type nativeint" in
      mkpat loc (Ppat_constant (Const_nativeint nati))
  | `Flo (loc,s) ->
      mkpat loc (Ppat_constant (Const_float (remove_underscores s)))
  | `Label (loc,_,_) -> error loc "labeled pattern not allowed here"
  | `PaOlbi (loc,_,_,_) -> error loc "labeled pattern not allowed here"
  | `Or (loc,p1,p2) -> mkpat loc (Ppat_or ((patt p1), (patt p2)))
  | `PaRng (loc,p1,p2) ->
      (match (p1, p2) with
       | (`Chr (loc1,c1),`Chr (loc2,c2)) ->
           let c1 = char_of_char_token loc1 c1 in
           let c2 = char_of_char_token loc2 c2 in mkrangepat loc c1 c2
       | _ -> error loc "range pattern allowed only for characters")
  | `PaRec (loc,p) ->
      let ps = list_of_sem' p [] in
      let is_wildcard = function | `Any _loc -> true | _ -> false in
      let (wildcards,ps) = List.partition is_wildcard ps in
      let is_closed = if wildcards = [] then Closed else Open in
      mkpat loc (Ppat_record ((List.map mklabpat ps), is_closed))
  | `Str (loc,s) ->
      mkpat loc (Ppat_constant (Const_string (string_of_string_token loc s)))
  | `Tup (loc,`Com (_,p1,p2)) ->
      mkpat loc
        (Ppat_tuple (List.map patt (list_of_com' p1 (list_of_com' p2 []))))
  | `Tup (loc,_) -> error loc "singleton tuple pattern"
  | `Constraint (loc,p,t) -> mkpat loc (Ppat_constraint ((patt p), (ctyp t)))
  | `ClassPath (loc,i) -> mkpat loc (Ppat_type (long_type_ident i))
  | `Vrn (loc,s) -> mkpat loc (Ppat_variant (s, None))
  | `Lazy (loc,p) -> mkpat loc (Ppat_lazy (patt p))
  | `ModuleUnpack (loc,m,ty) ->
      (match m with
       | `Uid (sloc,m) ->
           (match ty with
            | `None -> mkpat loc (Ppat_unpack (with_loc m sloc))
            | `Some ty ->
                mkpat loc
                  (Ppat_constraint
                     ((mkpat sloc (Ppat_unpack (with_loc m sloc))),
                       (ctyp ty)))
            | `Ant (_loc,_) -> error _loc "antiquotation not expected here")
       | `Ant (_loc,_) -> error _loc "antiquotation not expected here")
  | `PaEq (_,_,_)|`Sem (_,_,_)|`Com (_,_,_)|`Nil _ as p ->
      error (loc_of p) "invalid pattern"
and mklabpat: patt -> (Longident.t Asttypes.loc* pattern) =
  function
  | `PaEq (_loc,i,p) -> ((ident i), (patt p))
  | p -> error (loc_of p) "invalid pattern"
let override_flag loc =
  function
  | `Override _loc -> Override
  | `OvNil _loc -> Fresh
  | _ -> error loc "antiquotation not allowed here"
let rec expr (x : expr) =
  match x with
  | `Dot (_loc,_,_)|`Id (_loc,`Dot _) ->
      let (e,l) =
        match Expr.sep_dot_expr [] x with
        | (loc,ml,`Id (sloc,`Uid (_,s)))::l ->
            ((mkexp loc (Pexp_construct ((mkli sloc s ml), None, false))), l)
        | (loc,ml,`Id (sloc,`Lid (_,s)))::l ->
            ((mkexp loc (Pexp_ident (mkli sloc s ml))), l)
        | (_,[],e)::l -> ((expr e), l)
        | _ -> errorf (loc_of x) "expr: %s" (dump_expr x) in
      let (_,e) =
        List.fold_left
          (fun (loc_bp,e1)  (loc_ep,ml,e2)  ->
             match e2 with
             | `Id (sloc,`Lid (_,s)) ->
                 let loc = FanLoc.merge loc_bp loc_ep in
                 (loc, (mkexp loc (Pexp_field (e1, (mkli sloc s ml)))))
             | _ -> error (loc_of e2) "lowercase identifier expected")
          (_loc, e) l in
      e
  | `App (loc,_,_) as f ->
      let (f,al) = view_app [] f in
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
  | `ArrayDot (loc,e1,e2) ->
      mkexp loc
        (Pexp_apply
           ((mkexp loc (Pexp_ident (array_function loc "Array" "get"))),
             [("", (expr e1)); ("", (expr e2))]))
  | `Array (loc,e) ->
      mkexp loc (Pexp_array (List.map expr (list_of_sem' e [])))
  | `ExAsr (loc,e) -> mkexp loc (Pexp_assert (expr e))
  | `ExAsf loc -> mkexp loc Pexp_assertfalse
  | `Assign (loc,e,v) ->
      let e =
        match e with
        | `Dot (loc,x,`Id (_,`Lid (_,"contents"))) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (lident_with_loc ":=" loc))),
                [("", (expr x)); ("", (expr v))])
        | `Dot (loc,_,_) ->
            (match (expr e).pexp_desc with
             | Pexp_field (e,lab) -> Pexp_setfield (e, lab, (expr v))
             | _ -> error loc "bad record access")
        | `ArrayDot (loc,e1,e2) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (array_function loc "Array" "set"))),
                [("", (expr e1)); ("", (expr e2)); ("", (expr v))])
        | `Id (_,`Lid (lloc,lab)) ->
            Pexp_setinstvar ((with_loc lab lloc), (expr v))
        | `StringDot (loc,e1,e2) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (array_function loc "String" "set"))),
                [("", (expr e1)); ("", (expr e2)); ("", (expr v))])
        | x -> errorf loc "bad left part of assignment:%s" (dump_expr x) in
      mkexp loc e
  | `Chr (loc,s) ->
      mkexp loc (Pexp_constant (Const_char (char_of_char_token loc s)))
  | `Coercion (loc,e,t1,t2) ->
      let t1 = match t1 with | `Nil _ -> None | t -> Some (ctyp t) in
      mkexp loc (Pexp_constraint ((expr e), t1, (Some (ctyp t2))))
  | `Flo (loc,s) ->
      mkexp loc (Pexp_constant (Const_float (remove_underscores s)))
  | `For (loc,`Lid (sloc,i),e1,e2,df,el) ->
      let e3 = `Seq (loc, el) in
      mkexp loc
        (Pexp_for
           ((with_loc i sloc), (expr e1), (expr e2), (mkdirection df),
             (expr e3)))
  | `Fun (loc,`Case (_,`Label (_,`Lid (_,lab),po),w,e)) ->
      mkexp loc
        (Pexp_function
           (lab, None, [((patt_of_lab loc lab po), (when_expr e w))]))
  | `Fun (loc,`Case (_,`PaOlbi (_,`Lid (_,lab),p,e1),w,e2)) ->
      (match e1 with
       | `None ->
           let lab = paolab lab p in
           mkexp loc
             (Pexp_function
                (("?" ^ lab), None,
                  [((patt_of_lab loc lab p), (when_expr e2 w))]))
       | `Some e1 ->
           let lab = paolab lab p in
           mkexp loc
             (Pexp_function
                (("?" ^ lab), (Some (expr e1)),
                  [((patt p), (when_expr e2 w))]))
       | `Ant (_loc,_) -> error _loc "antiquotation not expected here")
  | `Fun (loc,a) -> mkexp loc (Pexp_function ("", None, (match_case a)))
  | `IfThenElse (loc,e1,e2,e3) ->
      mkexp loc (Pexp_ifthenelse ((expr e1), (expr e2), (Some (expr e3))))
  | `IfThen (loc,e1,e2) ->
      mkexp loc (Pexp_ifthenelse ((expr e1), (expr e2), None))
  | `Int (loc,s) ->
      let i =
        try int_of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int" in
      mkexp loc (Pexp_constant (Const_int i))
  | `Int32 (loc,s) ->
      let i32 =
        try Int32.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int32" in
      mkexp loc (Pexp_constant (Const_int32 i32))
  | `Int64 (loc,s) ->
      let i64 =
        try Int64.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type int64" in
      mkexp loc (Pexp_constant (Const_int64 i64))
  | `NativeInt (loc,s) ->
      let nati =
        try Nativeint.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type nativeint" in
      mkexp loc (Pexp_constant (Const_nativeint nati))
  | `Label (loc,_,_) -> error loc "labeled expression not allowed here"
  | `Lazy (loc,e) -> mkexp loc (Pexp_lazy (expr e))
  | `LetIn (loc,rf,bi,e) ->
      mkexp loc (Pexp_let ((mkrf rf), (binding bi []), (expr e)))
  | `LetModule (loc,`Uid (sloc,i),me,e) ->
      mkexp loc
        (Pexp_letmodule ((with_loc i sloc), (module_expr me), (expr e)))
  | `Match (loc,e,a) -> mkexp loc (Pexp_match ((expr e), (match_case a)))
  | `New (loc,id) -> mkexp loc (Pexp_new (long_type_ident id))
  | `Obj (loc,po,cfl) ->
      let p = match po with | `Nil _loc -> `Any _loc | p -> p in
      let cil = class_str_item cfl [] in
      mkexp loc (Pexp_object { pcstr_pat = (patt p); pcstr_fields = cil })
  | `OvrInst (loc,iel) -> mkexp loc (Pexp_override (mkideexp iel []))
  | `Record (loc,lel,eo) ->
      (match lel with
       | `Nil _ -> error loc "empty record"
       | _ ->
           let eo = match eo with | `Nil _loc -> None | e -> Some (expr e) in
           mkexp loc (Pexp_record ((mklabexp lel), eo)))
  | `Seq (_loc,e) ->
      let rec loop =
        function
        | [] -> expr (`Id (_loc, (`Uid (_loc, "()"))))
        | e::[] -> expr e
        | e::el ->
            let _loc = FanLoc.merge (loc_of e) _loc in
            mkexp _loc (Pexp_sequence ((expr e), (loop el))) in
      loop (list_of_sem' e [])
  | `Send (loc,e,`Lid (_,s)) -> mkexp loc (Pexp_send ((expr e), s))
  | `StringDot (loc,e1,e2) ->
      mkexp loc
        (Pexp_apply
           ((mkexp loc (Pexp_ident (array_function loc "String" "get"))),
             [("", (expr e1)); ("", (expr e2))]))
  | `Str (loc,s) ->
      mkexp loc (Pexp_constant (Const_string (string_of_string_token loc s)))
  | `Try (loc,e,a) -> mkexp loc (Pexp_try ((expr e), (match_case a)))
  | `Tup (loc,e) ->
      let l = list_of_com' e [] in
      (match l with
       | []|_::[] ->
           errorf loc "tuple should have at least two items" (dump_expr x)
       | _ -> mkexp loc (Pexp_tuple (List.map expr l)))
  | `Constraint (loc,e,t) ->
      mkexp loc (Pexp_constraint ((expr e), (Some (ctyp t)), None))
  | `Id (_loc,`Uid (_,"()")) ->
      mkexp _loc (Pexp_construct ((lident_with_loc "()" _loc), None, true))
  | `Id (_loc,`Lid (_,("true"|"false" as s))) ->
      mkexp _loc (Pexp_construct ((lident_with_loc s _loc), None, true))
  | `Id (_,`Lid (_loc,s)) -> mkexp _loc (Pexp_ident (lident_with_loc s _loc))
  | `Id (_,`Uid (_loc,s)) ->
      mkexp _loc (Pexp_construct ((lident_with_loc s _loc), None, true))
  | `Vrn (loc,s) -> mkexp loc (Pexp_variant (s, None))
  | `While (loc,e1,el) ->
      let e2 = `Seq (loc, el) in
      mkexp loc (Pexp_while ((expr e1), (expr e2)))
  | `LetOpen (_loc,i,e) -> mkexp _loc (Pexp_open ((long_uident i), (expr e)))
  | `Package_expr (_loc,`Constraint (_,me,pt)) ->
      mkexp _loc
        (Pexp_constraint
           ((mkexp _loc (Pexp_pack (module_expr me))),
             (Some (mktyp _loc (Ptyp_package (package_type pt)))), None))
  | `Package_expr (loc,me) -> mkexp loc (Pexp_pack (module_expr me))
  | `LocalTypeFun (loc,`Lid (_,i),e) ->
      mkexp loc (Pexp_newtype (i, (expr e)))
  | x -> errorf (loc_of x) "expr:%s" (dump_expr x)
and patt_of_lab _loc lab (x : patt) =
  match x with
  | `Nil _ -> patt (`Id (_loc, (`Lid (_loc, lab))))
  | p -> patt p
and expr_of_lab _loc lab (x : expr) =
  match x with
  | `Nil _ -> expr (`Id (_loc, (`Lid (_loc, lab))))
  | e -> expr e
and label_expr (x : expr) =
  match x with
  | `Label (loc,`Lid (_,lab),eo) -> (lab, (expr_of_lab loc lab eo))
  | `OptLabl (loc,`Lid (_,lab),eo) -> (("?" ^ lab), (expr_of_lab loc lab eo))
  | e -> ("", (expr e))
and binding (x : binding) acc =
  match x with
  | `And (_,x,y) -> binding x (binding y acc)
  | `Bind
      (_loc,`Id (sloc,`Lid (_,bind_name)),`Constraint
                                            (_,e,`TyTypePol (_,vs,ty)))
      ->
      let rec id_to_string x =
        match x with
        | `Id (_loc,`Lid (_,x)) -> [x]
        | `App (_loc,x,y) -> (id_to_string x) @ (id_to_string y)
        | x -> errorf (loc_of x) "id_to_string %s" (dump_ctyp x) in
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
  | `Bind (_loc,p,`Constraint (_,e,`TyPol (_,vs,ty))) ->
      ((patt (`Constraint (_loc, p, (`TyPol (_loc, vs, ty))))), (expr e)) ::
      acc
  | `Bind (_,p,e) -> ((patt p), (expr e)) :: acc
  | `Nil _ -> acc
  | _ -> assert false
and match_case (x : match_case) =
  let cases = list_of_or' x [] in
  List.filter_map
    (function
     | `Nil _ -> None
     | `Case (_,p,w,e) -> Some ((patt p), (when_expr e w))
     | x -> errorf (loc_of x) "match_case %s" (dump_match_case x)) cases
and when_expr (e : expr) (w : expr) =
  (match w with
   | `Nil _ -> expr e
   | w -> mkexp (loc_of w) (Pexp_when ((expr w), (expr e))) : expression )
and mklabexp (x : rec_binding) =
  let bindings = list_of_sem x [] in
  List.filter_map
    (function
     | `Nil _ -> None
     | `RecBind (_,i,e) -> Some ((ident i), (expr e))
     | x -> errorf (loc_of x) "mklabexp : %s" (dump_rec_binding x)) bindings
and mkideexp (x : rec_binding) (acc : (string Asttypes.loc* expression) list)
  =
  (match x with
   | `Nil _ -> acc
   | `Sem (_,x,y) -> mkideexp x (mkideexp y acc)
   | `RecBind (_,`Lid (sloc,s),e) -> ((with_loc s sloc), (expr e)) :: acc
   | _ -> assert false : (string Asttypes.loc* expression) list )
and mktype_decl (x : ctyp) =
  let tys = list_of_and x [] in
  List.map
    (function
     | `TyDcl (cloc,`Lid (sloc,c),tl,td,cl) ->
         let cl =
           List.map
             (fun (t1,t2)  ->
                let loc = FanLoc.merge (loc_of t1) (loc_of t2) in
                ((ctyp t1), (ctyp t2), loc)) cl in
         ((c +> sloc),
           (type_decl
              (List.fold_right
                 (fun x  acc  -> (optional_type_parameters x) @ acc) tl [])
              cl td cloc))
     | t -> errorf (loc_of t) "mktype_decl %s" (dump_ctyp t)) tys
and module_type: Ast.module_type -> Parsetree.module_type =
  let mkwithc (wc : with_constr) =
    let opt_private_ctyp (x : ctyp) =
      match x with
      | `Priv (_,t) -> (Ptype_abstract, Private, (ctyp t))
      | t -> (Ptype_abstract, Public, (ctyp t)) in
    let mkwithtyp pwith_type loc id_tpl ct =
      let (id,tpl) = type_parameters_and_type_name id_tpl in
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
           })) in
    let constrs = list_of_and' wc [] in
    List.filter_map
      (function
       | `TypeEq (_loc,id_tpl,ct) ->
           Some (mkwithtyp (fun x  -> Pwith_type x) _loc id_tpl ct)
       | `ModuleEq (_loc,i1,i2) ->
           Some ((long_uident i1), (Pwith_module (long_uident i2)))
       | `TypeSubst (_loc,id_tpl,ct) ->
           Some (mkwithtyp (fun x  -> Pwith_typesubst x) _loc id_tpl ct)
       | `ModuleSubst (_loc,i1,i2) ->
           Some ((long_uident i1), (Pwith_modsubst (long_uident i2)))
       | t ->
           errorf (loc_of t) "bad with constraint (antiquotation) : %s"
             (dump_with_constr t)) constrs in
  function
  | `Id (loc,i) -> mkmty loc (Pmty_ident (long_uident i))
  | `MtFun (loc,`Uid (sloc,n),nt,mt) ->
      mkmty loc
        (Pmty_functor ((with_loc n sloc), (module_type nt), (module_type mt)))
  | `Sig (loc,sl) -> mkmty loc (Pmty_signature (sig_item sl []))
  | `With (loc,mt,wc) ->
      mkmty loc (Pmty_with ((module_type mt), (mkwithc wc)))
  | `ModuleTypeOf (_loc,me) -> mkmty _loc (Pmty_typeof (module_expr me))
  | t -> errorf (loc_of t) "module_type: %s" (dump_module_type t)
and sig_item (s : sig_item) (l : signature) =
  (match s with
   | `Nil _ -> l
   | `Class (loc,cd) ->
       (mksig loc
          (Psig_class (List.map class_info_class_type (list_of_and' cd []))))
       :: l
   | `ClassType (loc,ctd) ->
       (mksig loc
          (Psig_class_type
             (List.map class_info_class_type (list_of_and' ctd []))))
       :: l
   | `Sem (_,sg1,sg2) -> sig_item sg1 (sig_item sg2 l)
   | `Directive (_,_,_) -> l
   | `Exception (_loc,`Id (_,`Uid (_,s))) ->
       (mksig _loc (Psig_exception ((with_loc s _loc), []))) :: l
   | `Exception (_loc,`Of (_,`Id (_,`Uid (sloc,s)),t)) ->
       (mksig _loc
          (Psig_exception
             ((with_loc s sloc), (List.map ctyp (list_of_and' t [])))))
       :: l
   | `Exception (_,_) -> assert false
   | `External (loc,`Lid (sloc,n),t,sl) ->
       (mksig loc
          (Psig_value
             ((with_loc n sloc), (mkvalue_desc loc t (list_of_meta_list sl)))))
       :: l
   | `Include (loc,mt) -> (mksig loc (Psig_include (module_type mt))) :: l
   | `Module (loc,`Uid (sloc,n),mt) ->
       (mksig loc (Psig_module ((with_loc n sloc), (module_type mt)))) :: l
   | `RecModule (loc,mb) ->
       (mksig loc (Psig_recmodule (module_sig_binding mb []))) :: l
   | `ModuleType (loc,`Uid (sloc,n),mt) ->
       let si =
         match mt with
         | `Nil _ -> Pmodtype_abstract
         | _ -> Pmodtype_manifest (module_type mt) in
       (mksig loc (Psig_modtype ((with_loc n sloc), si))) :: l
   | `Open (loc,id) -> (mksig loc (Psig_open (long_uident id))) :: l
   | `Type (loc,tdl) -> (mksig loc (Psig_type (mktype_decl tdl))) :: l
   | `Val (loc,`Lid (sloc,n),t) ->
       (mksig loc (Psig_value ((with_loc n sloc), (mkvalue_desc loc t []))))
       :: l
   | t -> errorf (loc_of t) "sig_item: %s" (dump_sig_item t) : signature )
and module_sig_binding (x : module_binding)
  (acc : (string Asttypes.loc* Parsetree.module_type) list) =
  match x with
  | `And (_,x,y) -> module_sig_binding x (module_sig_binding y acc)
  | `Constraint (_loc,`Uid (sloc,s),mt) ->
      ((with_loc s sloc), (module_type mt)) :: acc
  | t -> errorf (loc_of t) "module_sig_binding: %s" (dump_module_binding t)
and module_str_binding (x : Ast.module_binding) acc =
  match x with
  | `And (_,x,y) -> module_str_binding x (module_str_binding y acc)
  | `ModuleBind (_loc,`Uid (sloc,s),mt,me) ->
      ((with_loc s sloc), (module_type mt), (module_expr me)) :: acc
  | t -> errorf (loc_of t) "module_str_binding: %s" (dump_module_binding t)
and module_expr (x : Ast.module_expr) =
  match x with
  | `Id (loc,i) -> mkmod loc (Pmod_ident (long_uident i))
  | `App (loc,me1,me2) ->
      mkmod loc (Pmod_apply ((module_expr me1), (module_expr me2)))
  | `Functor (loc,`Uid (sloc,n),mt,me) ->
      mkmod loc
        (Pmod_functor ((with_loc n sloc), (module_type mt), (module_expr me)))
  | `Struct (loc,sl) -> mkmod loc (Pmod_structure (str_item sl []))
  | `Constraint (loc,me,mt) ->
      mkmod loc (Pmod_constraint ((module_expr me), (module_type mt)))
  | `PackageModule (loc,`Constraint (_,e,`Package (_,pt))) ->
      mkmod loc
        (Pmod_unpack
           (mkexp loc
              (Pexp_constraint
                 ((expr e),
                   (Some (mktyp loc (Ptyp_package (package_type pt)))), None))))
  | `PackageModule (loc,e) -> mkmod loc (Pmod_unpack (expr e))
  | t -> errorf (loc_of t) "module_expr: %s" (dump_module_expr t)
and str_item (s : str_item) (l : structure) =
  (match s with
   | `Nil _ -> l
   | `Class (loc,cd) ->
       (mkstr loc
          (Pstr_class (List.map class_info_class_expr (list_of_and' cd []))))
       :: l
   | `ClassType (loc,ctd) ->
       (mkstr loc
          (Pstr_class_type
             (List.map class_info_class_type (list_of_and' ctd []))))
       :: l
   | `Sem (_,st1,st2) -> str_item st1 (str_item st2 l)
   | `Directive (_,_,_) -> l
   | `Exception (loc,`Id (_,`Uid (_,s))) ->
       (mkstr loc (Pstr_exception ((with_loc s loc), []))) :: l
   | `Exception (loc,`Of (_,`Id (_,`Uid (_,s)),t)) ->
       (mkstr loc
          (Pstr_exception
             ((with_loc s loc), (List.map ctyp (list_of_and' t [])))))
       :: l
   | `Exception (_,_) -> assert false
   | `StExp (loc,e) -> (mkstr loc (Pstr_eval (expr e))) :: l
   | `External (loc,`Lid (sloc,n),t,sl) ->
       (mkstr loc
          (Pstr_primitive
             ((with_loc n sloc), (mkvalue_desc loc t (list_of_meta_list sl)))))
       :: l
   | `Include (loc,me) -> (mkstr loc (Pstr_include (module_expr me))) :: l
   | `Module (loc,`Uid (sloc,n),me) ->
       (mkstr loc (Pstr_module ((with_loc n sloc), (module_expr me)))) :: l
   | `RecModule (loc,mb) ->
       (mkstr loc (Pstr_recmodule (module_str_binding mb []))) :: l
   | `ModuleType (loc,`Uid (sloc,n),mt) ->
       (mkstr loc (Pstr_modtype ((with_loc n sloc), (module_type mt)))) :: l
   | `Open (loc,id) -> (mkstr loc (Pstr_open (long_uident id))) :: l
   | `Type (loc,tdl) -> (mkstr loc (Pstr_type (mktype_decl tdl))) :: l
   | `Value (loc,rf,bi) ->
       (mkstr loc (Pstr_value ((mkrf rf), (binding bi [])))) :: l
   | x -> errorf (loc_of x) "str_item : %s" (dump_str_item x) : structure )
and class_type (x : Ast.class_type) =
  match x with
  | `CtCon (loc,`ViNil _,id,tl) ->
      mkcty loc
        (Pcty_constr
           ((long_class_ident id), (List.map ctyp (list_of_com' tl []))))
  | `CtFun (loc,`Label (_,`Lid (_,lab),t),ct) ->
      mkcty loc (Pcty_fun (lab, (ctyp t), (class_type ct)))
  | `CtFun (loc,`TyOlb (loc1,`Lid (_,lab),t),ct) ->
      let t = `App (loc1, (predef_option loc1), t) in
      mkcty loc (Pcty_fun (("?" ^ lab), (ctyp t), (class_type ct)))
  | `CtFun (loc,t,ct) -> mkcty loc (Pcty_fun ("", (ctyp t), (class_type ct)))
  | `CtSig (loc,t_o,ctfl) ->
      let t = match t_o with | `Nil _loc -> `Any loc | t -> t in
      let cil = class_sig_item ctfl [] in
      mkcty loc
        (Pcty_signature
           { pcsig_self = (ctyp t); pcsig_fields = cil; pcsig_loc = loc })
  | x -> errorf (loc_of x) "class type: %s" (dump_class_type x)
and class_info_class_expr (ci : class_expr) =
  match ci with
  | `Eq (_,`CeCon (loc,vir,`Lid (nloc,name),params),ce) ->
      let (loc_params,(params,variance)) =
        match params with
        | `Nil _loc -> (loc, ([], []))
        | t -> ((loc_of t), (List.split (class_parameters t))) in
      {
        pci_virt = (mkvirtual vir);
        pci_params = (params, loc_params);
        pci_name = (with_loc name nloc);
        pci_expr = (class_expr ce);
        pci_loc = loc;
        pci_variance = variance
      }
  | ce -> errorf (loc_of ce) "class_info_class_expr: %s" (dump_class_expr ce)
and class_info_class_type (ci : class_type) =
  match ci with
  | `CtEq (_,`CtCon (loc,vir,`Lid (nloc,name),params),ct)
    |`CtCol (_,`CtCon (loc,vir,`Lid (nloc,name),params),ct) ->
      let (loc_params,(params,variance)) =
        match params with
        | `Nil _loc -> (loc, ([], []))
        | t -> ((loc_of t), (List.split (class_parameters t))) in
      {
        pci_virt = (mkvirtual vir);
        pci_params = (params, loc_params);
        pci_name = (with_loc name nloc);
        pci_expr = (class_type ct);
        pci_loc = loc;
        pci_variance = variance
      }
  | ct ->
      errorf (loc_of ct) "bad class/class type declaration/definition %s "
        (dump_class_type ct)
and class_sig_item (c : class_sig_item) (l : class_type_field list) =
  (match c with
   | `Nil _ -> l
   | `Eq (loc,t1,t2) -> (mkctf loc (Pctf_cstr ((ctyp t1), (ctyp t2)))) :: l
   | `Sem (_,csg1,csg2) -> class_sig_item csg1 (class_sig_item csg2 l)
   | `SigInherit (loc,ct) -> (mkctf loc (Pctf_inher (class_type ct))) :: l
   | `Method (loc,`Lid (_,s),pf,t) ->
       (mkctf loc (Pctf_meth (s, (mkprivate pf), (mkpolytype (ctyp t))))) ::
       l
   | `CgVal (loc,`Lid (_,s),b,v,t) ->
       (mkctf loc (Pctf_val (s, (mkmutable b), (mkvirtual v), (ctyp t)))) ::
       l
   | `CgVir (loc,`Lid (_,s),b,t) ->
       (mkctf loc (Pctf_virt (s, (mkprivate b), (mkpolytype (ctyp t))))) :: l
   | t -> errorf (loc_of t) "class_sig_item :%s" (dump_class_sig_item t) : 
  class_type_field list )
and class_expr (x : Ast.class_expr) =
  match x with
  | `CeApp (loc,_,_) as c ->
      let (ce,el) = ClassExpr.view_app [] c in
      let el = List.map label_expr el in
      mkcl loc (Pcl_apply ((class_expr ce), el))
  | `CeCon (loc,`ViNil _,id,tl) ->
      mkcl loc
        (Pcl_constr
           ((long_class_ident id), (List.map ctyp (list_of_com' tl []))))
  | `CeFun (loc,`Label (_,`Lid (_loc,lab),po),ce) ->
      mkcl loc
        (Pcl_fun (lab, None, (patt_of_lab loc lab po), (class_expr ce)))
  | `CeFun (loc,`PaOlbi (_,`Lid (_loc,lab),p,e),ce) ->
      let lab = paolab lab p in
      (match e with
       | `None ->
           mkcl loc
             (Pcl_fun
                (("?" ^ lab), None, (patt_of_lab loc lab p), (class_expr ce)))
       | `Some e ->
           mkcl loc
             (Pcl_fun
                (("?" ^ lab), (Some (expr e)), (patt p), (class_expr ce)))
       | `Ant (_loc,_) -> error _loc "antiquotation not expected here")
  | `CeFun (loc,p,ce) ->
      mkcl loc (Pcl_fun ("", None, (patt p), (class_expr ce)))
  | `CeLet (loc,rf,bi,ce) ->
      mkcl loc (Pcl_let ((mkrf rf), (binding bi []), (class_expr ce)))
  | `Obj (loc,po,cfl) ->
      let p = match po with | `Nil _loc -> `Any _loc | p -> p in
      let cil = class_str_item cfl [] in
      mkcl loc (Pcl_structure { pcstr_pat = (patt p); pcstr_fields = cil })
  | `CeTyc (loc,ce,ct) ->
      mkcl loc (Pcl_constraint ((class_expr ce), (class_type ct)))
  | t -> errorf (loc_of t) "class_expr: %s" (dump_class_expr t)
and class_str_item (c : class_str_item) l =
  match c with
  | `Nil _ -> l
  | `Eq (loc,t1,t2) -> (mkcf loc (Pcf_constr ((ctyp t1), (ctyp t2)))) :: l
  | `Sem (_,cst1,cst2) -> class_str_item cst1 (class_str_item cst2 l)
  | `Inherit (loc,ov,ce,pb) ->
      let opb =
        match pb with
        | `None -> None
        | `Some `Lid (_,x) -> Some x
        | `Some `Ant (_loc,_)|`Ant (_loc,_) ->
            error _loc "antiquotation not allowed here" in
      (mkcf loc (Pcf_inher ((override_flag loc ov), (class_expr ce), opb)))
        :: l
  | `Initializer (loc,e) -> (mkcf loc (Pcf_init (expr e))) :: l
  | `CrMth (loc,`Lid (sloc,s),ov,pf,e,t) ->
      let t = match t with | `Nil _ -> None | t -> Some (mkpolytype (ctyp t)) in
      let e = mkexp loc (Pexp_poly ((expr e), t)) in
      (mkcf loc
         (Pcf_meth
            ((with_loc s sloc), (mkprivate pf), (override_flag loc ov), e)))
        :: l
  | `CrVal (loc,`Lid (sloc,s),ov,mf,e) ->
      (mkcf loc
         (Pcf_val
            ((with_loc s sloc), (mkmutable mf), (override_flag loc ov),
              (expr e))))
      :: l
  | `CrVir (loc,`Lid (sloc,s),pf,t) ->
      (mkcf loc
         (Pcf_virt ((with_loc s sloc), (mkprivate pf), (mkpolytype (ctyp t)))))
      :: l
  | `CrVvr (loc,`Lid (sloc,s),mf,t) ->
      (mkcf loc (Pcf_valvirt ((with_loc s sloc), (mkmutable mf), (ctyp t))))
      :: l
  | x -> errorf (loc_of x) "class_str_item: %s" (dump_class_str_item x)
let sig_item (ast : sig_item) = (sig_item ast [] : signature )
let str_item ast = str_item ast []
let directive (x : expr) =
  match x with
  | `Nil _ -> Pdir_none
  | `Str (_,s) -> Pdir_string s
  | `Int (_,i) -> Pdir_int (int_of_string i)
  | `Id (_loc,`Lid (_,"true")) -> Pdir_bool true
  | `Id (_loc,`Lid (_,"false")) -> Pdir_bool false
  | e -> Pdir_ident (ident_noloc (ident_of_expr e))
let phrase (x : str_item) =
  match x with
  | `Directive (_,`Lid (_,d),dp) -> Ptop_dir (d, (directive dp))
  | `Directive (_,`Ant (_loc,_),_) -> error _loc "antiquotation not allowed"
  | si -> Ptop_def (str_item si)
open Format
let pp = fprintf
let print_expr f e = pp f "@[%a@]@." AstPrint.expression (expr e)
let print_patt f e = pp f "@[%a@]@." AstPrint.pattern (patt e)
let print_str_item f e = pp f "@[%a@]@." AstPrint.structure (str_item e)
let print_ctyp f e = pp f "@[%a@]@." AstPrint.core_type (ctyp e)