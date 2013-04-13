open Parsetree

open Longident

open Asttypes

open LibUtil

open FanUtil

open ParsetreeHelper

open FanLoc

open FanOps

open Ast

open AstLoc

open Objs

let _ = ()

let rec normalize_acc =
  function
  | (`Dot (_loc,i1,i2) : Ast.ident) ->
      (`Field (_loc, (normalize_acc i1), (normalize_acc i2)) : Ast.exp )
  | `Apply (_loc,i1,i2) ->
      (`App (_loc, (normalize_acc i1), (normalize_acc i2)) : Ast.exp )
  | `Ant (_loc,_)|(`Uid (_loc,_) : Ast.ident)|(`Lid (_loc,_) : Ast.ident) as
      i -> (i : Ast.exp )

let rec sep_dot_exp acc =
  (function
   | `Field (_,e1,e2) -> sep_dot_exp (sep_dot_exp acc e2) e1
   | `Dot (_l,_,_) as i -> sep_dot_exp acc (normalize_acc (i : vid  :>ident))
   | `Uid (loc,s) as e ->
       (match acc with
        | [] -> [(loc, [], e)]
        | (loc',sl,e)::l -> ((FanLoc.merge loc loc'), (s :: sl), e) :: l)
   | e -> ((loc_of e), [], e) :: acc : exp -> (loc * string list * exp) list )

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
    | (`Dot (_loc,`Lid (_,"*predef*"),`Lid (_,"option")) : Ast.ident) ->
        Some ((ldot (lident "*predef*") "option"), `lident)
    | (`Dot (_loc,i1,i2) : Ast.ident) -> self i2 (self i1 acc)
    | `Apply (_loc,i1,i2) ->
        (match ((self i1 None), (self i2 None), acc) with
         | (Some (l,_),Some (r,_),None ) -> Some ((Lapply (l, r)), `app)
         | _ -> errorf (loc_of i) "invalid long identifer %s" (dump_ident i))
    | (`Uid (_loc,s) : Ast.ident) ->
        (match (acc, s) with
         | (None ,"") -> None
         | (None ,s) -> Some ((lident s), `uident)
         | (Some (_,(`uident|`app)),"") -> acc
         | (Some (x,(`uident|`app)),s) -> Some ((ldot x s), `uident)
         | _ -> errorf (loc_of i) "invalid long identifier %s" (dump_ident i))
    | (`Lid (_loc,s) : Ast.ident) ->
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
  | _ -> errorf (loc_of i) "uppercase identifier expected %s" ""

let long_uident i = (long_uident_noloc i) +> (loc_of i)

let rec ctyp_long_id_prefix (t : ctyp) =
  (match t with
   | #ident' as i -> ident_noloc i
   | `App (_loc,m1,m2) ->
       let li1 = ctyp_long_id_prefix m1 in
       let li2 = ctyp_long_id_prefix m2 in Lapply (li1, li2)
   | t -> errorf (loc_of t) "invalid module expression %s" (dump_ctyp t) : 
  Longident.t )

let ctyp_long_id (t : ctyp) =
  (match t with
   | #ident' as i -> (false, (long_type_ident i))
   | `ClassPath (_,i) -> (true, (ident i))
   | t -> errorf (loc_of t) "invalid type %s" (dump_ctyp t) : (bool *
                                                                Longident.t
                                                                Location.loc) )

let predef_option loc =
  (`Dot (loc, (`Lid (loc, "*predef*")), (`Lid (loc, "option"))) : ctyp )

let rec ctyp (x : ctyp) =
  match x with
  | #ident' as i ->
      let li = long_type_ident (i :>ident) in
      let _loc = loc_of i in mktyp _loc (Ptyp_constr (li, []))
  | `Alias (_loc,t1,`Lid (_,s)) -> mktyp _loc (Ptyp_alias ((ctyp t1), s))
  | `Any _loc -> mktyp _loc Ptyp_any
  | `App (_loc,_,_) as f ->
      let (f,al) = view_app [] f in
      let (is_cls,li) = ctyp_long_id f in
      if is_cls
      then mktyp _loc (Ptyp_class (li, (List.map ctyp al), []))
      else mktyp _loc (Ptyp_constr (li, (List.map ctyp al)))
  | `Arrow (loc,`Label (_,`Lid (_,lab),t1),t2) ->
      mktyp loc (Ptyp_arrow (lab, (ctyp t1), (ctyp t2)))
  | `Arrow (loc,`OptLabl (loc1,`Lid (_,lab),t1),t2) ->
      let t1 = `App (loc1, (predef_option loc1), t1) in
      mktyp loc (Ptyp_arrow (("?" ^ lab), (ctyp t1), (ctyp t2)))
  | `Arrow (loc,t1,t2) -> mktyp loc (Ptyp_arrow ("", (ctyp t1), (ctyp t2)))
  | `TyObjEnd (_loc,row) ->
      let xs =
        match row with
        | `RvNil _ -> []
        | `RowVar _ -> [mkfield _loc Pfield_var]
        | `Ant _ -> error _loc "antiquotation not expected here" in
      mktyp _loc (Ptyp_object xs)
  | `TyObj (_loc,fl,row) ->
      let xs =
        match row with
        | `RvNil _ -> []
        | `RowVar _ -> [mkfield _loc Pfield_var]
        | `Ant _ -> error _loc "antiquotation not expected here" in
      mktyp _loc (Ptyp_object (meth_list fl xs))
  | `ClassPath (loc,id) -> mktyp loc (Ptyp_class ((ident id), [], []))
  | `Package (_loc,pt) ->
      let (i,cs) = package_type pt in mktyp _loc (Ptyp_package (i, cs))
  | `TyPolEnd (loc,t2) -> mktyp loc (Ptyp_poly ([], (ctyp t2)))
  | `TyPol (loc,t1,t2) ->
      let rec to_var_list =
        function
        | `App (_loc,t1,t2) -> (to_var_list t1) @ (to_var_list t2)
        | `Quote (_loc,`Normal _,`Lid (_,s))
          |`Quote (_loc,`Positive _,`Lid (_,s))
          |`Quote (_loc,`Negative _,`Lid (_,s)) -> [s]
        | _ -> assert false in
      mktyp loc (Ptyp_poly ((to_var_list t1), (ctyp t2)))
  | `Quote (_loc,`Normal _,`Lid (_,s)) -> mktyp _loc (Ptyp_var s)
  | `Par (loc,`Sta (_,t1,t2)) ->
      mktyp loc
        (Ptyp_tuple (List.map ctyp (list_of_star t1 (list_of_star t2 []))))
  | `PolyEq (_loc,t) ->
      mktyp _loc (Ptyp_variant ((row_field t []), true, None))
  | `PolySup (_loc,t) ->
      mktyp _loc (Ptyp_variant ((row_field t []), false, None))
  | `PolyInf (_loc,t) ->
      mktyp _loc (Ptyp_variant ((row_field t []), true, (Some [])))
  | `PolyInfSup (_loc,t,t') ->
      let rec name_tags (x : tag_names) =
        match x with
        | `App (_,t1,t2) -> (name_tags t1) @ (name_tags t2)
        | `TyVrn (_,`C (_,s)) -> [s]
        | _ -> assert false in
      mktyp _loc
        (Ptyp_variant ((row_field t []), true, (Some (name_tags t'))))
  | x -> errorf (loc_of x) "ctyp: %s" (dump_ctyp x)
and row_field (x : row_field) acc =
  match x with
  | `TyVrn (_loc,`C (_,i)) -> (Rtag (i, true, [])) :: acc
  | `TyVrnOf (_loc,`C (_,i),t) -> (Rtag (i, false, [ctyp t])) :: acc
  | `Bar (_loc,t1,t2) -> row_field t1 (row_field t2 acc)
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"
  | `Ctyp (_,t) -> (Rinherit (ctyp t)) :: acc
  | t -> errorf (loc_of t) "row_field: %s" (dump_row_field t)
and meth_list (fl : name_ctyp) acc =
  (match fl with
   | `Sem (_loc,t1,t2) -> meth_list t1 (meth_list t2 acc)
   | `TyCol (_loc,`Lid (_,lab),t) ->
       (mkfield _loc (Pfield (lab, (mkpolytype (ctyp t))))) :: acc
   | x -> errorf (loc_of x) "meth_list: %s" (dump_name_ctyp x) : core_field_type
                                                                   list )
and package_type_constraints (wc : constr)
  (acc : (Longident.t Asttypes.loc * core_type) list) =
  (match wc with
   | `TypeEq (_loc,(#ident' as id),ct) -> ((ident id), (ctyp ct)) :: acc
   | `And (_loc,wc1,wc2) ->
       package_type_constraints wc1 (package_type_constraints wc2 acc)
   | x ->
       errorf (loc_of x) "unexpected `with constraint:%s' for a package type"
         (dump_constr x) : (Longident.t Asttypes.loc * core_type) list )
and package_type (x : mtyp) =
  match x with
  | (`With (_loc,(#ident' as i),wc) : mtyp) ->
      ((long_uident i), (package_type_constraints wc []))
  | #ident' as i -> ((long_uident i), [])
  | mt -> errorf (loc_of mt) "unexpected package type: %s" (dump_mtyp mt)

let mktype loc tl cl ~type_kind  ~priv  ~manifest  =
  let (params,variance) = List.split tl in
  {
    ptype_params = params;
    ptype_cstrs = cl;
    ptype_kind = type_kind;
    ptype_private = priv;
    ptype_manifest = manifest;
    ptype_loc = loc;
    ptype_variance = variance
  }

let mkprivate' m = if m then Private else Public

let mkprivate (x : private_flag) =
  match x with
  | `Private _ -> Private
  | `PrNil _ -> Public
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"

let mktrecord (x : name_ctyp) =
  match x with
  | `TyColMut (_loc,`Lid (sloc,s),t) ->
      ((with_loc s sloc), Mutable, (mkpolytype (ctyp t)), _loc)
  | `TyCol (_loc,`Lid (sloc,s),t) ->
      ((with_loc s sloc), Immutable, (mkpolytype (ctyp t)), _loc)
  | t -> errorf (loc_of t) "mktrecord %s " (dump_name_ctyp t)

let mkvariant (x : or_ctyp) =
  match x with
  | `Uid (sloc,s) -> ((with_loc s sloc), [], None, sloc)
  | `Of (_loc,`Uid (sloc,s),t) ->
      ((with_loc s sloc), (List.map ctyp (list_of_star t [])), None, _loc)
  | `TyCol (_loc,`Uid (sloc,s),`Arrow (_,t,u)) ->
      ((with_loc s sloc), (List.map ctyp (list_of_star t [])),
        (Some (ctyp u)), _loc)
  | `TyCol (_loc,`Uid (sloc,s),t) ->
      ((with_loc s sloc), [], (Some (ctyp t)), _loc)
  | t -> errorf (loc_of t) "mkvariant %s " (dump_or_ctyp t)

let type_kind (x : type_repr) =
  match x with
  | `Record (_loc,t) -> Ptype_record (List.map mktrecord (list_of_sem t []))
  | `Sum (_loc,t) -> Ptype_variant (List.map mkvariant (list_of_or t []))
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"

let mkvalue_desc loc t (p : strings list) =
  let ps =
    List.map
      (fun p  ->
         match p with | `Str (_,p) -> p | _ -> failwithf "mkvalue_desc") p in
  { pval_type = (ctyp t); pval_prim = ps; pval_loc = loc }

let mkmutable (x : mutable_flag) =
  match x with
  | `Mutable _ -> Mutable
  | `MuNil _ -> Immutable
  | `Ant (_loc,_) -> error _loc "antiquotation not expected here"

let paolab (lab : string) (p : pat) =
  (match (lab, p) with
   | ("",(`Lid (_loc,i)|`Constraint (_loc,`Lid (_,i),_))) -> i
   | ("",p) -> errorf (loc_of p) "paolab %s" (dump_pat p)
   | _ -> lab : string )

let quote_map x =
  match x with
  | `Quote (_loc,p,`Lid (sloc,s)) ->
      let tuple =
        match p with
        | `Positive _ -> (true, false)
        | `Negative _ -> (false, true)
        | `Normal _ -> (false, false)
        | `Ant (_loc,_) -> error _loc "antiquotation not expected here" in
      ((Some (s +> sloc)), tuple)
  | `QuoteAny (_loc,p) ->
      let tuple =
        match p with
        | `Positive _ -> (true, false)
        | `Negative _ -> (false, true)
        | `Normal _ -> (false, false)
        | `Ant (_loc,_) -> error _loc "antiquotation not expected here" in
      (None, tuple)
  | _ -> errorf (loc_of x) "quote_map %s" (dump_ctyp x)

let optional_type_parameters (t : ctyp) =
  List.map quote_map (list_of_app t [])

let mk_type_parameters (tl : opt_decl_params) =
  (match tl with
   | `None _ -> []
   | `Some (_,x) ->
       let xs = list_of_com x [] in
       List.map
         (function
          | #decl_param as x -> quote_map (x :>ctyp)
          | _ -> assert false) xs : (string Asttypes.loc option * (bool *
                                      bool)) list )

let class_parameters (t : type_parameters) =
  List.filter_map
    (function
     | `Ctyp (_,x) ->
         (match quote_map x with
          | (Some x,v) -> Some (x, v)
          | (None ,_) ->
              errorf (loc_of t) "class_parameters %s"
                (dump_type_parameters t))
     | _ -> errorf (loc_of t) "class_parameters %s" (dump_type_parameters t))
    (list_of_com t [])

let type_parameters_and_type_name (t : ctyp) =
  let rec aux (t : ctyp) acc =
    match t with
    | `App (_loc,t1,t2) ->
        aux (t1 :>ctyp) ((optional_type_parameters (t2 :>ctyp)) @ acc)
    | #ident' as i -> ((ident i), acc)
    | x -> errorf (loc_of x) "type_parameters_and_type_name %s" (dump_ctyp x) in
  aux t []

let rec pat_fa (al : pat list) (x : pat) =
  match x with | `App (_,f,a) -> pat_fa (a :: al) f | f -> (f, al)

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

let rec pat (x : pat) =
  match x with
  | `Lid (_loc,("true"|"false" as txt)) ->
      let p =
        Ppat_construct ({ txt = (Lident txt); loc = _loc }, None, false) in
      mkpat _loc p
  | `Lid (sloc,s) -> mkpat sloc (Ppat_var (with_loc s sloc))
  | `Uid (_loc,_)|`Dot (_loc,_,_) as i ->
      let p = Ppat_construct ((long_uident (i : vid  :>ident)), None, false) in
      mkpat _loc p
  | (`Alias (_loc,p1,x) : Ast.pat) ->
      (match x with
       | `Lid (sloc,s) ->
           mkpat _loc (Ppat_alias ((pat p1), (with_loc s sloc)))
       | `Ant (_loc,_) -> error _loc "invalid antiquotations")
  | `Ant (loc,_) -> error loc "antiquotation not allowed here"
  | (`Any _loc : Ast.pat) -> mkpat _loc Ppat_any
  | (`App (_loc,`Uid (sloc,s),`Par (_,`Any loc_any)) : Ast.pat) ->
      mkpat _loc
        (Ppat_construct
           ((lident_with_loc s sloc), (Some (mkpat loc_any Ppat_any)), false))
  | `App (loc,_,_) as f ->
      let (f,al) = pat_fa [] f in
      let al = List.map pat al in
      (match (pat f).ppat_desc with
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
  | (`Array (_loc,p) : Ast.pat) ->
      mkpat _loc (Ppat_array (List.map pat (list_of_sem p [])))
  | (`ArrayEmpty _loc : Ast.pat) -> mkpat _loc (Ppat_array [])
  | (`Chr (_loc,s) : Ast.pat) ->
      mkpat _loc (Ppat_constant (Const_char (char_of_char_token _loc s)))
  | (`Int (_loc,s) : Ast.pat) ->
      let i =
        try int_of_string s
        with
        | Failure _ ->
            error _loc
              "Integer literal exceeds the range of representable integers of type int" in
      mkpat _loc (Ppat_constant (Const_int i))
  | (`Int32 (_loc,s) : Ast.pat) ->
      let i32 =
        try Int32.of_string s
        with
        | Failure _ ->
            error _loc
              "Integer literal exceeds the range of representable integers of type int32" in
      mkpat _loc (Ppat_constant (Const_int32 i32))
  | (`Int64 (_loc,s) : Ast.pat) ->
      let i64 =
        try Int64.of_string s
        with
        | Failure _ ->
            error _loc
              "Integer literal exceeds the range of representable integers of type int64" in
      mkpat _loc (Ppat_constant (Const_int64 i64))
  | (`Nativeint (_loc,s) : Ast.pat) ->
      let nati =
        try Nativeint.of_string s
        with
        | Failure _ ->
            error _loc
              "Integer literal exceeds the range of representable integers of type nativeint" in
      mkpat _loc (Ppat_constant (Const_nativeint nati))
  | (`Flo (_loc,s) : Ast.pat) ->
      mkpat _loc (Ppat_constant (Const_float (remove_underscores s)))
  | (`Bar (_loc,p1,p2) : Ast.pat) ->
      mkpat _loc (Ppat_or ((pat p1), (pat p2)))
  | (`Str (_loc,s) : Ast.pat) ->
      mkpat _loc
        (Ppat_constant (Const_string (string_of_string_token _loc s)))
  | (`PaRng (_loc,p1,p2) : Ast.pat) ->
      (match (p1, p2) with
       | (`Chr (loc1,c1),`Chr (loc2,c2)) ->
           let c1 = char_of_char_token loc1 c1 in
           let c2 = char_of_char_token loc2 c2 in mkrangepat _loc c1 c2
       | _ -> error _loc "range pattern allowed only for characters")
  | `Label (loc,_,_)|`LabelS (loc,_)|`OptLablExpr (loc,_,_,_)
    |`OptLabl (loc,_,_)|`OptLablS (loc,_) ->
      error loc "labeled pattern not allowed here"
  | `Record (loc,p) ->
      let ps = list_of_sem p [] in
      let (wildcards,ps) =
        List.partition (function | `Any _ -> true | _ -> false) ps in
      let is_closed = if wildcards = [] then Closed else Open in
      let mklabpat (p : rec_pat) =
        match p with
        | `RecBind (_loc,i,p) -> ((ident i), (pat p))
        | p -> error (loc_of p) "invalid pattern" in
      mkpat loc (Ppat_record ((List.map mklabpat ps), is_closed))
  | (`Par (_loc,`Com (_,p1,p2)) : Ast.pat) ->
      mkpat _loc
        (Ppat_tuple (List.map pat (list_of_com p1 (list_of_com p2 []))))
  | `Par (loc,_) -> error loc "singleton tuple pattern"
  | (`Constraint (_loc,p,t) : Ast.pat) ->
      mkpat _loc (Ppat_constraint ((pat p), (ctyp t)))
  | (`ClassPath (_loc,i) : Ast.pat) ->
      mkpat _loc (Ppat_type (long_type_ident i))
  | (`Vrn (_loc,s) : Ast.pat) -> mkpat _loc (Ppat_variant (s, None))
  | (`Lazy (_loc,p) : Ast.pat) -> mkpat _loc (Ppat_lazy (pat p))
  | (`ModuleUnpack (_loc,`Uid (sloc,m)) : Ast.pat) ->
      mkpat _loc (Ppat_unpack (with_loc m sloc))
  | `ModuleConstraint (loc,`Uid (sloc,m),ty) ->
      mkpat loc
        (Ppat_constraint
           ((mkpat sloc (Ppat_unpack (with_loc m sloc))), (ctyp ty)))
  | p -> errorf (loc_of p) "invalid pattern %s" (dump_pat p)

let override_flag loc (x : override_flag) =
  match x with
  | `Override _ -> Override
  | `OvNil _ -> Fresh
  | _ -> error loc "antiquotation not allowed here"

let rec exp (x : exp) =
  match x with
  | `Field (_loc,_,_)|`Dot (_loc,_,_) ->
      let (e,l) =
        match sep_dot_exp [] x with
        | (loc,ml,`Uid (sloc,s))::l ->
            ((mkexp loc (Pexp_construct ((mkli sloc s ml), None, false))), l)
        | (loc,ml,`Lid (sloc,s))::l ->
            ((mkexp loc (Pexp_ident (mkli sloc s ml))), l)
        | (_,[],e)::l -> ((exp e), l)
        | _ -> errorf (loc_of x) "exp: %s" (dump_exp x) in
      let (_,e) =
        List.fold_left
          (fun (loc_bp,e1)  (loc_ep,ml,e2)  ->
             match e2 with
             | `Lid (sloc,s) ->
                 let loc = FanLoc.merge loc_bp loc_ep in
                 (loc, (mkexp loc (Pexp_field (e1, (mkli sloc s ml)))))
             | _ -> error (loc_of e2) "lowercase identifier expected")
          (_loc, e) l in
      e
  | `App (loc,_,_) as f ->
      let (f,al) = view_app [] f in
      let al = List.map label_exp al in
      (match (exp f).pexp_desc with
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
       | _ -> mkexp loc (Pexp_apply ((exp f), al)))
  | `ArrayDot (loc,e1,e2) ->
      mkexp loc
        (Pexp_apply
           ((mkexp loc (Pexp_ident (array_function loc "Array" "get"))),
             [("", (exp e1)); ("", (exp e2))]))
  | `Array (loc,e) ->
      mkexp loc (Pexp_array (List.map exp (list_of_sem e [])))
  | `ArrayEmpty loc -> mkexp loc (Pexp_array [])
  | (`Assert (_loc,`Lid (_,"false")) : Ast.exp) ->
      mkexp _loc Pexp_assertfalse
  | `Assert (_loc,e) -> mkexp _loc (Pexp_assert (exp e))
  | `Assign (loc,e,v) ->
      let e =
        match e with
        | (`Field (loc,x,`Lid (_,"contents")) : Ast.exp) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (lident_with_loc ":=" loc))),
                [("", (exp x)); ("", (exp v))])
        | `Field (loc,_,_) ->
            (match (exp e).pexp_desc with
             | Pexp_field (e,lab) -> Pexp_setfield (e, lab, (exp v))
             | _ -> error loc "bad record access")
        | `ArrayDot (loc,e1,e2) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (array_function loc "Array" "set"))),
                [("", (exp e1)); ("", (exp e2)); ("", (exp v))])
        | `Lid (lloc,lab) -> Pexp_setinstvar ((with_loc lab lloc), (exp v))
        | `StringDot (loc,e1,e2) ->
            Pexp_apply
              ((mkexp loc (Pexp_ident (array_function loc "String" "set"))),
                [("", (exp e1)); ("", (exp e2)); ("", (exp v))])
        | x -> errorf loc "bad left part of assignment:%s" (dump_exp x) in
      mkexp loc e
  | `Chr (loc,s) ->
      mkexp loc (Pexp_constant (Const_char (char_of_char_token loc s)))
  | `Subtype (loc,e,t2) ->
      mkexp loc (Pexp_constraint ((exp e), None, (Some (ctyp t2))))
  | `Coercion (loc,e,t1,t2) ->
      let t1 = Some (ctyp t1) in
      mkexp loc (Pexp_constraint ((exp e), t1, (Some (ctyp t2))))
  | `Flo (loc,s) ->
      mkexp loc (Pexp_constant (Const_float (remove_underscores s)))
  | `For (loc,`Lid (sloc,i),e1,e2,df,el) ->
      let e3 = `Seq (loc, el) in
      mkexp loc
        (Pexp_for
           ((with_loc i sloc), (exp e1), (exp e2), (mkdirection df),
             (exp e3)))
  | `Fun (loc,`Case (_,`LabelS (_,`Lid (sloc,lab)),e)) ->
      mkexp loc
        (Pexp_function (lab, None, [((pat (`Lid (sloc, lab))), (exp e))]))
  | `Fun (loc,`Case (_,`Label (_,`Lid (_,lab),po),e)) ->
      mkexp loc (Pexp_function (lab, None, [((pat po), (exp e))]))
  | `Fun (loc,`CaseWhen (_,`LabelS (_,`Lid (sloc,lab)),w,e)) ->
      mkexp loc
        (Pexp_function
           (lab, None,
             [((pat (`Lid (sloc, lab))),
                (mkexp (loc_of w) (Pexp_when ((exp w), (exp e)))))]))
  | `Fun (loc,`CaseWhen (_,`Label (_,`Lid (_,lab),po),w,e)) ->
      mkexp loc
        (Pexp_function
           (lab, None,
             [((pat po), (mkexp (loc_of w) (Pexp_when ((exp w), (exp e)))))]))
  | `Fun (loc,`Case (_,`OptLablS (_,`Lid (sloc,lab)),e2)) ->
      mkexp loc
        (Pexp_function
           (("?" ^ lab), None, [((pat (`Lid (sloc, lab))), (exp e2))]))
  | `Fun (loc,`Case (_,`OptLabl (_,`Lid (_,lab),p),e2)) ->
      let lab = paolab lab p in
      mkexp loc (Pexp_function (("?" ^ lab), None, [((pat p), (exp e2))]))
  | `Fun (loc,`CaseWhen (_,`OptLablS (_,`Lid (sloc,lab)),w,e2)) ->
      mkexp loc
        (Pexp_function
           (("?" ^ lab), None,
             [((pat (`Lid (sloc, lab))),
                (mkexp (loc_of w) (Pexp_when ((exp w), (exp e2)))))]))
  | `Fun (loc,`CaseWhen (_,`OptLabl (_,`Lid (_,lab),p),w,e2)) ->
      let lab = paolab lab p in
      mkexp loc
        (Pexp_function
           (("?" ^ lab), None,
             [((pat p), (mkexp (loc_of w) (Pexp_when ((exp w), (exp e2)))))]))
  | `Fun (loc,`Case (_,`OptLablExpr (_,`Lid (_,lab),p,e1),e2)) ->
      let lab = paolab lab p in
      mkexp loc
        (Pexp_function (("?" ^ lab), (Some (exp e1)), [((pat p), (exp e2))]))
  | `Fun (loc,`CaseWhen (_,`OptLablExpr (_,`Lid (_,lab),p,e1),w,e2)) ->
      let lab = paolab lab p in
      mkexp loc
        (Pexp_function
           (("?" ^ lab), (Some (exp e1)),
             [((pat p), (mkexp (loc_of w) (Pexp_when ((exp w), (exp e2)))))]))
  | `Fun (loc,a) -> mkexp loc (Pexp_function ("", None, (case a)))
  | `IfThenElse (loc,e1,e2,e3) ->
      mkexp loc (Pexp_ifthenelse ((exp e1), (exp e2), (Some (exp e3))))
  | `IfThen (loc,e1,e2) ->
      mkexp loc (Pexp_ifthenelse ((exp e1), (exp e2), None))
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
  | `Nativeint (loc,s) ->
      let nati =
        try Nativeint.of_string s
        with
        | Failure _ ->
            error loc
              "Integer literal exceeds the range of representable integers of type nativeint" in
      mkexp loc (Pexp_constant (Const_nativeint nati))
  | `Any _loc ->
      errorf _loc "Any should not appear in the position of expression"
  | `Label (loc,_,_)|`LabelS (loc,_) ->
      error loc "labeled expression not allowed here"
  | `Lazy (loc,e) -> mkexp loc (Pexp_lazy (exp e))
  | `LetIn (loc,rf,bi,e) ->
      mkexp loc (Pexp_let ((mkrf rf), (binding bi []), (exp e)))
  | `LetTryInWith (_loc,rf,bi,e,cas) ->
      let cas =
        let rec f x =
          match x with
          | `Case (_loc,p,e) ->
              `Case
                (_loc, p,
                  (`Fun (_loc, (`Case (_loc, (`Uid (_loc, "()")), e)))))
          | `CaseWhen (_loc,p,c,e) ->
              `CaseWhen
                (_loc, p, c,
                  (`Fun (_loc, (`Case (_loc, (`Uid (_loc, "()")), e)))))
          | `Bar (_loc,a1,a2) -> `Bar (_loc, (f a1), (f a2))
          | `Ant (_loc,_) -> error _loc "antiquotation not expected here" in
        f cas in
      exp
        (`App
           (_loc,
             (`Try
                (_loc,
                  (`LetIn
                     (_loc, rf, bi,
                       (`Fun (_loc, (`Case (_loc, (`Uid (_loc, "()")), e)))))),
                  cas)), (`Uid (_loc, "()"))) : Ast.exp )
  | `LetModule (loc,`Uid (sloc,i),me,e) ->
      mkexp loc (Pexp_letmodule ((with_loc i sloc), (mexp me), (exp e)))
  | `Match (loc,e,a) -> mkexp loc (Pexp_match ((exp e), (case a)))
  | `New (loc,id) -> mkexp loc (Pexp_new (long_type_ident id))
  | `ObjEnd loc ->
      mkexp loc
        (Pexp_object { pcstr_pat = (pat (`Any loc)); pcstr_fields = [] })
  | `Obj (loc,cfl) ->
      let p = `Any loc in
      let cil = clfield cfl [] in
      mkexp loc (Pexp_object { pcstr_pat = (pat p); pcstr_fields = cil })
  | `ObjPatEnd (loc,p) ->
      mkexp loc (Pexp_object { pcstr_pat = (pat p); pcstr_fields = [] })
  | `ObjPat (loc,p,cfl) ->
      let cil = clfield cfl [] in
      mkexp loc (Pexp_object { pcstr_pat = (pat p); pcstr_fields = cil })
  | `OvrInstEmpty loc -> mkexp loc (Pexp_override [])
  | `OvrInst (loc,iel) ->
      let rec mkideexp (x : rec_exp) acc =
        match x with
        | `Sem (_,x,y) -> mkideexp x (mkideexp y acc)
        | `RecBind (_,`Lid (sloc,s),e) -> ((with_loc s sloc), (exp e)) :: acc
        | _ -> assert false in
      mkexp loc (Pexp_override (mkideexp iel []))
  | `Record (loc,lel) -> mkexp loc (Pexp_record ((mklabexp lel), None))
  | `RecordWith (loc,lel,eo) ->
      mkexp loc (Pexp_record ((mklabexp lel), (Some (exp eo))))
  | `Seq (_loc,e) ->
      let rec loop =
        function
        | [] -> exp (`Uid (_loc, "()") : Ast.exp )
        | e::[] -> exp e
        | e::el ->
            let _loc = FanLoc.merge (loc_of e) _loc in
            mkexp _loc (Pexp_sequence ((exp e), (loop el))) in
      loop (list_of_sem e [])
  | `Send (loc,e,`Lid (_,s)) -> mkexp loc (Pexp_send ((exp e), s))
  | `StringDot (loc,e1,e2) ->
      mkexp loc
        (Pexp_apply
           ((mkexp loc (Pexp_ident (array_function loc "String" "get"))),
             [("", (exp e1)); ("", (exp e2))]))
  | `Str (loc,s) ->
      mkexp loc (Pexp_constant (Const_string (string_of_string_token loc s)))
  | `Try (loc,e,a) -> mkexp loc (Pexp_try ((exp e), (case a)))
  | `Par (loc,e) ->
      let l = list_of_com e [] in
      (match l with
       | []|_::[] ->
           errorf loc "tuple should have at least two items" (dump_exp x)
       | _ -> mkexp loc (Pexp_tuple (List.map exp l)))
  | `Constraint (loc,e,t) ->
      mkexp loc (Pexp_constraint ((exp e), (Some (ctyp t)), None))
  | `Uid (_loc,"()") ->
      mkexp _loc (Pexp_construct ((lident_with_loc "()" _loc), None, true))
  | `Lid (_loc,("true"|"false" as s)) ->
      mkexp _loc (Pexp_construct ((lident_with_loc s _loc), None, true))
  | `Lid (_loc,s) -> mkexp _loc (Pexp_ident (lident_with_loc s _loc))
  | `Uid (_loc,s) ->
      mkexp _loc (Pexp_construct ((lident_with_loc s _loc), None, true))
  | `Vrn (loc,s) -> mkexp loc (Pexp_variant (s, None))
  | `While (loc,e1,el) ->
      let e2 = `Seq (loc, el) in mkexp loc (Pexp_while ((exp e1), (exp e2)))
  | `LetOpen (_loc,i,e) -> mkexp _loc (Pexp_open ((long_uident i), (exp e)))
  | `Package_exp (_loc,`Constraint (_,me,pt)) ->
      mkexp _loc
        (Pexp_constraint
           ((mkexp _loc (Pexp_pack (mexp me))),
             (Some (mktyp _loc (Ptyp_package (package_type pt)))), None))
  | `Package_exp (loc,me) -> mkexp loc (Pexp_pack (mexp me))
  | `LocalTypeFun (loc,`Lid (_,i),e) -> mkexp loc (Pexp_newtype (i, (exp e)))
  | x -> errorf (loc_of x) "exp:%s" (dump_exp x)
and label_exp (x : exp) =
  match x with
  | `Label (_loc,`Lid (_,lab),eo) -> (lab, (exp eo))
  | `LabelS (_loc,`Lid (sloc,lab)) -> (lab, (exp (`Lid (sloc, lab))))
  | `OptLabl (_loc,`Lid (_,lab),eo) -> (("?" ^ lab), (exp eo))
  | `OptLablS (loc,`Lid (_,lab)) -> (("?" ^ lab), (exp (`Lid (loc, lab))))
  | e -> ("", (exp e))
and binding (x : binding) acc =
  match x with
  | (`And (_loc,x,y) : Ast.binding) -> binding x (binding y acc)
  | (`Bind
       (_loc,(`Lid (sloc,bind_name) : Ast.pat),`Constraint
                                                 (_,e,`TyTypePol (_,vs,ty)))
      : Ast.binding) ->
      let rec id_to_string (x : ctyp) =
        match x with
        | `Lid (_,x) -> [x]
        | `App (_loc,x,y) -> (id_to_string x) @ (id_to_string y)
        | x -> errorf (loc_of x) "id_to_string %s" (dump_ctyp x) in
      let vars = id_to_string vs in
      let ampersand_vars = List.map (fun x  -> "&" ^ x) vars in
      let ty' = varify_constructors vars (ctyp ty) in
      let mkexp = mkexp _loc in
      let mkpat = mkpat _loc in
      let e = mkexp (Pexp_constraint ((exp e), (Some (ctyp ty)), None)) in
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
  | (`Bind (_loc,p,`Constraint (_,e,`TyPol (_,vs,ty))) : Ast.binding) ->
      ((pat (`Constraint (_loc, p, (`TyPol (_loc, vs, ty))))), (exp e)) ::
      acc
  | (`Bind (_loc,p,e) : Ast.binding) -> ((pat p), (exp e)) :: acc
  | _ -> assert false
and case (x : case) =
  let cases = list_of_or x [] in
  List.filter_map
    (function
     | (`Case (_loc,p,e) : Ast.case) -> Some ((pat p), (exp e))
     | (`CaseWhen (_loc,p,w,e) : Ast.case) ->
         Some ((pat p), (mkexp (loc_of w) (Pexp_when ((exp w), (exp e)))))
     | x -> errorf (loc_of x) "case %s" (dump_case x)) cases
and mklabexp (x : rec_exp) =
  let bindings = list_of_sem x [] in
  List.filter_map
    (function
     | (`RecBind (_loc,i,e) : Ast.rec_exp) -> Some ((ident i), (exp e))
     | x -> errorf (loc_of x) "mklabexp : %s" (dump_rec_exp x)) bindings
and mktype_decl (x : typedecl) =
  let type_decl tl cl loc (x : type_info) =
    match x with
    | `TyMan (_,t1,p,t2) ->
        mktype loc tl cl ~type_kind:(type_kind t2) ~priv:(mkprivate p)
          ~manifest:(Some (ctyp t1))
    | `TyRepr (_,p1,repr) ->
        mktype loc tl cl ~type_kind:(type_kind repr) ~priv:(mkprivate p1)
          ~manifest:None
    | `TyEq (_loc,p1,t1) ->
        mktype loc tl cl ~type_kind:Ptype_abstract ~priv:(mkprivate p1)
          ~manifest:(Some (ctyp t1))
    | `Ant (_loc,_) -> error _loc "antiquotation not expected here" in
  let tys = list_of_and x [] in
  List.map
    (function
     | `TyDcl (cloc,`Lid (sloc,c),tl,td,cl) ->
         let cl =
           match cl with
           | `None _ -> []
           | `Some (_,cl) ->
               (list_of_and cl []) |>
                 (List.map
                    (function
                     | `Eq (loc,t1,t2) -> ((ctyp t1), (ctyp t2), loc)
                     | _ ->
                         errorf (loc_of x) "invalid constraint: %s"
                           (dump_type_constr cl))) in
         ((c +> sloc), (type_decl (mk_type_parameters tl) cl cloc td))
     | `TyAbstr (cloc,`Lid (sloc,c),tl,cl) ->
         let cl =
           match cl with
           | `None _ -> []
           | `Some (_,cl) ->
               (list_of_and cl []) |>
                 (List.map
                    (function
                     | `Eq (loc,t1,t2) -> ((ctyp t1), (ctyp t2), loc)
                     | _ ->
                         errorf (loc_of x) "invalid constraint: %s"
                           (dump_type_constr cl))) in
         ((c +> sloc),
           (mktype cloc (mk_type_parameters tl) cl ~type_kind:Ptype_abstract
              ~priv:Private ~manifest:None))
     | (t : typedecl) -> errorf (loc_of t) "mktype_decl %s" (dump_typedecl t))
    tys
and mtyp: Ast.mtyp -> Parsetree.module_type =
  let mkwithc (wc : constr) =
    let mkwithtyp pwith_type loc priv id_tpl ct =
      let (id,tpl) = type_parameters_and_type_name id_tpl in
      let (params,variance) = List.split tpl in
      (id,
        (pwith_type
           {
             ptype_params = params;
             ptype_cstrs = [];
             ptype_kind = Ptype_abstract;
             ptype_private = priv;
             ptype_manifest = (Some (ctyp ct));
             ptype_loc = loc;
             ptype_variance = variance
           })) in
    let constrs = list_of_and wc [] in
    List.filter_map
      (function
       | `TypeEq (_loc,id_tpl,ct) ->
           Some (mkwithtyp (fun x  -> Pwith_type x) _loc Public id_tpl ct)
       | `TypeEqPriv (_loc,id_tpl,ct) ->
           Some (mkwithtyp (fun x  -> Pwith_type x) _loc Private id_tpl ct)
       | `ModuleEq (_loc,i1,i2) ->
           Some ((long_uident i1), (Pwith_module (long_uident i2)))
       | `TypeSubst (_loc,id_tpl,ct) ->
           Some
             (mkwithtyp (fun x  -> Pwith_typesubst x) _loc Public id_tpl ct)
       | `ModuleSubst (_loc,i1,i2) ->
           Some ((long_uident i1), (Pwith_modsubst (long_uident i2)))
       | t ->
           errorf (loc_of t) "bad with constraint (antiquotation) : %s"
             (dump_constr t)) constrs in
  function
  | #ident' as i ->
      let loc = loc_of i in mkmty loc (Pmty_ident (long_uident i))
  | `Functor (loc,`Uid (sloc,n),nt,mt) ->
      mkmty loc (Pmty_functor ((with_loc n sloc), (mtyp nt), (mtyp mt)))
  | `Sig (loc,sl) -> mkmty loc (Pmty_signature (sigi sl []))
  | `SigEnd loc -> mkmty loc (Pmty_signature [])
  | `With (loc,mt,wc) -> mkmty loc (Pmty_with ((mtyp mt), (mkwithc wc)))
  | `ModuleTypeOf (_loc,me) -> mkmty _loc (Pmty_typeof (mexp me))
  | t -> errorf (loc_of t) "mtyp: %s" (dump_mtyp t)
and sigi (s : sigi) (l : signature) =
  (match s with
   | `Class (loc,cd) ->
       (mksig loc
          (Psig_class (List.map class_info_cltyp (list_of_and cd []))))
       :: l
   | `ClassType (loc,ctd) ->
       (mksig loc
          (Psig_class_type (List.map class_info_cltyp (list_of_and ctd []))))
       :: l
   | `Sem (_,sg1,sg2) -> sigi sg1 (sigi sg2 l)
   | `Directive _|`DirectiveSimple _ -> l
   | `Exception (_loc,`Uid (_,s)) ->
       (mksig _loc (Psig_exception ((with_loc s _loc), []))) :: l
   | `Exception (_loc,`Of (_,`Uid (sloc,s),t)) ->
       (mksig _loc
          (Psig_exception
             ((with_loc s sloc), (List.map ctyp (list_of_star t [])))))
       :: l
   | `Exception (_,_) -> assert false
   | `External (loc,`Lid (sloc,n),t,sl) ->
       (mksig loc
          (Psig_value
             ((with_loc n sloc), (mkvalue_desc loc t (list_of_app sl [])))))
       :: l
   | `Include (loc,mt) -> (mksig loc (Psig_include (mtyp mt))) :: l
   | `Module (loc,`Uid (sloc,n),mt) ->
       (mksig loc (Psig_module ((with_loc n sloc), (mtyp mt)))) :: l
   | `RecModule (loc,mb) ->
       (mksig loc (Psig_recmodule (module_sig_binding mb []))) :: l
   | `ModuleTypeEnd (loc,`Uid (sloc,n)) ->
       (mksig loc (Psig_modtype ((with_loc n sloc), Pmodtype_abstract))) :: l
   | `ModuleType (loc,`Uid (sloc,n),mt) ->
       let si = Pmodtype_manifest (mtyp mt) in
       (mksig loc (Psig_modtype ((with_loc n sloc), si))) :: l
   | `Open (loc,id) -> (mksig loc (Psig_open (long_uident id))) :: l
   | `Type (loc,tdl) -> (mksig loc (Psig_type (mktype_decl tdl))) :: l
   | `Val (loc,`Lid (sloc,n),t) ->
       (mksig loc (Psig_value ((with_loc n sloc), (mkvalue_desc loc t []))))
       :: l
   | t -> errorf (loc_of t) "sigi: %s" (dump_sigi t) : signature )
and module_sig_binding (x : mbind)
  (acc : (string Asttypes.loc * Parsetree.module_type) list) =
  match x with
  | `And (_,x,y) -> module_sig_binding x (module_sig_binding y acc)
  | `Constraint (_loc,`Uid (sloc,s),mt) -> ((with_loc s sloc), (mtyp mt)) ::
      acc
  | t -> errorf (loc_of t) "module_sig_binding: %s" (dump_mbind t)
and module_str_binding (x : Ast.mbind) acc =
  match x with
  | `And (_,x,y) -> module_str_binding x (module_str_binding y acc)
  | `ModuleBind (_loc,`Uid (sloc,s),mt,me) ->
      ((with_loc s sloc), (mtyp mt), (mexp me)) :: acc
  | t -> errorf (loc_of t) "module_str_binding: %s" (dump_mbind t)
and mexp (x : Ast.mexp) =
  match x with
  | #vid' as i ->
      let loc = loc_of i in
      mkmod loc (Pmod_ident (long_uident (i : vid'  :>ident)))
  | `App (loc,me1,me2) -> mkmod loc (Pmod_apply ((mexp me1), (mexp me2)))
  | `Functor (loc,`Uid (sloc,n),mt,me) ->
      mkmod loc (Pmod_functor ((with_loc n sloc), (mtyp mt), (mexp me)))
  | `Struct (loc,sl) -> mkmod loc (Pmod_structure (stru sl []))
  | `StructEnd loc -> mkmod loc (Pmod_structure [])
  | `Constraint (loc,me,mt) ->
      mkmod loc (Pmod_constraint ((mexp me), (mtyp mt)))
  | `PackageModule (loc,`Constraint (_,e,`Package (_,pt))) ->
      mkmod loc
        (Pmod_unpack
           (mkexp loc
              (Pexp_constraint
                 ((exp e),
                   (Some (mktyp loc (Ptyp_package (package_type pt)))), None))))
  | `PackageModule (loc,e) -> mkmod loc (Pmod_unpack (exp e))
  | t -> errorf (loc_of t) "mexp: %s" (dump_mexp t)
and stru (s : stru) (l : structure) =
  (match s with
   | (`Class (loc,cd) : stru) ->
       (mkstr loc
          (Pstr_class (List.map class_info_clexp (list_of_and cd []))))
       :: l
   | `ClassType (loc,ctd) ->
       (mkstr loc
          (Pstr_class_type (List.map class_info_cltyp (list_of_and ctd []))))
       :: l
   | `Sem (_,st1,st2) -> stru st1 (stru st2 l)
   | `Directive _|`DirectiveSimple _ -> l
   | `Exception (loc,`Uid (_,s)) ->
       (mkstr loc (Pstr_exception ((with_loc s loc), []))) :: l
   | `Exception (loc,`Of (_,`Uid (_,s),t)) ->
       (mkstr loc
          (Pstr_exception
             ((with_loc s loc), (List.map ctyp (list_of_star t [])))))
       :: l
   | `Exception (_,_) -> assert false
   | `StExp (loc,e) -> (mkstr loc (Pstr_eval (exp e))) :: l
   | `External (loc,`Lid (sloc,n),t,sl) ->
       (mkstr loc
          (Pstr_primitive
             ((with_loc n sloc), (mkvalue_desc loc t (list_of_app sl [])))))
       :: l
   | `Include (loc,me) -> (mkstr loc (Pstr_include (mexp me))) :: l
   | `Module (loc,`Uid (sloc,n),me) ->
       (mkstr loc (Pstr_module ((with_loc n sloc), (mexp me)))) :: l
   | `RecModule (loc,mb) ->
       (mkstr loc (Pstr_recmodule (module_str_binding mb []))) :: l
   | `ModuleType (loc,`Uid (sloc,n),mt) ->
       (mkstr loc (Pstr_modtype ((with_loc n sloc), (mtyp mt)))) :: l
   | `Open (loc,id) -> (mkstr loc (Pstr_open (long_uident id))) :: l
   | `Type (loc,tdl) -> (mkstr loc (Pstr_type (mktype_decl tdl))) :: l
   | `Value (loc,rf,bi) ->
       (mkstr loc (Pstr_value ((mkrf rf), (binding bi [])))) :: l
   | x -> errorf (loc_of x) "stru : %s" (dump_stru x) : structure )
and cltyp (x : Ast.cltyp) =
  match x with
  | `ClApply (loc,id,tl) ->
      mkcty loc
        (Pcty_constr
           ((long_class_ident (id :>ident)),
             (List.map
                (function | `Ctyp (_loc,x) -> ctyp x | _ -> assert false)
                (list_of_com tl []))))
  | #vid' as id ->
      let loc = loc_of id in
      mkcty loc (Pcty_constr ((long_class_ident (id : vid'  :>ident)), []))
  | `CtFun (loc,`Label (_,`Lid (_,lab),t),ct) ->
      mkcty loc (Pcty_fun (lab, (ctyp t), (cltyp ct)))
  | `CtFun (loc,`OptLabl (loc1,`Lid (_,lab),t),ct) ->
      let t = `App (loc1, (predef_option loc1), t) in
      mkcty loc (Pcty_fun (("?" ^ lab), (ctyp t), (cltyp ct)))
  | `CtFun (loc,t,ct) -> mkcty loc (Pcty_fun ("", (ctyp t), (cltyp ct)))
  | `ObjEnd loc ->
      mkcty loc
        (Pcty_signature
           {
             pcsig_self = (ctyp (`Any loc));
             pcsig_fields = [];
             pcsig_loc = loc
           })
  | `ObjTyEnd (loc,t) ->
      mkcty loc
        (Pcty_signature
           { pcsig_self = (ctyp t); pcsig_fields = []; pcsig_loc = loc })
  | `Obj (loc,ctfl) ->
      let cli = clsigi ctfl [] in
      mkcty loc
        (Pcty_signature
           {
             pcsig_self = (ctyp (`Any loc));
             pcsig_fields = cli;
             pcsig_loc = loc
           })
  | `ObjTy (loc,t,ctfl) ->
      let cil = clsigi ctfl [] in
      mkcty loc
        (Pcty_signature
           { pcsig_self = (ctyp t); pcsig_fields = cil; pcsig_loc = loc })
  | x -> errorf (loc_of x) "class type: %s" (dump_cltyp x)
and class_info_clexp (ci : cldecl) =
  match ci with
  | (`ClDecl (loc,vir,`Lid (nloc,name),params,ce) : cldecl) ->
      let (loc_params,(params,variance)) =
        ((loc_of params), (List.split (class_parameters params))) in
      {
        pci_virt = (mkvirtual vir);
        pci_params = (params, loc_params);
        pci_name = (with_loc name nloc);
        pci_expr = (clexp ce);
        pci_loc = loc;
        pci_variance = variance
      }
  | `ClDeclS (loc,vir,`Lid (nloc,name),ce) ->
      {
        pci_virt = (mkvirtual vir);
        pci_params = ([], loc);
        pci_name = (with_loc name nloc);
        pci_expr = (clexp ce);
        pci_loc = loc;
        pci_variance = []
      }
  | ce -> errorf (loc_of ce) "class_info_clexp: %s" (dump_cldecl ce)
and class_info_cltyp (ci : cltdecl) =
  match ci with
  | (`CtDecl (loc,vir,`Lid (nloc,name),params,ct) : cltdecl) ->
      let (loc_params,(params,variance)) =
        ((loc_of params), (List.split (class_parameters params))) in
      {
        pci_virt = (mkvirtual vir);
        pci_params = (params, loc_params);
        pci_name = (with_loc name nloc);
        pci_expr = (cltyp ct);
        pci_loc = loc;
        pci_variance = variance
      }
  | (`CtDeclS (loc,vir,`Lid (nloc,name),ct) : cltdecl) ->
      {
        pci_virt = (mkvirtual vir);
        pci_params = ([], loc);
        pci_name = (with_loc name nloc);
        pci_expr = (cltyp ct);
        pci_loc = loc;
        pci_variance = []
      }
  | ct ->
      errorf (loc_of ct) "bad class/class type declaration/definition %s "
        (dump_cltdecl ct)
and clsigi (c : clsigi) (l : class_type_field list) =
  (match c with
   | `Eq (loc,t1,t2) -> (mkctf loc (Pctf_cstr ((ctyp t1), (ctyp t2)))) :: l
   | `Sem (_,csg1,csg2) -> clsigi csg1 (clsigi csg2 l)
   | `SigInherit (loc,ct) -> (mkctf loc (Pctf_inher (cltyp ct))) :: l
   | `Method (loc,`Lid (_,s),pf,t) ->
       (mkctf loc (Pctf_meth (s, (mkprivate pf), (mkpolytype (ctyp t))))) ::
       l
   | `CgVal (loc,`Lid (_,s),b,v,t) ->
       (mkctf loc (Pctf_val (s, (mkmutable b), (mkvirtual v), (ctyp t)))) ::
       l
   | `VirMeth (loc,`Lid (_,s),b,t) ->
       (mkctf loc (Pctf_virt (s, (mkprivate b), (mkpolytype (ctyp t))))) :: l
   | t -> errorf (loc_of t) "clsigi :%s" (dump_clsigi t) : class_type_field
                                                             list )
and clexp (x : Ast.clexp) =
  match x with
  | (`CeApp (loc,_,_) : clexp) as c ->
      let rec view_app acc (x : clexp) =
        match x with
        | (`CeApp (_loc,ce,(a : exp)) : clexp) -> view_app (a :: acc) ce
        | ce -> (ce, acc) in
      let (ce,el) = view_app [] c in
      let el = List.map label_exp el in mkcl loc (Pcl_apply ((clexp ce), el))
  | (`ClApply (loc,id,tl) : clexp) ->
      mkcl loc
        (Pcl_constr
           ((long_class_ident (id :>ident)),
             (List.map
                (function | `Ctyp (_loc,x) -> ctyp x | _ -> assert false)
                (list_of_com tl []))))
  | #vid' as id ->
      let _loc = loc_of id in
      mkcl _loc (Pcl_constr ((long_class_ident (id : vid'  :>ident)), []))
  | `CeFun (loc,`Label (_,`Lid (_loc,lab),po),ce) ->
      mkcl loc (Pcl_fun (lab, None, (pat po), (clexp ce)))
  | `CeFun (loc,`OptLablExpr (_,`Lid (_loc,lab),p,e),ce) ->
      let lab = paolab lab p in
      mkcl loc (Pcl_fun (("?" ^ lab), (Some (exp e)), (pat p), (clexp ce)))
  | `CeFun (loc,`OptLabl (_,`Lid (_loc,lab),p),ce) ->
      let lab = paolab lab p in
      mkcl loc (Pcl_fun (("?" ^ lab), None, (pat p), (clexp ce)))
  | `CeFun (loc,p,ce) -> mkcl loc (Pcl_fun ("", None, (pat p), (clexp ce)))
  | `LetIn (loc,rf,bi,ce) ->
      mkcl loc (Pcl_let ((mkrf rf), (binding bi []), (clexp ce)))
  | `ObjEnd loc ->
      mkcl loc
        (Pcl_structure { pcstr_pat = (pat (`Any loc)); pcstr_fields = [] })
  | `Obj (loc,cfl) ->
      let p = `Any loc in
      let cil = clfield cfl [] in
      mkcl loc (Pcl_structure { pcstr_pat = (pat p); pcstr_fields = cil })
  | `ObjPatEnd (loc,p) ->
      mkcl loc (Pcl_structure { pcstr_pat = (pat p); pcstr_fields = [] })
  | `ObjPat (loc,p,cfl) ->
      let cil = clfield cfl [] in
      mkcl loc (Pcl_structure { pcstr_pat = (pat p); pcstr_fields = cil })
  | `Constraint (loc,ce,ct) ->
      mkcl loc (Pcl_constraint ((clexp ce), (cltyp ct)))
  | t -> errorf (loc_of t) "clexp: %s" (dump_clexp t)
and clfield (c : clfield) l =
  match c with
  | `Eq (loc,t1,t2) -> (mkcf loc (Pcf_constr ((ctyp t1), (ctyp t2)))) :: l
  | `Sem (_,cst1,cst2) -> clfield cst1 (clfield cst2 l)
  | `Inherit (loc,ov,ce) ->
      (mkcf loc (Pcf_inher ((override_flag loc ov), (clexp ce), None))) :: l
  | `InheritAs (loc,ov,ce,`Lid (_,x)) ->
      (mkcf loc (Pcf_inher ((override_flag loc ov), (clexp ce), (Some x))))
      :: l
  | `Initializer (loc,e) -> (mkcf loc (Pcf_init (exp e))) :: l
  | `CrMthS (loc,`Lid (sloc,s),ov,pf,e) ->
      let e = mkexp loc (Pexp_poly ((exp e), None)) in
      (mkcf loc
         (Pcf_meth
            ((with_loc s sloc), (mkprivate pf), (override_flag loc ov), e)))
        :: l
  | `CrMth (loc,`Lid (sloc,s),ov,pf,e,t) ->
      let t = Some (mkpolytype (ctyp t)) in
      let e = mkexp loc (Pexp_poly ((exp e), t)) in
      (mkcf loc
         (Pcf_meth
            ((with_loc s sloc), (mkprivate pf), (override_flag loc ov), e)))
        :: l
  | `CrVal (loc,`Lid (sloc,s),ov,mf,e) ->
      (mkcf loc
         (Pcf_val
            ((with_loc s sloc), (mkmutable mf), (override_flag loc ov),
              (exp e))))
      :: l
  | `VirMeth (loc,`Lid (sloc,s),pf,t) ->
      (mkcf loc
         (Pcf_virt ((with_loc s sloc), (mkprivate pf), (mkpolytype (ctyp t)))))
      :: l
  | `VirVal (loc,`Lid (sloc,s),mf,t) ->
      (mkcf loc (Pcf_valvirt ((with_loc s sloc), (mkmutable mf), (ctyp t))))
      :: l
  | x -> errorf (loc_of x) "clfield: %s" (dump_clfield x)

let sigi (ast : sigi) = (sigi ast [] : signature )

let stru ast = stru ast []

let directive (x : exp) =
  match x with
  | `Str (_,s) -> Pdir_string s
  | `Int (_,i) -> Pdir_int (int_of_string i)
  | (`Lid (_loc,"true") : Ast.exp) -> Pdir_bool true
  | (`Lid (_loc,"false") : Ast.exp) -> Pdir_bool false
  | e -> Pdir_ident (ident_noloc (ident_of_exp e))

let phrase (x : stru) =
  match x with
  | `Directive (_,`Lid (_,d),dp) -> Ptop_dir (d, (directive dp))
  | `DirectiveSimple (_,`Lid (_,d)) -> Ptop_dir (d, Pdir_none)
  | `Directive (_,`Ant (_loc,_),_) -> error _loc "antiquotation not allowed"
  | si -> Ptop_def (stru si)

open Format

let pp = fprintf

let print_exp f e = pp f "@[%a@]@." AstPrint.expression (exp e)

let to_string_exp = to_string_of_printer print_exp

let print_pat f e = pp f "@[%a@]@." AstPrint.pattern (pat e)

let print_stru f e = pp f "@[%a@]@." AstPrint.structure (stru e)

let print_ctyp f e = pp f "@[%a@]@." AstPrint.core_type (ctyp e)