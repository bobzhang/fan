open Parsetree;
open Longident;
open Asttypes;
open Lib;
open FanUtil;
open FanAst;
open ParsetreeHelper;

DEFINE ANT_ERROR = error _loc "antiquotation not expected here";
let mkvirtual = with virtual_flag fun
  [ {| virtual |} -> Virtual
  | {||} -> Concrete
  | _ -> assert false ];

let mkdirection = with direction_flag fun
  [ {| to |} -> Upto
  | {| downto |} -> Downto
  | _ -> assert false ];

let mkrf = with rec_flag fun
  [ {| rec |} -> Recursive
  | {||} -> Nonrecursive
  | _ -> assert false ];


(*
  {[
  ident_tag {:ident| $(uid:"").B.t|}
  - : Longident.t * [> `app | `lident | `uident ] =
  (Longident.Ldot (Longident.Lident "B", "t"), `lident)

  ident_tag {:ident| A B |}
  (Longident.Lapply (Longident.Lident "A", Longident.Lident "B"), `app)

  ident_tag {:ident| (A B).t|}
  (Longident.Ldot
  (Longident.Lapply (Longident.Lident "A", Longident.Lident "B"), "t"),
  `lident)

  ident_tag {:ident| B.C |}
  (Longident.Ldot (Longident.Lident "B", "C"), `uident)

  ident_tag {:ident| B.u.g|}
  Exception: FanLoc.Exc_located (, Failure "invalid long identifier").

  ]}

  If "", just remove it, this behavior should appear in other identifier as well FIXME
 *)
let ident_tag i =
  let rec self i acc =  match i with
    [ {:ident| $(lid:"*predef*").$(lid:"option") |} ->
      (Some ((ldot (lident "*predef*") "option"), `lident))
    | {:ident| $i1.$i2 |} ->
        self i2 (self i1 acc) (* take care of the order *)
    | {:ident| ($i1 $i2) |} -> match ((self i1 None), (self i2 None),acc) with
        (* FIXME uid required here, more precise *)
        [ (Some (l,_),Some (r,_),None) ->
          Some(Lapply l r,`app)
        | _ -> error (FanAst.loc_of_ident i) "invalid long identifer" ]
    | {:ident| $uid:s |} -> match (acc,s) with
        [ (None,"") -> None 
        | (None,s) -> Some (lident s ,`uident) 
        | (Some (_, `uident | `app) ,"") -> acc
        | (Some (x, `uident | `app), s) -> Some (ldot x s, `uident)
        | _ -> error (FanAst.loc_of_ident i) "invalid long identifier" ]
    | {:ident| $lid:s |} ->
          let x = match acc with
            [ None -> lident s 
            | Some (acc, `uident | `app) -> ldot acc s
            | _ -> error (loc_of_ident i) "invalid long identifier" ]
          in Some (x, `lident)
    | _ -> error (loc_of_ident i) "invalid long identifier" ]
  in match self i None with [Some x -> x | None -> error (loc_of_ident i) "invalid long identifier "];

let ident_noloc i = fst (ident_tag  i);

let ident i =
  with_loc (ident_noloc  i) (loc_of_ident i);

let long_lident msg id =
    match ident_tag id with
    [ (i, `lident) -> with_loc i (loc_of_ident id)
    | _ -> error (loc_of_ident id) msg ];

let long_type_ident = long_lident "invalid long identifier type";
let long_class_ident = long_lident "invalid class name";

let long_uident_noloc  i =
    match ident_tag i with
    [ (Ldot (i, s), `uident) -> ldot i s
    | (Lident s, `uident) -> lident s
    | (i, `app) -> i
    | _ -> error (loc_of_ident i) "uppercase identifier expected" ];

let long_uident  i =
  with_loc (long_uident_noloc  i) (loc_of_ident i);

let rec ctyp_long_id_prefix (t:ctyp) : Longident.t =
  with ctyp match t with
  [ {| $id:i |} -> ident_noloc i
  | {| $m1 $m2 |} ->
        let li1 = ctyp_long_id_prefix m1 in
        let li2 = ctyp_long_id_prefix m2 in
        Lapply li1 li2
  | t -> error (loc_of_ctyp t) "invalid module expression" ] ;

let ctyp_long_id (t:ctyp) : (bool *  Location.loc Longident.t) =
  with ctyp match t with
  [ {| $id:i |} ->
    (false, long_type_ident i)
  | {| $_ $_ |} -> error _loc "invalid type name"
  | `TyCls (_, i) -> (true, ident i)
  | t -> error (loc_of_ctyp t) "invalid type" ] ;


let predef_option loc =
  `Id (loc, `IdAcc (loc, `Lid (loc, "*predef*"), `Lid (loc, "option")));

let rec ctyp : ctyp -> Parsetree.core_type = with ctyp fun 
  [ {|$id:i|} ->
    let li = long_type_ident i in
    mktyp _loc (Ptyp_constr li [])
  | {| $t1 as $t2 |}(* `Alias (loc, t1, t2) *) ->
      let (t, i) =  match (t1, t2) with
      [ (t, `TyQuo (_, s)) -> (t, s)
      | (`TyQuo (_, s), t) -> (t, s)
      | _ -> error _loc "invalid alias type" ] in
      mktyp _loc (Ptyp_alias (ctyp t) i)
  | {| _ |}(* `Any loc *) -> mktyp _loc Ptyp_any
  | ({| $_ $_ |} as f )(* `TyApp (loc, _, _) as f *) ->
      let (f, al) = Ctyp.view_app [] f in
      let (is_cls, li) = ctyp_long_id f in
      if is_cls then mktyp _loc (Ptyp_class li (List.map ctyp al) [])
      else mktyp _loc (Ptyp_constr li (List.map ctyp al))
  | `TyArr (loc, (`TyLab (_, lab, t1)), t2) ->
      let lab = match lab with
        [`Lid(_loc,lab) -> lab | `Ant(_loc,_) -> ANT_ERROR] in 
      mktyp loc (Ptyp_arrow (lab, (ctyp t1), (ctyp t2)))
  | `TyArr (loc, (`TyOlb (loc1, lab, t1)), t2) ->
      let lab =
        match lab with [`Lid(_loc,lab) -> lab | `Ant(_loc,_) -> ANT_ERROR] in 
      let t1 = `TyApp loc1 (predef_option loc1) t1 in
      mktyp loc (Ptyp_arrow ("?" ^ lab) (ctyp t1) (ctyp t2))
  | `TyArr (loc, t1, t2) -> mktyp loc (Ptyp_arrow "" (ctyp t1) (ctyp t2))
  | {| < $fl > |} -> mktyp _loc (Ptyp_object (meth_list fl []))
  | {| < $fl .. > |} ->
      mktyp _loc (Ptyp_object (meth_list fl [mkfield _loc Pfield_var]))
  | `TyCls (loc, id) ->
      mktyp loc (Ptyp_class (ident id) [] [])
  | {| (module $pt) |} ->
      let (i, cs) = package_type pt in
      mktyp _loc (Ptyp_package i cs)
  | `TyPol (loc, t1, t2) -> mktyp loc (Ptyp_poly (Ctyp.to_var_list t1) (ctyp t2))
  | `TyQuo (loc, s) -> mktyp loc (Ptyp_var s)
  | {@loc| ($t1 * $t2) |} ->
      mktyp loc (Ptyp_tuple (List.map ctyp (list_of_ctyp t1 (list_of_ctyp t2 []))))
  | {| [ = $t ] |} ->
      mktyp _loc (Ptyp_variant (row_field t) true None)
  | {| [ > $t ] |} ->
      mktyp _loc (Ptyp_variant (row_field t) false None)
  | {| [ < $t ] |} ->
      mktyp _loc (Ptyp_variant (row_field t) true (Some []))
  | {| [ < $t > $t' ] |} ->
      mktyp _loc (Ptyp_variant (row_field t) true (Some (Ctyp.name_tags t')))
  | `TyLab (loc, _, _) -> error loc "labelled type not allowed here"
  | `TyMan (loc, _, _) -> error loc "manifest type not allowed here"
  | `TyOlb (loc,_,_) -> error loc "labelled type not allowed here"
  | `TyRec (loc,_) -> error loc "record type not allowed here"
  | `Sum (loc,_) -> error loc "sum type not allowed here"
  | `Private (loc,_) -> error loc "private type not allowed here"
  | `Mutable (loc,_) -> error loc "mutable type not allowed here"
  | `Or (loc,_,_) -> error loc "type1 | type2 not allowed here"
  | `And (loc,_,_) -> error loc "type1 and type2 not allowed here"
  | `Of (loc,_,_) -> error loc "type1 of type2 not allowed here"
  | `TyCol (loc,_,_) -> error loc "type1 : type2 not allowed here"
  | `TySem (loc,_,_) -> error loc "type1 ; type2 not allowed here"
  | `Ant (loc,_) -> error loc "antiquotation not allowed here"
  | `TyOfAmp (_, _, _) |`TyAmp (_, _, _) |`Sta (_, _, _) |
    `Com (_, _, _) |`TyVrn (_, _) |`TyQuM (_, _) |`TyQuP (_, _) |`TyDcl (_, _, _, _, _) |
    `TyAnP _ | `TyAnM _ | `TyTypePol (_, _, _) |
    `TyObj (_, _, (`Ant _)) | `Nil _ | `Tup (_,_) ->
      assert false ]
and row_field : ctyp -> list row_field = with ctyp fun 
  [ {||} -> []
  | {| `$i |} -> [Rtag i true []]
  | {| `$i of & $t |} -> [Rtag i true (List.map ctyp (list_of_ctyp t []))]
  | {| `$i of $t |} -> [Rtag i false (List.map ctyp (list_of_ctyp t []))]
  | {| $t1 | $t2 |} -> row_field t1 @ row_field t2
  | t -> [Rinherit (ctyp t)] ]
and meth_list (fl:ctyp) (acc: list core_field_type) : list core_field_type =
  with ctyp match fl with
  [ {||} -> acc
  | {| $t1; $t2 |} -> meth_list t1 (meth_list t2 acc)
  | {| $lid:lab : $t |} ->
      [mkfield _loc (Pfield lab (mkpolytype (ctyp t))) :: acc]
  | _ -> assert false ]

and package_type_constraints (wc:with_constr)
    (acc: list (Asttypes.loc Longident.t  *core_type))
    : list (Asttypes.loc Longident.t  *core_type) =
    with with_constr match wc with
    [ {||} -> acc
    | {| type $id:id = $ct |} ->  [(ident id, ctyp ct) :: acc]
    | {| $wc1 and $wc2 |} ->
        package_type_constraints wc1 (package_type_constraints wc2 acc)
    | _ -> error (loc_of_with_constr wc) "unexpected `with constraint' for a package type" ]

and package_type : module_type -> package_type =
    with module_type fun
    [ {| $id:i with $wc |} ->
      (long_uident i, package_type_constraints wc [])
    | {| $id:i |} -> (long_uident i, [])
    | mt -> error (loc_of_module_type mt) "unexpected package type" ] ;

let mktype loc tl cl tk tp tm =
  let (params, variance) = List.split tl in
  {ptype_params = params; ptype_cstrs = cl; ptype_kind = tk;
   ptype_private = tp; ptype_manifest = tm; ptype_loc =  loc;
   ptype_variance = variance} ;
let mkprivate' m = if m then Private else Public;

let mkprivate = with private_flag fun
  [ {| private |} -> Private
  | {||} -> Public
  | _ -> assert false ];
let mktrecord : ctyp ->  (Asttypes.loc string * Asttypes.mutable_flag * core_type *  loc)=
  with ctyp fun
  [ {| $(id:{:ident@sloc| $lid:s |}) : mutable $t |} ->
    (with_loc s sloc, Mutable, mkpolytype (ctyp t),  _loc)
  | {| $(id:{:ident@sloc| $lid:s |}) : $t |} ->
      (with_loc s sloc, Immutable, mkpolytype (ctyp t),  _loc)
  | _ -> assert false (*FIXME*) ];
  
let mkvariant : ctyp ->
  (  Asttypes.loc string  * list core_type * option core_type * loc ) =
  with ctyp fun
  [ {| $(id:{:ident@sloc| $uid:s |}) |} ->
    (with_loc  s sloc, [], None,  _loc)
  | {| $(id:{:ident@sloc| $uid:s |}) of $t |} ->
      (with_loc  s sloc, List.map ctyp (list_of_ctyp t []), None,  _loc)
  | {| $(id:{:ident@sloc| $uid:s |}) : ($t -> $u) |} ->
      (with_loc s sloc, List.map ctyp (list_of_ctyp t []), Some (ctyp u),  _loc)
  | {| $(id:{:ident@sloc| $uid:s |}) : $t |} ->
      (with_loc  s sloc, [], Some (ctyp t),  _loc)

  | _ -> assert false (*FIXME*) ];
  
let rec type_decl (tl: list (option (Asttypes.loc string) * (bool * bool)))
    (cl: list (core_type * core_type * Location.t))
    loc m pflag : ctyp -> type_declaration = with ctyp fun
  [ {| $t1 == $t2 |} ->
    type_decl tl cl loc (Some (ctyp t1)) pflag t2
  | {| private $t |} ->
      if pflag then
        error _loc "multiple private keyword used, use only one instead"
      else
        type_decl tl cl loc m true t
  | {| { $t } |} ->
      mktype loc tl cl
        (Ptype_record (List.map mktrecord (list_of_ctyp t []))) (mkprivate' pflag) m
  | {| [ $t ] |} ->
      mktype loc tl cl
        (Ptype_variant (List.map mkvariant (list_of_ctyp t []))) (mkprivate' pflag) m
  | t ->
      if m <> None then
        error loc "only one manifest type allowed by definition" else
      let m = match t with
      [ {||} -> None
      | _ -> Some (ctyp t) ] in
      mktype loc tl cl Ptype_abstract (mkprivate' pflag) m ] ;

let type_decl tl cl t loc = type_decl tl cl loc None false t;

let mkvalue_desc loc t p = {pval_type = ctyp t; pval_prim = p; pval_loc =  loc};

let rec list_of_meta_list =fun
  [ `LNil _ -> []
  | `LCons (x, xs) -> [x :: list_of_meta_list xs]
  | `Ant _ -> assert false ];

let mkmutable = with mutable_flag fun
  [ {| mutable |} -> Mutable
  | {||} -> Immutable
  | _ -> assert false ];

let paolab (lab:string) (p:patt) : string =
  with patt match (lab, p) with
  [ ("", {| $lid:i |} | {| ($lid:i : $_) |}) -> i
  | ("", p) -> error (loc_of_patt p) "bad ast in label"
  | _ -> lab ] ;

let opt_private_ctyp : ctyp ->
  (type_kind * Asttypes.private_flag * core_type) = with ctyp fun
  [ {| private $t |} -> (Ptype_abstract, Private, ctyp t)
  | t -> (Ptype_abstract, Public, ctyp t) ];

let rec type_parameters (t:ctyp) acc =
  with ctyp match t with
  [ {| $t1 $t2 |} -> type_parameters t1 (type_parameters t2 acc)
  | {| +'$s |} -> [(s, (true, false)) :: acc]
  | {| -'$s |} -> [(s, (false, true)) :: acc]
  | {| '$s |} -> [(s, (false, false)) :: acc]
  | _ -> assert false ];

let rec optional_type_parameters (t:ctyp) acc =
  with ctyp match t with
  [ {| $t1 $t2 |} -> optional_type_parameters t1 (optional_type_parameters t2 acc)
  | {| +'$s |} -> [(Some (with_loc s _loc), (true, false)) :: acc]
  | `TyAnP _loc  -> [(None, (true, false)) :: acc]
  | {| -'$s |} -> [(Some (with_loc s _loc), (false, true)) :: acc]
  | `TyAnM _loc -> [(None, (false, true)) :: acc]
  | {| '$s |} -> [(Some (with_loc s _loc), (false, false)) :: acc]
  | {| _ |}  -> [(None, (false, false)) :: acc]
  | _ -> assert false ];

let rec class_parameters (t:ctyp) acc =
  with ctyp match t with
  [ {| $t1, $t2 |} -> class_parameters t1 (class_parameters t2 acc)
  | {| +'$s |} -> [(with_loc s _loc, (true, false)) :: acc]
  | {| -'$s |} -> [(with_loc s _loc, (false, true)) :: acc]
  | {| '$s |} -> [(with_loc s _loc, (false, false)) :: acc]
  | _ -> assert false ];

let rec type_parameters_and_type_name t acc =  match t with
  [ {:ctyp| $t1 $t2 |} ->
    type_parameters_and_type_name t1
      (optional_type_parameters t2 acc)
  | {:ctyp| $id:i |} -> (ident i, acc)
  | _ -> assert false ];

let mkwithtyp pwith_type loc id_tpl ct =
  let (id, tpl) = type_parameters_and_type_name id_tpl [] in
  let (params, variance) = List.split tpl in
  let (kind, priv, ct) = opt_private_ctyp ct in
  (id, pwith_type
     {ptype_params = params; ptype_cstrs = [];
      ptype_kind = kind;
      ptype_private = priv;
      ptype_manifest = Some ct;
      ptype_loc =  loc; ptype_variance = variance});
  
let rec mkwithc (wc:with_constr) acc =
    with with_constr match wc with
    [ {||} -> acc
    | {| type $id_tpl = $ct |} ->
        [mkwithtyp (fun x -> Pwith_type x) _loc id_tpl ct :: acc]
    | {| module $i1 = $i2 |} ->
        [(long_uident i1, Pwith_module (long_uident i2)) :: acc]
    | {| type $id_tpl := $ct |} ->
        [mkwithtyp (fun x -> Pwith_typesubst x) _loc id_tpl ct :: acc]
    | {| module $i1 := $i2 |} ->
        [(long_uident i1, Pwith_modsubst (long_uident i2)) :: acc]
    | {| $wc1 and $wc2 |} -> mkwithc wc1 (mkwithc wc2 acc)
    | {| $anti:_ |} ->
        error _loc "bad with constraint (antiquotation)" ];

let rec patt_fa al = fun
  [ `PaApp (_,f,a) -> patt_fa [a :: al] f
  | f -> (f, al) ];

let rec deep_mkrangepat loc c1 c2 =
  if c1 = c2 then mkghpat loc (Ppat_constant (Const_char c1))
  else
    mkghpat loc
      (Ppat_or (mkghpat loc (Ppat_constant (Const_char c1)))
         (deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2));

let rec mkrangepat loc c1 c2 =
  if c1 > c2 then mkrangepat loc c2 c1
  else if c1 = c2 then mkpat loc (Ppat_constant (Const_char c1))
  else
    mkpat loc
      (Ppat_or (mkghpat loc (Ppat_constant (Const_char c1)))
         (deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2));

let rec patt : patt -> pattern = with patt fun
  [ {| $(lid:("true"|"false" as txt)) |}  ->
    let p = Ppat_construct ({txt=Lident txt;loc=_loc}) None false in
    mkpat _loc p 
  | {| $(id:{:ident@sloc| $lid:s |}) |} -> mkpat _loc (Ppat_var (with_loc s sloc))
  | {| $id:i |} ->
      let p = Ppat_construct (long_uident  i) None false 
      in mkpat _loc p
  | (* {| ($p1 as $p2) |} *) `Alias (_loc, p1, x)->
      match x with
      [`Lid (sloc,s) -> mkpat _loc (Ppat_alias ((patt p1), with_loc s sloc))
      | `Ant (_loc,_) -> error _loc "invalid antiquotations"]  
      (* let (p, i) = *)
      (*   match (p1, p2) with *)
      (*   [ (p, {| $(id:{:ident@sloc| $lid:s |}) |}) -> (p, with_loc s sloc) *)
      (*   (\* | ({| $(id:{:ident@sloc| $lid:s |}) |}, p) -> (p, with_loc s sloc) *\) *)
      (*   | _ -> error _loc "invalid alias pattern" ] in *)
       (* mkpat _loc (Ppat_alias (patt p) i) *)
  | `Ant (loc,_) -> error loc "antiquotation not allowed here"
  | {| _ |} -> mkpat _loc Ppat_any
  | {| $(id:{:ident@sloc| $uid:s |}) $(tup:{@loc_any| _ |}) |} ->
      mkpat _loc (Ppat_construct (lident_with_loc  s sloc)
                   (Some (mkpat loc_any Ppat_any)) false)
  | `PaApp (loc, _, _) as f ->
     let (f, al) = patt_fa [] f in
     let al = List.map patt al in
     match (patt f).ppat_desc with
     [ Ppat_construct (li, None, _) ->
         let a =  match al with
         [ [a] -> a
         | _ -> mkpat loc (Ppat_tuple al) ] in
         mkpat loc (Ppat_construct li (Some a) false)
     | Ppat_variant (s, None) ->
         let a =
             match al with
             [ [a] -> a
             | _ -> mkpat loc (Ppat_tuple al) ] in
         mkpat loc (Ppat_variant s (Some a))
     | _ ->
         error (loc_of_patt f)
           "this is not a constructor, it cannot be applied in a pattern" ]
     | `Array (loc,p) -> mkpat loc (Ppat_array (List.map patt (list_of_patt p [])))
     | `Chr (loc,s) ->
         mkpat loc (Ppat_constant (Const_char (char_of_char_token loc s)))
     | `Int (loc,s) ->
         let i = try int_of_string s with [
           Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int"
         ] in mkpat loc (Ppat_constant (Const_int i))
     | `Int32 (loc, s) ->
         let i32 = try Int32.of_string s with [
           Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int32"
         ] in mkpat loc (Ppat_constant (Const_int32 i32))
     | `Int64 (loc, s) ->
         let i64 = try Int64.of_string s with [
           Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int64"
         ] in mkpat loc (Ppat_constant (Const_int64 i64))
     | `NativeInt (loc,s) ->
         let nati = try Nativeint.of_string s with [
           Failure _ -> error loc "Integer literal exceeds the range of representable integers of type nativeint"
         ] in mkpat loc (Ppat_constant (Const_nativeint nati))
     | `Flo (loc,s) -> mkpat loc (Ppat_constant (Const_float (remove_underscores s)))
     | `Label (loc,_,_) -> error loc "labeled pattern not allowed here"
     (* | `PaOlb (loc, _, _) *) | `PaOlbi (loc,_,_,_) -> error loc "labeled pattern not allowed here"
     | `PaOrp (loc, p1, p2) -> mkpat loc (Ppat_or (patt p1) (patt p2))
     | `PaRng (loc, p1, p2) ->
         match (p1, p2) with
         [ (`Chr (loc1, c1), `Chr (loc2, c2)) ->
           let c1 = char_of_char_token loc1 c1 in
            let c2 = char_of_char_token loc2 c2 in
           mkrangepat loc c1 c2
         | _ -> error loc "range pattern allowed only for characters" ]
         | `PaRec (loc,p) ->
             let ps = list_of_patt p [] in
             let is_wildcard = fun [ {| _ |} -> true | _ -> false ] in
             let (wildcards,ps) = List.partition is_wildcard ps in
             let is_closed = if wildcards = [] then Closed else Open in
             mkpat loc (Ppat_record (List.map mklabpat ps, is_closed))
         | `Str (loc,s) ->
             mkpat loc (Ppat_constant (Const_string (string_of_string_token loc s)))
         | {@loc| ($p1, $p2) |} ->
             mkpat loc (Ppat_tuple
                          (List.map patt (list_of_patt p1 (list_of_patt p2 []))))
         | {@loc| ($tup:_) |} -> error loc "singleton tuple pattern"
         | `PaTyc (loc,p,t) -> mkpat loc (Ppat_constraint (patt p) (ctyp t))
         | `PaTyp (loc,i) -> mkpat loc (Ppat_type (long_type_ident i))
         | `PaVrn (loc,s) -> mkpat loc (Ppat_variant s None)
         | `Lazy (loc,p) -> mkpat loc (Ppat_lazy (patt p))
         | `PaMod (loc,m) -> mkpat loc (Ppat_unpack (with_loc m loc))
         | `PaEq (_, _, _) | `Sem (_, _, _) | `PaCom (_, _, _) | `Nil _ as p ->
             error (loc_of_patt p) "invalid pattern" ]

and mklabpat : patt -> (Asttypes.loc Longident.t  * pattern) = with patt fun
  [ {| $i = $p |} -> (ident  i, patt p)
  | p -> error (loc_of_patt p) "invalid pattern" ];
  


let override_flag loc = with override_flag fun
  [ {| ! |} -> Override
  | {||} -> Fresh
  |  _ -> error loc "antiquotation not allowed here" ];

  


(*
  {[
  expr (`Id (_loc, ( (`IdAcc (_loc, `Uid (_loc, "U"), `Lid(_loc,"g"))) )));;
  - : Parsetree.expression =
  {Parsetree.pexp_desc =
  Parsetree.Pexp_ident
  {Asttypes.txt = Longident.Ldot (Longident.Lident "U", "g"); loc = };
  pexp_loc = }

  expr {:expr| $(uid:"A").b |} ; ;       
  - : Parsetree.expression =
  {Parsetree.pexp_desc =
  Parsetree.Pexp_ident
  {Asttypes.txt = Longident.Ldot (Longident.Lident "A", "b"); loc = };
  pexp_loc = }
  Ast2pt.expr {:expr| $(uid:"").b |} ; 
  - : Parsetree.expression =
  {Parsetree.pexp_desc =
  Parsetree.Pexp_ident
  {Asttypes.txt = Longident.Ldot (Longident.Lident "", "b"); loc = };
  pexp_loc = }
  ]}
 *)
let rec expr : expr -> expression = with expr fun (* expr -> expression*)
  [ {| $_ . $_ |}|
   {| $(id:{:ident@_| $_ . $_ |}) |} as e ->
    let (e, l) =
      match Expr.sep_dot_expr [] e with
      [ [(loc, ml, {@sloc| $uid:s |}) :: l] ->
        (mkexp loc (Pexp_construct (mkli sloc  s ml) None false(* ca *)), l)
      | [(loc, ml, {@sloc| $lid:s |}) :: l] ->
          (mkexp loc (Pexp_ident (mkli sloc s ml)), l)
      | [(_, [], e) :: l] -> (expr e, l)
      | _ -> error _loc "bad ast in expression" ] in
    let (_, e) =
      List.fold_left
        (fun (loc_bp, e1) (loc_ep, ml, e2) ->
          match e2 with
          [ {@sloc| $lid:s |} ->
              let loc = FanLoc.merge loc_bp loc_ep
              in  (loc, mkexp loc (Pexp_field e1 (mkli sloc s ml)))
          | _ -> error (loc_of_expr e2) "lowercase identifier expected" ])
        (_loc, e) l in
    e
  | `Ant (loc,_) -> error loc "antiquotation not allowed here"
  | `ExApp (loc, _, _) as f ->
      let (f, al) = Expr.view_app [] f in
      let al = List.map label_expr al in
      match (expr f).pexp_desc with
      [ Pexp_construct (li, None, _) ->
        let al = List.map snd al in
          let a = match al with
          [ [a] -> a
          | _ -> mkexp loc (Pexp_tuple al) ] in
          mkexp loc (Pexp_construct li (Some a) false)
      | Pexp_variant (s, None) ->
          let al = List.map snd al in
          let a =
              match al with
              [ [a] -> a
              | _ -> mkexp loc (Pexp_tuple al) ]
          in mkexp loc (Pexp_variant s (Some a))
      | _ -> mkexp loc (Pexp_apply (expr f) al) ]
      | `ExAre (loc, e1, e2) ->
          mkexp loc
            (Pexp_apply (mkexp loc (Pexp_ident (array_function loc "Array" "get")))
               [("", expr e1); ("", expr e2)])
      | `Array (loc,e) -> mkexp loc (Pexp_array (List.map expr (list_of_expr e [])))
      | `ExAsf loc -> mkexp loc Pexp_assertfalse
      | `ExAss (loc,e,v) ->
          let e =
            match e with
            [ {@loc| $x.contents |} -> (* FIXME *)
              Pexp_apply (mkexp loc (Pexp_ident (lident_with_loc ":=" loc)))
                [("", expr x); ("", expr v)]
            | `ExAcc (loc,_,_) ->
                match (expr e).pexp_desc with
                [ Pexp_field (e, lab) -> Pexp_setfield e lab (expr v)
                | _ -> error loc "bad record access" ]
                | `ExAre (loc, e1, e2) ->
                    Pexp_apply (mkexp loc (Pexp_ident (array_function loc "Array" "set")))
                      [("", expr e1); ("", expr e2); ("", expr v)]
                | {@lloc| $lid:lab |}  ->
                    (* FIXME `Id (lloc, `Lid (_, lab)) vs `Id(_,`Lid(lloc,lab)) *)
                    Pexp_setinstvar (with_loc lab lloc) (expr v)
                | `StringDot (loc, e1, e2) ->
                    Pexp_apply
                      (mkexp loc (Pexp_ident (array_function loc "String" "set")))
                      [("", expr e1); ("", expr e2); ("", expr v)]
                | _ -> error loc "bad left part of assignment" ] in
          mkexp loc e
      | `ExAsr (loc,e) -> mkexp loc (Pexp_assert (expr e))
      | `Chr (loc,s) ->
          mkexp loc (Pexp_constant (Const_char (char_of_char_token loc s)))
      | `ExCoe (loc, e, t1, t2) ->
          let t1 =
            match t1 with
            [ {:ctyp||} -> None
            | t -> Some (ctyp t) ] in
          mkexp loc (Pexp_constraint (expr e) t1 (Some (ctyp t2)))
      | `Flo (loc,s) -> mkexp loc (Pexp_constant (Const_float (remove_underscores s)))
      | `For (loc, i, e1, e2, df, el) ->
          match i with
          [`Lid(sloc,i) ->
            let e3 = `Seq loc el in
            mkexp loc (Pexp_for (with_loc i sloc)
                         (expr e1) (expr e2) (mkdirection df) (expr e3))
          | `Ant(_loc,_) -> ANT_ERROR]  
      | {@loc| fun [ $(pat:`Label (_, lab, po)) when $w -> $e ] |} ->
          match lab with
         [`Lid (_loc,lab) ->    
          mkexp loc
            (Pexp_function lab None
               [(patt_of_lab loc lab po, when_expr e w)])
          |`Ant(_loc,_) -> ANT_ERROR]
      | {@loc| fun [ $(pat:`PaOlbi (_, lab, p, e1)) when $w -> $e2 ] |} ->
          let lab = match lab with
            [`Lid(_loc,l) -> l
            |`Ant(_loc,_) -> ANT_ERROR] in
          (* let e1 = *)
          match e1 with
          [`None _ ->
            let lab = paolab lab p in
            mkexp loc
              (Pexp_function ("?" ^ lab) None [(patt_of_lab loc lab p, when_expr e2 w)])
          |`Some e1 ->
              let lab = paolab lab p in
              mkexp loc
            (Pexp_function ("?" ^ lab) (Some (expr e1)) [(patt p, when_expr e2 w)])
          |`Ant(_loc,_) -> ANT_ERROR
          ]
          
      (* | {@loc|fun [ $(pat:`PaOlb (_, lab, p)) when $w -> $e ] |} -> *)
      (*     let lab = match lab with *)
      (*       [ `Lid(_loc,l) -> l *)
      (*       | `Ant (_loc,_) -> ANT_ERROR ] in  *)
      (*     let lab = paolab lab p in *)
      (*     mkexp loc *)
      (*       (Pexp_function ("?" ^ lab) None [(patt_of_lab loc lab p, when_expr e w)]) *)
      | `Fun (loc,a) -> mkexp loc (Pexp_function "" None (match_case a []))
      | `IfThenElse (loc, e1, e2, e3) ->
          mkexp loc (Pexp_ifthenelse (expr e1) (expr e2) (Some (expr e3)))
      | `Int (loc,s) ->
          let i = try int_of_string s with [
            Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int"
          ] in mkexp loc (Pexp_constant (Const_int i))
      | `Int32 (loc, s) ->
          let i32 = try Int32.of_string s with [
            Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int32"
          ] in mkexp loc (Pexp_constant (Const_int32 i32))
      | `Int64 (loc, s) ->
          let i64 = try Int64.of_string s with [
            Failure _ -> error loc "Integer literal exceeds the range of representable integers of type int64"
          ] in mkexp loc (Pexp_constant (Const_int64 i64))
      | `NativeInt (loc,s) ->
          let nati = try Nativeint.of_string s with [
            Failure _ -> error loc "Integer literal exceeds the range of representable integers of type nativeint"
          ] in mkexp loc (Pexp_constant (Const_nativeint nati))
      | `Label (loc,_,_) -> error loc "labeled expression not allowed here"
      | `Lazy (loc,e) -> mkexp loc (Pexp_lazy (expr e))
      | `LetIn (loc,rf,bi,e) ->
          mkexp loc (Pexp_let (mkrf rf) (binding bi []) (expr e))
      | `LetModule (loc,i,me,e) ->
          match i with
          [`Uid(sloc,i) ->
            mkexp loc (Pexp_letmodule (with_loc i sloc) (module_expr me) (expr e))
          |`Ant(_loc,_) -> ANT_ERROR]
      | `Match (loc,e,a) -> mkexp loc (Pexp_match (expr e) (match_case a []))
      | `New (loc,id) -> mkexp loc (Pexp_new (long_type_ident id))
    | `Obj (loc,po,cfl) ->
        let p =
          match po with
          [ {:patt||} -> {:patt@loc| _ |}
          | p -> p ] in
        let cil = class_str_item cfl [] in
        mkexp loc (Pexp_object { pcstr_pat = patt p; pcstr_fields = cil })
    | `OptLabl (loc,_,_) -> error loc "labeled expression not allowed here"
    | `OvrInst (loc,iel) -> mkexp loc (Pexp_override (mkideexp iel []))
    | `Record (loc,lel,eo) ->
        match lel with
        [ {:rec_binding||} -> error loc "empty record"
        | _ ->
            let eo =
              match eo with
              [ {||} -> None
              | e -> Some (expr e) ] in
            mkexp loc (Pexp_record (mklabexp lel []) eo) ]
        | `Seq (_loc,e) ->
            let rec loop = fun
              [ [] -> expr {| () |}
              | [e] -> expr e
              | [e :: el] ->
                  let _loc = FanLoc.merge (loc_of_expr e) _loc in
                  mkexp _loc (Pexp_sequence (expr e) (loop el)) ] in
            loop (list_of_expr e [])
        | `Send (loc,e,s) -> mkexp loc (Pexp_send (expr e) s)
        | `StringDot (loc, e1, e2) ->
            mkexp loc
              (Pexp_apply (mkexp loc (Pexp_ident (array_function loc "String" "get")))
                 [("", expr e1); ("", expr e2)])
        | `Str (loc,s) ->
            mkexp loc (Pexp_constant (Const_string (string_of_string_token loc s)))
        | `Try (loc,e,a) -> mkexp loc (Pexp_try (expr e) (match_case a []))
        | {@loc| ($e1, $e2) |} ->
            mkexp loc (Pexp_tuple (List.map expr (list_of_expr e1 (list_of_expr e2 []))))
        | {@loc| ($tup:_) |} -> error loc "singleton tuple"
        | `Constraint_exp (loc,e,t) -> mkexp loc (Pexp_constraint (expr e) (Some (ctyp t)) None)
        | {@loc| () |} ->
            mkexp loc (Pexp_construct (lident_with_loc "()" loc) None true)

        | {@loc| $(lid:("true"|"false" as s)) |} ->
            mkexp loc (Pexp_construct (lident_with_loc s loc) None true)
              
        | {@loc| $lid:s |} ->
            mkexp loc (Pexp_ident (lident_with_loc s loc))
        | {@loc| $uid:s |} ->
            mkexp loc (Pexp_construct (lident_with_loc  s loc) None true)
        | `ExVrn (loc,s) -> mkexp loc (Pexp_variant  s None)
        | `While (loc, e1, el) ->
            let e2 = `Seq loc el in
            mkexp loc (Pexp_while (expr e1) (expr e2))
        | {@loc| let open $i in $e |} ->
            mkexp loc (Pexp_open (long_uident i) (expr e))
        | {@loc| (module $me : $pt) |} ->
            mkexp loc (Pexp_constraint (mkexp loc (Pexp_pack (module_expr me)),
                                        Some (mktyp loc (Ptyp_package (package_type pt))), None))
        | {@loc| (module $me) |} ->
            mkexp loc (Pexp_pack (module_expr me))
        | `LocalTypeFun (loc,i,e) ->
            match i with 
            [ `Lid(_loc,i) -> mkexp loc (Pexp_newtype i (expr e))
            | `Ant(_loc,_) -> ANT_ERROR ]
        | {@loc| $_,$_ |} -> error loc "expr, expr: not allowed here"
        | {@loc| $_;$_ |} ->
            error loc "expr; expr: not allowed here, use begin ... end or [|...|] to surround them" (* FIXME *)
        | `Id (_, _) | `Nil _ as e ->
            error (loc_of_expr e) "invalid expr" ]
and patt_of_lab _loc lab =  fun (* loc -> string -> patt -> pattern *)
  [ {:patt||} -> patt {:patt| $lid:lab |}
  | p -> patt p ]
and expr_of_lab _loc lab = fun (* loc -> string -> expr -> expression*)
  [ {:expr||} -> expr {:expr| $lid:lab |}
  | e -> expr e ]
and label_expr : expr -> (Asttypes.label*expression) = fun (* expr -> label * expression *)
  [ `Label (loc,lab,eo) ->
    match lab with
    [ `Lid (_,lab) ->
      (lab, expr_of_lab loc lab eo)
    | `Ant(_loc,_) -> ANT_ERROR]
  | `OptLabl (loc,lab,eo) ->
      match lab with 
      [`Lid (_loc,lab) -> ("?" ^ lab, expr_of_lab loc lab eo)
      |`Ant(_loc,_) -> ANT_ERROR]
  | e -> ("", expr e) ]
and binding x acc =  match x with (* binding -> (pattern * expression) list ->  (pattern * expression) list *)
  [ {:binding| $x and $y |} ->
    binding x (binding y acc)
  | {:binding@_loc| $(pat: {:patt@sloc| $lid:bind_name |} ) = ($e : $(`TyTypePol (_, vs, ty))) |} ->
      (* this code is not pretty because it is temporary *)
      let rec id_to_string x = match x with
      [ {:ctyp| $lid:x |} -> [x]
      | {:ctyp| $x $y |} -> (id_to_string x) @ (id_to_string y)
      | _ -> assert false]   in
      let vars = id_to_string vs in
      let ampersand_vars = List.map (fun x -> "&" ^ x) vars in
      let ty' = varify_constructors vars (ctyp ty) in
      let mkexp = mkexp _loc in
      let mkpat = mkpat _loc in
      let e = mkexp (Pexp_constraint (expr e) (Some (ctyp ty)) None) in
      let rec mk_newtypes x =
        match x with
         [ [newtype :: []] -> mkexp (Pexp_newtype(newtype, e))
        | [newtype :: newtypes] ->
            mkexp(Pexp_newtype (newtype,mk_newtypes newtypes))
        | [] -> assert false] in
      let pat =
        mkpat (Ppat_constraint (mkpat (Ppat_var (with_loc bind_name sloc)),
                                mktyp _loc (Ptyp_poly ampersand_vars ty'))) in
      let e = mk_newtypes vars in
      [( pat, e) :: acc]
  | {:binding@_loc| $p = ($e : ! $vs . $ty) |} ->
      [(patt {:patt| ($p : ! $vs . $ty ) |}, expr e) :: acc]
  | {:binding| $p = $e |} -> [(patt p, expr e) :: acc]
  | {:binding||} -> acc
  | _ -> assert false ]
and match_case (x:match_case) (acc: list (pattern*expression))
    : list (pattern * expression) =
  with match_case match x with
  [ {| $x | $y |} -> match_case x (match_case y acc)
  | {| $pat:p when $w -> $e |} ->
      [(patt p, when_expr e w) :: acc]
  | {||} -> acc
  | _ -> assert false ]
and when_expr (e:expr) (w:expr) : expression  =
  with expr match w with 
  [ {||} -> expr e
  | w -> mkexp (loc_of_expr w) (Pexp_when (expr w) (expr e)) ]
and mklabexp (x:rec_binding)
    (acc: list (Asttypes.loc Longident.t  * expression)) :
    list (Asttypes.loc Longident.t  * expression) =
  with rec_binding match x with 
  [ {| $x; $y |} ->
    mklabexp x (mklabexp y acc)
  | {| $id:i = $e |} -> [(ident  i, expr e) :: acc]
  | _ -> assert false ]
and mkideexp (x:rec_binding)
    (acc: list (Asttypes.loc string * expression)) :
    list (Asttypes.loc string * expression) = 
  with rec_binding match x with 
  [ {||} -> acc
  | {| $x; $y |} ->  mkideexp x (mkideexp y acc)
  | {| $(id: {:ident@sloc| $lid:s |}) = $e |} ->
      [(with_loc s sloc, expr e) :: acc]
  | _ -> assert false ]

(* Example:
   {[
   (Lib.Ctyp.of_str_item {:str_item|type u = int and v  = [A of u and b ] |},[])
     ||> mktype_decl |> AstPrint.default#type_def_list f;
   type u = int 
   and v =  
   | A of u* b
   ]}
   
 *)    
and mktype_decl x acc =
  match x with (* ctyp -> (string loc * type_declaration) list -> (string loc * type_declaration) list*)
  [ {:ctyp| $x and $y |} ->
    mktype_decl x (mktype_decl y acc)
  | `TyDcl (cloc, c, tl, td, cl) ->
      let cl =
        List.map
          (fun (t1, t2) ->
            let loc = FanLoc.merge (loc_of_ctyp t1) (loc_of_ctyp t2) in
            (ctyp t1, ctyp t2,  loc))
          cl  in
      [(with_loc c cloc,
        type_decl (List.fold_right optional_type_parameters tl []) cl td cloc) :: acc]
  | _ -> assert false ]
and module_type : Ast.module_type -> Parsetree.module_type =
  with module_type fun 
  [ {@loc||} -> error loc "abstract/nil module type not allowed here"
  | {@loc| $id:i |} -> mkmty loc (Pmty_ident (long_uident i))
  | {@loc| functor ($n : $nt) -> $mt |} ->
      mkmty loc (Pmty_functor (with_loc n loc) (module_type nt) (module_type mt))
  | {@loc| '$_ |} -> error loc "module type variable not allowed here"
  | {@loc| sig $sl end |} ->
      mkmty loc (Pmty_signature (sig_item sl []))
  | {@loc| $mt with $wc |} ->
      mkmty loc (Pmty_with (module_type mt) (mkwithc wc []))
  | {@loc| module type of $me |} ->
      mkmty loc (Pmty_typeof (module_expr me))
  | {| $anti:_ |} -> assert false ]
and sig_item (s:sig_item) (l:signature) :signature =
  with sig_item match s with 
  [ {||} -> l
  | `Class (loc,cd) ->
      [mksig loc (Psig_class
                    (List.map class_info_class_type (list_of_class_type cd []))) :: l]
  | `ClassType (loc,ctd) ->
      [mksig loc (Psig_class_type
                    (List.map class_info_class_type (list_of_class_type ctd []))) :: l]
  | {| $sg1; $sg2 |} -> sig_item sg1 (sig_item sg2 l)
  | `Directive (_,_,_) -> l
  | {| exception $uid:s |} ->
      [mksig _loc (Psig_exception (with_loc s _loc) []) :: l]
  | {| exception $uid:s of $t |} ->
      [mksig _loc (Psig_exception (with_loc s _loc)
                    (List.map ctyp (list_of_ctyp t []))) :: l]
  | `Exception (_,_) -> assert false (*FIXME*)
  | `External (loc, n, t, sl) ->
      let n = match n with
        [`Lid (_,n) -> n | `Ant(loc,_) -> error loc "antiquotation in sig_item"] in
      [mksig loc (Psig_value (with_loc n loc) (mkvalue_desc loc t (list_of_meta_list sl))) :: l]
  | `Include (loc,mt) -> [mksig loc (Psig_include (module_type mt)) :: l]
  | `Module (loc,n,mt) -> [mksig loc (Psig_module (with_loc n loc) (module_type mt)) :: l]
  | `RecModule (loc,mb) ->
      [mksig loc (Psig_recmodule (module_sig_binding mb [])) :: l]
  | `ModuleType (loc,n,mt) ->
      let si =  match mt with
      [ `MtQuo (_,_) -> Pmodtype_abstract
      | _ -> Pmodtype_manifest (module_type mt) ] in
      [mksig loc (Psig_modtype (with_loc n loc) si) :: l]
  | `Open (loc,id) ->
      [mksig loc (Psig_open (long_uident id)) :: l]
  | `Type (loc,tdl) -> [mksig loc (Psig_type (mktype_decl tdl [])) :: l]
  | `Val (loc,n,t) ->
        match n with
        [`Lid(sloc,n) ->
          [mksig loc (Psig_value (with_loc n sloc) (mkvalue_desc loc t [])) :: l]
        |`Ant(_loc,_) -> ANT_ERROR ] 
      (* [mksig loc (Psig_value (with_loc n loc) (mkvalue_desc loc t [])) :: l] *)
  | {| $anti:_ |} -> error _loc "antiquotation in sig_item" ]
and module_sig_binding x acc = match x with (* module_binding -> (string loc * module_type) list -> (string loc * module_type) list*)
  [ {:module_binding| $x and $y |} ->
    module_sig_binding x (module_sig_binding y acc)
  | {:module_binding@loc| $uid:s : $mt |} ->
      [(with_loc s loc, module_type mt) :: acc]
  | _ -> assert false ]
and module_str_binding x acc =  match x with (* module_binding ->  (string loc * module_type * module_expr) list ->  (string loc * module_type * module_expr) list*)
  [ {:module_binding| $x and $y |} ->
      module_str_binding x (module_str_binding y acc)
  | {:module_binding@loc| $uid:s : $mt = $me |} ->
      [(with_loc s loc, module_type mt, module_expr me) :: acc]
  | _ -> assert false ]
and module_expr =   fun (* module_expr -> module_expr *)
  [ {:module_expr@loc| |} -> error loc "nil module expression"
  | {:module_expr@loc| $id:i |} -> mkmod loc (Pmod_ident (long_uident i))
  | {:module_expr@loc| $me1 $me2 |} ->
      mkmod loc (Pmod_apply (module_expr me1) (module_expr me2))
  | {:module_expr@loc| functor ($n : $mt) -> $me |} ->
      mkmod loc (Pmod_functor (with_loc n loc) (module_type mt) (module_expr me))
  | {:module_expr@loc| struct $sl end |} ->
      mkmod loc (Pmod_structure (str_item sl []))
  | {:module_expr@loc| ($me : $mt) |} ->
        mkmod loc (Pmod_constraint (module_expr me) (module_type mt))
  | {:module_expr@loc| (val $e : $pt) |} ->
      mkmod loc (Pmod_unpack (
                 mkexp loc (Pexp_constraint (expr e,
                                             Some (mktyp loc (Ptyp_package (package_type pt))),
                                             None))))
  | {:module_expr@loc| (val $e) |} ->
      mkmod loc (Pmod_unpack (expr e))
  | {:module_expr@loc| $anti:_ |} -> error loc "antiquotation in module_expr" ]
and str_item (s:str_item) (l:structure) : structure =
 with str_item match s with 
  [ {||} -> l
  | `Class (loc,cd) ->
      [mkstr loc (Pstr_class
           (List.map class_info_class_expr (list_of_class_expr cd []))) :: l]
  | `ClassType (loc,ctd) ->
      [mkstr loc (Pstr_class_type
                    (List.map class_info_class_type (list_of_class_type ctd []))) :: l]
  | {| $st1; $st2 |} -> str_item st1 (str_item st2 l)
  | `Directive (_,_,_) -> l
  | {@loc| exception $uid:s |} ->
      [mkstr loc (Pstr_exception (with_loc s loc) []) :: l ]
  | {@loc| exception $uid:s of $t |} ->
      [mkstr loc (Pstr_exception (with_loc s loc)
                    (List.map ctyp (list_of_ctyp t []))) :: l ]
  | {@loc| exception $uid:s = $i |} ->
      [mkstr loc (Pstr_exn_rebind (with_loc s loc) (ident i)) :: l ]
  | {@loc| exception $uid:_ of $_ = $_ |} ->
      error loc "type in exception alias"
  | `Exception (_,_,_) -> assert false (*FIXME*)
  | `StExp (loc,e) -> [mkstr loc (Pstr_eval (expr e)) :: l]
  | {@loc|external $(lid:n) : $t = $sl |} ->
      (*$lid:n behave different here mainl the location*)
      [mkstr loc
         (Pstr_primitive (with_loc n loc) (mkvalue_desc loc t (list_of_meta_list sl))) :: l]
  | `Include (loc,me) -> [mkstr loc (Pstr_include (module_expr me)) :: l]
  | `Module (loc,n,me) -> [mkstr loc (Pstr_module (with_loc n loc) (module_expr me)) :: l]
  | `RecModule (loc,mb) ->
      [mkstr loc (Pstr_recmodule (module_str_binding mb [])) :: l]
  | `ModuleType (loc,n,mt) -> [mkstr loc (Pstr_modtype (with_loc n loc) (module_type mt)) :: l]
  | `Open (loc,id) ->
      [mkstr loc (Pstr_open (long_uident id)) :: l]
  | `Type (loc,tdl) -> [mkstr loc (Pstr_type (mktype_decl tdl [])) :: l]
  | `Value (loc,rf,bi) ->
      [mkstr loc (Pstr_value (mkrf rf) (binding bi [])) :: l]

  | x (* {@loc| $anti:_ |} *) ->
      let loc = FanAst.loc_of_str_item x in
      error loc "antiquotation in str_item" ]
and class_type = fun (* class_type -> class_type *)
  [ `CtCon (loc, `ViNil _, id,tl) ->
    mkcty loc
      (Pcty_constr (long_class_ident id) (List.map ctyp (Ctyp.list_of_opt tl [])))
  | `CtFun (loc, (`TyLab (_, lab, t)), ct) ->
      let lab = match lab with
        [`Lid(_loc,lab) -> lab | `Ant(_loc,_) -> ANT_ERROR] in
      mkcty loc (Pcty_fun lab (ctyp t) (class_type ct))
  | `CtFun (loc, (`TyOlb (loc1, lab, t)), ct) ->
      let lab = match lab with
        [`Lid(_loc,lab) -> lab | `Ant(_loc,_) -> ANT_ERROR] in
      let t = `TyApp loc1 (predef_option loc1) t in
      mkcty loc (Pcty_fun ("?" ^ lab) (ctyp t) (class_type ct))
  | `CtFun (loc,t,ct) -> mkcty loc (Pcty_fun "" (ctyp t) (class_type ct))
  | `CtSig (loc,t_o,ctfl) ->
      let t = match t_o with
      [ {:ctyp||} -> {:ctyp@loc| _ |}
      | t -> t ] in
      let cil = class_sig_item ctfl [] in
      mkcty loc (Pcty_signature {
                 pcsig_self = ctyp t;
                 pcsig_fields = cil;
                 pcsig_loc =  loc;
               })
  | `CtCon (loc,_,_,_) ->
        error loc "invalid virtual class inside a class type"
  | `Ant (_, _) | `CtEq (_, _, _) | `CtCol (_, _, _) | `CtAnd (_, _, _) | `Nil _ ->
      assert false ]
    
and class_info_class_expr ci =
    match ci with (* class_expr -> class_declaration*)
    [ `Eq (_, (`CeCon (loc, vir, (`Lid (nloc, name)), params)), ce) ->
      let (loc_params, (params, variance)) =
        match params with
        [ {:ctyp||} -> (loc, ([], []))
        | t -> (loc_of_ctyp t, List.split (class_parameters t [])) ]  in
      {pci_virt = mkvirtual vir;
       pci_params = (params,  loc_params);
       pci_name = with_loc name nloc;
       pci_expr = class_expr ce;
       pci_loc =  loc;
       pci_variance = variance}
  | ce -> error (loc_of_class_expr ce) "bad class definition" ]
and class_info_class_type ci =
    match ci with (* class_type -> class_description*)
    [ `CtEq (_, (`CtCon (loc, vir, (`Lid (nloc, name)), params)), ct)
    | `CtCol (_, (`CtCon (loc, vir, (`Lid (nloc, name)), params)), ct)
      ->
        let (loc_params, (params, variance)) =
          match params with
          [ {:ctyp||} -> (loc, ([], []))
          | t -> (loc_of_ctyp t, List.split (class_parameters t [])) ] in
      {pci_virt = mkvirtual vir;
       pci_params = (params,  loc_params);
       pci_name = with_loc name nloc;
       pci_expr = class_type ct;
       pci_loc =  loc;
       pci_variance = variance}
  | ct -> error (loc_of_class_type ct)
        "bad class/class type declaration/definition" ]
and class_sig_item c l = match c with (* class_sig_item -> class_type_field list -> class_type_field list *)
  [ {:class_sig_item||} -> l
  | `Eq (loc, t1, t2) ->
      [mkctf loc (Pctf_cstr (ctyp t1, ctyp t2)) :: l]
  | {:class_sig_item| $csg1; $csg2 |} ->
      class_sig_item csg1 (class_sig_item csg2 l)
  | `Inherit (loc,ct) ->
      [mkctf loc (Pctf_inher (class_type ct)) :: l]
  | `Method (loc,s,pf,t) ->
      [mkctf loc (Pctf_meth (s, mkprivate pf, mkpolytype (ctyp t))) :: l]
  | `CgVal (loc, s, b, v, t) ->
      [mkctf loc (Pctf_val (s, mkmutable b, mkvirtual v, ctyp t)) :: l]
  | `CgVir (loc,s,b,t) ->
      [mkctf loc (Pctf_virt (s, mkprivate b, mkpolytype (ctyp t))) :: l]
  | `Ant (_,_) -> assert false ]
and class_expr : class_expr -> Parsetree.class_expr = fun (* class_expr -> class_expr *)
  [ `CeApp (loc, _, _) as c ->
    let (ce, el) = ClassExpr.view_app [] c in
    let el = List.map label_expr el in
    mkcl loc (Pcl_apply (class_expr ce) el)
  | `CeCon (loc, `ViNil _, id,tl) ->
      mkcl loc
        (Pcl_constr (long_class_ident id) (List.map ctyp (Ctyp.list_of_opt tl [])))
  | `CeFun (loc, (`Label (_, lab, po)), ce) ->
      match lab with
      [`Lid (_loc,lab) ->   
      mkcl loc
        (Pcl_fun lab None (patt_of_lab loc lab po) (class_expr ce))
      |`Ant(_loc,_) -> ANT_ERROR ]
  | `CeFun (loc, (`PaOlbi (_, lab, p, e)), ce) ->
      let lab = match lab with [`Lid(_loc,i) -> i | `Ant(_loc,_) -> ANT_ERROR] in
      let lab = paolab lab p in
      match e with
      [`None _ ->
        mkcl loc (Pcl_fun ("?" ^ lab) None (patt_of_lab loc lab p) (class_expr ce))
      |`Some e ->
          mkcl loc (Pcl_fun ("?" ^ lab) (Some (expr e)) (patt p) (class_expr ce))
      |`Ant(_loc,_) -> ANT_ERROR]  
  | `CeFun (loc,p,ce) -> mkcl loc (Pcl_fun "" None (patt p) (class_expr ce))
  | `CeLet (loc, rf, bi, ce) ->
      mkcl loc (Pcl_let (mkrf rf) (binding bi []) (class_expr ce))
  | `Obj (loc,po,cfl) ->
      let p = match po with
          [ {:patt||} -> {:patt@loc| _ |}
          | p -> p ] in
      let cil = class_str_item cfl [] in
      mkcl loc (Pcl_structure {
                pcstr_pat = patt p;
                pcstr_fields = cil;
              })
  | `CeTyc (loc,ce,ct) ->
      mkcl loc (Pcl_constraint (class_expr ce) (class_type ct))
  | `CeCon (loc,_,_,_) ->
      error loc "invalid virtual class inside a class expression"
  | `Ant (_, _) | `Eq (_, _, _) | `And (_, _, _) | `Nil _ -> assert false ]
and class_str_item (c:class_str_item) l =
    match c with (*class_str_item -> class_field list -> class_field list*)
  [ `Nil _ -> l
  | `Eq (loc, t1, t2) -> [mkcf loc (Pcf_constr (ctyp t1, ctyp t2)) :: l]
  | {:class_str_item| $cst1; $cst2 |} ->
      class_str_item cst1 (class_str_item cst2 l)
  | `Inherit (loc, ov, ce, pb) ->
      let opb = match pb with
      [`None _ -> None
      |`Some (`Lid (_,x) ) -> Some x
      |`Some (`Ant (_loc,_))
      |`Ant (_loc,_) -> error _loc "antiquotation not allowed here"] in  
      (* let opb = if pb = "" then None else Some pb in *)
      [mkcf loc (Pcf_inher (override_flag loc ov) (class_expr ce) opb) :: l]
  | `Initializer (loc,e) -> [mkcf loc (Pcf_init (expr e)) :: l]
  | `CrMth (loc, s, ov, pf, e, t) ->
      let t = match t with
      [ {:ctyp||} -> None
      | t -> Some (mkpolytype (ctyp t)) ] in
      let e = mkexp loc (Pexp_poly (expr e) t) in
      [mkcf loc (Pcf_meth (with_loc s loc, mkprivate pf, override_flag loc ov, e)) :: l]
  | `CrVal (loc, s, ov, mf, e) ->
      [mkcf loc (Pcf_val (with_loc s loc, mkmutable mf, override_flag loc ov, expr e)) :: l]
  | `CrVir (loc,s,pf,t) ->
      [mkcf loc (Pcf_virt (with_loc s loc, mkprivate pf, mkpolytype (ctyp t))) :: l]
  | `CrVvr (loc,s,mf,t) ->
      [mkcf loc (Pcf_valvirt (with_loc s loc, mkmutable mf, ctyp t)) :: l]
  | `Ant (_,_) -> assert false ];

let sig_item (ast:sig_item) : signature = sig_item ast [];
let str_item ast = str_item ast [];

let directive : expr -> directive_argument = with expr fun
  [ {||} -> Pdir_none
  | {|$str:s|} -> Pdir_string s
  | {|$int:i|} -> Pdir_int (int_of_string i)
  | {| true |} -> Pdir_bool true
  | {| false |} -> Pdir_bool false
  | e -> Pdir_ident (ident_noloc (ident_of_expr e)) ] ;
(* str_item -> phrase *)  
let phrase : str_item -> toplevel_phrase = fun
  [ `Directive (_, `Lid(_,d),dp) -> Ptop_dir d (directive dp)
  | `Directive (_, `Ant(_loc,_),_) -> error _loc "antiquotation not allowed"
  | si -> Ptop_def (str_item si) ];

open Format;
let pp = fprintf;  

let print_expr f  e =
  pp f "@[%a@]@." AstPrint.expression (expr e);
(* let p_ident = eprintf "@[%a@]@." opr#ident ;     *)
let print_patt f e =
  pp f "@[%a@]@." AstPrint.pattern (patt e);
  
let print_str_item f e =
  pp f "@[%a@]@." AstPrint.structure (str_item e);

(* FIXME allow more interfaces later *)  
(* let p_ident f e = *)
(*   eprintf "@[%a@]@." Pprintast.fmt_longident (ident e) ;     *)
let print_ctyp f e =
  pp f "@[%a@]@." AstPrint.core_type (ctyp e) ;

(* Ctyp.to_string := LibUtil.to_string_of_printer print_ctyp;   *)
