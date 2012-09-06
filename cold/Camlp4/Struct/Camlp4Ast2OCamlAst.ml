module Make =
 functor (Ast : Sig.Camlp4Ast) ->
  struct
   open Format

   open Parsetree

   open Longident

   open Asttypes

   open Ast

   let constructors_arity = fun ()  -> !FanConfig.constructors_arity

   let error = fun loc -> fun str -> (Loc.raise loc ( (Failure (str)) ))

   let char_of_char_token =
    fun loc ->
     fun s ->
      (try (Token.Eval.char s) with
       (Failure (_) as exn) -> (Loc.raise loc exn))

   let string_of_string_token =
    fun loc ->
     fun s ->
      (try (Token.Eval.string s) with
       (Failure (_) as exn) -> (Loc.raise loc exn))

   let remove_underscores =
    fun s ->
     let l = (String.length s) in
     let rec remove =
      fun src ->
       fun dst ->
        if (src >= l) then ( if (dst >= l) then s else (String.sub s 0 dst) )
        else
         (match (String.get s src) with
          | '_' -> (remove ( (src + 1) ) dst)
          | c ->
             ( (String.set s dst c) ); (remove ( (src + 1) ) ( (dst + 1) ))) in
     (remove 0 0)

   let mkloc = Loc.to_ocaml_location

   let mkghloc = fun loc -> (Loc.to_ocaml_location ( (Loc.ghostify loc) ))

   let with_loc = fun txt -> fun loc -> (Location.mkloc txt ( (mkloc loc) ))

   let mktyp =
    fun loc -> fun d -> {ptyp_desc = d; ptyp_loc = ( (mkloc loc) )}

   let mkpat =
    fun loc -> fun d -> {ppat_desc = d; ppat_loc = ( (mkloc loc) )}

   let mkghpat =
    fun loc -> fun d -> {ppat_desc = d; ppat_loc = ( (mkghloc loc) )}

   let mkexp =
    fun loc -> fun d -> {pexp_desc = d; pexp_loc = ( (mkloc loc) )}

   let mkmty =
    fun loc -> fun d -> {pmty_desc = d; pmty_loc = ( (mkloc loc) )}

   let mksig =
    fun loc -> fun d -> {psig_desc = d; psig_loc = ( (mkloc loc) )}

   let mkmod =
    fun loc -> fun d -> {pmod_desc = d; pmod_loc = ( (mkloc loc) )}

   let mkstr =
    fun loc -> fun d -> {pstr_desc = d; pstr_loc = ( (mkloc loc) )}

   let mkfield =
    fun loc -> fun d -> {pfield_desc = d; pfield_loc = ( (mkloc loc) )}

   let mkcty =
    fun loc -> fun d -> {pcty_desc = d; pcty_loc = ( (mkloc loc) )}

   let mkcl = fun loc -> fun d -> {pcl_desc = d; pcl_loc = ( (mkloc loc) )}

   let mkcf = fun loc -> fun d -> {pcf_desc = d; pcf_loc = ( (mkloc loc) )}

   let mkctf =
    fun loc -> fun d -> {pctf_desc = d; pctf_loc = ( (mkloc loc) )}

   let mkpolytype =
    fun t ->
     (match t.ptyp_desc with
      | Ptyp_poly (_, _) -> t
      | _ -> {t with ptyp_desc = ( (Ptyp_poly ([] , t)) )})

   let mkvirtual =
    function
    | Ast.ViVirtual -> (Virtual)
    | Ast.ViNil -> (Concrete)
    | _ -> assert false

   let mkdirection =
    function
    | Ast.DiTo -> (Upto)
    | Ast.DiDownto -> (Downto)
    | _ -> assert false

   let lident = fun s -> (Lident (s))

   let lident_with_loc = fun s -> fun loc -> (with_loc ( (Lident (s)) ) loc)

   let ldot = fun l -> fun s -> (Ldot (l, s))

   let lapply = fun l -> fun s -> (Lapply (l, s))

   let conv_con =
    let t = (Hashtbl.create 73) in
    (
    (List.iter ( fun (s, s') -> (Hashtbl.add t s s') ) (
      [("True", "true"); ("False", "false"); (" True", "True");
       (" False", "False")] ))
    );
    fun s -> (try (Hashtbl.find t s) with
              Not_found -> s)

   let conv_lab =
    let t = (Hashtbl.create 73) in
    (
    (List.iter ( fun (s, s') -> (Hashtbl.add t s s') ) (
      [("val", "contents")] ))
    );
    fun s -> (try (Hashtbl.find t s) with
              Not_found -> s)

   let array_function_no_loc =
    fun str ->
     fun name ->
      (ldot ( (lident str) ) (
        if !FanConfig.unsafe then ( ("unsafe_" ^ name) ) else name ))

   let array_function =
    fun loc ->
     fun str ->
      fun name -> (with_loc ( (array_function_no_loc str name) ) loc)

   let mkrf =
    function
    | Ast.ReRecursive -> (Recursive)
    | Ast.ReNil -> (Nonrecursive)
    | _ -> assert false

   let mkli =
    fun sloc ->
     fun s ->
      fun list ->
       let rec loop =
        fun f ->
         function | (i :: il) -> (loop ( (ldot ( (f i) )) ) il) | [] -> (f s) in
       (with_loc ( (loop lident list) ) sloc)

   let rec ctyp_fa =
    fun al ->
     function
     | TyApp (_, f, a) -> (ctyp_fa ( ( a ) :: al  ) f)
     | f -> (f, al)

   let ident_tag =
    fun ?(conv_lid = fun x -> x) ->
     fun i ->
      let rec self =
       fun i ->
        fun acc ->
         (match i with
          | Ast.IdAcc (_, Ast.IdLid (_, "*predef*"), Ast.IdLid (_, "option")) ->
             (( (ldot ( (lident "*predef*") ) "option") ), `lident)
          | Ast.IdAcc (_, i1, i2) -> (self i2 ( (Some (self i1 acc)) ))
          | Ast.IdApp (_, i1, i2) ->
             let i' =
              (Lapply
                (( (fst ( (self i1 None ) )) ), ( (fst ( (self i2 None ) ))
                 ))) in
             let x =
              (match acc with
               | None -> i'
               | _ -> (error ( (loc_of_ident i) ) "invalid long identifier")) in
             (x, `app)
          | Ast.IdUid (_, s) ->
             let x =
              (match acc with
               | None -> (lident s)
               | Some (acc, (`uident | `app)) -> (ldot acc s)
               | _ -> (error ( (loc_of_ident i) ) "invalid long identifier")) in
             (x, `uident)
          | Ast.IdLid (_, s) ->
             let x =
              (match acc with
               | None -> (lident ( (conv_lid s) ))
               | Some (acc, (`uident | `app)) -> (ldot acc ( (conv_lid s) ))
               | _ -> (error ( (loc_of_ident i) ) "invalid long identifier")) in
             (x, `lident)
          | _ -> (error ( (loc_of_ident i) ) "invalid long identifier")) in
      (self i None )

   let ident_noloc =
    fun ?conv_lid -> fun i -> (fst ( (ident_tag ?conv_lid:conv_lid i) ))

   let ident =
    fun ?conv_lid ->
     fun i ->
      (with_loc ( (ident_noloc ?conv_lid:conv_lid i) ) ( (loc_of_ident i) ))

   let long_lident =
    fun msg ->
     fun id ->
      (match (ident_tag id) with
       | (i, `lident) -> (with_loc i ( (loc_of_ident id) ))
       | _ -> (error ( (loc_of_ident id) ) msg))

   let long_type_ident = (long_lident "invalid long identifier type")

   let long_class_ident = (long_lident "invalid class name")

   let long_uident_noloc =
    fun ?(conv_con = fun x -> x) ->
     fun i ->
      (match (ident_tag i) with
       | (Ldot (i, s), `uident) -> (ldot i ( (conv_con s) ))
       | (Lident (s), `uident) -> (lident ( (conv_con s) ))
       | (i, `app) -> i
       | _ -> (error ( (loc_of_ident i) ) "uppercase identifier expected"))

   let long_uident =
    fun ?conv_con ->
     fun i ->
      (with_loc ( (long_uident_noloc ?conv_con:conv_con i) ) (
        (loc_of_ident i) ))

   let rec ctyp_long_id_prefix =
    fun t ->
     (match t with
      | Ast.TyId (_, i) -> (ident_noloc i)
      | Ast.TyApp (_, m1, m2) ->
         let li1 = (ctyp_long_id_prefix m1) in
         let li2 = (ctyp_long_id_prefix m2) in (Lapply (li1, li2))
      | t -> (error ( (loc_of_ctyp t) ) "invalid module expression"))

   let ctyp_long_id =
    fun t ->
     (match t with
      | Ast.TyId (_, i) -> (false , ( (long_type_ident i) ))
      | TyApp (loc, _, _) -> (error loc "invalid type name")
      | TyCls (_, i) -> (true , ( (ident i) ))
      | t -> (error ( (loc_of_ctyp t) ) "invalid type"))

   let rec ty_var_list_of_ctyp =
    function
    | Ast.TyApp (_, t1, t2) ->
       (( (ty_var_list_of_ctyp t1) ) @ ( (ty_var_list_of_ctyp t2) ))
    | Ast.TyQuo (_, s) -> [s]
    | _ -> assert false

   let predef_option =
    fun loc ->
     (TyId
       (loc, (
        (IdAcc
          (loc, ( (IdLid (loc, "*predef*")) ), ( (IdLid (loc, "option")) )))
        )))

   let rec ctyp =
    function
    | TyId (loc, i) ->
       let li = (long_type_ident i) in
       (mktyp loc ( (Ptyp_constr (li, [] )) ))
    | TyAli (loc, t1, t2) ->
       let (t, i) =
        (match (t1, t2) with
         | (t, TyQuo (_, s)) -> (t, s)
         | (TyQuo (_, s), t) -> (t, s)
         | _ -> (error loc "invalid alias type")) in
       (mktyp loc ( (Ptyp_alias (( (ctyp t) ), i)) ))
    | TyAny (loc) -> (mktyp loc Ptyp_any )
    | (TyApp (loc, _, _) as f) ->
       let (f, al) = (ctyp_fa []  f) in
       let (is_cls, li) = (ctyp_long_id f) in
       if is_cls then
        (
        (mktyp loc ( (Ptyp_class (li, ( (List.map ctyp al) ), [] )) ))
        )
       else (mktyp loc ( (Ptyp_constr (li, ( (List.map ctyp al) ))) ))
    | TyArr (loc, TyLab (_, lab, t1), t2) ->
       (mktyp loc ( (Ptyp_arrow (lab, ( (ctyp t1) ), ( (ctyp t2) ))) ))
    | TyArr (loc, TyOlb (loc1, lab, t1), t2) ->
       let t1 = (TyApp (loc1, ( (predef_option loc1) ), t1)) in
       (mktyp loc (
         (Ptyp_arrow (( ("?" ^ lab) ), ( (ctyp t1) ), ( (ctyp t2) ))) ))
    | TyArr (loc, t1, t2) ->
       (mktyp loc ( (Ptyp_arrow ("", ( (ctyp t1) ), ( (ctyp t2) ))) ))
    | Ast.TyObj (loc, fl, Ast.RvNil) ->
       (mktyp loc ( (Ptyp_object (meth_list fl [] )) ))
    | Ast.TyObj (loc, fl, Ast.RvRowVar) ->
       (mktyp loc (
         (Ptyp_object (meth_list fl ( [( (mkfield loc Pfield_var ) )] ))) ))
    | TyCls (loc, id) ->
       (mktyp loc ( (Ptyp_class (( (ident id) ), [] , [] )) ))
    | Ast.TyPkg (loc, pt) ->
       let (i, cs) = (package_type pt) in
       (mktyp loc ( (Ptyp_package (i, cs)) ))
    | TyLab (loc, _, _) -> (error loc "labelled type not allowed here")
    | TyMan (loc, _, _) -> (error loc "manifest type not allowed here")
    | TyOlb (loc, _, _) -> (error loc "labelled type not allowed here")
    | TyPol (loc, t1, t2) ->
       (mktyp loc ( (Ptyp_poly (( (ty_var_list_of_ctyp t1) ), ( (ctyp t2) )))
         ))
    | TyQuo (loc, s) -> (mktyp loc ( (Ptyp_var (s)) ))
    | TyRec (loc, _) -> (error loc "record type not allowed here")
    | TySum (loc, _) -> (error loc "sum type not allowed here")
    | TyPrv (loc, _) -> (error loc "private type not allowed here")
    | TyMut (loc, _) -> (error loc "mutable type not allowed here")
    | TyOr (loc, _, _) -> (error loc "type1 | type2 not allowed here")
    | TyAnd (loc, _, _) -> (error loc "type1 and type2 not allowed here")
    | TyOf (loc, _, _) -> (error loc "type1 of type2 not allowed here")
    | TyCol (loc, _, _) -> (error loc "type1 : type2 not allowed here")
    | TySem (loc, _, _) -> (error loc "type1 ; type2 not allowed here")
    | Ast.TyTup (loc, Ast.TySta (_, t1, t2)) ->
       (mktyp loc (
         (Ptyp_tuple
           (List.map ctyp ( (list_of_ctyp t1 ( (list_of_ctyp t2 [] ) )) )))
         ))
    | Ast.TyVrnEq (loc, t) ->
       (mktyp loc ( (Ptyp_variant (( (row_field t) ), true , None )) ))
    | Ast.TyVrnSup (loc, t) ->
       (mktyp loc ( (Ptyp_variant (( (row_field t) ), false , None )) ))
    | Ast.TyVrnInf (loc, t) ->
       (mktyp loc (
         (Ptyp_variant (( (row_field t) ), true , ( (Some (([]))) ))) ))
    | Ast.TyVrnInfSup (loc, t, t') ->
       (mktyp loc (
         (Ptyp_variant (( (row_field t) ), true , ( (Some (name_tags t')) )))
         ))
    | TyAnt (loc, _) -> (error loc "antiquotation not allowed here")
    | (((((((((((((TyOfAmp (_, _, _) | TyAmp (_, _, _)) | TySta (_, _, _))
                 | TyCom (_, _, _)) | TyVrn (_, _)) | TyQuM (_, _))
              | TyQuP (_, _)) | TyDcl (_, _, _, _, _)) | TyAnP (_))
           | TyAnM (_)) | TyTypePol (_, _, _)) | TyObj (_, _, RvAnt (_)))
        | TyNil (_)) | TyTup (_, _)) ->
       assert false
   and row_field =
    function
    | Ast.TyNil (_) -> ([])
    | Ast.TyVrn (_, i) -> [( (Rtag (i, true , [] )) )]
    | Ast.TyOfAmp (_, Ast.TyVrn (_, i), t) ->
       [( (Rtag (i, true , ( (List.map ctyp ( (list_of_ctyp t [] ) )) ))) )]
    | Ast.TyOf (_, Ast.TyVrn (_, i), t) ->
       [( (Rtag (i, false , ( (List.map ctyp ( (list_of_ctyp t [] ) )) ))) )]
    | Ast.TyOr (_, t1, t2) -> (( (row_field t1) ) @ ( (row_field t2) ))
    | t -> [( (Rinherit (ctyp t)) )]
   and name_tags =
    function
    | Ast.TyApp (_, t1, t2) -> (( (name_tags t1) ) @ ( (name_tags t2) ))
    | Ast.TyVrn (_, s) -> [s]
    | _ -> assert false
   and meth_list =
    fun fl ->
     fun acc ->
      (match fl with
       | Ast.TyNil (_) -> acc
       | Ast.TySem (_, t1, t2) -> (meth_list t1 ( (meth_list t2 acc) ))
       | Ast.TyCol (loc, Ast.TyId (_, Ast.IdLid (_, lab)), t) ->
          ( ( (mkfield loc ( (Pfield (lab, ( (mkpolytype ( (ctyp t) )) ))) ))
           ) ) :: acc 
       | _ -> assert false)
   and package_type_constraints =
    fun wc ->
     fun acc ->
      (match wc with
       | Ast.WcNil (_) -> acc
       | Ast.WcTyp (_, Ast.TyId (_, id), ct) ->
          ( (( (ident id) ), ( (ctyp ct) )) ) :: acc 
       | Ast.WcAnd (_, wc1, wc2) ->
          (package_type_constraints wc1 ( (package_type_constraints wc2 acc)
            ))
       | _ ->
          (error ( (loc_of_with_constr wc) )
            "unexpected `with constraint' for a package type"))
   and package_type =
    (function
     | Ast.MtWit (_, Ast.MtId (_, i), wc) ->
        (( (long_uident i) ), ( (package_type_constraints wc [] ) ))
     | Ast.MtId (_, i) -> (( (long_uident i) ), [] )
     | mt -> (error ( (loc_of_module_type mt) ) "unexpected package type") :
      (module_type -> package_type))

   let mktype =
    fun loc ->
     fun tl ->
      fun cl ->
       fun tk ->
        fun tp ->
         fun tm ->
          let (params, variance) = (List.split tl) in
          {ptype_params = params; ptype_cstrs = cl; ptype_kind = tk;
           ptype_private = tp; ptype_manifest = tm;
           ptype_loc = ( (mkloc loc) ); ptype_variance = variance}

   let mkprivate' = fun m -> if m then Private  else (Public)

   let mkprivate =
    function
    | Ast.PrPrivate -> (Private)
    | Ast.PrNil -> (Public)
    | _ -> assert false

   let mktrecord =
    function
    | Ast.TyCol (loc, Ast.TyId (_, Ast.IdLid (sloc, s)), Ast.TyMut (_, t)) ->
       (( (with_loc s sloc) ), Mutable , ( (mkpolytype ( (ctyp t) )) ), (
        (mkloc loc) ))
    | Ast.TyCol (loc, Ast.TyId (_, Ast.IdLid (sloc, s)), t) ->
       (( (with_loc s sloc) ), Immutable , ( (mkpolytype ( (ctyp t) )) ), (
        (mkloc loc) ))
    | _ -> assert false

   let mkvariant =
    function
    | Ast.TyId (loc, Ast.IdUid (sloc, s)) ->
       (( (with_loc ( (conv_con s) ) sloc) ), [] , None , ( (mkloc loc) ))
    | Ast.TyOf (loc, Ast.TyId (_, Ast.IdUid (sloc, s)), t) ->
       (( (with_loc ( (conv_con s) ) sloc) ), (
        (List.map ctyp ( (list_of_ctyp t [] ) )) ), None , ( (mkloc loc) ))
    | Ast.TyCol (loc, Ast.TyId (_, Ast.IdUid (sloc, s)), Ast.TyArr (_, t, u)) ->
       (( (with_loc ( (conv_con s) ) sloc) ), (
        (List.map ctyp ( (list_of_ctyp t [] ) )) ), ( (Some (ctyp u)) ), (
        (mkloc loc) ))
    | Ast.TyCol (loc, Ast.TyId (_, Ast.IdUid (sloc, s)), t) ->
       (( (with_loc ( (conv_con s) ) sloc) ), [] , ( (Some (ctyp t)) ), (
        (mkloc loc) ))
    | _ -> assert false

   let rec type_decl =
    fun tl ->
     fun cl ->
      fun loc ->
       fun m ->
        fun pflag ->
         function
         | Ast.TyMan (_, t1, t2) ->
            (type_decl tl cl loc ( (Some (ctyp t1)) ) pflag t2)
         | Ast.TyPrv (_, t) -> (type_decl tl cl loc m true  t)
         | Ast.TyRec (_, t) ->
            (mktype loc tl cl (
              (Ptype_record (List.map mktrecord ( (list_of_ctyp t [] ) ))) )
              ( (mkprivate' pflag) ) m)
         | Ast.TySum (_, t) ->
            (mktype loc tl cl (
              (Ptype_variant (List.map mkvariant ( (list_of_ctyp t [] ) ))) )
              ( (mkprivate' pflag) ) m)
         | t ->
            if (m <> None ) then
             (
             (error loc "only one manifest type allowed by definition")
             )
            else
             let m =
              (match t with | Ast.TyNil (_) -> (None) | _ -> (Some (ctyp t))) in
             (mktype loc tl cl Ptype_abstract  ( (mkprivate' pflag) ) m)

   let type_decl =
    fun tl ->
     fun cl -> fun t -> fun loc -> (type_decl tl cl loc None  false  t)

   let mkvalue_desc =
    fun loc ->
     fun t ->
      fun p ->
       {pval_type = ( (ctyp t) ); pval_prim = p; pval_loc = ( (mkloc loc) )}

   let rec list_of_meta_list =
    function
    | Ast.LNil -> ([])
    | Ast.LCons (x, xs) -> ( x ) :: (list_of_meta_list xs) 
    | Ast.LAnt (_) -> assert false

   let mkmutable =
    function
    | Ast.MuMutable -> (Mutable)
    | Ast.MuNil -> (Immutable)
    | _ -> assert false

   let paolab =
    fun lab ->
     fun p ->
      (match (lab, p) with
       | ("",
          (Ast.PaId (_, Ast.IdLid (_, i))
           | Ast.PaTyc (_, Ast.PaId (_, Ast.IdLid (_, i)), _))) ->
          i
       | ("", p) -> (error ( (loc_of_patt p) ) "bad ast in label")
       | _ -> lab)

   let opt_private_ctyp =
    function
    | Ast.TyPrv (_, t) -> (Ptype_abstract , Private , ( (ctyp t) ))
    | t -> (Ptype_abstract , Public , ( (ctyp t) ))

   let rec type_parameters =
    fun t ->
     fun acc ->
      (match t with
       | Ast.TyApp (_, t1, t2) ->
          (type_parameters t1 ( (type_parameters t2 acc) ))
       | Ast.TyQuP (_, s) -> ( (s, (true , false )) ) :: acc 
       | Ast.TyQuM (_, s) -> ( (s, (false , true )) ) :: acc 
       | Ast.TyQuo (_, s) -> ( (s, (false , false )) ) :: acc 
       | _ -> assert false)

   let rec optional_type_parameters =
    fun t ->
     fun acc ->
      (match t with
       | Ast.TyApp (_, t1, t2) ->
          (optional_type_parameters t1 ( (optional_type_parameters t2 acc) ))
       | Ast.TyQuP (loc, s) ->
          ( (( (Some (with_loc s loc)) ), (true , false )) ) :: acc 
       | Ast.TyAnP (_loc) -> ( (None , (true , false )) ) :: acc 
       | Ast.TyQuM (loc, s) ->
          ( (( (Some (with_loc s loc)) ), (false , true )) ) :: acc 
       | Ast.TyAnM (_loc) -> ( (None , (false , true )) ) :: acc 
       | Ast.TyQuo (loc, s) ->
          ( (( (Some (with_loc s loc)) ), (false , false )) ) :: acc 
       | Ast.TyAny (_loc) -> ( (None , (false , false )) ) :: acc 
       | _ -> assert false)

   let rec class_parameters =
    fun t ->
     fun acc ->
      (match t with
       | Ast.TyCom (_, t1, t2) ->
          (class_parameters t1 ( (class_parameters t2 acc) ))
       | Ast.TyQuP (loc, s) ->
          ( (( (with_loc s loc) ), (true , false )) ) :: acc 
       | Ast.TyQuM (loc, s) ->
          ( (( (with_loc s loc) ), (false , true )) ) :: acc 
       | Ast.TyQuo (loc, s) ->
          ( (( (with_loc s loc) ), (false , false )) ) :: acc 
       | _ -> assert false)

   let rec type_parameters_and_type_name =
    fun t ->
     fun acc ->
      (match t with
       | Ast.TyApp (_, t1, t2) ->
          (type_parameters_and_type_name t1 (
            (optional_type_parameters t2 acc) ))
       | Ast.TyId (_, i) -> (( (ident i) ), acc)
       | _ -> assert false)

   let mkwithtyp =
    fun pwith_type ->
     fun loc ->
      fun id_tpl ->
       fun ct ->
        let (id, tpl) = (type_parameters_and_type_name id_tpl [] ) in
        let (params, variance) = (List.split tpl) in
        let (kind, priv, ct) = (opt_private_ctyp ct) in
        (id, (
         (pwith_type
           {ptype_params = params; ptype_cstrs = [] ; ptype_kind = kind;
            ptype_private = priv; ptype_manifest = ( (Some (ct)) );
            ptype_loc = ( (mkloc loc) ); ptype_variance = variance}) ))

   let rec mkwithc =
    fun wc ->
     fun acc ->
      (match wc with
       | Ast.WcNil (_) -> acc
       | Ast.WcTyp (loc, id_tpl, ct) ->
          ( ( (mkwithtyp ( fun x -> (Pwith_type (x)) ) loc id_tpl ct) ) ) ::
           acc 
       | Ast.WcMod (_, i1, i2) ->
          ( (( (long_uident i1) ), ( (Pwith_module (long_uident i2)) )) ) ::
           acc 
       | Ast.WcTyS (loc, id_tpl, ct) ->
          ( ( (mkwithtyp ( fun x -> (Pwith_typesubst (x)) ) loc id_tpl ct)
           ) ) :: acc 
       | Ast.WcMoS (_, i1, i2) ->
          (
           (( (long_uident i1) ), ( (Pwith_modsubst (long_uident i2)) )) ) ::
           acc 
       | Ast.WcAnd (_, wc1, wc2) -> (mkwithc wc1 ( (mkwithc wc2 acc) ))
       | Ast.WcAnt (loc, _) ->
          (error loc "bad with constraint (antiquotation)"))

   let rec patt_fa =
    fun al ->
     function
     | PaApp (_, f, a) -> (patt_fa ( ( a ) :: al  ) f)
     | f -> (f, al)

   let rec deep_mkrangepat =
    fun loc ->
     fun c1 ->
      fun c2 ->
       if (c1 = c2) then
        (
        (mkghpat loc ( (Ppat_constant ((Const_char (c1)))) ))
        )
       else
        (mkghpat loc (
          (Ppat_or
            (( (mkghpat loc ( (Ppat_constant ((Const_char (c1)))) )) ), (
             (deep_mkrangepat loc ( (Char.chr ( (( (Char.code c1) ) + 1) )) )
               c2) ))) ))

   let rec mkrangepat =
    fun loc ->
     fun c1 ->
      fun c2 ->
       if (c1 > c2) then ( (mkrangepat loc c2 c1) )
       else if (c1 = c2) then
             (
             (mkpat loc ( (Ppat_constant ((Const_char (c1)))) ))
             )
       else
        (mkpat loc (
          (Ppat_or
            (( (mkghpat loc ( (Ppat_constant ((Const_char (c1)))) )) ), (
             (deep_mkrangepat loc ( (Char.chr ( (( (Char.code c1) ) + 1) )) )
               c2) ))) ))

   let rec patt =
    function
    | Ast.PaId (loc, Ast.IdLid (sloc, s)) ->
       (mkpat loc ( (Ppat_var (with_loc s sloc)) ))
    | Ast.PaId (loc, i) ->
       let p =
        (Ppat_construct
          (( (long_uident ~conv_con:conv_con i) ), None , (
           (constructors_arity () ) ))) in
       (mkpat loc p)
    | PaAli (loc, p1, p2) ->
       let (p, i) =
        (match (p1, p2) with
         | (p, Ast.PaId (_, Ast.IdLid (sloc, s))) ->
            (p, ( (with_loc s sloc) ))
         | (Ast.PaId (_, Ast.IdLid (sloc, s)), p) ->
            (p, ( (with_loc s sloc) ))
         | _ -> (error loc "invalid alias pattern")) in
       (mkpat loc ( (Ppat_alias (( (patt p) ), i)) ))
    | PaAnt (loc, _) -> (error loc "antiquotation not allowed here")
    | PaAny (loc) -> (mkpat loc Ppat_any )
    | Ast.PaApp
       (loc, Ast.PaId (_, Ast.IdUid (sloc, s)),
        Ast.PaTup (_, Ast.PaAny (loc_any))) ->
       (mkpat loc (
         (Ppat_construct
           (( (lident_with_loc ( (conv_con s) ) sloc) ), (
            (Some (mkpat loc_any Ppat_any )) ), false )) ))
    | (PaApp (loc, _, _) as f) ->
       let (f, al) = (patt_fa []  f) in
       let al = (List.map patt al) in
       (match (patt f).ppat_desc with
        | Ppat_construct (li, None, _) ->
           if (constructors_arity () ) then
            (
            (mkpat loc (
              (Ppat_construct
                (li, ( (Some (mkpat loc ( (Ppat_tuple (al)) ))) ), true )) ))
            )
           else
            let a =
             (match al with
              | (a :: []) -> a
              | _ -> (mkpat loc ( (Ppat_tuple (al)) ))) in
            (mkpat loc ( (Ppat_construct (li, ( (Some (a)) ), false )) ))
        | Ppat_variant (s, None) ->
           let a =
            if (constructors_arity () ) then
             (
             (mkpat loc ( (Ppat_tuple (al)) ))
             )
            else
             (match al with
              | (a :: []) -> a
              | _ -> (mkpat loc ( (Ppat_tuple (al)) ))) in
           (mkpat loc ( (Ppat_variant (s, ( (Some (a)) ))) ))
        | _ ->
           (error ( (loc_of_patt f) )
             "this is not a constructor, it cannot be applied in a pattern"))
    | PaArr (loc, p) ->
       (mkpat loc ( (Ppat_array (List.map patt ( (list_of_patt p [] ) ))) ))
    | PaChr (loc, s) ->
       (mkpat loc ( (Ppat_constant ((Const_char (char_of_char_token loc s))))
         ))
    | PaInt (loc, s) ->
       let i =
        (try (int_of_string s) with
         Failure (_) ->
          (error loc
            "Integer literal exceeds the range of representable integers of type int")) in
       (mkpat loc ( (Ppat_constant ((Const_int (i)))) ))
    | PaInt32 (loc, s) ->
       let i32 =
        (try (Int32.of_string s) with
         Failure (_) ->
          (error loc
            "Integer literal exceeds the range of representable integers of type int32")) in
       (mkpat loc ( (Ppat_constant ((Const_int32 (i32)))) ))
    | PaInt64 (loc, s) ->
       let i64 =
        (try (Int64.of_string s) with
         Failure (_) ->
          (error loc
            "Integer literal exceeds the range of representable integers of type int64")) in
       (mkpat loc ( (Ppat_constant ((Const_int64 (i64)))) ))
    | PaNativeInt (loc, s) ->
       let nati =
        (try (Nativeint.of_string s) with
         Failure (_) ->
          (error loc
            "Integer literal exceeds the range of representable integers of type nativeint")) in
       (mkpat loc ( (Ppat_constant ((Const_nativeint (nati)))) ))
    | PaFlo (loc, s) ->
       (mkpat loc ( (Ppat_constant ((Const_float (remove_underscores s)))) ))
    | PaLab (loc, _, _) -> (error loc "labeled pattern not allowed here")
    | (PaOlb (loc, _, _) | PaOlbi (loc, _, _, _)) ->
       (error loc "labeled pattern not allowed here")
    | PaOrp (loc, p1, p2) ->
       (mkpat loc ( (Ppat_or (( (patt p1) ), ( (patt p2) ))) ))
    | PaRng (loc, p1, p2) ->
       (match (p1, p2) with
        | (PaChr (loc1, c1), PaChr (loc2, c2)) ->
           let c1 = (char_of_char_token loc1 c1) in
           let c2 = (char_of_char_token loc2 c2) in (mkrangepat loc c1 c2)
        | _ -> (error loc "range pattern allowed only for characters"))
    | PaRec (loc, p) ->
       let ps = (list_of_patt p [] ) in
       let is_wildcard = function | Ast.PaAny (_) -> (true) | _ -> (false) in
       let (wildcards, ps) = (List.partition is_wildcard ps) in
       let is_closed = if (wildcards = [] ) then Closed  else (Open) in
       (mkpat loc ( (Ppat_record (( (List.map mklabpat ps) ), is_closed)) ))
    | PaStr (loc, s) ->
       (mkpat loc (
         (Ppat_constant ((Const_string (string_of_string_token loc s)))) ))
    | Ast.PaTup (loc, Ast.PaCom (_, p1, p2)) ->
       (mkpat loc (
         (Ppat_tuple
           (List.map patt ( (list_of_patt p1 ( (list_of_patt p2 [] ) )) )))
         ))
    | Ast.PaTup (loc, _) -> (error loc "singleton tuple pattern")
    | PaTyc (loc, p, t) ->
       (mkpat loc ( (Ppat_constraint (( (patt p) ), ( (ctyp t) ))) ))
    | PaTyp (loc, i) -> (mkpat loc ( (Ppat_type (long_type_ident i)) ))
    | PaVrn (loc, s) ->
       (mkpat loc ( (Ppat_variant (( (conv_con s) ), None )) ))
    | PaLaz (loc, p) -> (mkpat loc ( (Ppat_lazy (patt p)) ))
    | PaMod (loc, m) -> (mkpat loc ( (Ppat_unpack (with_loc m loc)) ))
    | ((((PaEq (_, _, _) | PaSem (_, _, _)) | PaCom (_, _, _)) | PaNil (_)) as
       p) ->
       (error ( (loc_of_patt p) ) "invalid pattern")
   and mklabpat =
    function
    | Ast.PaEq (_, i, p) -> (( (ident ~conv_lid:conv_lab i) ), ( (patt p) ))
    | p -> (error ( (loc_of_patt p) ) "invalid pattern")

   let rec expr_fa =
    fun al ->
     function
     | ExApp (_, f, a) -> (expr_fa ( ( a ) :: al  ) f)
     | f -> (f, al)

   let rec class_expr_fa =
    fun al ->
     function
     | CeApp (_, ce, a) -> (class_expr_fa ( ( a ) :: al  ) ce)
     | ce -> (ce, al)

   let rec sep_expr_acc =
    fun l ->
     function
     | ExAcc (_, e1, e2) -> (sep_expr_acc ( (sep_expr_acc l e2) ) e1)
     | (Ast.ExId (loc, Ast.IdUid (_, s)) as e) ->
        (match l with
         | [] -> [(loc, [] , e)]
         | ((loc', sl, e) :: l) ->
            ( (( (Loc.merge loc loc') ), ( ( s ) :: sl  ), e) ) :: l )
     | Ast.ExId (_, (Ast.IdAcc (_, _, _) as i)) ->
        let rec normalize_acc =
         function
         | Ast.IdAcc (_loc, i1, i2) ->
            (Ast.ExAcc (_loc, ( (normalize_acc i1) ), ( (normalize_acc i2) )))
         | Ast.IdApp (_loc, i1, i2) ->
            (Ast.ExApp (_loc, ( (normalize_acc i1) ), ( (normalize_acc i2) )))
         | (((Ast.IdAnt (_loc, _) | Ast.IdUid (_loc, _))
             | Ast.IdLid (_loc, _)) as i) ->
            (Ast.ExId (_loc, i)) in
        (sep_expr_acc l ( (normalize_acc i) ))
     | e -> ( (( (loc_of_expr e) ), [] , e) ) :: l 

   let override_flag =
    fun loc ->
     function
     | Ast.OvOverride -> (Override)
     | Ast.OvNil -> (Fresh)
     | _ -> (error loc "antiquotation not allowed here")

   let list_of_opt_ctyp =
    fun ot ->
     fun acc ->
      (match ot with | Ast.TyNil (_) -> acc | t -> (list_of_ctyp t acc))

   let varify_constructors =
    fun var_names ->
     let rec loop =
      fun t ->
       let desc =
        (match t.ptyp_desc with
         | Ptyp_any -> (Ptyp_any)
         | Ptyp_var (x) -> (Ptyp_var (x))
         | Ptyp_arrow (label, core_type, core_type') ->
            (Ptyp_arrow (label, ( (loop core_type) ), ( (loop core_type') )))
         | Ptyp_tuple (lst) -> (Ptyp_tuple (List.map loop lst))
         | Ptyp_constr ({txt = Lident (s)}, []) when (List.mem s var_names) ->
            (Ptyp_var ("&" ^ s))
         | Ptyp_constr (longident, lst) ->
            (Ptyp_constr (longident, ( (List.map loop lst) )))
         | Ptyp_object (lst) -> (Ptyp_object (List.map loop_core_field lst))
         | Ptyp_class (longident, lst, lbl_list) ->
            (Ptyp_class (longident, ( (List.map loop lst) ), lbl_list))
         | Ptyp_alias (core_type, string) ->
            (Ptyp_alias (( (loop core_type) ), string))
         | Ptyp_variant (row_field_list, flag, lbl_lst_option) ->
            (Ptyp_variant
              (( (List.map loop_row_field row_field_list) ), flag,
               lbl_lst_option))
         | Ptyp_poly (string_lst, core_type) ->
            (Ptyp_poly (string_lst, ( (loop core_type) )))
         | Ptyp_package (longident, lst) ->
            (Ptyp_package
              (longident, (
               (List.map ( fun (n, typ) -> (n, ( (loop typ) )) ) lst) )))) in
       {t with ptyp_desc = desc}
     and loop_core_field =
      fun t ->
       let desc =
        (match t.pfield_desc with
         | Pfield (n, typ) -> (Pfield (n, ( (loop typ) )))
         | Pfield_var -> (Pfield_var)) in
       {t with pfield_desc = desc}
     and loop_row_field =
      fun x ->
       (match x with
        | Rtag (label, flag, lst) ->
           (Rtag (label, flag, ( (List.map loop lst) )))
        | Rinherit (t) -> (Rinherit (loop t))) in
     loop

   let rec expr =
    function
    | Ast.ExAcc (loc, x, Ast.ExId (_, Ast.IdLid (_, "val"))) ->
       (mkexp loc (
         (Pexp_apply
           (( (mkexp loc ( (Pexp_ident (lident_with_loc "!" loc)) )) ), (
            [("", ( (expr x) ))] ))) ))
    | ((ExAcc (loc, _, _) | Ast.ExId (loc, Ast.IdAcc (_, _, _))) as e) ->
       let (e, l) =
        (match (sep_expr_acc []  e) with
         | ((loc, ml, Ast.ExId (sloc, Ast.IdUid (_, s))) :: l) ->
            let ca = (constructors_arity () ) in
            ((
             (mkexp loc (
               (Pexp_construct
                 (( (mkli sloc ( (conv_con s) ) ml) ), None , ca)) )) ), l)
         | ((loc, ml, Ast.ExId (sloc, Ast.IdLid (_, s))) :: l) ->
            (( (mkexp loc ( (Pexp_ident (mkli sloc s ml)) )) ), l)
         | ((_, [], e) :: l) -> (( (expr e) ), l)
         | _ -> (error loc "bad ast in expression")) in
       let (_, e) =
        (List.fold_left (
          fun (loc_bp, e1) ->
           fun (loc_ep, ml, e2) ->
            (match e2 with
             | Ast.ExId (sloc, Ast.IdLid (_, s)) ->
                let loc = (Loc.merge loc_bp loc_ep) in
                (loc, (
                 (mkexp loc (
                   (Pexp_field (e1, ( (mkli sloc ( (conv_lab s) ) ml) ))) ))
                 ))
             | _ ->
                (error ( (loc_of_expr e2) ) "lowercase identifier expected"))
          ) (loc, e) l) in
       e
    | ExAnt (loc, _) -> (error loc "antiquotation not allowed here")
    | (ExApp (loc, _, _) as f) ->
       let (f, al) = (expr_fa []  f) in
       let al = (List.map label_expr al) in
       (match (expr f).pexp_desc with
        | Pexp_construct (li, None, _) ->
           let al = (List.map snd al) in
           if (constructors_arity () ) then
            (
            (mkexp loc (
              (Pexp_construct
                (li, ( (Some (mkexp loc ( (Pexp_tuple (al)) ))) ), true )) ))
            )
           else
            let a =
             (match al with
              | (a :: []) -> a
              | _ -> (mkexp loc ( (Pexp_tuple (al)) ))) in
            (mkexp loc ( (Pexp_construct (li, ( (Some (a)) ), false )) ))
        | Pexp_variant (s, None) ->
           let al = (List.map snd al) in
           let a =
            if (constructors_arity () ) then
             (
             (mkexp loc ( (Pexp_tuple (al)) ))
             )
            else
             (match al with
              | (a :: []) -> a
              | _ -> (mkexp loc ( (Pexp_tuple (al)) ))) in
           (mkexp loc ( (Pexp_variant (s, ( (Some (a)) ))) ))
        | _ -> (mkexp loc ( (Pexp_apply (( (expr f) ), al)) )))
    | ExAre (loc, e1, e2) ->
       (mkexp loc (
         (Pexp_apply
           (( (mkexp loc ( (Pexp_ident (array_function loc "Array" "get")) ))
            ), ( [("", ( (expr e1) )); ("", ( (expr e2) ))] ))) ))
    | ExArr (loc, e) ->
       (mkexp loc ( (Pexp_array (List.map expr ( (list_of_expr e [] ) ))) ))
    | ExAsf (loc) -> (mkexp loc Pexp_assertfalse )
    | ExAss (loc, e, v) ->
       let e =
        (match e with
         | Ast.ExAcc (loc, x, Ast.ExId (_, Ast.IdLid (_, "val"))) ->
            (Pexp_apply
              (( (mkexp loc ( (Pexp_ident (lident_with_loc ":=" loc)) )) ), (
               [("", ( (expr x) )); ("", ( (expr v) ))] )))
         | ExAcc (loc, _, _) ->
            (match (expr e).pexp_desc with
             | Pexp_field (e, lab) -> (Pexp_setfield (e, lab, ( (expr v) )))
             | _ -> (error loc "bad record access"))
         | ExAre (loc, e1, e2) ->
            (Pexp_apply
              ((
               (mkexp loc ( (Pexp_ident (array_function loc "Array" "set"))
                 )) ), (
               [("", ( (expr e1) )); ("", ( (expr e2) )); ("", ( (expr v) ))]
               )))
         | Ast.ExId (_, Ast.IdLid (lloc, lab)) ->
            (Pexp_setinstvar (( (with_loc lab lloc) ), ( (expr v) )))
         | ExSte (loc, e1, e2) ->
            (Pexp_apply
              ((
               (mkexp loc ( (Pexp_ident (array_function loc "String" "set"))
                 )) ), (
               [("", ( (expr e1) )); ("", ( (expr e2) )); ("", ( (expr v) ))]
               )))
         | _ -> (error loc "bad left part of assignment")) in
       (mkexp loc e)
    | ExAsr (loc, e) -> (mkexp loc ( (Pexp_assert (expr e)) ))
    | ExChr (loc, s) ->
       (mkexp loc ( (Pexp_constant ((Const_char (char_of_char_token loc s))))
         ))
    | ExCoe (loc, e, t1, t2) ->
       let t1 =
        (match t1 with | Ast.TyNil (_) -> (None) | t -> (Some (ctyp t))) in
       (mkexp loc (
         (Pexp_constraint (( (expr e) ), t1, ( (Some (ctyp t2)) ))) ))
    | ExFlo (loc, s) ->
       (mkexp loc ( (Pexp_constant ((Const_float (remove_underscores s)))) ))
    | ExFor (loc, i, e1, e2, df, el) ->
       let e3 = (ExSeq (loc, el)) in
       (mkexp loc (
         (Pexp_for
           (( (with_loc i loc) ), ( (expr e1) ), ( (expr e2) ), (
            (mkdirection df) ), ( (expr e3) ))) ))
    | Ast.ExFun (loc, Ast.McArr (_, PaLab (_, lab, po), w, e)) ->
       (mkexp loc (
         (Pexp_function
           (lab, None , (
            [(( (patt_of_lab loc lab po) ), ( (when_expr e w) ))] ))) ))
    | Ast.ExFun (loc, Ast.McArr (_, PaOlbi (_, lab, p, e1), w, e2)) ->
       let lab = (paolab lab p) in
       (mkexp loc (
         (Pexp_function
           (( ("?" ^ lab) ), ( (Some (expr e1)) ), (
            [(( (patt p) ), ( (when_expr e2 w) ))] ))) ))
    | Ast.ExFun (loc, Ast.McArr (_, PaOlb (_, lab, p), w, e)) ->
       let lab = (paolab lab p) in
       (mkexp loc (
         (Pexp_function
           (( ("?" ^ lab) ), None , (
            [(( (patt_of_lab loc lab p) ), ( (when_expr e w) ))] ))) ))
    | ExFun (loc, a) ->
       (mkexp loc ( (Pexp_function ("", None , ( (match_case a [] ) ))) ))
    | ExIfe (loc, e1, e2, e3) ->
       (mkexp loc (
         (Pexp_ifthenelse
           (( (expr e1) ), ( (expr e2) ), ( (Some (expr e3)) ))) ))
    | ExInt (loc, s) ->
       let i =
        (try (int_of_string s) with
         Failure (_) ->
          (error loc
            "Integer literal exceeds the range of representable integers of type int")) in
       (mkexp loc ( (Pexp_constant ((Const_int (i)))) ))
    | ExInt32 (loc, s) ->
       let i32 =
        (try (Int32.of_string s) with
         Failure (_) ->
          (error loc
            "Integer literal exceeds the range of representable integers of type int32")) in
       (mkexp loc ( (Pexp_constant ((Const_int32 (i32)))) ))
    | ExInt64 (loc, s) ->
       let i64 =
        (try (Int64.of_string s) with
         Failure (_) ->
          (error loc
            "Integer literal exceeds the range of representable integers of type int64")) in
       (mkexp loc ( (Pexp_constant ((Const_int64 (i64)))) ))
    | ExNativeInt (loc, s) ->
       let nati =
        (try (Nativeint.of_string s) with
         Failure (_) ->
          (error loc
            "Integer literal exceeds the range of representable integers of type nativeint")) in
       (mkexp loc ( (Pexp_constant ((Const_nativeint (nati)))) ))
    | ExLab (loc, _, _) -> (error loc "labeled expression not allowed here")
    | ExLaz (loc, e) -> (mkexp loc ( (Pexp_lazy (expr e)) ))
    | ExLet (loc, rf, bi, e) ->
       (mkexp loc (
         (Pexp_let (( (mkrf rf) ), ( (binding bi [] ) ), ( (expr e) ))) ))
    | ExLmd (loc, i, me, e) ->
       (mkexp loc (
         (Pexp_letmodule
           (( (with_loc i loc) ), ( (module_expr me) ), ( (expr e) ))) ))
    | ExMat (loc, e, a) ->
       (mkexp loc ( (Pexp_match (( (expr e) ), ( (match_case a [] ) ))) ))
    | ExNew (loc, id) -> (mkexp loc ( (Pexp_new (long_type_ident id)) ))
    | ExObj (loc, po, cfl) ->
       let p = (match po with | Ast.PaNil (_) -> (Ast.PaAny (loc)) | p -> p) in
       let cil = (class_str_item cfl [] ) in
       (mkexp loc (
         (Pexp_object ({pcstr_pat = ( (patt p) ); pcstr_fields = cil})) ))
    | ExOlb (loc, _, _) -> (error loc "labeled expression not allowed here")
    | ExOvr (loc, iel) -> (mkexp loc ( (Pexp_override (mkideexp iel [] )) ))
    | ExRec (loc, lel, eo) ->
       (match lel with
        | Ast.RbNil (_) -> (error loc "empty record")
        | _ ->
           let eo =
            (match eo with | Ast.ExNil (_) -> (None) | e -> (Some (expr e))) in
           (mkexp loc ( (Pexp_record (( (mklabexp lel [] ) ), eo)) )))
    | ExSeq (_loc, e) ->
       let rec loop =
        function
        | [] -> (expr ( (Ast.ExId (_loc, ( (Ast.IdUid (_loc, "()")) ))) ))
        | (e :: []) -> (expr e)
        | (e :: el) ->
           let _loc = (Loc.merge ( (loc_of_expr e) ) _loc) in
           (mkexp _loc ( (Pexp_sequence (( (expr e) ), ( (loop el) ))) )) in
       (loop ( (list_of_expr e [] ) ))
    | ExSnd (loc, e, s) -> (mkexp loc ( (Pexp_send (( (expr e) ), s)) ))
    | ExSte (loc, e1, e2) ->
       (mkexp loc (
         (Pexp_apply
           ((
            (mkexp loc ( (Pexp_ident (array_function loc "String" "get")) ))
            ), ( [("", ( (expr e1) )); ("", ( (expr e2) ))] ))) ))
    | ExStr (loc, s) ->
       (mkexp loc (
         (Pexp_constant ((Const_string (string_of_string_token loc s)))) ))
    | ExTry (loc, e, a) ->
       (mkexp loc ( (Pexp_try (( (expr e) ), ( (match_case a [] ) ))) ))
    | Ast.ExTup (loc, Ast.ExCom (_, e1, e2)) ->
       (mkexp loc (
         (Pexp_tuple
           (List.map expr ( (list_of_expr e1 ( (list_of_expr e2 [] ) )) )))
         ))
    | Ast.ExTup (loc, _) -> (error loc "singleton tuple")
    | ExTyc (loc, e, t) ->
       (mkexp loc (
         (Pexp_constraint (( (expr e) ), ( (Some (ctyp t)) ), None )) ))
    | Ast.ExId (loc, Ast.IdUid (_, "()")) ->
       (mkexp loc (
         (Pexp_construct (( (lident_with_loc "()" loc) ), None , true )) ))
    | Ast.ExId (loc, Ast.IdLid (_, s)) ->
       (mkexp loc ( (Pexp_ident (lident_with_loc s loc)) ))
    | Ast.ExId (loc, Ast.IdUid (_, s)) ->
       (mkexp loc (
         (Pexp_construct
           (( (lident_with_loc ( (conv_con s) ) loc) ), None , true )) ))
    | ExVrn (loc, s) ->
       (mkexp loc ( (Pexp_variant (( (conv_con s) ), None )) ))
    | ExWhi (loc, e1, el) ->
       let e2 = (ExSeq (loc, el)) in
       (mkexp loc ( (Pexp_while (( (expr e1) ), ( (expr e2) ))) ))
    | Ast.ExOpI (loc, i, e) ->
       (mkexp loc ( (Pexp_open (( (long_uident i) ), ( (expr e) ))) ))
    | Ast.ExPkg (loc, Ast.MeTyc (_, me, pt)) ->
       (mkexp loc (
         (Pexp_constraint
           (( (mkexp loc ( (Pexp_pack (module_expr me)) )) ), (
            (Some (mktyp loc ( (Ptyp_package (package_type pt)) ))) ), None ))
         ))
    | Ast.ExPkg (loc, me) -> (mkexp loc ( (Pexp_pack (module_expr me)) ))
    | ExFUN (loc, i, e) -> (mkexp loc ( (Pexp_newtype (i, ( (expr e) ))) ))
    | Ast.ExCom (loc, _, _) -> (error loc "expr, expr: not allowed here")
    | Ast.ExSem (loc, _, _) ->
       (error loc
         "expr; expr: not allowed here, use do {...} or [|...|] to surround them")
    | ((ExId (_, _) | ExNil (_)) as e) ->
       (error ( (loc_of_expr e) ) "invalid expr")
   and patt_of_lab =
    fun _loc ->
     fun lab ->
      function
      | Ast.PaNil (_) ->
         (patt ( (Ast.PaId (_loc, ( (Ast.IdLid (_loc, lab)) ))) ))
      | p -> (patt p)
   and expr_of_lab =
    fun _loc ->
     fun lab ->
      function
      | Ast.ExNil (_) ->
         (expr ( (Ast.ExId (_loc, ( (Ast.IdLid (_loc, lab)) ))) ))
      | e -> (expr e)
   and label_expr =
    function
    | ExLab (loc, lab, eo) -> (lab, ( (expr_of_lab loc lab eo) ))
    | ExOlb (loc, lab, eo) -> (( ("?" ^ lab) ), ( (expr_of_lab loc lab eo) ))
    | e -> ("", ( (expr e) ))
   and binding =
    fun x ->
     fun acc ->
      (match x with
       | Ast.BiAnd (_, x, y) -> (binding x ( (binding y acc) ))
       | Ast.BiEq
          (_loc, Ast.PaId (sloc, Ast.IdLid (_, bind_name)),
           Ast.ExTyc (_, e, TyTypePol (_, vs, ty))) ->
          let rec id_to_string =
           fun x ->
            (match x with
             | Ast.TyId (_, Ast.IdLid (_, x)) -> [x]
             | Ast.TyApp (_, x, y) ->
                (( (id_to_string x) ) @ ( (id_to_string y) ))
             | _ -> assert false) in
          let vars = (id_to_string vs) in
          let ampersand_vars = (List.map ( fun x -> ("&" ^ x) ) vars) in
          let ty' = (varify_constructors vars ( (ctyp ty) )) in
          let mkexp = (mkexp _loc) in
          let mkpat = (mkpat _loc) in
          let e =
           (mkexp (
             (Pexp_constraint (( (expr e) ), ( (Some (ctyp ty)) ), None )) )) in
          let rec mk_newtypes =
           fun x ->
            (match x with
             | (newtype :: []) -> (mkexp ( (Pexp_newtype (newtype, e)) ))
             | (newtype :: newtypes) ->
                (mkexp ( (Pexp_newtype (newtype, ( (mk_newtypes newtypes) )))
                  ))
             | [] -> assert false) in
          let pat =
           (mkpat (
             (Ppat_constraint
               (( (mkpat ( (Ppat_var (with_loc bind_name sloc)) )) ), (
                (mktyp _loc ( (Ptyp_poly (ampersand_vars, ty')) )) ))) )) in
          let e = (mk_newtypes vars) in ( (pat, e) ) :: acc 
       | Ast.BiEq (_loc, p, Ast.ExTyc (_, e, Ast.TyPol (_, vs, ty))) ->
          (
           ((
            (patt ( (Ast.PaTyc (_loc, p, ( (Ast.TyPol (_loc, vs, ty)) ))) ))
            ), ( (expr e) )) ) :: acc 
       | Ast.BiEq (_, p, e) -> ( (( (patt p) ), ( (expr e) )) ) :: acc 
       | Ast.BiNil (_) -> acc
       | _ -> assert false)
   and match_case =
    fun x ->
     fun acc ->
      (match x with
       | Ast.McOr (_, x, y) -> (match_case x ( (match_case y acc) ))
       | Ast.McArr (_, p, w, e) ->
          ( (( (patt p) ), ( (when_expr e w) )) ) :: acc 
       | Ast.McNil (_) -> acc
       | _ -> assert false)
   and when_expr =
    fun e ->
     fun w ->
      (match w with
       | Ast.ExNil (_) -> (expr e)
       | w ->
          (mkexp ( (loc_of_expr w) ) (
            (Pexp_when (( (expr w) ), ( (expr e) ))) )))
   and mklabexp =
    fun x ->
     fun acc ->
      (match x with
       | Ast.RbSem (_, x, y) -> (mklabexp x ( (mklabexp y acc) ))
       | Ast.RbEq (_, i, e) ->
          ( (( (ident ~conv_lid:conv_lab i) ), ( (expr e) )) ) :: acc 
       | _ -> assert false)
   and mkideexp =
    fun x ->
     fun acc ->
      (match x with
       | Ast.RbNil (_) -> acc
       | Ast.RbSem (_, x, y) -> (mkideexp x ( (mkideexp y acc) ))
       | Ast.RbEq (_, Ast.IdLid (sloc, s), e) ->
          ( (( (with_loc s sloc) ), ( (expr e) )) ) :: acc 
       | _ -> assert false)
   and mktype_decl =
    fun x ->
     fun acc ->
      (match x with
       | Ast.TyAnd (_, x, y) -> (mktype_decl x ( (mktype_decl y acc) ))
       | Ast.TyDcl (cloc, c, tl, td, cl) ->
          let cl =
           (List.map (
             fun (t1, t2) ->
              let loc = (Loc.merge ( (loc_of_ctyp t1) ) ( (loc_of_ctyp t2) )) in
              (( (ctyp t1) ), ( (ctyp t2) ), ( (mkloc loc) )) ) cl) in
          (
           (( (with_loc c cloc) ), (
            (type_decl ( (List.fold_right optional_type_parameters tl [] ) )
              cl td cloc) )) ) :: acc 
       | _ -> assert false)
   and module_type =
    function
    | Ast.MtNil (loc) ->
       (error loc "abstract/nil module type not allowed here")
    | Ast.MtId (loc, i) -> (mkmty loc ( (Pmty_ident (long_uident i)) ))
    | Ast.MtFun (loc, n, nt, mt) ->
       (mkmty loc (
         (Pmty_functor
           (( (with_loc n loc) ), ( (module_type nt) ), ( (module_type mt) )))
         ))
    | Ast.MtQuo (loc, _) ->
       (error loc "module type variable not allowed here")
    | Ast.MtSig (loc, sl) ->
       (mkmty loc ( (Pmty_signature (sig_item sl [] )) ))
    | Ast.MtWit (loc, mt, wc) ->
       (mkmty loc ( (Pmty_with (( (module_type mt) ), ( (mkwithc wc [] ) )))
         ))
    | Ast.MtOf (loc, me) -> (mkmty loc ( (Pmty_typeof (module_expr me)) ))
    | Ast.MtAnt (_, _) -> assert false
   and sig_item =
    fun s ->
     fun l ->
      (match s with
       | Ast.SgNil (_) -> l
       | SgCls (loc, cd) ->
          ( (
           (mksig loc (
             (Psig_class
               (List.map class_info_class_type ( (list_of_class_type cd [] )
                 ))) )) ) ) :: l 
       | SgClt (loc, ctd) ->
          ( (
           (mksig loc (
             (Psig_class_type
               (List.map class_info_class_type ( (list_of_class_type ctd [] )
                 ))) )) ) ) :: l 
       | Ast.SgSem (_, sg1, sg2) -> (sig_item sg1 ( (sig_item sg2 l) ))
       | SgDir (_, _, _) -> l
       | Ast.SgExc (loc, Ast.TyId (_, Ast.IdUid (_, s))) ->
          ( (
           (mksig loc (
             (Psig_exception (( (with_loc ( (conv_con s) ) loc) ), [] )) ))
           ) ) :: l 
       | Ast.SgExc (loc, Ast.TyOf (_, Ast.TyId (_, Ast.IdUid (_, s)), t)) ->
          ( (
           (mksig loc (
             (Psig_exception
               (( (with_loc ( (conv_con s) ) loc) ), (
                (List.map ctyp ( (list_of_ctyp t [] ) )) ))) )) ) ) :: l 
       | SgExc (_, _) -> assert false
       | SgExt (loc, n, t, sl) ->
          ( (
           (mksig loc (
             (Psig_value
               (( (with_loc n loc) ), (
                (mkvalue_desc loc t ( (list_of_meta_list sl) )) ))) )) ) ) ::
           l 
       | SgInc (loc, mt) ->
          ( ( (mksig loc ( (Psig_include (module_type mt)) )) ) ) :: l 
       | SgMod (loc, n, mt) ->
          ( (
           (mksig loc (
             (Psig_module (( (with_loc n loc) ), ( (module_type mt) ))) ))
           ) ) :: l 
       | SgRecMod (loc, mb) ->
          ( ( (mksig loc ( (Psig_recmodule (module_sig_binding mb [] )) ))
           ) ) :: l 
       | SgMty (loc, n, mt) ->
          let si =
           (match mt with
            | MtQuo (_, _) -> (Pmodtype_abstract)
            | _ -> (Pmodtype_manifest (module_type mt))) in
          ( ( (mksig loc ( (Psig_modtype (( (with_loc n loc) ), si)) ))
           ) ) :: l 
       | SgOpn (loc, id) ->
          ( ( (mksig loc ( (Psig_open (long_uident id)) )) ) ) :: l 
       | SgTyp (loc, tdl) ->
          ( ( (mksig loc ( (Psig_type (mktype_decl tdl [] )) )) ) ) :: l 
       | SgVal (loc, n, t) ->
          ( (
           (mksig loc (
             (Psig_value (( (with_loc n loc) ), ( (mkvalue_desc loc t [] ) )))
             )) ) ) :: l 
       | Ast.SgAnt (loc, _) -> (error loc "antiquotation in sig_item"))
   and module_sig_binding =
    fun x ->
     fun acc ->
      (match x with
       | Ast.MbAnd (_, x, y) ->
          (module_sig_binding x ( (module_sig_binding y acc) ))
       | Ast.MbCol (loc, s, mt) ->
          ( (( (with_loc s loc) ), ( (module_type mt) )) ) :: acc 
       | _ -> assert false)
   and module_str_binding =
    fun x ->
     fun acc ->
      (match x with
       | Ast.MbAnd (_, x, y) ->
          (module_str_binding x ( (module_str_binding y acc) ))
       | Ast.MbColEq (loc, s, mt, me) ->
          (
           (( (with_loc s loc) ), ( (module_type mt) ), ( (module_expr me) )) ) ::
           acc 
       | _ -> assert false)
   and module_expr =
    function
    | Ast.MeNil (loc) -> (error loc "nil module expression")
    | Ast.MeId (loc, i) -> (mkmod loc ( (Pmod_ident (long_uident i)) ))
    | Ast.MeApp (loc, me1, me2) ->
       (mkmod loc (
         (Pmod_apply (( (module_expr me1) ), ( (module_expr me2) ))) ))
    | Ast.MeFun (loc, n, mt, me) ->
       (mkmod loc (
         (Pmod_functor
           (( (with_loc n loc) ), ( (module_type mt) ), ( (module_expr me) )))
         ))
    | Ast.MeStr (loc, sl) ->
       (mkmod loc ( (Pmod_structure (str_item sl [] )) ))
    | Ast.MeTyc (loc, me, mt) ->
       (mkmod loc (
         (Pmod_constraint (( (module_expr me) ), ( (module_type mt) ))) ))
    | Ast.MePkg (loc, Ast.ExTyc (_, e, Ast.TyPkg (_, pt))) ->
       (mkmod loc (
         (Pmod_unpack
           (mkexp loc (
             (Pexp_constraint
               (( (expr e) ), (
                (Some (mktyp loc ( (Ptyp_package (package_type pt)) ))) ),
                None )) ))) ))
    | Ast.MePkg (loc, e) -> (mkmod loc ( (Pmod_unpack (expr e)) ))
    | Ast.MeAnt (loc, _) -> (error loc "antiquotation in module_expr")
   and str_item =
    fun s ->
     fun l ->
      (match s with
       | Ast.StNil (_) -> l
       | StCls (loc, cd) ->
          ( (
           (mkstr loc (
             (Pstr_class
               (List.map class_info_class_expr ( (list_of_class_expr cd [] )
                 ))) )) ) ) :: l 
       | StClt (loc, ctd) ->
          ( (
           (mkstr loc (
             (Pstr_class_type
               (List.map class_info_class_type ( (list_of_class_type ctd [] )
                 ))) )) ) ) :: l 
       | Ast.StSem (_, st1, st2) -> (str_item st1 ( (str_item st2 l) ))
       | StDir (_, _, _) -> l
       | Ast.StExc (loc, Ast.TyId (_, Ast.IdUid (_, s)), Ast.ONone) ->
          ( (
           (mkstr loc (
             (Pstr_exception (( (with_loc ( (conv_con s) ) loc) ), [] )) ))
           ) ) :: l 
       | Ast.StExc
          (loc, Ast.TyOf (_, Ast.TyId (_, Ast.IdUid (_, s)), t), Ast.ONone) ->
          ( (
           (mkstr loc (
             (Pstr_exception
               (( (with_loc ( (conv_con s) ) loc) ), (
                (List.map ctyp ( (list_of_ctyp t [] ) )) ))) )) ) ) :: l 
       | Ast.StExc (loc, Ast.TyId (_, Ast.IdUid (_, s)), Ast.OSome (i)) ->
          ( (
           (mkstr loc (
             (Pstr_exn_rebind
               (( (with_loc ( (conv_con s) ) loc) ), ( (ident i) ))) ))
           ) ) :: l 
       | Ast.StExc
          (loc, Ast.TyOf (_, Ast.TyId (_, Ast.IdUid (_, _)), _),
           Ast.OSome (_)) ->
          (error loc "type in exception alias")
       | StExc (_, _, _) -> assert false
       | StExp (loc, e) -> ( ( (mkstr loc ( (Pstr_eval (expr e)) )) ) ) :: l 
       | StExt (loc, n, t, sl) ->
          ( (
           (mkstr loc (
             (Pstr_primitive
               (( (with_loc n loc) ), (
                (mkvalue_desc loc t ( (list_of_meta_list sl) )) ))) )) ) ) ::
           l 
       | StInc (loc, me) ->
          ( ( (mkstr loc ( (Pstr_include (module_expr me)) )) ) ) :: l 
       | StMod (loc, n, me) ->
          ( (
           (mkstr loc (
             (Pstr_module (( (with_loc n loc) ), ( (module_expr me) ))) ))
           ) ) :: l 
       | StRecMod (loc, mb) ->
          ( ( (mkstr loc ( (Pstr_recmodule (module_str_binding mb [] )) ))
           ) ) :: l 
       | StMty (loc, n, mt) ->
          ( (
           (mkstr loc (
             (Pstr_modtype (( (with_loc n loc) ), ( (module_type mt) ))) ))
           ) ) :: l 
       | StOpn (loc, id) ->
          ( ( (mkstr loc ( (Pstr_open (long_uident id)) )) ) ) :: l 
       | StTyp (loc, tdl) ->
          ( ( (mkstr loc ( (Pstr_type (mktype_decl tdl [] )) )) ) ) :: l 
       | StVal (loc, rf, bi) ->
          ( (
           (mkstr loc ( (Pstr_value (( (mkrf rf) ), ( (binding bi [] ) ))) ))
           ) ) :: l 
       | Ast.StAnt (loc, _) -> (error loc "antiquotation in str_item"))
   and class_type =
    function
    | CtCon (loc, ViNil, id, tl) ->
       (mkcty loc (
         (Pcty_constr
           (( (long_class_ident id) ), (
            (List.map ctyp ( (list_of_opt_ctyp tl [] ) )) ))) ))
    | CtFun (loc, TyLab (_, lab, t), ct) ->
       (mkcty loc ( (Pcty_fun (lab, ( (ctyp t) ), ( (class_type ct) ))) ))
    | CtFun (loc, TyOlb (loc1, lab, t), ct) ->
       let t = (TyApp (loc1, ( (predef_option loc1) ), t)) in
       (mkcty loc (
         (Pcty_fun (( ("?" ^ lab) ), ( (ctyp t) ), ( (class_type ct) ))) ))
    | CtFun (loc, t, ct) ->
       (mkcty loc ( (Pcty_fun ("", ( (ctyp t) ), ( (class_type ct) ))) ))
    | CtSig (loc, t_o, ctfl) ->
       let t = (match t_o with | Ast.TyNil (_) -> (Ast.TyAny (loc)) | t -> t) in
       let cil = (class_sig_item ctfl [] ) in
       (mkcty loc (
         (Pcty_signature
           ({pcsig_self = ( (ctyp t) ); pcsig_fields = cil;
             pcsig_loc = ( (mkloc loc) )})) ))
    | CtCon (loc, _, _, _) ->
       (error loc "invalid virtual class inside a class type")
    | ((((CtAnt (_, _) | CtEq (_, _, _)) | CtCol (_, _, _))
        | CtAnd (_, _, _)) | CtNil (_)) ->
       assert false
   and class_info_class_expr =
    fun ci ->
     (match ci with
      | CeEq (_, CeCon (loc, vir, IdLid (nloc, name), params), ce) ->
         let (loc_params, (params, variance)) =
          (match params with
           | Ast.TyNil (_) -> (loc, ([] , [] ))
           | t ->
              (( (loc_of_ctyp t) ), (
               (List.split ( (class_parameters t [] ) )) ))) in
         {pci_virt = ( (mkvirtual vir) );
          pci_params = (params, ( (mkloc loc_params) ));
          pci_name = ( (with_loc name nloc) );
          pci_expr = ( (class_expr ce) ); pci_loc = ( (mkloc loc) );
          pci_variance = variance}
      | ce -> (error ( (loc_of_class_expr ce) ) "bad class definition"))
   and class_info_class_type =
    fun ci ->
     (match ci with
      | (CtEq (_, CtCon (loc, vir, IdLid (nloc, name), params), ct)
         | CtCol (_, CtCon (loc, vir, IdLid (nloc, name), params), ct)) ->
         let (loc_params, (params, variance)) =
          (match params with
           | Ast.TyNil (_) -> (loc, ([] , [] ))
           | t ->
              (( (loc_of_ctyp t) ), (
               (List.split ( (class_parameters t [] ) )) ))) in
         {pci_virt = ( (mkvirtual vir) );
          pci_params = (params, ( (mkloc loc_params) ));
          pci_name = ( (with_loc name nloc) );
          pci_expr = ( (class_type ct) ); pci_loc = ( (mkloc loc) );
          pci_variance = variance}
      | ct ->
         (error ( (loc_of_class_type ct) )
           "bad class/class type declaration/definition"))
   and class_sig_item =
    fun c ->
     fun l ->
      (match c with
       | Ast.CgNil (_) -> l
       | CgCtr (loc, t1, t2) ->
          ( ( (mkctf loc ( (Pctf_cstr (( (ctyp t1) ), ( (ctyp t2) ))) ))
           ) ) :: l 
       | Ast.CgSem (_, csg1, csg2) ->
          (class_sig_item csg1 ( (class_sig_item csg2 l) ))
       | CgInh (loc, ct) ->
          ( ( (mkctf loc ( (Pctf_inher (class_type ct)) )) ) ) :: l 
       | CgMth (loc, s, pf, t) ->
          ( (
           (mkctf loc (
             (Pctf_meth
               (s, ( (mkprivate pf) ), ( (mkpolytype ( (ctyp t) )) ))) ))
           ) ) :: l 
       | CgVal (loc, s, b, v, t) ->
          ( (
           (mkctf loc (
             (Pctf_val
               (s, ( (mkmutable b) ), ( (mkvirtual v) ), ( (ctyp t) ))) ))
           ) ) :: l 
       | CgVir (loc, s, b, t) ->
          ( (
           (mkctf loc (
             (Pctf_virt (s, ( (mkprivate b) ), ( (mkpolytype ( (ctyp t) )) )))
             )) ) ) :: l 
       | CgAnt (_, _) -> assert false)
   and class_expr =
    function
    | (CeApp (loc, _, _) as c) ->
       let (ce, el) = (class_expr_fa []  c) in
       let el = (List.map label_expr el) in
       (mkcl loc ( (Pcl_apply (( (class_expr ce) ), el)) ))
    | CeCon (loc, ViNil, id, tl) ->
       (mkcl loc (
         (Pcl_constr
           (( (long_class_ident id) ), (
            (List.map ctyp ( (list_of_opt_ctyp tl [] ) )) ))) ))
    | CeFun (loc, PaLab (_, lab, po), ce) ->
       (mkcl loc (
         (Pcl_fun
           (lab, None , ( (patt_of_lab loc lab po) ), ( (class_expr ce) )))
         ))
    | CeFun (loc, PaOlbi (_, lab, p, e), ce) ->
       let lab = (paolab lab p) in
       (mkcl loc (
         (Pcl_fun
           (( ("?" ^ lab) ), ( (Some (expr e)) ), ( (patt p) ), (
            (class_expr ce) ))) ))
    | CeFun (loc, PaOlb (_, lab, p), ce) ->
       let lab = (paolab lab p) in
       (mkcl loc (
         (Pcl_fun
           (( ("?" ^ lab) ), None , ( (patt_of_lab loc lab p) ), (
            (class_expr ce) ))) ))
    | CeFun (loc, p, ce) ->
       (mkcl loc ( (Pcl_fun ("", None , ( (patt p) ), ( (class_expr ce) )))
         ))
    | CeLet (loc, rf, bi, ce) ->
       (mkcl loc (
         (Pcl_let (( (mkrf rf) ), ( (binding bi [] ) ), ( (class_expr ce) )))
         ))
    | CeStr (loc, po, cfl) ->
       let p = (match po with | Ast.PaNil (_) -> (Ast.PaAny (loc)) | p -> p) in
       let cil = (class_str_item cfl [] ) in
       (mkcl loc (
         (Pcl_structure ({pcstr_pat = ( (patt p) ); pcstr_fields = cil})) ))
    | CeTyc (loc, ce, ct) ->
       (mkcl loc (
         (Pcl_constraint (( (class_expr ce) ), ( (class_type ct) ))) ))
    | CeCon (loc, _, _, _) ->
       (error loc "invalid virtual class inside a class expression")
    | (((CeAnt (_, _) | CeEq (_, _, _)) | CeAnd (_, _, _)) | CeNil (_)) ->
       assert false
   and class_str_item =
    fun c ->
     fun l ->
      (match c with
       | CrNil (_) -> l
       | CrCtr (loc, t1, t2) ->
          ( ( (mkcf loc ( (Pcf_constr (( (ctyp t1) ), ( (ctyp t2) ))) ))
           ) ) :: l 
       | Ast.CrSem (_, cst1, cst2) ->
          (class_str_item cst1 ( (class_str_item cst2 l) ))
       | CrInh (loc, ov, ce, pb) ->
          let opb = if (pb = "") then None  else (Some (pb)) in
          ( (
           (mkcf loc (
             (Pcf_inher
               (( (override_flag loc ov) ), ( (class_expr ce) ), opb)) ))
           ) ) :: l 
       | CrIni (loc, e) -> ( ( (mkcf loc ( (Pcf_init (expr e)) )) ) ) :: l 
       | CrMth (loc, s, ov, pf, e, t) ->
          let t =
           (match t with
            | Ast.TyNil (_) -> (None)
            | t -> (Some (mkpolytype ( (ctyp t) )))) in
          let e = (mkexp loc ( (Pexp_poly (( (expr e) ), t)) )) in
          ( (
           (mkcf loc (
             (Pcf_meth
               (( (with_loc s loc) ), ( (mkprivate pf) ), (
                (override_flag loc ov) ), e)) )) ) ) :: l 
       | CrVal (loc, s, ov, mf, e) ->
          ( (
           (mkcf loc (
             (Pcf_val
               (( (with_loc s loc) ), ( (mkmutable mf) ), (
                (override_flag loc ov) ), ( (expr e) ))) )) ) ) :: l 
       | CrVir (loc, s, pf, t) ->
          ( (
           (mkcf loc (
             (Pcf_virt
               (( (with_loc s loc) ), ( (mkprivate pf) ), (
                (mkpolytype ( (ctyp t) )) ))) )) ) ) :: l 
       | CrVvr (loc, s, mf, t) ->
          ( (
           (mkcf loc (
             (Pcf_valvirt
               (( (with_loc s loc) ), ( (mkmutable mf) ), ( (ctyp t) ))) ))
           ) ) :: l 
       | CrAnt (_, _) -> assert false)

   let sig_item = fun ast -> (sig_item ast [] )

   let str_item = fun ast -> (str_item ast [] )

   let directive =
    function
    | Ast.ExNil (_) -> (Pdir_none)
    | ExStr (_, s) -> (Pdir_string (s))
    | ExInt (_, i) -> (Pdir_int (int_of_string i))
    | Ast.ExId (_, Ast.IdUid (_, "True")) -> (Pdir_bool ((true)))
    | Ast.ExId (_, Ast.IdUid (_, "False")) -> (Pdir_bool ((false)))
    | e -> (Pdir_ident (ident_noloc ( (ident_of_expr e) )))

   let phrase =
    function
    | StDir (_, d, dp) -> (Ptop_dir (d, ( (directive dp) )))
    | si -> (Ptop_def (str_item si))

  end
