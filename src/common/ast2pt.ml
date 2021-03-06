

(** Dump the FanAst to Parsetree, this file should introduce minimal
    dependency or only dependent on those generated files*)



let view_app = Ast_basic.view_app
let list_of_star = Ast_basic.list_of_star
let listr_of_arrow = Ast_basic.listr_of_arrow
let list_of_sem = Ast_basic.list_of_sem
let list_of_bar = Ast_basic.list_of_bar
let list_of_app = Ast_basic.list_of_app
let list_of_com = Ast_basic.list_of_com
let list_of_and = Ast_basic.list_of_and

let failwithf  = Util.failwithf
let error = Parsetree_util.error 
let lident = Parsetree_util.lident
let with_loc = Parsetree_util.with_loc
let ldot = Parsetree_util.ldot
let mktyp = Parsetree_util.mktyp
(* let mkfield = Parsetree_util.mkfield *)
let mkpolytype = Parsetree_util.mkpolytype
let mkghpat = Parsetree_util.mkghpat
let mkpat  = Parsetree_util.mkpat
let mkexp = Parsetree_util.mkexp
let mkli = Parsetree_util.mkli
let array_function = Parsetree_util.array_function
let varify_constructors = Parsetree_util.varify_constructors
let mkmty = Parsetree_util.mkmty
let mksig = Parsetree_util.mksig
let mkstr = Parsetree_util.mkstr
let mkcty = Parsetree_util.mkcty
let mkctf = Parsetree_util.mkctf
let mkcl = Parsetree_util.mkcl
let mkcf = Parsetree_util.mkcf

(** An unsafe version introduced is mainly for reducing
    unnecessary dependency when bootstrapping
    relies on the fact:
    Location.t and Lexing.position have different sizes *)
let unsafe_loc_of node =
  let open Obj in
  let u = field (repr node) 1 in
  let s  = Obj.size u in
    if s = 3 && size (field u 0) = 4 then
      (magic u : Locf.t) (* `a loc *)
    else (* `a (loc,...)  => size (field u 0) = 3 *)
      (magic @@ field u 0 : Locf.t)


(*************************************************************************)
(* utility begin *)

(*
   {[remove_underscores "_a" = "a"
       remove_underscores "__a" = "a"
       remove_underscores "__a___b" =  "ab"
       remove_underscores "__a___b__" = "ab"]}
*)    
let remove_underscores s =
  let l = String.length s in
  let buf = Buffer.create l in
  let () = String.iter (fun ch ->
      if ch <> '_' then ignore (Buffer.add_char buf ch) else () ) s in
  Buffer.contents buf 


(** Forward declarations FIXME, should find a easy way to get rid of such duplications *)
let dump_ident =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_ident")
let dump_ctyp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_ctyp")
let dump_row_field =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_row_field")
let dump_name_ctyp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_name_ctyp")
let dump_constr =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_constr")
let dump_mtyp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_mtyp")
let dump_ctyp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_ctyp")
let dump_or_ctyp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_or_ctyp")
let dump_pat =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_pat")
let dump_type_parameters =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_type_parameters")
let dump_exp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_exp")
let dump_case =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_case")
let dump_rec_exp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_rec_exp")
let dump_type_constr =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_type_constr")
let dump_typedecl =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_typedecl")
let dump_sigi =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_sigi")
let dump_mbind =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_mbind")
let dump_mexp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_mexp")
let dump_stru =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" "dump_stru")
let dump_cltyp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_cltyp")
let dump_cldecl =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_cldecl")
let dump_cltdecl =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_cltdecl")
let dump_clsigi =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_clsigi")
let dump_clexp =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_clexp")
let dump_clfield =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "dump_clfield")

let generate_type_code :
  (Astf.loc -> Astf.typedecl -> Astf.strings -> Astf.stru) ref =
  ref
    (fun _  ->
       Format.ksprintf failwith "%s.%s not implemented " "Ast2pt"
         "generate_type_code")

(** Filled by [Ast_gen.meta_here ] *)
let meta_here : (Location.t -> Location.t -> Astf.ep) ref = 
  ref (fun _ -> Format.ksprintf failwith "%s.%s not implemented " "Ast2pt" 
      "meta_here")

(*********************************************************************)

(** the module path -- recorded during dumping  *)
let module_path :  string list ref = ref []
let current_top_bind : string list ref = ref []
(*****************************)
(* Util functions            *)    
(*****************************)
let mk_constant_exp _loc (x: Astf.literal) : Parsetree.expression_desc =
  match x with 
  | `Chr (_,s) -> Pexp_constant (Const_char (Escape.char_of_char_token _loc s))
  | `Int (_,s) ->
      Pexp_constant
      (try Const_int (int_of_string s)
      with Failure _ ->
        error _loc
          "Integer literal exceeds the range of representable integers of type int" )
  | `Int32 (_,s) ->
      Pexp_constant
        (try Const_int32 (Int32.of_string s)
        with Failure _ ->
          error _loc
          "Integer literal exceeds the range of representable integers of type int32" )
  | `Int64 (_,s) ->
      Pexp_constant
        (try Const_int64 (Int64.of_string s)
        with
          Failure _ ->
            error _loc
              "Integer literal exceeds the range of representable integers of type int64" )
  | `Nativeint (_,s) ->
      Pexp_constant
        (try Const_nativeint (Nativeint.of_string s)
        with
          Failure _ ->
            error _loc
              "Integer literal exceeds the range of representable integers of type nativeint" )
  | `Flo (_,s) -> Pexp_constant (Const_float (remove_underscores s))
  | `Str (_,s) -> 
      Pexp_constant 
        (Const_string 
           (
            Escape.string_of_string_token _loc s
              , None (* FIXME *)
           ))
  | `Bool(_,b) ->
      if b then Pexp_construct 
          ({txt = Lident"true";loc=_loc},None(* ,false *))
      else Pexp_construct ({txt=Lident"false";loc=_loc},None
                             (* ,false *))
  | `Unit _ ->
      Pexp_construct ({txt = Lident"()";loc=_loc},None(* ,false *))

let mk_constant_pat _loc ( x : Astf.literal) : Parsetree.pattern_desc =
  match x with 
  | `Chr (_,s) -> Ppat_constant (Const_char (Escape.char_of_char_token _loc s))
  | `Int (_,s) ->
      Ppat_constant
      (try Const_int (int_of_string s)
      with Failure _ ->
        error _loc
          "Integer literal exceeds the range of representable integers of type int" )
  | `Int32 (_,s) ->
      Ppat_constant
        (try Const_int32 (Int32.of_string s)
        with Failure _ ->
          error _loc
            "Integer literal exceeds the range of representable integers of type int32" )
  | `Int64 (_,s) ->
      Ppat_constant
        (try Const_int64 (Int64.of_string s)
        with
          Failure _ ->
            error _loc
              "Integer literal exceeds the range of representable integers of type int64" )
  | `Nativeint (_,s) ->
      Ppat_constant
        (try Const_nativeint (Nativeint.of_string s)
        with
          Failure _ ->
            error _loc
              "Integer literal exceeds the range of representable integers of type nativeint" )
  | `Flo (_,s) -> Ppat_constant (Const_float (remove_underscores s))
  | `Str (_,s) -> 
      Ppat_constant
        (Const_string
           (Escape.string_of_string_token _loc s,
            None (*FIXME*)
           ))
  | `Bool (_,b) ->
      if b then Ppat_construct ({txt=Lident"true";loc=_loc},None(* ,false *))
      else Ppat_construct ({txt=Lident"false";loc=_loc},None(* ,false *))
  | `Unit _ ->
      Ppat_construct ({txt=Lident"()";loc=_loc},None(* ,false *))

    
                                     
    


let ant_error loc = error loc "antiquotation not expected here"

let mkvirtual  (x: Astf.flag)  : Asttypes.virtual_flag =
  match x with 
  | `Positive _ -> Virtual
  | `Negative _  -> Concrete
  | `Ant (_loc,_) -> ant_error _loc 

let flag loc (x: Astf.flag) : Asttypes.override_flag =
  match x with
  | `Positive _ -> Override
  | `Negative _  -> Fresh
  |  _ -> error loc "antiquotation not allowed here" 

let mkdirection (x: Astf.flag) : Asttypes.direction_flag =
  match x with 
  | `Positive _ -> Upto
  | `Negative _ -> Downto
  | `Ant (_loc,_) -> ant_error _loc 

let mkrf (x: Astf.flag) : Asttypes.rec_flag =
  match x with 
  | `Positive _  -> Recursive
  | `Negative _  -> Nonrecursive
  | `Ant(_loc,_) -> ant_error _loc

let ident_tag (i : Astf.ident) :
    (Longident.t * [> `app | `lident | `uident ]) =
  let rec self (i:Astf.ident) acc : (Longident.t * _) option=
    let _loc = unsafe_loc_of i in
    match i with
    | `Dot (_,`Lid (_,"*predef*"),`Lid (_,"option")) ->
      Some (Ldot ((Lident "*predef*"), "option"), `lident)
    | `Dot (_,i1,i2)  -> self i2 (self i1 acc)
    | `Apply (_,i1,i2) ->
        begin 
          match (self i1 None, self i2 None, acc) with
          | (Some (l,_),Some (r,_),None)  ->
              Some (Lapply (l, r), `app)
          | _ ->
              Locf.failf  _loc "invalid long identifer %s" @@ !dump_ident i
        end
    | `Uid (_,s) ->
        begin 
          match (acc, s) with
          | (None ,"") -> None
          | (None ,s) -> Some (Lident s, `uident)
          | (Some (_,(`uident|`app)),"") -> acc
          | (Some (x,(`uident|`app)),s) -> Some ((Ldot (x, s)), `uident)
          | _ ->
              Locf.failf _loc "invalid long identifier %s" @@ !dump_ident i
        end
    | `Lid (_,s) ->
      let x =
        match acc with
        | None  -> lident s
        | Some (acc,(`uident|`app)) -> Ldot (acc, s)
        | _ ->
          Locf.failf _loc "invalid long identifier %s" @@ !dump_ident i in
      Some (x, `lident)
    | `Ant (_,_) -> error _loc "invalid long identifier" in
  match self i None with
  | Some x -> x
  | None  -> error (unsafe_loc_of i) "invalid long identifier "        

let ident_noloc i = fst (ident_tag  i)

let ident (i:Astf.ident) :  Longident.t Location.loc  =
  with_loc (ident_noloc  i) (unsafe_loc_of i)

let long_lident  id =
  match ident_tag id with
  | (i,`lident) -> with_loc i (unsafe_loc_of id)
  | _ ->
    Locf.failf (unsafe_loc_of id)  "invalid long identifier %s" (!dump_ident id)

let long_type_ident: Astf.ident -> Longident.t Location.loc =
  long_lident 

let long_class_ident = long_lident

let long_uident_noloc  (i: Astf.ident) =
  match ident_tag i with
  | (Ldot (i, s), `uident) -> ldot i s
  | (Lident s, `uident) -> lident s
  | (i, `app) -> i
  | _ -> Locf.failf (unsafe_loc_of i) "uppercase identifier expected %s" (!dump_ident i) 

let long_uident  i =
  with_loc (long_uident_noloc  i) (unsafe_loc_of i)

let rec ctyp_long_id_prefix (t:Astf.ctyp) : Longident.t =
  match t with
  | #Astf.ident' as i  -> ident_noloc i
  | `App(_loc,m1,m2) ->
    let li1 = ctyp_long_id_prefix m1 in
    let li2 = ctyp_long_id_prefix m2 in
    Lapply (li1, li2)
  | t -> Locf.failf (unsafe_loc_of t) "invalid module expression %s" (!dump_ctyp t) 

let ctyp_long_id (t:Astf.ctyp) : (bool *   Longident.t Location.loc) =
  match t with
  | #Astf.ident' as i -> (false, long_type_ident i)
  | `ClassPath (_, i) -> (true, ident i)
  | t -> Locf.failf (unsafe_loc_of t) "invalid type %s" @@ !dump_ctyp t

let predef_option loc : Astf.ctyp =
  `Dot (loc, `Lid (loc, "*predef*"), `Lid (loc, "option"))


let mkprivate (x : Astf.flag) : Asttypes.private_flag =
  match x with 
  | `Positive _ -> Private
  | `Negative _ -> Public
  | `Ant(_loc,_)-> ant_error _loc 




(** [ctyp] Transformation *)
let rec ctyp (x:Astf.ctyp) : Parsetree.core_type =
  let _loc = unsafe_loc_of x in
  match x with 
  |  (#Astf.ident' as i) ->
    mktyp _loc @@ Ptyp_constr (long_type_ident (i:>Astf.ident), [])
  | `Alias(_,t1,`Lid(_,s)) -> 
    mktyp _loc @@ Ptyp_alias (ctyp t1, s)
  | `Any _ -> mktyp _loc Ptyp_any
  | `App _ as f ->
    let (f, al) =view_app [] f in
    let (is_cls, li) = ctyp_long_id f in
    if is_cls then mktyp _loc  @@ 
      (Ptyp_class (li, List.map ctyp al(* , [] *)) )
    else mktyp _loc @@ Ptyp_constr (li, (List.map ctyp al))
  | `Arrow (_, (`Label (_,  `Lid(_,lab), t1)), t2) ->
    mktyp _loc @@ Ptyp_arrow (lab, (ctyp t1), ctyp t2)
  | `Arrow (_, (`OptLabl (loc1, `Lid(_,lab), t1)), t2) ->
    mktyp _loc @@ Ptyp_arrow ("?" ^ lab, ctyp (`App (loc1, predef_option loc1, t1) ), ctyp t2)
  | `Arrow (_loc, t1, t2) ->
      mktyp _loc @@ Ptyp_arrow ("", ctyp t1, ctyp t2)
  | `TyObjEnd(_,row) ->
    (* let xs = *)
    (*   match row with *)
    (*   |`Negative _ -> [] *)
    (*   | `Positive _ -> [mkfield _loc Pfield_var] *)
    (*   | `Ant _ -> ant_error _loc in *)
    mktyp _loc @@ Ptyp_object 
                    ([], 
                     (match row with 
                    |`Negative _ -> Closed
                    |`Positive _ -> Open
                    |`Ant _ -> ant_error _loc))
(* xs *)
  | `TyObj(_,fl,row) ->
    (* let xs  = *)
    (*   match row with *)
    (*   | `Negative _ -> [] *)
    (*   | `Positive _  -> [mkfield _loc Pfield_var] *)
    (*   | `Ant _ -> ant_error _loc  in *)
    mktyp _loc (Ptyp_object (meth_list fl [](* xs *), (match row with
      | `Negative _ -> Closed 
      | `Positive _  -> Open (* [mkfield _loc Pfield_var] *)
      | `Ant _ -> ant_error _loc)))

  | `ClassPath (_, id) -> mktyp _loc  @@ Ptyp_class ((ident id), [](* , [] *))
  | `Package(_,pt) ->
    mktyp _loc @@ Ptyp_package (package_type pt)
  | `TyPolEnd (_,t2) ->
    mktyp _loc @@ Ptyp_poly ([], ctyp t2)
  | `TyPol (_, t1, t2) ->
    let rec to_var_list  =
      function
      | `App (_,t1,t2) -> to_var_list t1 @ to_var_list t2
      | `Quote (_, (`Normal _ | `Positive _ | `Negative _), `Lid (_,s)) -> [s]
      | _ -> assert false in 
    mktyp _loc @@ Ptyp_poly (to_var_list t1, ctyp t2)
  (* QuoteAny should not appear here? *)      
  | `Quote (_,`Normal _, `Lid(_,s)) ->
      mktyp _loc @@ Ptyp_var s
  | `Par(_,`Sta(_,t1,t2)) ->
    mktyp _loc @@
      Ptyp_tuple (List.map ctyp (list_of_star t1 (list_of_star t2 [])))

  | `PolyEq(_,t) ->
    mktyp _loc @@ Ptyp_variant (row_field t [], (* true *) Closed, None)
  | `PolySup(_,t) ->
    mktyp _loc @@ Ptyp_variant (row_field t [], (* false *) Open, None)
  | `PolyInf(_,t) ->
    mktyp _loc @@ Ptyp_variant (row_field t [], (* true *) Closed, Some [])
  | `PolyInfSup(_,t,t') ->
    let rec name_tags (x: Astf.tag_names) =
      match x with 
      | `App(_,t1,t2) -> name_tags t1 @ name_tags t2
      | `TyVrn (_, `C (_,s))    -> [s]
      | _ -> assert false  in 
    mktyp _loc @@ Ptyp_variant (row_field t [], (* true *) Closed, Some (name_tags t'))
  |  x -> Locf.failf _loc "ctyp: %s" @@ !dump_ctyp x

and row_field (x: Astf.row_field) acc : Parsetree.row_field list =
  match x with 
  |`TyVrn (_loc,`C(_,i)) -> Rtag (i, [], (* arttributes*) true, []) :: acc
  | `TyVrnOf(_loc,`C(_,i),t) ->
    Rtag (i, [], (* attributes *) false, [ctyp t]) :: acc 
  | `Bar(_,t1,t2) -> row_field t1 ( row_field t2 acc)
  | `Ant(_loc,_) -> ant_error _loc
  | `Ctyp(_,t) -> Rinherit (ctyp t) :: acc
  | t -> Locf.failf (unsafe_loc_of t) "row_field: %s" (!dump_row_field t)

and meth_list (fl: Astf.name_ctyp) acc : (string * Parsetree.attributes * Parsetree.core_type) list   =
  match fl with
  |`Sem (_loc,t1,t2) -> meth_list t1 (meth_list t2 acc)
  | `TyCol(_loc,`Lid(_,lab),t) ->
    (* mkfield _loc *) ((* Pfield *) (lab, [], (mkpolytype (ctyp t)))) :: acc
  | x -> Locf.failf (unsafe_loc_of x) "meth_list: %s" (!dump_name_ctyp x )

and package_type_constraints (wc: Astf.constr)
    (acc: (Longident.t Asttypes.loc  * Parsetree.core_type) list )
  : (Longident.t Asttypes.loc   * Parsetree.core_type) list  =
  match wc with
  | `TypeEq(_, (#Astf.ident' as id),ct) -> (ident id, ctyp ct) :: acc
  | `And(_,wc1,wc2) ->
    package_type_constraints wc1 @@ package_type_constraints wc2 acc
  | x ->
    Locf.failf (unsafe_loc_of x)
        "unexpected `with constraint:%s' for a package type"
        (!dump_constr x) 

and package_type (x : Astf.mtyp) :
  (Longident.t Asttypes.loc *
    (Longident.t Asttypes.loc * Parsetree.core_type) list)  =
  match x with 
  | `With(_loc,(# Astf.ident' as i),wc) ->
    (long_uident i, package_type_constraints wc [])
  | #Astf.ident' as i  -> (long_uident i, [])
  | mt -> Locf.failf (unsafe_loc_of mt) "unexpected package type: %s" @@ !dump_mtyp mt

(*************************************)
(***    Utils                        *)
(*************************************)                                                                           
let mktrecord (x: Astf.name_ctyp) : Parsetree.label_declaration
  (* (string Location.loc * Asttypes.mutable_flag * Parsetree.core_type *  Astf.loc) *)=
  match x with 
  |`TyColMut(_loc,`Lid(sloc,s),t) ->
      { pld_name = with_loc s sloc;
        pld_mutable = Mutable;
        pld_type = mkpolytype (ctyp t);
        pld_loc = _loc ; 
        pld_attributes = [];
      }
    (* (with_loc s sloc, Mutable, mkpolytype (ctyp t),  _loc) *)
  | `TyCol(_loc,`Lid(sloc,s),t) ->
      {
       pld_name = with_loc s sloc;
       pld_mutable = Immutable ; 
       pld_type = mkpolytype (ctyp t);
       pld_loc = _loc;
       pld_attributes = [] 
     }
    (* (with_loc s sloc, Immutable, mkpolytype (ctyp t),  _loc) *)
  | t -> Locf.failf (unsafe_loc_of t) "mktrecord %s "
           (!dump_name_ctyp t)

let mkvariant (x : Astf.or_ctyp) : Parsetree.constructor_declaration
  (* (string Location.loc * Parsetree.core_type list *  Parsetree.core_type option * Locf.t) *) =
  let _loc = unsafe_loc_of x in
  match x with
  | `Uid(_,s) -> 
      {
       pcd_name = with_loc  s _loc;
       pcd_args = [] ; 
       pcd_res = None ; 
       pcd_loc = _loc ; 
       pcd_attributes = [];
     }
      (* (with_loc  s _loc, [], None,  _loc) *)
  | `Of(_,`Uid(sloc,s),t) ->

      {
       pcd_name = with_loc  s sloc ; 
       pcd_args =  List.map ctyp (list_of_star t []);
       pcd_res = None ; 
       pcd_loc = _loc ; 
       pcd_attributes = [];
     }
    (* (with_loc  s sloc, List.map ctyp (list_of_star t []), None,  _loc) *)

  | `TyCol(_,`Uid(sloc,s),( `Arrow _  as t )) -> (*GADT*)
    (match List.rev (listr_of_arrow t []) with
     | u::t ->
         { 
           pcd_name = with_loc s sloc ; 
           pcd_args = List.map ctyp t;
           pcd_res = Some (ctyp u);
           pcd_loc = _loc ; 
           pcd_attributes = [];
         }
       (* (with_loc s sloc, List.map ctyp t, Some (ctyp u),  _loc) *)  
     | [] -> assert false)

  | `TyCol(_,`Uid(sloc,s),t) ->
      {
       pcd_name = with_loc  s sloc;
       pcd_args = [];
       pcd_res = Some (ctyp t);
       pcd_loc = _loc ; 
       pcd_attributes = []
     }
    (* (with_loc  s sloc, [], Some (ctyp t),  _loc) *)
  | t -> Locf.failf _loc "mkvariant %s " @@ !dump_or_ctyp t

                                                                           

let type_kind (x : Astf.type_repr) : Parsetree.type_kind  =
  match x with
  | `Record (_,t) ->
    Parsetree.Ptype_record (List.map mktrecord (list_of_sem t []))
  | `Sum(_,t) ->
    Ptype_variant (List.map mkvariant (list_of_bar t []))
  | `Ant(_loc,_) -> ant_error _loc



let mkvalue_desc name loc t (p:  Astf.strings list) : Parsetree.value_description =
  {pval_type = ctyp t;
   pval_name = name ; 
   pval_attributes = [];
   pval_prim =
   List.map (fun p ->
     match p with
     | `Str (_,p) -> p
     | _ -> failwithf  "mkvalue_desc") p ;
   pval_loc =  loc}


let mkmutable (x : Astf.flag) : Asttypes.mutable_flag =
  match x with
  |`Positive _ -> Mutable
  | `Negative _ -> Immutable
  | `Ant(_loc,_) -> ant_error _loc 

let paolab (lab : string) (p : Astf.pat) : string =
  match (lab, p) with
  | ("",
     (`Lid(_loc,i) | `Constraint(_loc,`Lid(_,i),_))) -> i
  | ("", p) ->
    Locf.failf (unsafe_loc_of p) "paolab %s" (!dump_pat p)
  | _ -> lab 


let quote_map x : (Parsetree.core_type * Asttypes.variance )=
  match x with
  |`Quote (_loc,p,`Lid(sloc,s)) ->
    let tuple : Asttypes.variance =
      match p with
      |`Positive _ -> Covariant (* (true,false) *)
      |`Negative _ -> Contravariant(* (false,true) *)
      |`Normal _ -> Invariant (* (false,false) *)
      |`Ant (_loc,_) -> ant_error _loc  in
    ((* Some *) (* ( with_loc s sloc) *)
       { ptyp_desc = Ptyp_var s ; ptyp_loc = sloc ; ptyp_attributes = [] }
       ,tuple)
  |`QuoteAny(_loc,p) ->
    let tuple : Asttypes.variance =
      match p with
      |`Positive _ -> Covariant (* (true,false) *)
      |`Negative _ -> Contravariant (* (false,true) *)
      |`Normal _ -> Invariant (* (false,false) *)
      |`Ant (_loc,_) -> ant_error _loc  in
    ((* None *) { ptyp_desc = Ptyp_any ; ptyp_loc = _loc; ptyp_attributes = []},tuple)
  | _ ->
    Locf.failf (unsafe_loc_of x) "quote_map %s" (!dump_ctyp x)

let optional_type_parameters (t : Astf.ctyp) = 
  List.map quote_map (list_of_app t [])

let mk_type_parameters (tl : Astf.opt_decl_params)
  : (Parsetree.core_type * Asttypes.variance) list 
    (* ( (string Asttypes.loc ) option   * (bool * bool))list *)  =
  match tl with
  | `None _ -> []
  | `Some(_,x) ->
    List.map
      (function
        | #Astf.decl_param as x ->
          quote_map (x :> Astf.ctyp)
        |  _ -> assert false) @@ list_of_com x []



(* ['a,'b,'c']*)
let  class_parameters (t : Astf.type_parameters)  : (Parsetree.core_type * Asttypes.variance) list =
  let _loc = unsafe_loc_of t in
  Listf.filter_map
    (fun (y : Astf.type_parameters) ->
       match y with 
      |`Ctyp(_, x) ->
        begin match quote_map x with
          |((* Some  *) { ptyp_desc = Ptyp_var _ ; _ } as x,v) -> Some (x,v) (* TODO: needed?*)
          | (* (None,_) *) _  ->
            Locf.failf _loc  "class_parameters %s" @@ !dump_type_parameters t
        end
      | _ ->  Locf.failf _loc "class_parameters %s" @@ !dump_type_parameters t) 
    (list_of_com t [])


let rec deep_mkrangepat loc c1 c2 =
  if c1 = c2 then mkghpat loc (Ppat_constant (Const_char c1))
  else
    mkghpat loc
      (Ppat_or
         (mkghpat loc (Ppat_constant (Const_char c1)),
          deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2))

let rec mkrangepat loc c1 c2 : Parsetree.pattern_desc =
  if c1 > c2 then mkrangepat loc c2 c1
  else if c1 = c2 then (* mkpat loc *) (Ppat_constant (Const_char c1))
  else
    (* mkpat loc *)
      (Ppat_or
         ((mkghpat loc (Ppat_constant (Const_char c1))),
          (deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2)))




(*************************************)
(* [exp] and [pat] Transformation    *)      
(*************************************)
        
let  pat_literal _loc (x: Astf.literal) : Parsetree.pattern =
  mkpat _loc (mk_constant_pat _loc x)


let exp_literal _loc (x : Astf.literal) : Parsetree.expression =
  mkexp _loc (mk_constant_exp _loc x )
    
let rec pat_desc _loc (x : Astf.pat) : Parsetree.pattern_desc =
  match x with
  | #Astf.literal as x -> mk_constant_pat _loc x 
  | `Lid (_,s) ->
      Ppat_var (with_loc s _loc)
  | `Uid _ |`Dot _ as i ->
      Ppat_construct ((long_uident (i : Astf.vid  :> Astf.ident)), None(* , false *))
  | `Vrn (_,s)  -> Ppat_variant (s, None)                    
  | `Alias (_,p1,`Lid(sloc,s)) -> Ppat_alias (pat p1, with_loc s sloc)
  | `Any _  ->  Ppat_any
  | `App(_,p, r) as f->
      let r =
        match r with
        | `Par (_,r) -> 
            mkpat _loc @@ Ppat_tuple (List.map pat @@ list_of_com r [])
        | _ -> pat r in
      begin
        match (pat p).ppat_desc  with
        | Ppat_variant (s,None) ->
            Ppat_variant (s, Some r)
        | Ppat_construct(li,None(* ,_ *)) ->
            Ppat_construct (li, Some r(* , false *))
        | _ -> Locf.failf _loc "invalid pattern %s" @@ !dump_pat f
      end 
  | `Array (_,p)  ->
     Ppat_array (List.map pat (list_of_sem p []))
  | `ArrayEmpty _ ->  Ppat_array []
  | `Bar (_,p1,p2) ->Ppat_or (pat p1, pat p2)
  | `PaRng (_,`Chr(loc1,c1),`Chr(loc2,c2)) ->
      mkrangepat _loc
        (Escape.char_of_char_token loc1 c1)
        (Escape.char_of_char_token loc2 c2)
  | `Record (_,p) ->
      let ps = list_of_sem p [] in
      let (wildcards,ps) =
        List.partition (function | `Any _ -> true | _ -> false) ps in
      let mklabpat (p : Astf.rec_pat) =
        match p with
        | `RecBind (_loc, i, p) -> (ident (i :> Astf.ident), pat p)
        | p -> error (unsafe_loc_of p) "invalid pattern" in
      Ppat_record
        (List.map mklabpat ps,
         if wildcards = [] then Closed else Open )
  | `Par (_,`Com (_,p1,p2)) ->
        Ppat_tuple (List.map pat (list_of_com p1 (list_of_com p2 [])))
  | `Constraint (_loc,p,t) -> Ppat_constraint (pat p, ctyp t)
  | `ClassPath (_,i) -> Ppat_type (long_type_ident i)
  | `Lazy (_,p) -> Ppat_lazy (pat p)
  | `ModuleUnpack (_,`Uid (sloc,m)) -> Ppat_unpack (with_loc m sloc)
  | `ModuleConstraint (_,`Uid (sloc,m),ty) ->
        Ppat_constraint (mkpat sloc (Ppat_unpack (with_loc m sloc)), ctyp ty)
  | p -> Locf.failf _loc "invalid pattern %s" @@ !dump_pat p

and pat (x : Astf.pat) : Parsetree.pattern =
  let _loc = unsafe_loc_of x in
  {ppat_desc =  pat_desc _loc x ; ppat_loc = _loc ; ppat_attributes = []} 

(**
   The input is either {|$_.$_|} or {|$(id:{:ident| $_.$_|})|}
   the type of return value and [acc] is
   [(loc* string list * exp) list]

   The [string list] is generally a module path, the [exp] is the last field
   Examples:

   
 *)
(**
   return value [true] means constructor otherwise identifier
 *)                                                   
let normalize_vid (x : Astf.vid) =
  let _loc = unsafe_loc_of x in
  match x with 
  | `Dot (loc,_,_)  -> (* FIXME *)
      let rec aux u =
        match u with 
        | `Lid(_,x) -> (`Lid x , [])
        | `Uid (_,x) -> (`Uid x, [])
        | `Ant _ -> assert false               
        | `Dot(_,a,b) ->
            let (x,rest) = aux b in
            (x, left a rest)
      and left x acc =
        match x with
        | `Lid(_,x)|`Uid (_,x) -> x::acc
        | `Dot(_,a,b) -> left a (left b acc)
        | `Ant _ -> assert false in
      begin
        match aux x with
        | (`Lid x , xs) ->
            (false,mkli loc x xs)
        | (`Uid x , xs) ->
            (true,mkli loc x xs)
      end
  | `Uid (loc,i) -> (true, mkli loc i [] )
  | `Lid (loc,i) -> (false,mkli loc i [] )
  | `Ant _ -> assert false
                                                   
let rec exp_desc _loc (x : Astf.exp) : Parsetree.expression_desc =
  match x with
  | #Astf.literal as x ->  mk_constant_exp _loc x 
  | `Uid(_,s) -> Pexp_construct ({loc = _loc; txt = Lident s}, None(* , true *))
  | `Lid(_, "__FILE__")  ->
      exp_desc _loc  
        (`Str (_loc, (String.escaped (Locf.file_name _loc))) :>Astf.exp)
        (* %exp{$str'{Locf.file_name _loc}}  *)
    (* TODO -- what's the case for lazy expansion
       and %exp{__FILE__} *)
  | `Lid(_,"__MODULE__") ->
      let s =
        let sl =
          (_loc |> Locf.file_name |> Filename.basename |> Filenamef.chop_extension_if |>  String.capitalize)
          :: List.rev !module_path in
        String.concat "." sl in 
      Pexp_constant 
        (Const_string (s, None) (* FIXME *)
        )
  | `Lid(_,"__BIND__") ->
      let s =
          String.concat "." (List.rev !current_top_bind) in
      Pexp_constant (Const_string (s , None)) (* FIXME *)
   | (`Lid (_loc,"__PWD__") : Astf.exp) ->
  (* | %exp{ __PWD__ } -> *)
      let s =  Filename.dirname (Locf.file_name _loc) in
      Pexp_constant (Const_string (s, None))
  (* | %exp{ __LOCATION__ } -> *)
   | (`Lid (_loc,"__LOCATION__") : Astf.exp) ->
      exp_desc _loc (!meta_here _loc _loc : Astf.ep  :> Astf.exp)
   | #Astf.vid as x ->
      let (b,id) = normalize_vid x  in
      if b then Pexp_construct (id,None(* ,false *))
      else Pexp_ident id
    (* a.A.b  a.(A.b)
       a.A.b.c (a.(A.b)).c
       a.A.B.b  a.((A.B).b)
     *)
  | `Field(_,x,b) -> Pexp_field ( exp x , snd (normalize_vid b) )
  | `App _ as f ->
    let (f, al) = view_app [] f in
    let al = List.map label_exp al in
    begin match (exp f).pexp_desc with
      | Pexp_construct (li, None(* , _ *)) ->
        let al = List.map snd al in
        let a =
          match al with
          | [a] -> a
          | _ -> mkexp _loc (Pexp_tuple al)  in
        Pexp_construct (li, (Some a)(* , false *))
      | Pexp_variant (s, None) ->
        let al = List.map snd al in
        let a =
          match al with
          | [a] -> a
          | _ -> mkexp _loc (Pexp_tuple al) in
        Pexp_variant (s, Some a)
      | _ -> Pexp_apply (exp f, al)
    end
  | `ArrayDot (_, e1, e2) ->
      Pexp_apply
        (mkexp _loc @@ Pexp_ident (array_function _loc "Array" "get"),
          [("", exp e1); ("", exp e2)])
  | `Array (_,e) -> 
      Pexp_array (List.map exp (list_of_sem e [])) (* be more precise*)
  | `ArrayEmpty _ ->Pexp_array []
  (* | `Assert(_,`Bool(_,false)) -> Pexp_assert (\* Pexp_assertfalse *\) *) (* TODO: assert false not special*)
  | `Assert(_,e) -> Pexp_assert (exp e)

   (* u.x <- b *)
  | `Assign(_, `Field(_,x,b),v) ->
      Pexp_setfield(exp x, snd (normalize_vid b), exp v)
  | `Assign(_, `ArrayDot(loc,e1,e2),v) ->
      Pexp_apply
        ((mkexp loc (Pexp_ident (array_function loc "Array" "set"))),
         [("", exp e1); ("", exp e2); ("", exp v)])
  | `Assign(_, `Lid(lloc,lab),v) ->
      Pexp_setinstvar (with_loc lab lloc, exp v)
  | `Assign (_, `StringDot (loc, e1, e2), v) ->
      Pexp_apply
        ((mkexp loc (Pexp_ident (array_function loc "String" "set"))),
         [("", exp e1); ("", exp e2); ("", exp v)])
  | `Subtype (_,e,t2) ->
      Pexp_coerce (exp e, None, ( (ctyp t2)))
      (* Pexp_constraint (exp e, None, (Some (ctyp t2))) *)
  | `Coercion (_, e, t1, t2) ->
      Pexp_coerce (exp e, Some (ctyp t1), (* Some *) (ctyp t2))
      (* Pexp_constraint (exp e, Some (ctyp t1), Some (ctyp t2)) *)

  | `For (_loc, `Lid(loc,txt), e1, e2, df, el) ->
      Pexp_for
         (
          Parsetree_util.mkpat loc (Ppat_var {txt; loc}),
          (* {txt;loc},  *)
          exp e1,
          exp e2,
          mkdirection df, exp (`Seq (_loc, el)))
  | `Fun(_,
         `Case(_,
               (`LabelS _ | `Label _ | `OptLablS _
               | `OptLabl _ | `OptLablExpr _  as l)  ,e)) ->
    let (lab, p,e1)  =
      match l with
      |`LabelS(_,(`Lid (_,lab) as l)) -> (lab,pat l,None)
      |`OptLablS(_,(`Lid(_,lab) as l)) -> ("?"^lab, pat l,None)
      |`Label(_,`Lid(_,lab),po) -> (lab,pat po,None)
      |`OptLabl(_,`Lid(_,lab),po) ->
        ("?" ^ paolab lab po, pat po,None)
      |`OptLablExpr(_,`Lid(_,lab),po,e1) ->
        ("?" ^ paolab lab po, pat po, Some (exp e1))
      | _ -> assert false in
    Pexp_fun (lab, e1, p, exp e)
    (* Pexp_function (lab, e1, [(p,exp e)]) *)
  | `Fun(_,
         `CaseWhen(_,
                   (`LabelS _ | `Label _ | `OptLablS _  (* | `OptLablExpr _ *) | `OptLabl _ as l ),
                   w,e)) ->
    let ((* lab *)_lab ,p,(* e1 *) _e1) =
      match l with
      |`LabelS(_,(`Lid(_,lab) as l)) -> (lab,pat l,None)
      |`Label (_,`Lid(_,lab),po) -> (lab, pat po,None)
      |`OptLablS(_,(`Lid(_,lab) as l)) -> ("?"^lab, pat l,None)
      |`OptLabl(_,`Lid(_,lab),po) -> ("?"^paolab lab po, pat po,None)
      (* |`OptLablExpr(_,`Lid(_,lab),po,e1) -> *)
      (*     ("?"^ paolab lab po, pat po, Some (exp e1)) *)
      | _ -> assert false in
    (* Pexp_fun *)
    (*     (lab, e1,  *)
    (*      p, mkexp (unsafe_loc_of w) (Pexp_when (exp w, exp e))) *)
      Pexp_function [ { pc_lhs =  p ; pc_guard = Some (exp w); pc_rhs = exp e }]
  (* | `Fun(_, *)
  (*        `CaseWhen(_, *)
  (*                  (  `OptLablExpr _  as l ), *)
  (*                  w,e)) -> *)

    (* Pexp_function *)
    (*     (lab, e1,  *)
    (*      [( p, mkexp (unsafe_loc_of w) (Pexp_when (exp w, exp e)))]) *)
  | `Fun (_,a) -> (* Pexp_function ("", None ,case a ) *)
      Pexp_function ( case a )
  | `IfThenElse (_, e1, e2, e3) -> Pexp_ifthenelse (exp e1,exp e2,Some (exp e3))
  | `IfThen (_,e1,e2) -> Pexp_ifthenelse (exp e1,exp e2, None)
  | `Lazy (_loc,e) -> Pexp_lazy (exp e)
  | `LetIn (_,rf,bi,e) -> Pexp_let (mkrf rf, top_bind bi , exp e)
  | `LetTryInWith(_,rf,bi,e,cas) ->
    let cas =
      let rec f (x : Astf.case)  : Astf.case=
        match x with
        | `Case (_loc,p,e)  ->
          `Case (_loc, p, `Fun (_loc, (`Case (_loc, `Unit _loc, e))))
        | `CaseWhen (_loc,p,c,e) ->
          `CaseWhen
            (_loc, p, c,
              (`Fun (_loc, (`Case (_loc, `Unit _loc, e)))))
        | `Bar (_loc,a1,a2) -> `Bar (_loc, f a1, f a2)
        | `Ant (_loc,_) -> ant_error _loc in
      f cas in
    exp_desc _loc 
      (`App
            (_loc,
              (`Try
                 (_loc,
                   (`LetIn
                      (_loc, (rf :>Astf.flag), (bi :>Astf.bind),
                        (`Fun
                           (_loc,
                             (`Case (_loc, (`Unit _loc), (e :>Astf.exp))))))),
                   cas)), (`Unit _loc)) :> Astf.exp)
      (* %exp{(try let $rec:rf $bi in fun () -> $e with | $cas  ) () } *)

  | `LetModule (_,`Uid(sloc,i),me,e) -> Pexp_letmodule (with_loc i sloc,mexp me,exp e)
  | `Match (_,e,a) -> Pexp_match (exp e,case a )
  | `New (_,id) -> Pexp_new (long_type_ident id)
  | `ObjEnd _ -> Pexp_object{pcstr_self = mkpat _loc Ppat_any; pcstr_fields=[]}
  | `Obj(_,cfl) ->
    Pexp_object { pcstr_self = mkpat _loc Ppat_any; pcstr_fields =  clfield cfl [] }
  | `ObjPatEnd(_,p) -> Pexp_object{pcstr_self=pat p; pcstr_fields=[]}
  | `ObjPat (_,p,cfl) -> Pexp_object { pcstr_self = pat p; pcstr_fields = clfield cfl [] }
  | `OvrInstEmpty _ -> Pexp_override []
  | `OvrInst (_,iel) ->
    let rec mkideexp (x : Astf.rec_exp) acc  = 
      match x with 
      |`Sem(_,x,y) ->  mkideexp x (mkideexp y acc)
      | `RecBind(_,`Lid(sloc,s),e) -> (with_loc s sloc, exp e) :: acc
      | _ -> assert false  in
    Pexp_override (mkideexp iel [])
  | `Record (_,lel) ->
      Pexp_record (mklabexp lel, None)
  | `RecordWith(_loc,lel,eo) ->
      Pexp_record (mklabexp lel,Some (exp eo))
  | `Seq (_,e) ->
    let rec loop = function
      | [] -> (_loc,exp_desc _loc  (`Unit _loc : Astf.exp ))
      | [e] -> (_loc,exp_desc _loc e)
      | e :: el ->
          let (loc,v) = loop el in
          (Locf.merge (unsafe_loc_of e) loc, Pexp_sequence (exp e,mkexp loc v))  in
    snd (loop (list_of_sem e []))
  | `Send (_,e,`Lid(_,s)) -> Pexp_send (exp e, s)
  | `StringDot (_, e1, e2) ->
      (Pexp_apply
         (mkexp _loc (Pexp_ident (array_function _loc "String" "get")),
          [("", exp e1); ("", exp e2)]))

  | `Try (_,e,a) -> Pexp_try (exp e,case a )
  | `Par (_,`Com(_,e1,e2)) ->
      Pexp_tuple (List.map exp (list_of_com e1 (list_of_com e2 [])))
  | `Constraint (_,e,t) ->
      Pexp_constraint (exp e,(* Some *) (ctyp t)(* , None *))
  | `Vrn (_,s) -> Pexp_variant  (s, None)
  | `While (_, e1, el) ->
      Pexp_while (exp e1,exp (`Seq (_loc, el)))
  | `LetOpen(_,f, i,e) ->
      Pexp_open (flag _loc f, long_uident i,exp e)
  | `Package_exp (_,`Constraint(_,me,pt)) -> 
      Pexp_constraint
         (mkexp _loc (Pexp_pack (mexp me)),
          (* Some *) (mktyp _loc (Ptyp_package (package_type pt)))(* , None *))
  | `Package_exp(_,me) -> Pexp_pack (mexp me)
  | `LocalTypeFun (_,`Lid(_,i),e) -> Pexp_newtype (i, exp e)
  | x -> Locf.failf _loc "Panic: invalid exp:%s" @@ !dump_exp x 

and exp (x : Astf.exp) : Parsetree.expression =
  let _loc = unsafe_loc_of x in
  mkexp _loc (exp_desc _loc x)
and label_exp (x : Astf.exp) =
  match x with 
  | `Label (_,`Lid(_,lab),eo) -> (lab,  exp eo)
  | `LabelS(_,`Lid(sloc,lab)) -> (lab,exp (`Lid(sloc,lab)))
  | `OptLabl (_,`Lid(_,lab),eo) -> ("?" ^ lab, exp eo)
  | `OptLablS(loc,`Lid(_,lab)) -> ("?"^lab, exp (`Lid(loc,lab)))
  | e -> ("", exp e) 


and top_bind (x : Astf.bind)  : Parsetree.value_binding list =
  let  rec aux (x : Astf.bind) acc : Parsetree.value_binding list =
  match x with
  | `And (_loc,x,y) -> aux x (aux y acc)
  | `Bind(_loc,`Lid(sloc,bind_name), e ) ->
      Ref.protect current_top_bind (bind_name :: !current_top_bind) (fun _ -> 
      begin match e with
      | `Constraint(_,e,`TyTypePol(_,vs,ty)) ->
          let rec id_to_string (x : Astf.ctyp) =
            match x with
            | `Lid (_,x) -> [x]
            | `App (_loc,x,y) -> id_to_string x @ id_to_string y
            | x ->
                Locf.failf (unsafe_loc_of x) "id_to_string %s" @@ !dump_ctyp x in
          let vars = id_to_string vs in
          let ampersand_vars = List.map (fun x  -> "&" ^ x) vars in
          let ty' = varify_constructors vars (ctyp ty) in
          let mkexp = mkexp _loc in
          let mkpat = mkpat _loc in
          let e = mkexp (Pexp_constraint ((exp e), ((* Some *) (ctyp ty))(* , None *))) in
          let rec mk_newtypes x =
            match x with
            | newtype::[] -> mkexp (Pexp_newtype (newtype, e))
            | newtype::newtypes ->
                mkexp (Pexp_newtype (newtype, (mk_newtypes newtypes)))
            | [] -> assert false in
          let pat =
            mkpat
              (Ppat_constraint
                 ((mkpat (Ppat_var {loc = sloc ; txt = bind_name})),
                  (mktyp _loc (Ptyp_poly (ampersand_vars, ty'))))) in
          let e = mk_newtypes vars in 
          { Parsetree.pvb_pat = pat; pvb_expr =  e; pvb_attributes = []; pvb_loc = _loc }:: acc
      |  _ ->
          { Parsetree.pvb_pat = 
            mkpat sloc (Ppat_var {loc = sloc;txt = bind_name}) ; 
            pvb_expr = exp e ; 
            pvb_attributes = [];
            pvb_loc = _loc
          }
          :: acc 
      end)
  | `Bind (_loc,p,`Constraint (_,e,`TyPol (_,vs,ty))) ->
      {Parsetree.pvb_pat = 
      (pat (`Constraint (_loc, p, (`TyPol (_loc, vs, ty))) : Astf.pat ));
      pvb_expr = exp e;
      pvb_attributes = [];
      pvb_loc = _loc}
      :: acc
  | `Bind (_loc,p,e)  -> { pvb_pat = pat p; pvb_expr =  exp e; pvb_attributes = []; pvb_loc = _loc} :: acc
  | _ -> assert false in
  aux x []
(** transoform [case], used by [exp] transformation *)        
and case (x : Astf.case) :  Parsetree.case list = 
  let cases = list_of_bar x [] in
  Listf.filter_map 
    (fun (x : Astf.case) ->
       let _loc = unsafe_loc_of x in
       match x with 
       | `Case (_,p,e) -> Some {Parsetree.pc_lhs = pat p; pc_guard = None; pc_rhs =  exp e}
       | `CaseWhen (_,p,w,e) ->
         Some
           { Parsetree.pc_lhs = pat p; pc_guard = Some (exp w); 
             pc_rhs = exp e; }
             (* mkexp (unsafe_loc_of w) (Pexp_when (exp w, exp e))) *)
       | x -> Locf.failf _loc "case %s" @@ !dump_case x)
    cases
(** utility, used by [exp] transoformation *)
and mklabexp (x : Astf.rec_exp)  =
  let binds = list_of_sem x [] in
  Listf.filter_map
    (function
      | (`RecBind (_loc,i,e) : Astf.rec_exp) -> Some (ident (i : Astf.vid :> Astf.ident), exp e)
      | x -> Locf.failf (unsafe_loc_of x) "mklabexp : %s" @@ !dump_rec_exp x)
    binds
(** used by [sigi] and [stru] transoformation *)
and mktype_decl (x : Astf.typedecl) : Parsetree.type_declaration list =
  let mktype loc (tl : (Parsetree.core_type * Asttypes.variance) list) cl ~name ~type_kind ~priv ~manifest 
    : Parsetree.type_declaration  =
  (* let (params, variance) = List.split tl in *)
  {ptype_params = (* params *) tl ;
   ptype_name =  name (* (assert false) *); (* FIXME *)
   ptype_cstrs = cl;
   ptype_kind = type_kind;
   ptype_private = priv;
   ptype_manifest = manifest;
   ptype_loc =  loc;
   ptype_attributes =  [] ; (* FIXME*)
    (* ptype_variance = variance *)} in 


  let type_decl name tl cl loc (x : Astf.type_info) =
    match x with
    |`TyMan(_,t1,p,t2) ->
      mktype loc tl cl ~name ~type_kind:(type_kind t2) ~priv:(mkprivate p) ~manifest:(Some (ctyp t1))
    | `TyRepr (_,p1,repr) ->
      mktype loc tl cl ~name ~type_kind:(type_kind repr) ~priv:(mkprivate p1) ~manifest:None
    | `TyEq (_loc,p1,t1) ->
      mktype loc tl cl ~name ~type_kind:(Ptype_abstract) ~priv:(mkprivate p1) ~manifest:(Some (ctyp t1 ))
    | `Ant (_loc,_) -> ant_error _loc  in
  let tys = list_of_and x [] in
  List.map
    (function 
      |`TyDcl (cloc,`Lid(sloc,c),tl,td,cl) ->
        let cl =
          match cl with
          |`None _ -> []
          | `Some(_,cl) ->
            list_of_and cl []
            |> List.map
              (function
                |`Eq(loc,t1,t2) ->
                  (ctyp t1, ctyp t2, loc)
                | _ -> Locf.failf (unsafe_loc_of x) "invalid constraint: %s" (!dump_type_constr cl)) in
        (
         type_decl (with_loc c sloc) (* TODO: see changes with type_declaration *)
           (mk_type_parameters tl)
           cl cloc td)
      | `TyAbstr(cloc,`Lid(sloc,c),tl,cl) -> (* type 'a t  constraint 'a = int *)
        let cl =
          match cl with
          |`None _ -> []
          | `Some(_,cl) ->
            list_of_and cl []
            |> List.map
              (function
                |`Eq(loc,t1,t2) ->
                  (ctyp t1, ctyp t2, loc)
                | _ -> Locf.failf (unsafe_loc_of x) "invalid constraint: %s" (!dump_type_constr cl)) in            
        (
         mktype ~name:(with_loc c sloc) cloc
           (mk_type_parameters tl) cl
           ~type_kind:Ptype_abstract 
           (* ~priv:Private  FIXME: #16*)
           ~priv:Public
           ~manifest:None)

      | (t : Astf.typedecl) ->
        Locf.failf (unsafe_loc_of t) "mktype_decl %s" (!dump_typedecl t)) tys

(** utility , used by [mtyp] transoformation *)    
and  mkwithc (wc : Astf.constr)  : Parsetree.with_constraint list =
  let aux (x : Longident.t Location.loc) : string Location.loc = 
    match x with 
    | { txt = (Lident  s | Ldot (_, s)) ; loc } -> { loc; txt = s}
    | _ -> assert false 
  in
  let mkwithtyp
      (pwith_type : Longident.t Location.loc * Parsetree.type_declaration 
       -> Parsetree.with_constraint)
      loc priv id_tpl ct : Parsetree.with_constraint =
    let type_parameters_and_type_name (t : Astf.ctyp)  =
      let rec aux (t : Astf.ctyp) acc =
        match t with
        |`App(_,t1,t2) ->
            aux (t1 :> Astf.ctyp)
              (optional_type_parameters (t2 :> Astf.ctyp) @ acc)
        (* | `Lid(loc,  s) | `Uid(loc, s) -> *)
        (*     ({Location.txt = s; loc = loc}, acc) *)
        | #Astf.ident' as i  -> (ident i, acc) (* FIXME: todo *)
        | x ->
            Locf.failf (unsafe_loc_of x) "type_parameters_and_type_name %s"
            @@ !dump_ctyp x  in
      aux t [] in
    let (id, tpl) = type_parameters_and_type_name id_tpl in
    (* let (params, variance) = List.split tpl in *)
     pwith_type (id,
       {ptype_params = (* params *) tpl ; ptype_cstrs = [];
        ptype_name = aux id ;
        ptype_kind =  Ptype_abstract;
        ptype_private = priv;
        ptype_manifest = Some (ctyp ct);
        ptype_loc =  loc; (* ptype_variance = variance *)
        ptype_attributes = [];
      }) 
  in
  let constrs = list_of_and wc [] in
  Listf.filter_map (function
    |`TypeEq(_loc,id_tpl,ct) ->
        Some (mkwithtyp (fun (li, x) -> Pwith_type (li,x)) _loc Public id_tpl ct)
    |`TypeEqPriv(_loc,id_tpl,ct) ->
        Some (mkwithtyp (fun (li,x) -> Pwith_type (li,x)) _loc Private id_tpl ct)
    | `ModuleEq(_loc,i1,i2) ->
        Some ( Pwith_module ((long_uident i1), long_uident i2))
    | `TypeSubst(_loc,id_tpl,ct) ->
        Some (mkwithtyp (fun (_, x) -> Pwith_typesubst x) _loc Public id_tpl ct )
    | `ModuleSubst(_loc,i1,i2) ->
        Some (Pwith_modsubst ( aux (long_uident i1), long_uident i2))
    | t -> Locf.failf (unsafe_loc_of t) 
          "bad with constraint (antiquotation) : %s" (!dump_constr t)) constrs 

and mtyp_desc loc (x:Astf.mtyp) : Parsetree.module_type_desc =
  match x with 
  | #Astf.ident' as i  -> Pmty_ident (long_uident i)
  | `Functor(_,`Uid(sloc,n),nt,mt) ->
      Pmty_functor ({loc = sloc; txt = n}, Some (mtyp nt) ,mtyp mt)
        (*FIXME*)
  | `Sig(_,sl) -> Pmty_signature (sigi sl [])
  | `SigEnd _ ->Pmty_signature []
  | `With(_,mt,wc) -> Pmty_with (mtyp mt,mkwithc wc )
  | `ModuleTypeOf(_,me) -> Pmty_typeof (mexp me)
  | t -> Locf.failf loc  "mtyp: %s" (!dump_mtyp t)

and mtyp  (x:Astf.mtyp) : Parsetree.module_type =
  let loc = unsafe_loc_of x in mkmty loc (mtyp_desc loc x)

and sigi_item_desc _loc (s : Astf.sigi)  : Parsetree.signature_item_desc =
  match s with 
  | `Class (_,cd) ->  Psig_class (List.map class_info_cltyp (list_of_and cd []))
  | `ClassType (_,ctd) -> Psig_class_type (List.map class_info_cltyp (list_of_and ctd []))
  | `Exception(_loc,`Uid(_,s)) -> 
      Psig_exception { pext_name = with_loc s _loc; 
                       pext_kind = Pext_decl ([], None);
                       pext_attributes = [];
                       pext_loc = _loc;
                     }
      (* Psig_exception (with_loc s _loc, []) *)
  | `Exception(_loc,`Of(_,`Uid(sloc,s),t)) ->
      Psig_exception { pext_name  = 
                       {loc = sloc; txt = s}; 
                       pext_attributes = [];
                       pext_kind = Pext_decl (List.map ctyp (list_of_star t []),
                                              None);
                       pext_loc = _loc
                     }
      (* Psig_exception ({loc = sloc; txt = s}, List.map ctyp (list_of_star t [])) *)
  | `Exception (_,_) -> assert false (*FIXME*)
  | `External (loc, `Lid(sloc,n), t, sl) ->
      Psig_value ( mkvalue_desc {loc = sloc; txt = n} loc t (list_of_app sl []))
  | `Include (_loc,mt) -> Psig_include { pincl_mod = (mtyp mt); pincl_loc = _loc ; pincl_attributes = []}
  | `Module (_loc,`Uid(sloc,n),mt) -> 
      Psig_module { pmd_name = with_loc n sloc; pmd_type = mtyp mt; pmd_attributes = []; pmd_loc = _loc}
      (* Psig_module (with_loc n sloc,mtyp mt) *)
  | `RecModule (_,mb) -> Psig_recmodule (module_sig_bind mb [])
  | `ModuleTypeEnd(_loc,`Uid(sloc,n)) ->
      Psig_modtype { pmtd_name = with_loc n sloc; 
                     pmtd_type = None ;
                     pmtd_loc = _loc;
                     pmtd_attributes = []
                   }
      (* Psig_modtype (with_loc n sloc , Pmodtype_abstract)  *)
  | `ModuleType (_loc,`Uid(sloc,n),mt) ->
      Psig_modtype {
      pmtd_name = with_loc n sloc; 
      pmtd_type = (* None *) Some (mtyp mt) ;
      pmtd_loc = _loc;
      pmtd_attributes = []
    }
      (* Psig_modtype (with_loc n sloc,Pmodtype_manifest (mtyp mt)) *)
  | `Open (_loc, g,id) ->
      Psig_open { popen_lid = long_uident id; 
                  popen_override = flag _loc g;
                  popen_loc = _loc;
                  popen_attributes = []
                }
      (* Psig_open (flag _loc g, long_uident id) *)
  | `Type (_,tdl) -> Psig_type (mktype_decl tdl )
  | `Val (_,`Lid(sloc,n),t) ->
      Psig_value (mkvalue_desc  (with_loc n sloc ) _loc t [])
  | t -> Locf.failf (unsafe_loc_of t) "sigi: %s" (!dump_sigi t)
(* and module_sig_bind (x:mbind)  *)

and sigi (s : Astf.sigi) (l :Parsetree.signature) : Parsetree.signature =
  match s with
  | `Sem(_,sg1,sg2) -> sigi sg1 (sigi sg2 l)
  | `Directive _ | `DirectiveSimple _  -> l
  | _ -> let _loc = unsafe_loc_of s in mksig _loc (sigi_item_desc _loc s ) :: l

and stru_item_desc _loc (s : Astf.stru) : Parsetree.structure_item_desc =
  match s with 
  | `Class (_,cd) ->
      Pstr_class (List.map class_info_clexp (list_of_and cd []))
  | `ClassType (_,ctd) ->
    Pstr_class_type (List.map class_info_cltyp (list_of_and ctd []))
  | `Exception(_loc,`Uid(loc,s)) ->
      Pstr_exception { pext_loc = _loc;
                       pext_name = with_loc s loc ;
                       pext_attributes = [];
                       pext_kind = Pext_decl ([], None)}
      (* Pstr_exception ({loc;txt=s}, []) *)
  | `Exception (_loc, `Of (_,  `Uid (loc, s), t)) ->
      Pstr_exception {
      pext_loc = _loc ; 
      pext_name = with_loc s loc ; 
      pext_attributes = [];
      pext_kind = Pext_decl (List.map ctyp (list_of_star t []), None)
    }
      (* Pstr_exception ({loc ; txt = s}, List.map ctyp (list_of_star t [])) *)
  (* TODO *)     
  (* | {@loc| exception $uid:s = $i |} -> *)
  (*     [mkstr loc (Pstr_exn_rebind (with_loc s loc) (ident i)) :: l ] *)
  (* | {@loc| exception $uid:_ of $_ = $_ |} -> *)
  (*     error loc "type in exception alias" *)
  | `Exception (_,_) -> assert false (*FIXME*)
  | `StExp (_,e) -> Pstr_eval (exp e, [])
  | `External(_,`Lid(sloc,n),t,sl) ->
      Pstr_primitive
         (mkvalue_desc {loc=sloc;txt=n} _loc t (list_of_app sl [] ))
  | `Include (_loc,me) -> 
      Pstr_include { pincl_mod = mexp me; pincl_loc = _loc; pincl_attributes = []}
      (* Pstr_include (mexp me) *)
  | `Module (_loc,`Uid(sloc,n),me) -> (* change the [module_path] and restore it later *)
      Ref.protect module_path (n :: !module_path )
        (fun _ -> (Parsetree.Pstr_module 

                     {
                      pmb_name = {loc = sloc; txt = n};
                      pmb_expr = mexp me;
                      pmb_attributes = [];
                      pmb_loc = _loc
                    }
                     (* ({loc = sloc; txt = n},mexp me) *)
                  ))
  | `RecModule (_,mb) ->
      Pstr_recmodule (module_str_bind mb [])
  | `ModuleType (_loc,`Uid(sloc,n),mt) ->
      Pstr_modtype {
      pmtd_name = with_loc n sloc;
      pmtd_type = Some (mtyp mt);
      pmtd_attributes = [];
      pmtd_loc = _loc 
    }
    (* Pstr_modtype (with_loc n sloc,mtyp mt) *)
  | `Open (_loc,g,id) ->
      Pstr_open {
      popen_lid = long_uident id;
      popen_override = flag _loc g;
      popen_loc = _loc ; 
      popen_attributes = []
    }
      (* Pstr_open (flag _loc g ,long_uident id) *)
  | `Type (_,tdl) -> Pstr_type (mktype_decl tdl )
  | `Value (_,rf,bi) ->
    Pstr_value (mkrf rf, top_bind bi)
  | x-> Locf.failf _loc "stru : %s" (!dump_stru x)
and stru (s : Astf.stru) (l : Parsetree.structure) : Parsetree.structure =
  match s with
  (* ad-hoc removing the empty statement, a more elegant way is in need*)
  | (`StExp (_, `Unit _)) -> l
  | `Sem(_,st1,st2) ->  stru st1 (stru st2 l)        
  | `Directive _ | `DirectiveSimple _  -> l
  | `TypeWith(_loc,tdl, ns) ->
      (* FIXME all traversal needs to deal with TypeWith later .. *)
      stru (!generate_type_code _loc tdl ns) l

  | _ ->   let loc = unsafe_loc_of s in
     mkstr loc (stru_item_desc loc s) :: l               
and module_sig_bind (x : Astf.mbind) 
    (acc : Parsetree.module_declaration list ) : Parsetree.module_declaration list =
  match x with 
  | `And(_,x,y) -> module_sig_bind x (module_sig_bind y acc)
  | `Constraint(_loc,`Uid(loc,s),mt) ->
    (* ({txt= s; loc}, mtyp mt)  *)
      { pmd_name = {txt= s; loc} ; 
        pmd_type = mtyp mt ;
        pmd_attributes = []; 
        pmd_loc = _loc
      }
      :: acc
  | t -> Locf.failf (unsafe_loc_of t) "module_sig_bind: %s" (!dump_mbind t) 
and module_str_bind (x:Astf.mbind) acc  : Parsetree.module_binding list =
  match x with 
  | `And(_,x,y) -> module_str_bind x (module_str_bind y acc)
  | `ModuleBind(_loc,`Uid(loc,s),mt,me)->
      (* ({txt=s;loc}, mtyp mt, mexp me)  *)
      {
       pmb_name = {txt=s;loc};
       pmb_expr = mexp (`Constraint(_loc, me, mt));
       pmb_attributes = [];
       pmb_loc = _loc
     }
      :: acc
  | t -> Locf.failf (unsafe_loc_of t) "module_str_bind: %s" (!dump_mbind t)

and mexp_desc loc (x:Astf.mexp) : Parsetree.module_expr_desc =
  match x with 
  | #Astf.vid'  as i ->
      Pmod_ident (long_uident (i : Astf.vid' :> Astf.ident))
  | `App(_,me1,me2) -> Pmod_apply (mexp me1,mexp me2)
  | `Functor(_,`Uid(sloc,n),mt,me) ->
      Pmod_functor (with_loc n sloc,Some (mtyp mt),mexp me)
  | `Struct(_,sl) -> Pmod_structure (stru sl [])
  | `StructEnd _ -> Pmod_structure []
  | `Constraint(_,me,mt) ->
      Pmod_constraint (mexp me,mtyp mt)
  | `PackageModule(_,`Constraint(_,e,`Package(_,pt))) ->
      Pmod_unpack (
      mkexp loc
        (Pexp_constraint
           (exp e, (* Some *) (mktyp loc (Ptyp_package (package_type pt)))(* , None *))))
  | `PackageModule(_,e) -> Pmod_unpack (exp e)
  | t -> Locf.failf (unsafe_loc_of t) "mexp: %s" (!dump_mexp t) 

and mexp (x:Astf.mexp) : Parsetree.module_expr =
  let _loc = unsafe_loc_of x in
  {pmod_desc = mexp_desc _loc x ; pmod_loc = _loc; pmod_attributes = []}

and cltyp_desc loc (x:Astf.cltyp) : Parsetree.class_type_desc =
  match x with
  | `ClApply(_, id, tl) -> 
      Pcty_constr
         (long_class_ident (id :> Astf.ident),
          List.map (function
              | `Ctyp (_loc,x) -> ctyp x | _ -> assert false) (list_of_com tl []))
  | #Astf.vid' as id ->
      Pcty_constr (long_class_ident (id : Astf.vid' :> Astf.ident), [])
  | `CtFun (_, (`Label (_, `Lid(_,lab), t)), ct) ->
      Pcty_arrow (lab, ctyp t, cltyp ct)
  | `CtFun (_, (`OptLabl (loc1, `Lid(_,lab), t)), ct) ->
      let t = `App (loc1, predef_option loc1, t) in
      Pcty_arrow ("?" ^ lab, ctyp t, cltyp ct)
  | `CtFun (_,t,ct) -> Pcty_arrow ("" ,ctyp t, cltyp ct)
  | `ObjEnd _ ->
      Pcty_signature 
        (* { pcsig_self =  *)
        (* } *)
        {pcsig_self= mktyp loc Ptyp_any; pcsig_fields=[]; (* pcsig_loc=loc *)}
  | `ObjTyEnd(_,t) ->
      Pcty_signature {pcsig_self= ctyp t; pcsig_fields = [](* ; pcsig_loc = loc; *)}
  | `Obj(_,ctfl) ->
    Pcty_signature {pcsig_self = mktyp loc Ptyp_any; pcsig_fields= clsigi ctfl [] (* ;pcsig_loc=loc *)}
  | `ObjTy (_,t,ctfl) ->
    Pcty_signature {pcsig_self = ctyp t; pcsig_fields = clsigi ctfl []; (* pcsig_loc =  loc; *)}
  |  x -> Locf.failf (unsafe_loc_of x) "class type: %s" (!dump_cltyp x) 
and cltyp (x:Astf.cltyp) =
  let loc = unsafe_loc_of x  in
  mkcty loc (cltyp_desc loc x )
and class_info_clexp (ci : Astf.cldecl) : Parsetree.class_declaration =
  match ci with 
  | (`ClDecl(loc,vir,`Lid(nloc,name),params,ce) : Astf.cldecl) -> 
    (* let (loc_params, (params, variance)) = *)
    (*   (unsafe_loc_of params, List.split (class_parameters params)) in *)
    {pci_virt = mkvirtual vir;
     pci_params = (* (params,  loc_params) *) class_parameters params;
     pci_name = with_loc name nloc;
     pci_expr = clexp ce;
     pci_loc =  loc;
     pci_attributes = []
     (* pci_variance = variance *)
   }
  | `ClDeclS(loc,vir,`Lid(nloc,name),ce) -> 
    {pci_virt = mkvirtual vir;
     pci_params = [] (* ([],  loc) *);
     pci_name = with_loc name nloc;
     pci_expr = clexp ce;
     pci_loc =  loc;
     pci_attributes = []
     (* pci_variance = [] *)}
  | ce -> Locf.failf  (unsafe_loc_of ce) "class_info_clexp: %s" (!dump_cldecl ce) 
and class_info_cltyp (ci : Astf.cltdecl)  =
  match ci with 
  | (`CtDecl(loc, vir,`Lid(nloc,name),params,ct) : Astf.cltdecl) ->
    (* let (loc_params, (params, variance)) = *)
    (*   (unsafe_loc_of params, List.split (class_parameters params)) in *)
    {pci_virt = mkvirtual vir;
     pci_params = class_parameters params(* (params,  loc_params) *);
     pci_name = with_loc name nloc;
     pci_expr = cltyp ct;
     pci_loc =  loc;
     pci_attributes = []; 
     (* pci_variance = variance *)}
  | (`CtDeclS (loc,vir,`Lid(nloc,name),ct) : Astf.cltdecl) -> 
    {pci_virt = mkvirtual vir;
     pci_params = [] (* ([],  loc) *);
     pci_name = with_loc name nloc;
     pci_expr = cltyp ct;
     pci_loc =  loc;
     pci_attributes = [];
     (* pci_variance = [] *)
   }
  | ct -> Locf.failf (unsafe_loc_of ct) "bad class/class type declaration/definition %s " (!dump_cltdecl ct)
and clsigi_desc loc (c : Astf.clsigi) : Parsetree.class_type_field_desc = 
  match c with 
  |`Eq (_, t1, t2) -> (* Pctf_cstr *) Pctf_constraint (ctyp t1, ctyp t2)
  | `SigInherit (_,ct) -> Pctf_inherit (cltyp ct)
  | `Method (_,`Lid(_,s),pf,t) ->
      Pctf_method (s, mkprivate pf, Concrete,  mkpolytype (ctyp t))
  | `CgVal (_, `Lid(_,s), b, v, t) ->
      Pctf_val (s, mkmutable b, mkvirtual v, ctyp t)
  | `VirMeth (_,`Lid(_,s),b,t) ->
      Pctf_method (s,mkprivate b, Virtual, mkpolytype (ctyp t))
  | t -> Locf.failf loc "clsigi :%s" (!dump_clsigi t) 
         
and clsigi (c : Astf.clsigi) (l:  Parsetree.class_type_field list) : Parsetree.class_type_field list =
  match c with 
  | `Sem(_,csg1,csg2) -> clsigi csg1 (clsigi csg2 l)
  | _ -> let loc = unsafe_loc_of c in mkctf loc (clsigi_desc loc c) :: l
and clexp_desc loc (x:Astf.clexp) : Parsetree.class_expr_desc =
  match x with 
  | `CeApp _ ->
    let rec view_app acc (x : Astf.clexp)  =
      match x  with 
      | `CeApp (_loc,ce,(a : Astf.exp)) -> view_app (a :: acc) ce
      | ce -> (ce, acc) in
    let (ce, el) = view_app [] x in
    let el = List.map label_exp el in
    Pcl_apply (clexp ce, el)

  | `ClApply (_,id,tl) ->
      Pcl_constr (long_class_ident (id :> Astf.ident),
                  (List.map (function |`Ctyp (_loc,x) -> ctyp x | _ -> assert false)
                       (list_of_com tl [])))
  | #Astf.vid' as id  ->
      Pcl_constr (long_class_ident (id : Astf.vid' :> Astf.ident), [])
  | `CeFun (_, (`Label (_,`Lid(_,lab), po)), ce) ->
      Pcl_fun (lab, None, pat po , clexp ce)
  | `CeFun(_,`OptLablExpr(_,`Lid(_loc,lab),p,e),ce) ->
      let lab = paolab lab p in
      Pcl_fun ("?" ^ lab,Some (exp e), pat p, clexp ce)
  | `CeFun (_,`OptLabl(_,`Lid(_loc,lab),p), ce) -> 
    let lab = paolab lab p in
    Pcl_fun ("?" ^ lab, None, pat p, clexp ce)
  | `CeFun (_,p,ce) ->
      Pcl_fun ("", None, pat p, clexp ce)
  | `LetIn (_, rf, bi, ce) ->
      Pcl_let (mkrf rf, top_bind bi, clexp ce)
  | `ObjEnd _ ->
      Pcl_structure{pcstr_self= mkpat loc Ppat_any; pcstr_fields=[]}
  | `Obj(_,cfl) ->
    Pcl_structure {pcstr_self = mkpat loc Ppat_any; pcstr_fields = clfield cfl [];}
  | `ObjPatEnd(_,p) ->
      Pcl_structure {pcstr_self= pat p; pcstr_fields = []}
  | `ObjPat (_,p,cfl) ->
    let cil = clfield cfl [] in
    Pcl_structure {pcstr_self = pat p; pcstr_fields = cil;}
  | `Constraint (_,ce,ct) ->
      Pcl_constraint (clexp ce,cltyp ct)
  | t -> Locf.failf (unsafe_loc_of t) "clexp: %s" (!dump_clexp t)

and clexp  (x:Astf.clexp) :  Parsetree.class_expr =
  let loc = unsafe_loc_of x in
  mkcl loc (clexp_desc loc x)
and clfield_desc loc (c:Astf.clfield) : Parsetree.class_field_desc =
  match c with
  | `Eq (_, t1, t2) -> Pcf_constraint(ctyp t1, ctyp t2)
  | `Inherit (_, ov, ce) ->
      Pcf_inherit (flag loc ov,clexp ce, None)
  | `InheritAs(_,ov,ce,`Lid(_,x)) ->
      Pcf_inherit (flag loc ov,clexp ce,Some x)
  | `Initializer (_,e) -> Pcf_initializer (exp e)
  | `CrMthS(loc,`Lid(sloc,s),ov,pf,e) ->
    Pcf_method ({loc=sloc; txt = s}, mkprivate pf, Cfk_concrete(flag loc ov, mkexp loc (Pexp_poly (exp e,None))))
  | `CrMth (_, `Lid(sloc,s), ov, pf, e, t) ->
    let t = Some (mkpolytype (ctyp t)) in
    let e = mkexp loc (Pexp_poly (exp e, t)) in
    Pcf_method (with_loc s sloc, mkprivate pf, Cfk_concrete(flag loc ov, e))
  | `CrVal (_, `Lid(sloc,s), ov, mf, e) ->
      Pcf_val (with_loc s sloc, mkmutable mf, Cfk_concrete(flag loc ov, exp e))
  | `VirMeth (_,`Lid(sloc,s),pf,t) ->
      Pcf_method (with_loc s sloc, mkprivate pf, Cfk_virtual(mkpolytype (ctyp t)))
  | `VirVal (_,`Lid(sloc,s),mf,t) ->
      (* Pcf_valvirt (with_loc s sloc, mkmutable mf, ctyp t) *)
      Pcf_val( with_loc s sloc, mkmutable mf, Cfk_virtual(ctyp t))
  | x  -> Locf.failf  loc "clfield: %s" @@ !dump_clfield x
  
and clfield (c : Astf.clfield) l =
  let loc = unsafe_loc_of c in
  match c with
  | `Sem(_,cst1,cst2) -> clfield cst1 (clfield cst2 l)
  | _ ->  mkcf loc (clfield_desc loc c) :: l
let sigi (ast : Astf.sigi) : Parsetree.signature = sigi ast []
let stru ast = stru ast []

let directive (x : Astf.exp) : Parsetree.directive_argument =
  match x with 
  |`Str(_,s) -> Pdir_string s
  |`Int(_,i) -> Pdir_int (int_of_string i)
  | `Bool(_,x) ->  Pdir_bool x 
  (* |`Lid(_loc,("true"|"false" as x)) -> *)
  (*   if x ="true" then Pdir_bool true *)
  (*   else Pdir_bool false *)
  | e ->
    let ident_of_exp : Astf.exp -> Astf.ident =
      let error () = invalid_arg "ident_of_exp: this expession is not an identifier" in
      let rec self (x : Astf.exp) : Astf.ident =
        let _loc = unsafe_loc_of x in
        match x with 
        | `App(_,e1,e2) -> `Apply(_loc,self e1, self e2)
        | `Field(_,e1,e2) -> `Dot(_loc,self e1, (e2 :> Astf.ident))
        | `Lid _  -> error ()
        | `Uid _ | `Dot _ as i -> (i: Astf.vid :> Astf.ident)
        | _ -> error ()  in 
      function
      | #Astf.vid as i ->  (i: Astf.vid :> Astf.ident)
      | `App _ -> error ()
      | t -> self t  in
    Pdir_ident (ident_noloc (ident_of_exp e)) 

let phrase (x: Astf.stru) : Parsetree.toplevel_phrase =
  match x with 
  | `Directive (_, `Lid(_,d),dp) -> Ptop_dir (d ,directive dp)
  | `DirectiveSimple(_,`Lid(_,d)) -> Ptop_dir (d, Pdir_none)
  | `Directive (_, `Ant(_loc,_),_) -> error _loc "antiquotation not allowed"
  | si -> Ptop_def (stru si) 



let empty_signature  = []
let empty_structure = []



(* local variables: *)
(* compile-command: "cd .. && pmake common/ast2pt.cmo" *)
(* end: *)
