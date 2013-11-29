

(** Dump the FanAst to Parsetree, this file should introduce minimal
    dependency or only dependent on those generated files*)



open Util
open Parsetree_util
open Astf (* FIXME later*)
open Ast_basic


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


(** Forward declarations *)
let dump_ident =
  ref (fun _ -> failwithf "%s.dump_ident not implemented" __MODULE__ )
let dump_ctyp = 
  ref (fun _ -> failwithf "%s.dump_ctyp not implemented" __MODULE__)
let dump_row_field       =
  ref (fun _ -> failwithf "%s.dump_row_field not implemented" __MODULE__)

let dump_name_ctyp =
  ref (fun _ -> failwithf "%s.dump_name_ctyp not implemented" __MODULE__)
let dump_constr          =
  ref (fun _ -> failwithf "%s.dump_constr not implemented" __MODULE__)
let dump_mtyp            =
  ref (fun _ -> failwith "Ast2pt.dump_mtyp not implemented")

let dump_ctyp =
  ref (fun _ -> failwith "Ast2pt.dump_ctyp not implemented")
let dump_or_ctyp         = ref (fun _ -> failwith "Ast2pt.dump_or_ctyp not implemented")
let dump_pat             = ref (fun _ -> failwith "Ast2pt.dump_pat not implemented")
let dump_type_parameters = ref (fun _ -> failwith "Ast2pt.dump_type_parameters not implemented")
let dump_exp             = ref (fun _ -> failwith "Ast2pt.dump_exp not implemented")
let dump_case            = ref (fun _ -> failwith "Ast2pt.dump_case not implemented")
let dump_rec_exp         = ref (fun _ -> failwith "Ast2pt.dump_rec_exp not implemented")
let dump_type_constr     = ref (fun _ -> failwith "Ast2pt.dump_type_constr not implemented")
let dump_typedecl        = ref (fun _ -> failwith "Ast2pt.dump_typedecl not implemented")
let dump_sigi            = ref (fun _ -> failwith "Ast2pt.dump_sigi not implemented")
let dump_mbind           = ref (fun _ -> failwith "Ast2pt.dump_mbind not implemented")
let dump_mexp            = ref (fun _ -> failwith "Ast2pt.dump_mexp not implemented")
let dump_stru            = ref (fun _ -> failwith "Ast2pt.dump_stru not implemented")
let dump_cltyp           = ref (fun _ -> failwith "Ast2pt.dump_cltyp not implemented")
let dump_cldecl          = ref (fun _ -> failwith "Ast2pt.dump_cldecl not implemented")
let dump_cltdecl         = ref (fun _ -> failwith "Ast2pt.dump_cltdecl not implemented")
let dump_clsigi          = ref (fun _ -> failwith "Ast2pt.dump_clsigi not implemented")
let dump_clexp           = ref (fun _ -> failwith "Ast2pt.dump_clexp not implemented")
let dump_clfield         = ref (fun _ -> failwith "Ast2pt.dump_clfield not implemented")



(*****************************)
(* Util functions            *)    
(*****************************)
let mk_constant _loc (x:literal) : Asttypes.constant =
  match x with 
  | `Chr (_,s) -> Const_char (Escape.char_of_char_token _loc s)
  | `Int (_,s) ->
      (try Const_int (int_of_string s)
      with Failure _ ->
        error _loc
          "Integer literal exceeds the range of representable integers of type int" )
  | `Int32 (_,s) ->
      (try Const_int32 (Int32.of_string s)
      with Failure _ ->
        error _loc
          "Integer literal exceeds the range of representable integers of type int32" )
  | `Int64 (_,s) ->
      (try Const_int64 (Int64.of_string s)
      with
        Failure _ ->
          error _loc
            "Integer literal exceeds the range of representable integers of type int64" )
  | `Nativeint (_,s) ->
      (try Const_nativeint (Nativeint.of_string s)
      with
        Failure _ ->
          error _loc
            "Integer literal exceeds the range of representable integers of type nativeint" )
  | `Flo (_,s) -> Const_float (remove_underscores s)
  | `Str (_,s) -> Const_string (Escape.string_of_string_token _loc s)
    
let generate_type_code :
  (Astf.loc -> Astf.typedecl -> Astf.strings -> Astf.stru) ref =
  ref (fun _ -> failwith "Ast2pt.generate_type_code not implemented")


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
  let rec self (i:Astf.ident) acc =
    let _loc = unsafe_loc_of i in
    match i with
    | `Dot (_,`Lid (_,"*predef*"),`Lid (_,"option")) ->
      Some (ldot (lident "*predef*") "option", `lident)
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
          | (None ,s) -> Some (lident s, `uident)
          | (Some (_,(`uident|`app)),"") -> acc
          | (Some (x,(`uident|`app)),s) -> Some ((ldot x s), `uident)
          | _ ->
              Locf.failf _loc "invalid long identifier %s" @@ !dump_ident i
        end
    | `Lid (_,s) ->
      let x =
        match acc with
        | None  -> lident s
        | Some (acc,(`uident|`app)) -> ldot acc s
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


let mktype loc tl cl ~type_kind ~priv ~manifest =
  let (params, variance) = List.split tl in
  {Parsetree.ptype_params = params;
    ptype_cstrs = cl;
    ptype_kind = type_kind;
    ptype_private = priv;
    ptype_manifest = manifest;
    ptype_loc =  loc;
    ptype_variance = variance} 

let mkprivate (x:flag) : Asttypes.private_flag =
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
    if is_cls then mktyp _loc  @@ Ptyp_class (li, List.map ctyp al, [])
    else mktyp _loc @@ Ptyp_constr (li, (List.map ctyp al))
  | `Arrow (_, (`Label (_,  `Lid(_,lab), t1)), t2) ->
    mktyp _loc @@ Ptyp_arrow (lab, (ctyp t1), ctyp t2)
  | `Arrow (_, (`OptLabl (loc1, `Lid(_,lab), t1)), t2) ->
    mktyp _loc @@ Ptyp_arrow ("?" ^ lab, ctyp (`App (loc1, predef_option loc1, t1) ), ctyp t2)
  | `Arrow (_loc, t1, t2) ->
      mktyp _loc @@ Ptyp_arrow ("", ctyp t1, ctyp t2)
  | `TyObjEnd(_,row) ->
    let xs =
      match row with
      |`Negative _ -> []
      | `Positive _ -> [mkfield _loc Pfield_var]
      | `Ant _ -> ant_error _loc in
    mktyp _loc @@ Ptyp_object xs
  | `TyObj(_,fl,row) ->
    let xs  =
      match row with
      | `Negative _ -> []
      | `Positive _  -> [mkfield _loc Pfield_var]
      | `Ant _ -> ant_error _loc  in
    mktyp _loc (Ptyp_object (meth_list fl xs))

  | `ClassPath (_, id) -> mktyp _loc  @@ Ptyp_class ((ident id), [], [])
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
    mktyp _loc @@ Ptyp_variant (row_field t [], true, None)
  | `PolySup(_,t) ->
    mktyp _loc @@ Ptyp_variant (row_field t [], false, None)
  | `PolyInf(_,t) ->
    mktyp _loc @@ Ptyp_variant (row_field t [], true, Some [])
  | `PolyInfSup(_,t,t') ->
    let rec name_tags (x: Astf.tag_names) =
      match x with 
      | `App(_,t1,t2) -> name_tags t1 @ name_tags t2
      | `TyVrn (_, `C (_,s))    -> [s]
      | _ -> assert false  in 
    mktyp _loc @@ Ptyp_variant (row_field t [], true, Some (name_tags t'))
  |  x -> Locf.failf _loc "ctyp: %s" @@ !dump_ctyp x

and row_field (x: Astf.row_field) acc : Parsetree.row_field list =
  match x with 
  |`TyVrn (_loc,`C(_,i)) -> Rtag (i, true, []) :: acc
  | `TyVrnOf(_loc,`C(_,i),t) ->
    Rtag (i, false, [ctyp t]) :: acc 
  | `Bar(_,t1,t2) -> row_field t1 ( row_field t2 acc)
  | `Ant(_loc,_) -> ant_error _loc
  | `Ctyp(_,t) -> Rinherit (ctyp t) :: acc
  | t -> Locf.failf (unsafe_loc_of t) "row_field: %s" (!dump_row_field t)

and meth_list (fl: Astf.name_ctyp) acc : Parsetree.core_field_type list   =
  match fl with
  |`Sem (_loc,t1,t2) -> meth_list t1 (meth_list t2 acc)
  | `TyCol(_loc,`Lid(_,lab),t) ->
    mkfield _loc (Pfield (lab, (mkpolytype (ctyp t)))) :: acc
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
  | `With(_loc,(#ident' as i),wc) ->
    (long_uident i, package_type_constraints wc [])
  | #ident' as i  -> (long_uident i, [])
  | mt -> Locf.failf (unsafe_loc_of mt) "unexpected package type: %s" @@ !dump_mtyp mt

(*************************************)
(***    Utils                        *)
(*************************************)                                                                           
let mktrecord (x: name_ctyp) :
  (string Location.loc * Asttypes.mutable_flag * Parsetree.core_type *  loc)=
  match x with 
  |`TyColMut(_loc,`Lid(sloc,s),t) ->
    (with_loc s sloc, Mutable, mkpolytype (ctyp t),  _loc)
  | `TyCol(_loc,`Lid(sloc,s),t) ->
    (with_loc s sloc, Immutable, mkpolytype (ctyp t),  _loc)
  | t -> Locf.failf (unsafe_loc_of t) "mktrecord %s "
           (!dump_name_ctyp t)

let mkvariant (x:or_ctyp) :
  (string Location.loc * Parsetree.core_type list *  Parsetree.core_type option * Locf.t) =
  let _loc = unsafe_loc_of x in
  match x with
  | `Uid(_,s) -> (with_loc  s _loc, [], None,  _loc)
  | `Of(_,`Uid(sloc,s),t) ->
    (with_loc  s sloc, List.map ctyp (list_of_star t []), None,  _loc)

  | `TyCol(_,`Uid(sloc,s),( `Arrow _  as t )) -> (*GADT*)
    (match List.rev (listr_of_arrow t []) with
     | u::t ->
       (with_loc s sloc, List.map ctyp t, Some (ctyp u),  _loc)  
     | [] -> assert false)

  | `TyCol(_,`Uid(sloc,s),t) ->
    (with_loc  s sloc, [], Some (ctyp t),  _loc)
  | t -> Locf.failf _loc "mkvariant %s " @@ !dump_or_ctyp t

                                                                           

let type_kind (x:type_repr) : Parsetree.type_kind  =
  match x with
  | `Record (_,t) ->
    Parsetree.Ptype_record (List.map mktrecord (list_of_sem t []))
  | `Sum(_,t) ->
    Ptype_variant (List.map mkvariant (list_of_bar t []))
  | `Ant(_loc,_) -> ant_error _loc



let mkvalue_desc loc t (p:  strings list) : Parsetree.value_description =
  {pval_type = ctyp t;
   pval_prim =
   List.map (fun p ->
     match p with
     | `Str (_,p) -> p
     | _ -> failwithf  "mkvalue_desc") p ;
   pval_loc =  loc}


let mkmutable (x:flag) : Asttypes.mutable_flag =
  match x with
  |`Positive _ -> Mutable
  | `Negative _ -> Immutable
  | `Ant(_loc,_) -> ant_error _loc 

let paolab (lab:string) (p:pat) : string =
  match (lab, p) with
  | ("",
     (`Lid(_loc,i) | `Constraint(_loc,`Lid(_,i),_))) -> i
  | ("", p) ->
    Locf.failf (unsafe_loc_of p) "paolab %s" (!dump_pat p)
  | _ -> lab 


let quote_map x =
  match x with
  |`Quote (_loc,p,`Lid(sloc,s)) ->
    let tuple =
      match p with
      |`Positive _ -> (true,false)
      |`Negative _ -> (false,true)
      |`Normal _ -> (false,false)
      |`Ant (_loc,_) -> ant_error _loc  in
    (Some ( with_loc s sloc),tuple)
  |`QuoteAny(_loc,p) ->
    let tuple =
      match p with
      |`Positive _ -> (true,false)
      |`Negative _ -> (false,true)
      |`Normal _ -> (false,false)
      |`Ant (_loc,_) -> ant_error _loc  in
    (None,tuple)
  | _ ->
    Locf.failf (unsafe_loc_of x) "quote_map %s" (!dump_ctyp x)

let optional_type_parameters (t:ctyp) = 
  List.map quote_map (list_of_app t [])

let mk_type_parameters (tl:opt_decl_params)
  :  ( (string Asttypes.loc ) option   * (bool * bool))list  =
  match tl with
  | `None _ -> []
  | `Some(_,x) ->
    List.map
      (function
        | #decl_param as x ->
          quote_map (x:>ctyp)
        |  _ -> assert false) @@ list_of_com x []



(* ['a,'b,'c']*)
let  class_parameters (t:type_parameters) =
  let _loc = unsafe_loc_of t in
  Listf.filter_map
    (fun (y:type_parameters) ->
       match y with 
      |`Ctyp(_, x) ->
        begin match quote_map x with
          |(Some x,v) -> Some (x,v)
          | (None,_) ->
            Locf.failf _loc  "class_parameters %s" @@ !dump_type_parameters t
        end
      | _ ->  Locf.failf _loc "class_parameters %s" @@ !dump_type_parameters t) 
    (list_of_com t [])


let type_parameters_and_type_name (t:ctyp)  =
  let rec aux (t:ctyp) acc = 
    match t with
    |`App(_,t1,t2) ->
      aux (t1:>ctyp) (optional_type_parameters (t2:>ctyp) @ acc)
    | #ident' as i  -> (ident i, acc)
    | x ->
      Locf.failf (unsafe_loc_of x) "type_parameters_and_type_name %s"
      @@ !dump_ctyp x  in
  aux t []




let rec deep_mkrangepat loc c1 c2 =
  if c1 = c2 then mkghpat loc (Ppat_constant (Const_char c1))
  else
    mkghpat loc
      (Ppat_or
         (mkghpat loc (Ppat_constant (Const_char c1)),
          deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2))

let rec mkrangepat loc c1 c2 =
  if c1 > c2 then mkrangepat loc c2 c1
  else if c1 = c2 then mkpat loc (Ppat_constant (Const_char c1))
  else
    mkpat loc
      (Ppat_or
         ((mkghpat loc (Ppat_constant (Const_char c1))),
          (deep_mkrangepat loc (Char.chr (Char.code c1 + 1)) c2)))




(*************************************)
(* [exp] and [pat] Transformation    *)      
(*************************************)
        
let  pat_literal _loc (x:literal) : Parsetree.pattern =
  mkpat _loc (Ppat_constant (mk_constant _loc x))


let exp_literal _loc (x:literal) : Parsetree.expression =
  mkexp _loc (Pexp_constant (mk_constant _loc x ))

let rec pat (x : pat) : Parsetree.pattern =
  let _loc = unsafe_loc_of x in 
  match x with
  | #literal as x -> pat_literal _loc x 
  | `Lid (_,("true"|"false" as txt)) ->
    mkpat _loc @@ Ppat_construct ({ txt = Lident txt; loc = _loc }, None, false) 
  | `Lid (_,s) ->
    mkpat _loc @@ Ppat_var (with_loc s _loc)
  | `Uid _ |`Dot _ as i ->
    mkpat _loc @@ Ppat_construct ((long_uident (i : vid  :>ident)), None, false)
  | `Vrn (_,s)  -> mkpat _loc @@ Ppat_variant (s, None)                    
  | `Alias (_,p1,`Lid(sloc,s)) ->
       mkpat _loc  @@ Ppat_alias (pat p1, with_loc s sloc)
  | `Any _  -> mkpat _loc Ppat_any
  | `App(_,p, r) as f->
      let r =
        match r with
        | `Par (_,r) -> 
            mkpat _loc @@ Ppat_tuple (List.map pat @@ list_of_com r [])
        | _ -> pat r in
      begin
        match (pat p).ppat_desc  with
        | Ppat_variant (s,None) ->
            mkpat _loc @@ Ppat_variant (s, Some r)
        | Ppat_construct(li,None,_) ->
            mkpat _loc @@ Ppat_construct (li, Some r, false)
        | _ -> Locf.failf _loc "invalid pattern %s" @@ !dump_pat f
      end 
  | `Array (_,p)  ->
    mkpat _loc @@ Ppat_array (List.map pat (list_of_sem p []))
  | `ArrayEmpty _ -> mkpat _loc @@ Ppat_array []
  | `Bar (_,p1,p2) -> mkpat _loc  @@ Ppat_or (pat p1, pat p2)
  | `PaRng (_,`Chr(loc1,c1),`Chr(loc2,c2)) ->
       mkrangepat _loc
        (Escape.char_of_char_token loc1 c1)
        (Escape.char_of_char_token loc2 c2)
  | `Record (_,p) ->
    let ps = list_of_sem p [] in
    let (wildcards,ps) =
      List.partition (function | `Any _ -> true | _ -> false) ps in
    let mklabpat (p : rec_pat) =
      match p with
      | `RecBind (_loc,i,p) -> (ident (i:>ident), pat p)
      | p -> error (unsafe_loc_of p) "invalid pattern" in
    mkpat _loc
      (Ppat_record
         (List.map mklabpat ps,
          if wildcards = [] then Closed else Open ))
  | `Par (_,`Com (_,p1,p2)) ->
    mkpat _loc
      (Ppat_tuple (List.map pat (list_of_com p1 (list_of_com p2 []))))
  | `Constraint (_loc,p,t) ->
      mkpat _loc (Ppat_constraint (pat p, ctyp t))
  | `ClassPath (_,i) -> mkpat _loc @@ Ppat_type (long_type_ident i)
  | `Lazy (_,p) -> mkpat _loc @@ Ppat_lazy (pat p)
  | `ModuleUnpack (_,`Uid (sloc,m)) ->
      mkpat _loc (Ppat_unpack (with_loc m sloc))
  | `ModuleConstraint (_,`Uid (sloc,m),ty) ->
      mkpat _loc
        (Ppat_constraint
           (mkpat sloc (Ppat_unpack (with_loc m sloc)), ctyp ty))
  | p -> Locf.failf _loc "invalid pattern %s" @@ !dump_pat p


(**
   The input is either {|$_.$_|} or {|$(id:{:ident| $_.$_|})|}
   the type of return value and [acc] is
   [(loc* string list * exp) list]

   The [string list] is generally a module path, the [exp] is the last field
   Examples:
   
 *)
let normalize_vid (x:vid) =
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
                                                   
let rec exp_desc _loc (x:exp) : Parsetree.expression_desc =
  match x with
  | #literal as x ->  Pexp_constant (mk_constant _loc x)
  | `Uid(_,s) -> Pexp_construct (lident_with_loc  s _loc, None, true)
  | `Lid(_, "__FILE__")  ->
      exp_desc _loc  %exp{$str'{Locf.file_name _loc}}
    (* TODO -- what's the case for lazy expansion
       and %exp{__FILE__} *)
  | `Lid(_,"__MODULE__") ->
      exp_desc _loc  %exp{$str'{String.capitalize @@ Filenamef.chop_extension_if @@ Locf.file_name _loc}}
  | %exp{ __PWD__ } ->
      exp_desc  _loc %exp{$str'{Filename.dirname (Locf.file_name _loc) }}
  | %exp{ __LOCATION__ } ->
      exp_desc _loc (Ast_gen.meta_here _loc _loc :> exp)
  | `Lid(_,("true"|"false" as s)) ->
        Pexp_construct (lident_with_loc s _loc,None, true)
  | #vid as x ->
      let (b,id) = normalize_vid x  in
      if b then Pexp_construct (id,None,false)
      else Pexp_ident id
    (* a.A.b  a.(A.b)
       a.A.b.c (a.(A.b)).c
       a.A.B.b  a.((A.B).b)
     *)
  | `Field(_,x,b) ->
      let (_,v ) =  normalize_vid b in
      Pexp_field ( exp x , v)
  | `App _ as f ->
    let (f, al) = view_app [] f in
    let al = List.map label_exp al in
    begin match (exp f).pexp_desc with
      | Pexp_construct (li, None, _) ->
        let al = List.map snd al in
        let a =
          match al with
          | [a] -> a
          | _ -> mkexp _loc (Pexp_tuple al)  in
        Pexp_construct (li, (Some a), false)
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
  | `Assert(_,`Lid(_,"false")) -> Pexp_assertfalse
  | `Assert(_,e) -> Pexp_assert (exp e)

   (* u.x <- b *)
  | `Assign(_, (`Field(loc,_,_) as e),v) ->
        begin match (exp e).pexp_desc with
          | Pexp_field (e, lab) -> Pexp_setfield (e, lab, (exp v))
          | _ -> error loc "bad record access"  end
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
      Pexp_constraint (exp e, None, (Some (ctyp t2)))
  | `Coercion (_, e, t1, t2) ->
      Pexp_constraint (exp e, Some (ctyp t1), Some (ctyp t2))

  | `For (_loc, `Lid(sloc,i), e1, e2, df, el) ->
      Pexp_for
         (with_loc i sloc, exp e1, exp e2, mkdirection df, exp (`Seq (_loc, el)))
  | `Fun(_loc,
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
    Pexp_function (lab, e1, [(p,exp e)])
  | `Fun(_,
         `CaseWhen(_,
                   (`LabelS _ | `Label _ | `OptLablS _  | `OptLablExpr _ | `OptLabl _ as l ),
                   w,e)) ->
    let (lab,p,e1) =
      match l with
      |`LabelS(_,(`Lid(_,lab) as l)) -> (lab,pat l,None)
      |`Label (_,`Lid(_,lab),po) -> (lab, pat po,None)
      |`OptLablS(_,(`Lid(_,lab) as l)) -> ("?"^lab, pat l,None)
      |`OptLabl(_,`Lid(_,lab),po) -> ("?"^paolab lab po, pat po,None)
      |`OptLablExpr(_,`Lid(_,lab),po,e1) ->
          ("?"^ paolab lab po, pat po, Some (exp e1))
      | _ -> assert false in
    Pexp_function
        (lab, e1, [( p, mkexp (unsafe_loc_of w) (Pexp_when (exp w, exp e)))])
  | `Fun (_,a) -> Pexp_function ("", None ,case a )

  | `IfThenElse (_, e1, e2, e3) -> Pexp_ifthenelse (exp e1,exp e2,Some (exp e3))
  | `IfThen (_,e1,e2) -> Pexp_ifthenelse (exp e1,exp e2, None)

  | `Any _ -> Locf.failf _loc "Any should not appear in the position of expression"
  | `Label _ | `LabelS _ -> error _loc "labeled expression not allowed here"
  | `Lazy (_loc,e) -> Pexp_lazy (exp e)
  | `LetIn (_,rf,bi,e) -> Pexp_let (mkrf rf,bind bi [], exp e)
  | `LetTryInWith(_,rf,bi,e,cas) ->
    let cas =
      let rec f (x:case)  : case=
        match x with
        | `Case (_loc,p,e)  ->
          `Case (_loc, p, `Fun (_loc, (`Case (_loc, `Uid (_loc, "()"), e))))
        | `CaseWhen (_loc,p,c,e) ->
          `CaseWhen
            (_loc, p, c,
              (`Fun (_loc, (`Case (_loc, (`Uid (_loc, "()")), e)))))
        | `Bar (_loc,a1,a2) -> `Bar (_loc, f a1, f a2)
        | `Ant (_loc,_) -> ant_error _loc in
      f cas in
    exp_desc _loc 
      (`App
         (_loc,
          (`Try
             (_loc,
              (`LetIn
                 (_loc, rf, bi,
                  (`Fun (_loc, (`Case (_loc, (`Uid (_loc, "()")), e)))))),
              cas)), (`Uid (_loc, "()"))) : Astf.exp )
  (* {:exp| *)
  (*  (try let $rec:rf $bi in fun () -> $e with | $cas  ) () |} *)

  | `LetModule (_,`Uid(sloc,i),me,e) -> Pexp_letmodule (with_loc i sloc,mexp me,exp e)
  | `Match (_,e,a) -> Pexp_match (exp e,case a )
  | `New (_,id) -> Pexp_new (long_type_ident id)
  | `ObjEnd _ -> Pexp_object{pcstr_pat= pat (`Any _loc); pcstr_fields=[]}
  | `Obj(_,cfl) ->
    let p = `Any _loc in 
    let cil = clfield cfl [] in
    Pexp_object { pcstr_pat = pat p; pcstr_fields = cil }
  | `ObjPatEnd(_,p) -> Pexp_object{pcstr_pat=pat p; pcstr_fields=[]}
  | `ObjPat (_,p,cfl) -> Pexp_object { pcstr_pat = pat p; pcstr_fields = clfield cfl [] }
  | `OvrInstEmpty _ -> Pexp_override []
  | `OvrInst (_,iel) ->
    let rec mkideexp (x:rec_exp) acc  = 
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
      | [] -> (_loc,exp_desc _loc  (`Uid (_loc, "()") : Astf.exp ))
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
  | `Par (_,e) ->
    let l = list_of_com e [] in
    begin match l with
      | []
      | [_] -> Locf.failf _loc "tuple should have at least two items" (!dump_exp x)
      | _ -> Pexp_tuple (List.map exp l)
    end
  | `Constraint (_,e,t) ->Pexp_constraint (exp e,Some (ctyp t), None)
  | `Vrn (_,s) -> Pexp_variant  (s, None)
  | `While (_, e1, el) ->
      Pexp_while (exp e1,exp (`Seq (_loc, el)))
  | `LetOpen(_,f, i,e) ->
      Pexp_open (flag _loc f, long_uident i,exp e)
  | `Package_exp (_,`Constraint(_,me,pt)) -> 
      Pexp_constraint
         (mkexp _loc (Pexp_pack (mexp me)),
          Some (mktyp _loc (Ptyp_package (package_type pt))), None)
  | `Package_exp(_,me) -> Pexp_pack (mexp me)
  | `LocalTypeFun (_,`Lid(_,i),e) -> Pexp_newtype (i, exp e)
  | x -> Locf.failf _loc "Panic: invalid exp:%s" @@ !dump_exp x 

and exp (x : exp) : Parsetree.expression =
  let _loc = unsafe_loc_of x in
  mkexp _loc (exp_desc _loc x)
and label_exp (x : exp) =
  match x with 
  | `Label (_,`Lid(_,lab),eo) -> (lab,  exp eo)
  | `LabelS(_,`Lid(sloc,lab)) -> (lab,exp (`Lid(sloc,lab)))
  | `OptLabl (_,`Lid(_,lab),eo) -> ("?" ^ lab, exp eo)
  | `OptLablS(loc,`Lid(_,lab)) -> ("?"^lab, exp (`Lid(loc,lab)))
  | e -> ("", exp e) 

and bind (x:bind) acc =
  match x with
  | (`And (_loc,x,y) : Astf.bind) -> bind x (bind y acc)
  | (`Bind
       (_loc,(`Lid (sloc,bind_name) : Astf.pat),`Constraint
          (_,e,`TyTypePol (_,vs,ty)))
     : Astf.bind) ->
    let rec id_to_string (x : ctyp) =
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
  | (`Bind (_loc,p,`Constraint (_,e,`TyPol (_,vs,ty))) : Astf.bind) ->
    ((pat (`Constraint (_loc, p, (`TyPol (_loc, vs, ty))) : Astf.pat )),
     (exp e))
    :: acc
  | (`Bind (_loc,p,e) : Astf.bind) -> ((pat p), (exp e)) :: acc
  | _ -> assert false
and case (x:case) = 
  let cases = list_of_bar x [] in
  Listf.filter_map 
    (fun (x:case) ->
       let _loc = unsafe_loc_of x in
       match x with 
       | `Case (_,p,e) -> Some (pat p, exp e)
       | `CaseWhen (_,p,w,e) ->
         Some
           (pat p, mkexp (unsafe_loc_of w) (Pexp_when (exp w, exp e)))
       | x -> Locf.failf _loc "case %s" @@ !dump_case x)
    cases

and mklabexp (x:rec_exp)  =
  let binds = list_of_sem x [] in
  Listf.filter_map
    (function
      | (`RecBind (_loc,i,e) : Astf.rec_exp) -> Some (ident (i : vid :>ident), exp e)
      | x -> Locf.failf (unsafe_loc_of x) "mklabexp : %s" @@ !dump_rec_exp x)
    binds

and mktype_decl (x:typedecl)  =
  let type_decl tl cl loc (x:type_info) =
    match x with
    |`TyMan(_,t1,p,t2) ->
      mktype loc tl cl
        ~type_kind:(type_kind t2) ~priv:(mkprivate p) ~manifest:(Some (ctyp t1))
    | `TyRepr (_,p1,repr) ->
      mktype loc tl cl
        ~type_kind:(type_kind repr)
        ~priv:(mkprivate p1) ~manifest:None
    | `TyEq (_loc,p1,t1) ->
      mktype loc tl cl ~type_kind:(Ptype_abstract) ~priv:(mkprivate p1)
        ~manifest:(Some (ctyp t1 ))
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
        (with_loc c sloc,
         type_decl
           (mk_type_parameters tl)
           cl cloc td)
      | `TyAbstr(cloc,`Lid(sloc,c),tl,cl) ->
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
        (with_loc c sloc,
         mktype cloc
           (mk_type_parameters tl) cl
           ~type_kind:Ptype_abstract ~priv:Private ~manifest:None)

      | (t:typedecl) ->
        Locf.failf (unsafe_loc_of t) "mktype_decl %s" (!dump_typedecl t)) tys
and mtyp : Astf.mtyp -> Parsetree.module_type =
  let  mkwithc (wc:constr)  =
    let mkwithtyp (pwith_type: Parsetree.type_declaration -> Parsetree.with_constraint)
        loc priv id_tpl ct =
      let (id, tpl) = type_parameters_and_type_name id_tpl in
      let (params, variance) = List.split tpl in
      (id, pwith_type
         {ptype_params = params; ptype_cstrs = [];
          ptype_kind =  Ptype_abstract;
          ptype_private = priv;
          ptype_manifest = Some (ctyp ct);
          ptype_loc =  loc; ptype_variance = variance}) in
    let constrs = list_of_and wc [] in
    Listf.filter_map (function
        |`TypeEq(_loc,id_tpl,ct) ->
          Some (mkwithtyp (fun x -> Pwith_type x) _loc Public id_tpl ct)
        |`TypeEqPriv(_loc,id_tpl,ct) ->
          Some (mkwithtyp (fun x -> Pwith_type x) _loc Private id_tpl ct)
        | `ModuleEq(_loc,i1,i2) ->
          Some (long_uident i1, Pwith_module (long_uident i2))
        | `TypeSubst(_loc,id_tpl,ct) ->
          Some (mkwithtyp (fun x -> Pwith_typesubst x) _loc Public id_tpl ct )
        | `ModuleSubst(_loc,i1,i2) ->
          Some (long_uident i1, Pwith_modsubst (long_uident i2))
        | t -> Locf.failf (unsafe_loc_of t) "bad with constraint (antiquotation) : %s" (!dump_constr t)) constrs in
  function 
  | #ident' as i  -> let loc = unsafe_loc_of i in mkmty loc (Pmty_ident (long_uident i))
  | `Functor(loc,`Uid(sloc,n),nt,mt) ->
    mkmty loc (Pmty_functor (with_loc n sloc,mtyp nt,mtyp mt))
  | `Sig(loc,sl) ->
    mkmty loc (Pmty_signature (sigi sl []))
  | `SigEnd(loc) -> mkmty loc (Pmty_signature [])
  | `With(loc,mt,wc) -> mkmty loc (Pmty_with (mtyp mt,mkwithc wc ))
  | `ModuleTypeOf(_loc,me) ->
    mkmty _loc (Pmty_typeof (mexp me))
  | t -> Locf.failf (unsafe_loc_of t) "mtyp: %s" (!dump_mtyp t) 
and sigi (s:sigi) (l:Parsetree.signature) : Parsetree.signature =
  match s with 
  | `Class (loc,cd) ->
    mksig loc (Psig_class
                 (List.map class_info_cltyp (list_of_and cd []))) :: l
  | `ClassType (loc,ctd) ->
    mksig loc (Psig_class_type
                 (List.map class_info_cltyp (list_of_and ctd []))) :: l
  | `Sem(_,sg1,sg2) -> sigi sg1 (sigi sg2 l)
  | `Directive _ | `DirectiveSimple _  -> l

  | `Exception(_loc,`Uid(_,s)) ->
    mksig _loc (Psig_exception (with_loc s _loc, [])) :: l
  | `Exception(_loc,`Of(_,`Uid(sloc,s),t)) ->
    mksig _loc
      (Psig_exception
         (with_loc s sloc,
          List.map ctyp (list_of_star t []))) :: l
  | `Exception (_,_) -> assert false (*FIXME*)
  | `External (loc, `Lid(sloc,n), t, sl) ->
    mksig loc
      (Psig_value
         (with_loc n sloc,
          mkvalue_desc loc t (list_of_app sl []))) :: l
  | `Include (loc,mt) -> mksig loc (Psig_include (mtyp mt)) :: l
  | `Module (loc,`Uid(sloc,n),mt) ->
    mksig loc (Psig_module (with_loc n sloc,mtyp mt)) :: l
  | `RecModule (loc,mb) ->
    mksig loc (Psig_recmodule (module_sig_bind mb [])) :: l
  | `ModuleTypeEnd(loc,`Uid(sloc,n)) ->
    mksig loc (Psig_modtype (with_loc n sloc , Pmodtype_abstract) ) :: l
  | `ModuleType (loc,`Uid(sloc,n),mt) ->
    mksig loc (Psig_modtype (with_loc n sloc,Pmodtype_manifest (mtyp mt))) :: l
  | `Open (loc,g,id) ->
    mksig loc (Psig_open (flag loc g, long_uident id)) :: l
  | `Type (loc,tdl) -> mksig loc (Psig_type (mktype_decl tdl )) :: l
  | `Val (loc,`Lid(sloc,n),t) ->
    mksig loc (Psig_value (with_loc n sloc,mkvalue_desc loc t [])) :: l
  | t -> Locf.failf (unsafe_loc_of t) "sigi: %s" (!dump_sigi t)
and module_sig_bind (x:mbind) 
    (acc: (string Asttypes.loc * Parsetree.module_type) list )  =
  match x with 
  | `And(_,x,y) -> module_sig_bind x (module_sig_bind y acc)
  | `Constraint(_loc,`Uid(sloc,s),mt) ->
    (with_loc s sloc, mtyp mt) :: acc
  | t -> Locf.failf (unsafe_loc_of t) "module_sig_bind: %s" (!dump_mbind t) 
and module_str_bind (x:Astf.mbind) acc =
  match x with 
  | `And(_,x,y) -> module_str_bind x (module_str_bind y acc)
  | `ModuleBind(_loc,`Uid(sloc,s),mt,me)->
    (with_loc s sloc, mtyp mt, mexp me) :: acc
  | t -> Locf.failf (unsafe_loc_of t) "module_str_bind: %s" (!dump_mbind t)
and mexp (x:Astf.mexp)=
  match x with 
  | #vid'  as i ->
    let loc = unsafe_loc_of i in  mkmod loc (Pmod_ident (long_uident (i:vid':>ident)))
  | `App(loc,me1,me2) ->
    mkmod loc (Pmod_apply (mexp me1,mexp me2))
  | `Functor(loc,`Uid(sloc,n),mt,me) ->
    mkmod loc (Pmod_functor (with_loc n sloc,mtyp mt,mexp me))
  | `Struct(loc,sl) -> mkmod loc (Pmod_structure (stru sl []))
  | `StructEnd(loc) -> mkmod loc (Pmod_structure [])
  | `Constraint(loc,me,mt) ->
    mkmod loc (Pmod_constraint (mexp me,mtyp mt))
  | `PackageModule(loc,`Constraint(_,e,`Package(_,pt))) ->
    mkmod loc
      (Pmod_unpack (
          mkexp loc
            (Pexp_constraint
               (exp e, Some (mktyp loc (Ptyp_package (package_type pt))), None))))
  | `PackageModule(loc,e) -> mkmod loc (Pmod_unpack (exp e))
  | t -> Locf.failf (unsafe_loc_of t) "mexp: %s" (!dump_mexp t) 
and stru (s:stru) (l:Parsetree.structure) : Parsetree.structure =
  let loc = unsafe_loc_of s in
  match s with
  (* ad-hoc removing the empty statement, a more elegant way is in need*)
  | (`StExp (_, (`Uid (_, "()")))) -> l 
  | `Sem(_,st1,st2) ->  stru st1 (stru st2 l)        
  | (`Class (loc,cd) :stru) ->
    mkstr loc (Pstr_class
                 (List.map class_info_clexp (list_of_and cd []))) :: l
  | `ClassType (_,ctd) ->
    mkstr loc (Pstr_class_type
                 (List.map class_info_cltyp (list_of_and ctd []))) :: l

  | `Directive _ | `DirectiveSimple _  -> l
  | `Exception(_,`Uid(_,s)) ->
    mkstr loc (Pstr_exception (with_loc s loc, [])) :: l 
  | `Exception (_, `Of (_,  `Uid (_, s), t)) ->
    mkstr loc
      (Pstr_exception
         (with_loc s loc,
          List.map ctyp (list_of_star t []))) :: l 
  (* TODO *)     
  (* | {@loc| exception $uid:s = $i |} -> *)
  (*     [mkstr loc (Pstr_exn_rebind (with_loc s loc) (ident i)) :: l ] *)
  (* | {@loc| exception $uid:_ of $_ = $_ |} -> *)
  (*     error loc "type in exception alias" *)
  | `Exception (_,_) -> assert false (*FIXME*)
  | `StExp (_,e) -> mkstr loc (Pstr_eval (exp e)) :: l
  | `External(_,`Lid(sloc,n),t,sl) ->
    mkstr loc
      (Pstr_primitive
         (with_loc n sloc,
          mkvalue_desc loc t (list_of_app sl [] ))) :: l
  | `Include (_,me) -> mkstr loc (Pstr_include (mexp me)) :: l
  | `Module (_,`Uid(sloc,n),me) ->
    mkstr loc (Pstr_module (with_loc n sloc,mexp me)) :: l
  | `RecModule (_,mb) ->
    mkstr loc (Pstr_recmodule (module_str_bind mb [])) :: l
  | `ModuleType (_,`Uid(sloc,n),mt) ->
    mkstr loc (Pstr_modtype (with_loc n sloc,mtyp mt)) :: l
  | `Open (_,g,id) ->
    mkstr loc (Pstr_open (flag loc g ,long_uident id)) :: l
  | `Type (_,tdl) -> mkstr loc (Pstr_type (mktype_decl tdl )) :: l
  | `TypeWith(_loc,tdl, ns) ->
    (* FIXME all traversal needs to deal with TypeWith later .. *)
    stru (!generate_type_code _loc tdl ns) l
  (* stru (`Sem(_loc,x,code)) l *)

  | `Value (_,rf,bi) ->
    mkstr loc (Pstr_value (mkrf rf,bind bi [])) :: l
  | x-> Locf.failf loc "stru : %s" (!dump_stru x) 
and cltyp (x:Astf.cltyp) =
  match x with
  | `ClApply(loc, id, tl) -> 
    mkcty loc
      (Pcty_constr
         (long_class_ident (id:>ident),
          List.map (function
              | `Ctyp (_loc,x) -> ctyp x
              | _ -> assert false) (list_of_com tl [])))
  | #vid' as id ->
    let loc = unsafe_loc_of id in
    mkcty loc (Pcty_constr (long_class_ident (id:vid' :> ident), []))
  | `CtFun (loc, (`Label (_, `Lid(_,lab), t)), ct) ->
    mkcty loc (Pcty_fun (lab, ctyp t, cltyp ct))

  | `CtFun (loc, (`OptLabl (loc1, `Lid(_,lab), t)), ct) ->
    let t = `App (loc1, (predef_option loc1), t) in
    mkcty loc (Pcty_fun ("?" ^ lab, ctyp t, cltyp ct))

  | `CtFun (loc,t,ct) -> mkcty loc (Pcty_fun ("" ,ctyp t, cltyp ct))

  | `ObjEnd(loc) ->
    mkcty loc (Pcty_signature {pcsig_self=ctyp (`Any loc);pcsig_fields=[];pcsig_loc=loc})
  | `ObjTyEnd(loc,t) ->
    mkcty loc (Pcty_signature {pcsig_self= ctyp t; pcsig_fields = []; pcsig_loc = loc;})
  | `Obj(loc,ctfl) ->
    let cli = clsigi ctfl [] in
    mkcty loc (Pcty_signature {pcsig_self = ctyp(`Any loc);pcsig_fields=cli;pcsig_loc=loc})
  | `ObjTy (loc,t,ctfl) ->
    let cil = clsigi ctfl [] in
    mkcty loc (Pcty_signature {pcsig_self = ctyp t; pcsig_fields = cil; pcsig_loc =  loc;})
  |  x -> Locf.failf (unsafe_loc_of x) "class type: %s" (!dump_cltyp x) 

and class_info_clexp (ci:cldecl) =
  match ci with 
  | (`ClDecl(loc,vir,`Lid(nloc,name),params,ce) : cldecl) -> 
    let (loc_params, (params, variance)) =
      (unsafe_loc_of params, List.split (class_parameters params)) in
    {pci_virt = mkvirtual vir;
     pci_params = (params,  loc_params);
     pci_name = with_loc name nloc;
     pci_expr = clexp ce;
     pci_loc =  loc;
     pci_variance = variance}
  | `ClDeclS(loc,vir,`Lid(nloc,name),ce) -> 
    {pci_virt = mkvirtual vir;
     pci_params = ([],  loc);
     pci_name = with_loc name nloc;
     pci_expr = clexp ce;
     pci_loc =  loc;
     pci_variance = []}
  | ce -> Locf.failf  (unsafe_loc_of ce) "class_info_clexp: %s" (!dump_cldecl ce) 
and class_info_cltyp (ci:cltdecl)  =
  match ci with 
  | (`CtDecl(loc, vir,`Lid(nloc,name),params,ct) :cltdecl) ->
    let (loc_params, (params, variance)) =
      (unsafe_loc_of params, List.split (class_parameters params)) in
    {pci_virt = mkvirtual vir;
     pci_params = (params,  loc_params);
     pci_name = with_loc name nloc;
     pci_expr = cltyp ct;
     pci_loc =  loc;
     pci_variance = variance}
  | (`CtDeclS (loc,vir,`Lid(nloc,name),ct) : cltdecl) -> 
    {pci_virt = mkvirtual vir;
     pci_params = ([],  loc);
     pci_name = with_loc name nloc;
     pci_expr = cltyp ct;
     pci_loc =  loc;
     pci_variance = []}
  | ct -> Locf.failf (unsafe_loc_of ct) "bad class/class type declaration/definition %s " (!dump_cltdecl ct)
and clsigi (c:clsigi) (l:  Parsetree.class_type_field list) : Parsetree.class_type_field list =
  match c with 
  |`Eq (loc, t1, t2) ->
    mkctf loc (Pctf_cstr (ctyp t1, ctyp t2)) :: l
  | `Sem(_,csg1,csg2) -> clsigi csg1 (clsigi csg2 l)
  | `SigInherit (loc,ct) -> mkctf loc (Pctf_inher (cltyp ct)) :: l
  | `Method (loc,`Lid(_,s),pf,t) ->
    mkctf loc (Pctf_meth (s, mkprivate pf, mkpolytype (ctyp t))) :: l
  | `CgVal (loc, `Lid(_,s), b, v, t) ->
    mkctf loc (Pctf_val (s, mkmutable b, mkvirtual v, ctyp t)) :: l
  | `VirMeth (loc,`Lid(_,s),b,t) ->
    mkctf loc (Pctf_virt (s, mkprivate b, mkpolytype (ctyp t))) :: l
  | t -> Locf.failf (unsafe_loc_of t) "clsigi :%s" (!dump_clsigi t) 

and clexp  (x:Astf.clexp) :  Parsetree.class_expr =
  let loc = unsafe_loc_of x in
  match x with 
  | `CeApp _ ->
    let rec view_app acc (x:clexp)  =
      match x  with 
      | `CeApp (_loc,ce,(a:exp)) -> view_app (a :: acc) ce
      | ce -> (ce, acc) in
    let (ce, el) = view_app [] x in
    let el = List.map label_exp el in
    mkcl loc (Pcl_apply (clexp ce, el))

  | `ClApply (_,id,tl) ->
    mkcl loc
      (Pcl_constr (long_class_ident (id:>ident),
                   (List.map (function |`Ctyp (_loc,x) -> ctyp x | _ -> assert false)
                       (list_of_com tl []))))
  | #vid' as id  ->
    mkcl loc @@ Pcl_constr (long_class_ident (id : vid' :>ident), [])
  | `CeFun (_, (`Label (_,`Lid(_,lab), po)), ce) ->
    mkcl loc @@ Pcl_fun (lab, None, pat po , clexp ce)
  | `CeFun(_,`OptLablExpr(_,`Lid(_loc,lab),p,e),ce) ->
    let lab = paolab lab p in
    mkcl loc (Pcl_fun ("?" ^ lab,Some (exp e), pat p, clexp ce))
  | `CeFun (_,`OptLabl(_,`Lid(_loc,lab),p), ce) -> 
    let lab = paolab lab p in
    mkcl loc (Pcl_fun ("?" ^ lab, None, pat p, clexp ce))
  | `CeFun (_,p,ce) ->
    mkcl loc (Pcl_fun ("", None, pat p, clexp ce))
  | `LetIn (_, rf, bi, ce) ->
    mkcl loc (Pcl_let (mkrf rf, bind bi [], clexp ce))

  | `ObjEnd _ ->
    mkcl loc (Pcl_structure{pcstr_pat= pat (`Any loc); pcstr_fields=[]})
  | `Obj(_,cfl) ->
    let p = `Any loc in
    let cil = clfield cfl [] in
    mkcl loc (Pcl_structure {pcstr_pat = pat p; pcstr_fields = cil;})
  | `ObjPatEnd(_,p) ->
    mkcl loc (Pcl_structure {pcstr_pat= pat p; pcstr_fields = []})
  | `ObjPat (_,p,cfl) ->
    let cil = clfield cfl [] in
    mkcl loc (Pcl_structure {pcstr_pat = pat p; pcstr_fields = cil;})
  | `Constraint (_,ce,ct) ->
    mkcl loc (Pcl_constraint (clexp ce,cltyp ct))
  | t -> Locf.failf (unsafe_loc_of t) "clexp: %s" (!dump_clexp t)

and clfield (c:clfield) l =
  let loc = unsafe_loc_of c in 
  match c with
  | `Eq (_, t1, t2) -> mkcf loc (Pcf_constr (ctyp t1, ctyp t2)) :: l
  | `Sem(_,cst1,cst2) -> clfield cst1 (clfield cst2 l)
  | `Inherit (_, ov, ce) ->
    mkcf loc (Pcf_inher (flag loc ov,clexp ce, None)) :: l
  | `InheritAs(_,ov,ce,`Lid(_,x)) ->
    mkcf loc (Pcf_inher (flag loc ov,clexp ce,Some x)) :: l
  | `Initializer (_,e) -> mkcf loc (Pcf_init (exp e)) :: l
  | `CrMthS(loc,`Lid(sloc,s),ov,pf,e) ->
    let e = mkexp loc (Pexp_poly (exp e,None)) in
    mkcf loc (Pcf_meth (with_loc s sloc, mkprivate pf, flag loc ov, e)) :: l
  | `CrMth (_, `Lid(sloc,s), ov, pf, e, t) ->
    let t = Some (mkpolytype (ctyp t)) in
    let e = mkexp loc (Pexp_poly (exp e, t)) in
    mkcf loc (Pcf_meth (with_loc s sloc, mkprivate pf, flag loc ov, e)) :: l
  | `CrVal (_, `Lid(sloc,s), ov, mf, e) ->
    mkcf loc (Pcf_val (with_loc s sloc, mkmutable mf, flag loc ov, exp e)) :: l
  | `VirMeth (_,`Lid(sloc,s),pf,t) ->
    mkcf loc (Pcf_virt (with_loc s sloc, mkprivate pf, mkpolytype (ctyp t))) :: l
  | `VirVal (_,`Lid(sloc,s),mf,t) ->
    mkcf loc (Pcf_valvirt (with_loc s sloc, mkmutable mf, ctyp t)) :: l
  | x  -> Locf.failf  loc "clfield: %s" @@ !dump_clfield x

let sigi (ast:sigi) : Parsetree.signature = sigi ast []
let stru ast = stru ast []

let directive (x:exp) : Parsetree.directive_argument =
  match x with 
  |`Str(_,s) -> Pdir_string s
  |`Int(_,i) -> Pdir_int (int_of_string i)
  |`Lid(_loc,("true"|"false" as x)) ->
    if x ="true" then Pdir_bool true
    else Pdir_bool false
  | e ->
    let ident_of_exp : Astf.exp -> Astf.ident =
      let error () = invalid_arg "ident_of_exp: this expession is not an identifier" in
      let rec self (x:Astf.exp) : ident =
        let _loc = unsafe_loc_of x in
        match x with 
        | `App(_,e1,e2) -> `Apply(_loc,self e1, self e2)
        | `Field(_,e1,e2) -> `Dot(_loc,self e1, (e2 :>ident))
        | `Lid _  -> error ()
        | `Uid _ | `Dot _ as i -> (i:vid:>ident)
        | _ -> error ()  in 
      function
      | #vid as i ->  (i:vid :>ident)
      | `App _ -> error ()
      | t -> self t  in
    Pdir_ident (ident_noloc (ident_of_exp e)) 

let phrase (x: stru) : Parsetree.toplevel_phrase =
  match x with 
  | `Directive (_, `Lid(_,d),dp) -> Ptop_dir (d ,directive dp)
  | `DirectiveSimple(_,`Lid(_,d)) -> Ptop_dir (d, Pdir_none)
  | `Directive (_, `Ant(_loc,_),_) -> error _loc "antiquotation not allowed"
  | si -> Ptop_def (stru si) 


open Format
let pp = fprintf

let print_exp f  e =
  pp f "@[%a@]@." AstPrint.expression (exp e)

let to_string_exp = Formatf.to_string  print_exp


let print_pat f e =
  pp f "@[%a@]@." AstPrint.pattern (pat e)

let print_stru f e =
  pp f "@[%a@]@." AstPrint.structure (stru e)

let print_ctyp f e =
  pp f "@[%a@]@." AstPrint.core_type (ctyp e)





(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ast2pt.cmo" *)
(* end: *)
