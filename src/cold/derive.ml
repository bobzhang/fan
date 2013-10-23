open Format
open Util
open Astn_util
open FAstN
open Fid
open Ctyp
open Sigs_util
let check_valid str =
  let len = String.length str in
  if
    not
      ((len > 1) &&
         ((not @@ (Fchar.is_digit (str.[1]))) &&
            (not @@ (Fstring.starts_with str "all_"))))
  then
    (eprintf "%s is not a valid name" str;
     eprintf
       "For valid name its length should be more than 1\ncan not be a-[digit], can not start with [all_]";
     exit 2)
let preserve = ["self"; "self_type"; "unit"; "result"]
let check names =
  List.iter
    (fun name  ->
       if List.mem name preserve
       then
         (eprintf "%s is not a valid name\n" name;
          eprintf "preserved keywords:\n";
          List.iter (fun s  -> eprintf "%s\n" s) preserve;
          exit 2)
       else check_valid name) names
let mapi_exp ?(arity= 1)  ?(names= [])  ~f:(f : ctyp -> exp)  (i : int)
  (ty : ctyp) =
  (let name_exp = f ty in
   let base = name_exp +> names in
   let id_eps = (Listf.init arity) @@ (fun index  -> xid ~off:index i) in
   let ep0 = List.hd id_eps in
   let id_ep = tuple_com id_eps in
   let exp = appl_of_list (base :: (id_eps :>exp list)) in
   { name_exp; info_exp = exp; id_ep; id_eps; ep0; ty } : ty_info )
let tuple_exp_of_ctyp ?(arity= 1)  ?(names= [])  ~mk_tuple  ~f  (ty : ctyp) =
  (match ty with
   | `Par t ->
       let ls = Ast_basic.N.list_of_star t [] in
       let len = List.length ls in
       let pat = (EpN.mk_tuple ~arity ~number:len :>pat) in
       let tys = mk_tuple (List.mapi (mapi_exp ~arity ~names ~f) ls) in
       ExpN.mkfun names
         (ExpN.currying [(`Case (pat, tys) : FAstN.case )] ~arity)
   | _ -> failwithf "tuple_exp_of_ctyp %s" (ObjsN.dump_ctyp ty) : exp )
let rec normal_simple_exp_of_ctyp ?arity  ?names  ~mk_tuple  ~right_type_id 
  ~left_type_id  ~right_type_variable  cxt (ty : ctyp) =
  let right_trans = transform right_type_id in
  let left_trans = basic_transform left_type_id in
  let tyvar = right_transform right_type_variable in
  let rec aux =
    function
    | `Lid id ->
        if Hashset.mem cxt id
        then lid (left_trans id)
        else right_trans (`Lid id)
    | #ident' as id -> right_trans (IdN.to_vid id)
    | `App (t1,t2) -> (`App ((aux t1), (aux t2)) : FAstN.exp )
    | `Quote (_,`Lid s) -> tyvar s
    | `Arrow (t1,t2) ->
        aux (`App ((`App ((`Lid "arrow"), t1)), t2) : FAstN.ctyp )
    | `Par _ as ty ->
        tuple_exp_of_ctyp ?arity ?names ~mk_tuple
          ~f:(normal_simple_exp_of_ctyp ?arity ?names ~mk_tuple
                ~right_type_id ~left_type_id ~right_type_variable cxt) ty
    | (ty : ctyp) ->
        failwithf "normal_simple_exp_of_ctyp : %s" (ObjsN.dump_ctyp ty) in
  aux ty
let rec obj_simple_exp_of_ctyp ~right_type_id  ~left_type_variable 
  ~right_type_variable  ?names  ?arity  ~mk_tuple  ty =
  let trans = transform right_type_id in
  let var = basic_transform left_type_variable in
  let tyvar = right_transform right_type_variable in
  let rec aux: ctyp -> exp =
    function
    | #ident' as id -> trans (IdN.to_vid id)
    | `Quote (_,`Lid s) -> tyvar s
    | `App _ as ty ->
        (match Ast_basic.N.list_of_app ty [] with
         | (#ident' as tctor)::ls ->
             appl_of_list ((trans (IdN.to_vid tctor)) ::
               (ls |>
                  (List.map
                     (function
                      | `Quote (_,`Lid s) -> (`Lid (var s) : FAstN.exp )
                      | t ->
                          (`Fun (`Case ((`Lid "self"), (aux t))) : FAstN.exp )))))
         | _ ->
             failwithf "list_of_app in obj_simple_exp_of_ctyp: %s"
               (ObjsN.dump_ctyp ty))
    | `Arrow (t1,t2) ->
        aux (`App ((`App ((`Lid "arrow"), t1)), t2) : FAstN.ctyp )
    | `Par _ as ty ->
        tuple_exp_of_ctyp ?arity ?names ~mk_tuple
          ~f:(obj_simple_exp_of_ctyp ~right_type_id ~left_type_variable
                ~right_type_variable ?names ?arity ~mk_tuple) ty
    | ty -> failwithf "obj_simple_exp_of_ctyp: %s" (ObjsN.dump_ctyp ty) in
  aux ty
let exp_of_ctyp ?cons_transform  ?(arity= 1)  ?(names= [])  ~default 
  ~mk_variant  simple_exp_of_ctyp (ty : or_ctyp) =
  let f (cons : string) (tyargs : ctyp list) =
    (let args_length = List.length tyargs in
     let p: pat =
       (EpN.gen_tuple_n ?cons_transform ~arity cons args_length :>pat) in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
       mk_variant cons exps in
     let e = mk (cons, tyargs) in (`Case (p, e) : FAstN.case ) : case ) in
  let info = (Sum, (List.length (Ast_basic.N.list_of_bar ty []))) in
  let res: case list = Ctyp.reduce_data_ctors ty [] f ~compose:cons in
  let res =
    let t =
      if ((List.length res) >= 2) && (arity >= 2)
      then match default info with | Some x -> x :: res | None  -> res
      else res in
    List.rev t in
  ExpN.currying ~arity res
let exp_of_variant ?cons_transform  ?(arity= 1)  ?(names= [])  ~default 
  ~mk_variant  ~destination  simple_exp_of_ctyp ~result  ty =
  let f (cons,tyargs) =
    (let len = List.length tyargs in
     let p = (EpN.gen_tuple_n ?cons_transform ~arity cons len :>pat) in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
       mk_variant cons exps in
     let e = mk (cons, tyargs) in (`Case (p, e) : FAstN.case ) : case ) in
  let simple (lid : ident) =
    (let e = (simple_exp_of_ctyp (lid :>ctyp)) +> names in
     let (f,a) = Ast_basic.N.view_app [] result in
     let annot = appl_of_list (f :: (List.map (fun _  -> `Any) a)) in
     gen_tuple_abbrev ~arity ~annot ~destination lid e : case ) in
  let info = (TyVrnEq, (List.length (Ast_basic.N.list_of_bar ty []))) in
  let ls = Ctyp.view_variant ty in
  let res =
    let res =
      List.fold_left
        (fun acc  x  ->
           match x with
           | `variant (cons,args) -> (f (("`" ^ cons), args)) :: acc
           | `abbrev lid -> (simple lid) :: acc) [] ls in
    let t =
      if ((List.length res) >= 2) && (arity >= 2)
      then match default info with | Some x -> x :: res | None  -> res
      else res in
    List.rev t in
  ExpN.currying ~arity res
let mk_prefix (vars : opt_decl_params) (acc : exp) ?(names= []) 
  ~left_type_variable  =
  let varf = basic_transform left_type_variable in
  let f (var : decl_params) acc =
    match var with
    | `Quote (_,`Lid s) -> (`Fun (`Case ((`Lid (varf s)), acc)) : FAstN.exp )
    | t -> failwithf "mk_prefix: %s" (ObjsN.dump_decl_params t) in
  match vars with
  | `None -> ExpN.mkfun names acc
  | `Some xs ->
      let vars = Ast_basic.N.list_of_com xs [] in
      List.fold_right f vars (ExpN.mkfun names acc)
let fun_of_tydcl ?(names= [])  ?(arity= 1)  ~left_type_variable  ~mk_record 
  ~result  simple_exp_of_ctyp exp_of_ctyp exp_of_variant tydcl =
  (match (tydcl : typedecl ) with
   | `TyDcl (_,tyvars,ctyp,_constraints) ->
       (match ctyp with
        | `TyMan (_,_,repr)|`TyRepr (_,repr) ->
            (match repr with
             | `Record t ->
                 let cols = Ctyp.list_of_record t in
                 let pat = (EpN.mk_record ~arity cols :>pat) in
                 let info =
                   List.mapi
                     (fun i  x  ->
                        match (x : Ctyp.col ) with
                        | { label; is_mutable; ty } ->
                            {
                              info =
                                (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp
                                   i ty);
                              label;
                              is_mutable
                            }) cols in
                 mk_prefix ~names ~left_type_variable tyvars
                   (ExpN.currying ~arity
                      [(`Case (pat, (mk_record info)) : FAstN.case )])
             | `Sum ctyp ->
                 let funct = exp_of_ctyp ctyp in
                 mk_prefix ~names ~left_type_variable tyvars funct
             | t ->
                 failwithf "fun_of_tydcl outer %s" (ObjsN.dump_type_repr t))
        | `TyEq (_,ctyp) ->
            (match ctyp with
             | #ident'|`Par _|`Quote _|`Arrow _|`App _ as x ->
                 let exp = simple_exp_of_ctyp x in
                 let funct = ExpN.eta_expand (exp +> names) arity in
                 mk_prefix ~names ~left_type_variable tyvars funct
             | `PolyEq t|`PolySup t|`PolyInf t|`PolyInfSup (t,_) ->
                 let case = exp_of_variant ~result t in
                 mk_prefix ~names ~left_type_variable tyvars case
             | t -> failwithf "fun_of_tydcl inner %s" (ObjsN.dump_ctyp t))
        | t -> failwithf "fun_of_tydcl middle %s" (ObjsN.dump_type_info t))
   | t -> failwithf "fun_of_tydcl outer %s" (ObjsN.dump_typedecl t) : 
  exp )
let bind_of_tydcl ?cons_transform  simple_exp_of_ctyp ?(arity= 1)  ?(names=
  [])  ?(destination= Str_item)  ?annot  ~default  ~mk_variant 
  ~left_type_id:(left_type_id : basic_id_transform) 
  ~left_type_variable:(left_type_variable : basic_id_transform)  ~mk_record 
  tydcl =
  let tctor_var = basic_transform left_type_id in
  let (name,len) = Ctyp.name_length_of_tydcl tydcl in
  let fname = tctor_var name in
  let (_ty,result) =
    Ctyp.mk_method_type_of_name ~number:arity ~prefix:names (name, len)
      destination in
  let (annot,result) =
    match annot with
    | None  -> (None, result)
    | Some f -> let (a,b) = f name in ((Some a), b) in
  let fun_exp =
    if not (Ctyp.is_abstract tydcl)
    then
      fun_of_tydcl ~names ~arity ~left_type_variable ~mk_record ~result
        simple_exp_of_ctyp
        (exp_of_ctyp ?cons_transform ~arity ~names ~default ~mk_variant
           simple_exp_of_ctyp)
        (exp_of_variant ?cons_transform ~arity ~names ~default ~mk_variant
           ~destination simple_exp_of_ctyp) tydcl
    else
      (eprintf "Warning: %s as a abstract type no structure generated\n"
         (ObjsN.dump_typedecl tydcl);
       (`App ((`Lid "failwith"), (`Str "Abstract data type not implemented")) : 
       FAstN.exp )) in
  match annot with
  | None  -> (`Bind ((`Lid fname), fun_exp) : FAstN.bind )
  | Some x ->
      (`Bind ((`Lid fname), (`Constraint (fun_exp, x))) : FAstN.bind )
let stru_of_mtyps ?module_name  ?cons_transform  ?annot  ?arity  ?names 
  ~default  ~mk_variant  ~left_type_id  ~left_type_variable  ~mk_record 
  simple_exp_of_ctyp_with_cxt (lst : mtyps) =
  (let cxt = Hashset.create 50 in
   let mk_bind: typedecl -> bind =
     bind_of_tydcl ?cons_transform ?arity ?annot ?names ~default ~mk_variant
       ~left_type_id ~left_type_variable ~mk_record
       (simple_exp_of_ctyp_with_cxt cxt) in
   let fs (ty : types) =
     (match ty with
      | `Mutual named_types ->
          (match named_types with
           | [] -> (`StExp (`Uid "()") : FAstN.stru )
           | xs ->
               (List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs;
                (let bind =
                   Listf.reduce_right_with
                     ~compose:(fun x  y  -> (`And (x, y) : FAstN.bind ))
                     ~f:(fun (_name,ty)  -> mk_bind ty) xs in
                 (`Value (`Positive, bind) : FAstN.stru ))))
      | `Single (name,tydcl) ->
          (Hashset.add cxt name;
           (let flag =
              if Ctyp.is_recursive tydcl then `Positive else `Negative
            and bind = mk_bind tydcl in (`Value (flag, bind) : FAstN.stru ))) : 
     stru ) in
   let item =
     match lst with
     | [] -> (`StExp (`Uid "()") : FAstN.stru )
     | _ -> sem_of_list (List.map fs lst) in
   match module_name with
   | None  -> item
   | Some m -> (`Module ((`Uid m), (`Struct item)) : FAstN.stru ) : stru )
let obj_of_mtyps ?cons_transform  ?module_name  ?(arity= 1)  ?(names= []) 
  ~default  ~left_type_variable:(left_type_variable : basic_id_transform) 
  ~mk_record  ~mk_variant  base class_name simple_exp_of_ctyp
  ~kind:(k : kind)  (lst : mtyps) =
  (let tbl = Hashtbl.create 50 in
   let f tydcl result =
     fun_of_tydcl ~names ~arity ~left_type_variable ~mk_record
       simple_exp_of_ctyp
       (exp_of_ctyp ?cons_transform ~arity ~names ~default ~mk_variant
          simple_exp_of_ctyp)
       (exp_of_variant ?cons_transform ~destination:(Obj k) ~arity ~names
          ~default ~mk_variant simple_exp_of_ctyp) ~result tydcl in
   let mk_type tydcl =
     let (name,len) = Ctyp.name_length_of_tydcl tydcl in
     let (ty,result_type) =
       Ctyp.mk_method_type ~number:arity ~prefix:names
         ((`Lid name : FAstN.ident ), len) (Obj k) in
     (ty, result_type) in
   let mk_clfield (name,tydcl) =
     (let (ty,result_type) = mk_type tydcl in
      (`CrMth ((`Lid name), `Negative, `Negative, (f tydcl result_type), ty) : 
        FAstN.clfield ) : clfield ) in
   let fs (ty : types) =
     (match ty with
      | `Mutual named_types -> sem_of_list (List.map mk_clfield named_types)
      | `Single ((name,tydcl) as named_type) ->
          (match Ctyp.abstract_list tydcl with
           | Some n ->
               let ty_str: string = ObjsN.dump_typedecl tydcl in
               let () = Hashtbl.add tbl ty_str (Abstract ty_str) in
               let (ty,_) = mk_type tydcl in
               (`CrMth
                  ((`Lid name), `Negative, `Negative, (ExpN.unknown n), ty) : 
                 FAstN.clfield )
           | None  -> mk_clfield named_type) : clfield ) in
   let (extras,lst) = Sigs_util.transform_mtyps lst in
   let body = List.map fs lst in
   let body: clfield =
     let items =
       List.map
         (fun (dest,src,len)  ->
            let (ty,_dest) =
              Ctyp.mk_method_type ~number:arity ~prefix:names (src, len)
                (Obj k) in
            let () = Hashtbl.add tbl dest (Qualified dest) in
            (`CrMth
               ((`Lid dest), `Negative, `Negative, (ExpN.unknown len), ty) : 
              FAstN.clfield )) extras in
     sem_of_list (body @ items) in
   let v = Ctyp.mk_obj class_name base body in
   Hashtbl.iter (fun _  v  -> eprintf "@[%a@]@." pp_print_warning_type v) tbl;
   (match module_name with
    | None  -> v
    | Some u -> (`Module ((`Uid u), (`Struct v)) : FAstN.stru )) : stru )
let gen_stru ?module_name  ?(arity= 1)  ?(default=
  (`App ((`Lid "failwith"), (`Str "arity >= 2 in other branches")) : 
  FAstN.exp ))  ?cons_transform  ?annot  ~id:(id : basic_id_transform) 
  ?(names= [])  ~mk_tuple  ~mk_record  ~mk_variant  () =
  let left_type_variable = `Pre "mf_" in
  let right_type_variable = `Pre "mf_" in
  let left_type_id = id in
  let right_type_id =
    match module_name with
    | None  -> (id :>full_id_transform)
    | Some m ->
        `Last ((fun s  -> `Dot ((`Uid m), (`Lid (basic_transform id s))))) in
  let default (_,number) =
    if number > 1
    then
      let pat = (EpN.tuple_of_number `Any arity :>pat) in
      Some (`Case (pat, default) : FAstN.case )
    else None in
  let names = names in
  let mk_record = mk_record in
  let cons_transform = cons_transform in
  let () = check names in
  stru_of_mtyps ?module_name ?cons_transform ?annot ~arity ~names ~default
    ~mk_variant ~left_type_id ~left_type_variable ~mk_record
    (normal_simple_exp_of_ctyp ~arity ~names ~mk_tuple ~right_type_id
       ~left_type_id ~right_type_variable)
let gen_object ?module_name  ?(arity= 1)  ?(default=
  (`App ((`Lid "failwith"), (`Str "arity >= 2 in other branches")) : 
  FAstN.exp ))  ?cons_transform  ~kind  ~base  ~class_name  =
  let make ?(names= [])  ~mk_tuple  ~mk_record  ~mk_variant  () =
    let () = check names in
    let left_type_variable = `Pre "mf_" in
    let right_type_variable =
      `Exp
        (fun v  ->
           let v = basic_transform left_type_variable v in
           (`App ((`Lid v), (`Lid "self")) : FAstN.exp )) in
    let left_type_id = `Pre "" in
    let right_type_id = `Obj (basic_transform left_type_id) in
    let default (_,number) =
      if number > 1
      then
        let pat = (EpN.tuple_of_number `Any arity :>pat) in
        Some (`Case (pat, default) : FAstN.case )
      else None in
    obj_of_mtyps ?cons_transform ?module_name ~arity ~names ~default
      ~left_type_variable ~mk_record ~mk_variant base class_name
      (obj_simple_exp_of_ctyp ~right_type_id ~left_type_variable
         ~right_type_variable ~names ~arity ~mk_tuple) ~kind in
  make