open AstLib

open Ast

open Format

open LibUtil

open Basic

open FSig

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
   let id_eps = List.init arity (fun index  -> xid ~off:index i) in
   let ep0 = List.hd id_eps in
   let id_ep = tuple_com id_eps in
   let exp = appl_of_list (base :: (id_eps :>exp list)) in
   { name_exp; info_exp = exp; id_ep; id_eps; ep0; ty } : FSig.ty_info )

let tuple_exp_of_ctyp ?(arity= 1)  ?(names= [])  ~mk_tuple  ~f  (ty : ctyp) =
  (match ty with
   | `Par (_loc,t) ->
       let ls = list_of_star t [] in
       let len = List.length ls in
       let pat = (EP.mk_tuple ~arity ~number:len :>pat) in
       let tys = mk_tuple (List.mapi (mapi_exp ~arity ~names ~f) ls) in
       Exp.mkfun names
         (Exp.currying [(`Case (_loc, pat, tys) : Ast.case )] ~arity)
   | _ ->
       let _loc = loc_of ty in
       FanLoc.errorf _loc "tuple_exp_of_ctyp %s" (Objs.dump_ctyp ty) : 
  exp )

let rec normal_simple_exp_of_ctyp ?arity  ?names  ~mk_tuple  ~right_type_id 
  ~left_type_id  ~right_type_variable  cxt (ty : ctyp) =
  let open Transform in
    let right_trans = transform right_type_id in
    let left_trans = basic_transform left_type_id in
    let tyvar = right_transform right_type_variable in
    let rec aux =
      function
      | `Lid (_loc,id) ->
          if Hashset.mem cxt id
          then (`Lid (_loc, (left_trans id)) : Ast.exp )
          else right_trans (`Lid (_loc, id))
      | #ident' as id -> right_trans (Id.to_vid id)
      | `App (_loc,t1,t2) -> (`App (_loc, (aux t1), (aux t2)) : Ast.exp )
      | `Quote (_loc,_,`Lid (_,s)) -> tyvar s
      | `Arrow (_loc,t1,t2) ->
          aux
            (`App (_loc, (`App (_loc, (`Lid (_loc, "arrow")), t1)), t2) : 
            Ast.ctyp )
      | `Par _ as ty ->
          tuple_exp_of_ctyp ?arity ?names ~mk_tuple
            ~f:(normal_simple_exp_of_ctyp ?arity ?names ~mk_tuple
                  ~right_type_id ~left_type_id ~right_type_variable cxt) ty
      | (ty : ctyp) ->
          FanLoc.errorf (loc_of ty) "normal_simple_exp_of_ctyp : %s"
            (Objs.dump_ctyp ty) in
    aux ty

let rec obj_simple_exp_of_ctyp ~right_type_id  ~left_type_variable 
  ~right_type_variable  ?names  ?arity  ~mk_tuple  ty =
  let open Transform in
    let trans = transform right_type_id in
    let var = basic_transform left_type_variable in
    let tyvar = right_transform right_type_variable in
    let rec aux: ctyp -> exp =
      function
      | #ident' as id -> trans (Id.to_vid id)
      | `Quote (_loc,_,`Lid (_,s)) -> tyvar s
      | `App _ as ty ->
          (match list_of_app ty [] with
           | (#ident' as tctor)::ls ->
               appl_of_list ((trans (Id.to_vid tctor)) ::
                 (ls |>
                    (List.map
                       (function
                        | `Quote (_loc,_,`Lid (_,s)) ->
                            (`Lid (_loc, (var s)) : Ast.exp )
                        | t ->
                            let _loc = loc_of t in
                            (`Fun
                               (_loc,
                                 (`Case
                                    (_loc, (`Lid (_loc, "self")), (aux t)))) : 
                              Ast.exp )))))
           | _ ->
               FanLoc.errorf (loc_of ty)
                 "list_of_app in obj_simple_exp_of_ctyp: %s"
                 (Objs.dump_ctyp ty))
      | `Arrow (_loc,t1,t2) ->
          aux
            (`App (_loc, (`App (_loc, (`Lid (_loc, "arrow")), t1)), t2) : 
            Ast.ctyp )
      | `Par _ as ty ->
          tuple_exp_of_ctyp ?arity ?names ~mk_tuple
            ~f:(obj_simple_exp_of_ctyp ~right_type_id ~left_type_variable
                  ~right_type_variable ?names ?arity ~mk_tuple) ty
      | ty ->
          FanLoc.errorf (loc_of ty) "obj_simple_exp_of_ctyp: %s"
            (Objs.dump_ctyp ty) in
    aux ty

let exp_of_ctyp ?cons_transform  ?(arity= 1)  ?(names= [])  ~default 
  ~mk_variant  simple_exp_of_ctyp (ty : or_ctyp) =
  let f (cons : string) (tyargs : ctyp list) =
    (let args_length = List.length tyargs in
     let p: pat =
       (EP.gen_tuple_n ?cons_transform ~arity cons args_length :>pat) in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
       mk_variant cons exps in
     let e = mk (cons, tyargs) in
     let _loc = p <+> e in (`Case (_loc, p, e) : Ast.case ) : case ) in
  let info = (Sum, (List.length (list_of_or ty []))) in
  let res: case list = Ctyp.reduce_data_ctors ty [] f ~compose:cons in
  let res =
    let t =
      if ((List.length res) >= 2) && (arity >= 2)
      then match default info with | Some x -> x :: res | None  -> res
      else res in
    List.rev t in
  Exp.currying ~arity res

let exp_of_variant ?cons_transform  ?(arity= 1)  ?(names= [])  ~default 
  ~mk_variant  ~destination  simple_exp_of_ctyp ~result  ty =
  let f (cons,tyargs) =
    (let len = List.length tyargs in
     let p = (EP.gen_tuple_n ?cons_transform ~arity cons len :>pat) in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
       mk_variant cons exps in
     let e = mk (cons, tyargs) in
     let _loc = p <+> e in (`Case (_loc, p, e) : Ast.case ) : case ) in
  let simple (lid : ident) =
    (let e = (simple_exp_of_ctyp (lid :>ctyp)) +> names in
     let (f,a) = view_app [] result in
     let _loc = loc_of f in
     let annot =
       appl_of_list (f :: (List.map (fun _  -> (`Any _loc : Ast.ctyp )) a)) in
     Case.gen_tuple_abbrev ~arity ~annot ~destination lid e : case ) in
  let info = (TyVrnEq, (List.length (list_of_or ty []))) in
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
  Exp.currying ~arity res

let mk_prefix (vars : opt_decl_params) (acc : exp) ?(names= []) 
  ~left_type_variable  =
  let open Transform in
    let varf = basic_transform left_type_variable in
    let f (var : decl_params) acc =
      match var with
      | `Quote (_,_,`Lid (_loc,s)) ->
          (`Fun (_loc, (`Case (_loc, (`Lid (_loc, (varf s))), acc))) : 
          Ast.exp )
      | t ->
          FanLoc.errorf (loc_of t) "mk_prefix: %s" (Objs.dump_decl_params t) in
    match vars with
    | `None _ -> Exp.mkfun names acc
    | `Some (_,xs) ->
        let vars = list_of_com xs [] in
        List.fold_right f vars (Exp.mkfun names acc)

let fun_of_tydcl ?(names= [])  ?(arity= 1)  ~left_type_variable  ~mk_record 
  ~result  simple_exp_of_ctyp exp_of_ctyp exp_of_variant tydcl =
  (match (tydcl : typedecl ) with
   | `TyDcl (_,_,tyvars,ctyp,_constraints) ->
       (match ctyp with
        | `TyMan (_,_,_,repr)|`TyRepr (_,_,repr) ->
            (match repr with
             | `Record (_loc,t) ->
                 let cols = Ctyp.list_of_record t in
                 let pat = (EP.mk_record ~arity cols :>pat) in
                 let info =
                   List.mapi
                     (fun i  x  ->
                        match x with
                        | { col_label; col_mutable; col_ctyp } ->
                            {
                              re_info =
                                (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp
                                   i col_ctyp);
                              re_label = col_label;
                              re_mutable = col_mutable
                            }) cols in
                 mk_prefix ~names ~left_type_variable tyvars
                   (Exp.currying ~arity
                      [(`Case (_loc, pat, (mk_record info)) : Ast.case )])
             | `Sum (_,ctyp) ->
                 let funct = exp_of_ctyp ctyp in
                 mk_prefix ~names ~left_type_variable tyvars funct
             | t ->
                 FanLoc.errorf (loc_of t) "fun_of_tydcl outer %s"
                   (Objs.dump_type_repr t))
        | `TyEq (_,_,ctyp) ->
            (match ctyp with
             | #ident'|`Par _|`Quote _|`Arrow _|`App _ as x ->
                 let exp = simple_exp_of_ctyp x in
                 let funct = Exp.eta_expand (exp +> names) arity in
                 mk_prefix ~names ~left_type_variable tyvars funct
             | `PolyEq (_,t)|`PolySup (_,t)|`PolyInf (_,t)
               |`PolyInfSup (_,t,_) ->
                 let case = exp_of_variant ~result t in
                 mk_prefix ~names ~left_type_variable tyvars case
             | t ->
                 FanLoc.errorf (loc_of t) "fun_of_tydcl inner %s"
                   (Objs.dump_ctyp t))
        | t ->
            FanLoc.errorf (loc_of t) "fun_of_tydcl middle %s"
              (Objs.dump_type_info t))
   | t ->
       FanLoc.errorf (loc_of t) "fun_of_tydcl outer %s"
         (Objs.dump_typedecl t) : exp )

let bind_of_tydcl ?cons_transform  simple_exp_of_ctyp ?(arity= 1)  ?(names=
  [])  ?(destination= Str_item)  ?annot  ~default  ~mk_variant 
  ~left_type_id:(left_type_id : basic_id_transform) 
  ~left_type_variable:(left_type_variable : basic_id_transform)  ~mk_record 
  tydcl =
  let open Transform in
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
    let _loc = loc_of tydcl in
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
           (Objs.dump_typedecl tydcl);
         (`App
            (_loc, (`Lid (_loc, "failwith")),
              (`Str (_loc, "Abstract data type not implemented"))) : 
         Ast.exp )) in
    match annot with
    | None  -> (`Bind (_loc, (`Lid (_loc, fname)), fun_exp) : Ast.bind )
    | Some x ->
        (`Bind (_loc, (`Lid (_loc, fname)), (`Constraint (_loc, fun_exp, x))) : 
        Ast.bind )

let stru_of_mtyps ?module_name  ?cons_transform  ?annot  ?arity  ?names 
  ~default  ~mk_variant  ~left_type_id  ~left_type_variable  ~mk_record 
  simple_exp_of_ctyp_with_cxt (lst : mtyps) =
  let cxt = Hashset.create 50 in
  let mk_bind: typedecl -> bind =
    bind_of_tydcl ?cons_transform ?arity ?annot ?names ~default ~mk_variant
      ~left_type_id ~left_type_variable ~mk_record
      (simple_exp_of_ctyp_with_cxt cxt) in
  let fs (ty : types) =
    (match ty with
     | `Mutual named_types ->
         (match named_types with
          | [] -> (`StExp (ghost, (`Uid (ghost, "()"))) : Ast.stru )
          | xs ->
              (List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs;
               (let bind =
                  List.reduce_right_with
                    ~compose:(fun x  y  ->
                                let _loc = x <+> y in
                                (`And (_loc, x, y) : Ast.bind ))
                    ~f:(fun (_name,ty)  -> mk_bind ty) xs in
                let _loc = loc_of bind in
                (`Value (_loc, (`Positive _loc), bind) : Ast.stru ))))
     | `Single (name,tydcl) ->
         (Hashset.add cxt name;
          (let _loc = loc_of tydcl in
           let flag =
             if Ctyp.is_recursive tydcl
             then `Positive _loc
             else `Negative _loc
           and bind = mk_bind tydcl in
           (`Value (_loc, flag, bind) : Ast.stru ))) : stru ) in
  let item =
    match lst with
    | [] -> (`StExp (ghost, (`Uid (ghost, "()"))) : Ast.stru )
    | _ -> sem_of_list (List.map fs lst) in
  match module_name with
  | None  -> item
  | Some m ->
      let _loc = loc_of item in
      (`Module (_loc, (`Uid (_loc, m)), (`Struct (_loc, item))) : Ast.stru )

let obj_of_mtyps ?cons_transform  ?module_name  ?(arity= 1)  ?(names= []) 
  ~default 
  ~left_type_variable:(left_type_variable : FSig.basic_id_transform) 
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
     let _loc = loc_of tydcl in
     let (name,len) = Ctyp.name_length_of_tydcl tydcl in
     let (ty,result_type) =
       Ctyp.mk_method_type ~number:arity ~prefix:names
         ((`Lid (_loc, name) : Ast.ident ), len) (Obj k) in
     (ty, result_type) in
   let mk_clfield (name,tydcl) =
     (let _loc = loc_of tydcl in
      let (ty,result_type) = mk_type tydcl in
      (`CrMth
         (_loc, (`Lid (_loc, name)), (`Negative _loc), (`Negative _loc),
           (f tydcl result_type), ty) : Ast.clfield ) : clfield ) in
   let fs (ty : types) =
     (match ty with
      | `Mutual named_types -> sem_of_list (List.map mk_clfield named_types)
      | `Single ((name,tydcl) as named_type) ->
          (match Ctyp.abstract_list tydcl with
           | Some n ->
               let _loc = loc_of tydcl in
               let ty_str = "" in
               let () = Hashtbl.add tbl ty_str (Abstract ty_str) in
               let (ty,_) = mk_type tydcl in
               (`CrMth
                  (_loc, (`Lid (_loc, name)), (`Negative _loc),
                    (`Negative _loc), (Exp.unknown n), ty) : Ast.clfield )
           | None  -> mk_clfield named_type) : clfield ) in
   let (extras,lst) = Ctyp.transform_mtyps lst in
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
               (ghost, (`Lid (ghost, dest)), (`Negative ghost),
                 (`Negative ghost), (Exp.unknown len), ty) : Ast.clfield ))
         extras in
     sem_of_list (body @ items) in
   let v = Ctyp.mk_obj class_name base body in
   Hashtbl.iter
     (fun _  v  -> eprintf "@[%a@]@." FSig.pp_print_warning_type v) tbl;
   (match module_name with
    | None  -> v
    | Some u ->
        (`Module (ghost, (`Uid (ghost, u)), (`Struct (ghost, v))) : Ast.stru )) : 
  stru )

open Transform

let _loc = FanLoc.ghost

let gen_stru ?module_name  ?(arity= 1)  ?(default=
  (`App
     (_loc, (`Lid (_loc, "failwith")),
       (`Str (_loc, "arity >= 2 in other branches"))) : Ast.exp ))
   ?cons_transform  ?annot  ~id:(id : basic_id_transform)  ?(names= []) 
  ~mk_tuple  ~mk_record  ~mk_variant  () =
  let left_type_variable = `Pre "mf_" in
  let right_type_variable = `Pre "mf_" in
  let left_type_id = id in
  let right_type_id =
    match module_name with
    | None  -> (id :>full_id_transform)
    | Some m ->
        `Last
          ((fun s  ->
              `Dot
                (_loc, (`Uid (_loc, m)),
                  (`Lid (_loc, (basic_transform id s)))))) in
  let default (_,number) =
    if number > 1
    then
      let pat = (EP.tuple_of_number (`Any _loc) arity :>pat) in
      Some (`Case (_loc, pat, default) : Ast.case )
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
  (`App
     (_loc, (`Lid (_loc, "failwith")),
       (`Str (_loc, "arity >= 2 in other branches"))) : Ast.exp ))
   ?cons_transform  ~kind  ~base  ~class_name  =
  let make ?(names= [])  ~mk_tuple  ~mk_record  ~mk_variant  () =
    let () = check names in
    let left_type_variable = `Pre "mf_" in
    let right_type_variable =
      `Exp
        (fun v  ->
           let v = basic_transform left_type_variable v in
           (`App (_loc, (`Lid (_loc, v)), (`Lid (_loc, "self"))) : Ast.exp )) in
    let left_type_id = `Pre "" in
    let right_type_id = `Obj (basic_transform left_type_id) in
    let default (_,number) =
      if number > 1
      then
        let pat = (EP.tuple_of_number (`Any _loc) arity :>pat) in
        Some (`Case (_loc, pat, default) : Ast.case )
      else None in
    obj_of_mtyps ?cons_transform ?module_name ~arity ~names ~default
      ~left_type_variable ~mk_record ~mk_variant base class_name
      (obj_simple_exp_of_ctyp ~right_type_id ~left_type_variable
         ~right_type_variable ~names ~arity ~mk_tuple) ~kind in
  make