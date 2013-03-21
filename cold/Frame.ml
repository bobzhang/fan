open AstLoc
open Format
open LibUtil
open Lib
open Lib.Basic
open FSig
open Lib.EP
open Lib.Expr
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
   let id_exps =
     List.init arity (fun index  -> `Id (_loc, (xid ~off:index i))) in
   let exp0 = List.hd id_exps in
   let id_patts = id_exps in
   let pat0 = exp0 in
   let id_exp = tuple_com id_exps in
   let id_patt = id_exp in
   let exp = appl_of_list (base :: id_exps) in
   { name_exp; exp; id_exp; id_exps; id_patt; id_patts; exp0; pat0; ty } : 
  FSig.ty_info )
let tuple_exp_of_ctyp ?(arity= 1)  ?(names= [])  ~mk_tuple 
  simple_exp_of_ctyp (ty : ctyp) =
  (match ty with
   | `Tup (_loc,t) ->
       let ls = list_of_star t [] in
       let len = List.length ls in
       let patt = EP.mk_tuple ~arity ~number:len in
       let tys = List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) ls in
       names <+ (currying [`Case (_loc, patt, (mk_tuple tys))] ~arity)
   | _ -> FanLoc.errorf _loc "tuple_exp_of_ctyp %s" (FanObjs.dump_ctyp ty) : 
  exp )
let rec normal_simple_exp_of_ctyp ?arity  ?names  ~mk_tuple  ~right_type_id 
  ~left_type_id  ~right_type_variable  cxt ty =
  let open Transform in
    let right_trans = transform right_type_id in
    let left_trans = basic_transform left_type_id in
    let tyvar = right_transform right_type_variable in
    let rec aux =
      function
      | `Id (_loc,`Lid (_,id)) ->
          if Hashset.mem cxt id
          then `Id (_loc, (`Lid (_loc, (left_trans id))))
          else right_trans (`Lid (_loc, id))
      | `Id (_loc,id) -> right_trans id
      | `App (_loc,t1,t2) -> `App (_loc, (aux t1), (aux t2))
      | `Quote (_loc,_,`Lid (_,s)) -> tyvar s
      | `Arrow (_loc,t1,t2) ->
          aux
            (`App
               (_loc,
                 (`App (_loc, (`Id (_loc, (`Lid (_loc, "arrow")))), t1)), t2))
      | `Tup _ as ty ->
          tuple_exp_of_ctyp ?arity ?names ~mk_tuple
            (normal_simple_exp_of_ctyp ?arity ?names ~mk_tuple ~right_type_id
               ~left_type_id ~right_type_variable cxt) ty
      | ty ->
          FanLoc.errorf (loc_of ty) "normal_simple_exp_of_ctyp : %s"
            (FanObjs.dump_ctyp ty) in
    aux ty
let rec obj_simple_exp_of_ctyp ~right_type_id  ~left_type_variable 
  ~right_type_variable  ?names  ?arity  ~mk_tuple  ty =
  let open Transform in
    let trans = transform right_type_id in
    let var = basic_transform left_type_variable in
    let tyvar = right_transform right_type_variable in
    let rec aux =
      function
      | `Id (_loc,id) -> trans id
      | `Quote (_loc,_,`Lid (_,s)) -> tyvar s
      | `App _ as ty ->
          (match list_of_app ty [] with
           | (`Id (_loc,tctor))::ls ->
               appl_of_list ((trans tctor) ::
                 (ls |>
                    (List.map
                       (function
                        | `Quote (_loc,_,`Lid (_,s)) ->
                            `Id (_loc, (`Lid (_loc, (var s))))
                        | t ->
                            `Fun
                              (_loc,
                                (`Case
                                   (_loc,
                                     (`Id (_loc, (`Lid (_loc, "self")))),
                                     (aux t))))))))
           | _ ->
               FanLoc.errorf (loc_of ty)
                 "list_of_app in obj_simple_exp_of_ctyp: %s"
                 (FanObjs.dump_ctyp ty))
      | `Arrow (_loc,t1,t2) ->
          aux
            (`App
               (_loc,
                 (`App (_loc, (`Id (_loc, (`Lid (_loc, "arrow")))), t1)), t2))
      | `Tup _ as ty ->
          tuple_exp_of_ctyp ?arity ?names ~mk_tuple
            (obj_simple_exp_of_ctyp ~right_type_id ~left_type_variable
               ~right_type_variable ?names ?arity ~mk_tuple) ty
      | ty ->
          FanLoc.errorf (loc_of ty) "obj_simple_exp_of_ctyp: %s"
            (FanObjs.dump_ctyp ty) in
    aux ty
let exp_of_ctyp ?cons_transform  ?(arity= 1)  ?(names= [])  ~trail 
  ~mk_variant  simple_exp_of_ctyp (ty : or_ctyp) =
  let f (cons : string) (tyargs : ctyp list) =
    (let args_length = List.length tyargs in
     let p: patt = EP.gen_tuple_n ?cons_transform ~arity cons args_length in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
       mk_variant cons exps in
     let e = mk (cons, tyargs) in `Case (_loc, p, e) : case ) in
  let info = (Sum, (List.length (list_of_or ty []))) in
  let res: case list = Ctyp.reduce_data_ctors ty [] f ~compose:cons in
  let res =
    let t =
      if ((List.length res) >= 2) && (arity >= 2)
      then match trail info with | Some x -> x :: res | None  -> res
      else res in
    List.rev t in
  currying ~arity res
let exp_of_variant ?cons_transform  ?(arity= 1)  ?(names= [])  ~trail 
  ~mk_variant  ~destination  simple_exp_of_ctyp result ty =
  let f (cons,tyargs) =
    (let len = List.length tyargs in
     let p = EP.gen_tuple_n ?cons_transform ~arity cons len in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
       mk_variant cons exps in
     let e = mk (cons, tyargs) in `Case (_loc, p, e) : case ) in
  let simple lid =
    (let e = (simple_exp_of_ctyp (`Id (_loc, lid))) +> names in
     let (f,a) = view_app [] result in
     let annot = appl_of_list (f :: (List.map (fun _  -> `Any _loc) a)) in
     MatchCase.gen_tuple_abbrev ~arity ~annot ~destination lid e : case ) in
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
      then match trail info with | Some x -> x :: res | None  -> res
      else res in
    List.rev t in
  currying ~arity res
let mk_prefix (vars : opt_decl_params) (acc : exp) ?(names= []) 
  ~left_type_variable  =
  let open Transform in
    let varf = basic_transform left_type_variable in
    let f (var : decl_params) acc =
      match var with
      | `Quote (_,_,`Lid (_loc,s)) ->
          `Fun
            (_loc,
              (`Case (_loc, (`Id (_loc, (`Lid (_loc, (varf s))))), acc)))
      | t ->
          FanLoc.errorf (loc_of t) "mk_prefix: %s"
            (FanObjs.dump_decl_params t) in
    match vars with
    | `None _ -> names <+ acc
    | `Some (_,xs) ->
        let vars = list_of_com xs [] in List.fold_right f vars (names <+ acc)
let fun_of_tydcl ?(names= [])  ?(arity= 1)  ~left_type_variable  ~mk_record 
  ~destination  ~result_type  simple_exp_of_ctyp exp_of_ctyp exp_of_variant
  tydcl =
  (match (tydcl : typedecl ) with
   | `TyDcl (_,_,tyvars,ctyp,_constraints) ->
       (match ctyp with
        | `TyMan (_,_,_,repr)|`TyRepr (_,_,repr) ->
            (match repr with
             | `Record (_loc,t) ->
                 let cols = Ctyp.list_of_record t in
                 let patt: patt = EP.mk_record ~arity cols in
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
                   (currying ~arity [`Case (_loc, patt, (mk_record info))])
             | `Sum (_,ctyp) ->
                 let funct = exp_of_ctyp ctyp in
                 mk_prefix ~names ~left_type_variable tyvars funct
             | t ->
                 FanLoc.errorf (loc_of t) "fun_of_tydcl outer %s"
                   (FanObjs.dump_type_repr t))
        | `TyEq (_,_,ctyp) ->
            (match ctyp with
             | `Id _|`Tup _|`Quote _|`Arrow _|`App _ as x ->
                 let exp = simple_exp_of_ctyp x in
                 let funct = eta_expand (exp +> names) arity in
                 mk_prefix ~names ~left_type_variable tyvars funct
             | `PolyEq (_,t)|`PolySup (_,t)|`PolyInf (_,t)
               |`PolyInfSup (_,t,_) ->
                 let case = exp_of_variant result_type t in
                 mk_prefix ~names ~left_type_variable tyvars case
             | t ->
                 FanLoc.errorf (loc_of t) "fun_of_tydcl inner %s"
                   (FanObjs.dump_ctyp t))
        | t ->
            FanLoc.errorf (loc_of t) "fun_of_tydcl middle %s"
              (FanObjs.dump_type_info t))
   | t ->
       FanLoc.errorf (loc_of t) "fun_of_tydcl outer %s"
         (FanObjs.dump_typedecl t) : exp )
let binding_of_tydcl ?cons_transform  simple_exp_of_ctyp tydcl ?(arity= 1) 
  ?(names= [])  ~trail  ~mk_variant  ~left_type_id  ~left_type_variable 
  ~mk_record  =
  let open Transform in
    let tctor_var = basic_transform left_type_id in
    let (name,len) = Ctyp.name_length_of_tydcl tydcl in
    let (ty,result_type) =
      Ctyp.mk_method_type_of_name ~number:arity ~prefix:names (name, len)
        Str_item in
    if not (Ctyp.is_abstract tydcl)
    then
      let fun_exp =
        fun_of_tydcl ~destination:Str_item ~names ~arity ~left_type_variable
          ~mk_record ~result_type simple_exp_of_ctyp
          (exp_of_ctyp ?cons_transform ~arity ~names ~trail ~mk_variant
             simple_exp_of_ctyp)
          (exp_of_variant ?cons_transform ~arity ~names ~trail ~mk_variant
             ~destination:Str_item simple_exp_of_ctyp) tydcl in
      `Bind (_loc, (`Id (_loc, (`Lid (_loc, (tctor_var name))))), fun_exp)
    else
      (eprintf "Warning: %s as a abstract type no structure generated\n"
         (FanObjs.dump_typedecl tydcl);
       `Bind
         (_loc, (`Id (_loc, (`Lid (_loc, (tctor_var name))))),
           (`App
              (_loc, (`Id (_loc, (`Lid (_loc, "failwithf")))),
                (`Str (_loc, "Abstract data type not implemented"))))))
let stru_of_module_types ?module_name  ?cons_transform  ?arity  ?names 
  ~trail  ~mk_variant  ~left_type_id  ~left_type_variable  ~mk_record 
  simple_exp_of_ctyp_with_cxt (lst : module_types) =
  let cxt = Hashset.create 50 in
  let mk_binding =
    binding_of_tydcl ?cons_transform ?arity ?names ~trail ~mk_variant
      ~left_type_id ~left_type_variable ~mk_record
      (simple_exp_of_ctyp_with_cxt cxt) in
  let fs (ty : types) =
    (match ty with
     | `Mutual named_types ->
         (match named_types with
          | [] -> `StExp (_loc, (`Id (_loc, (`Uid (_loc, "()")))))
          | xs ->
              (List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs;
               (let binding =
                  List.reduce_right_with
                    ~compose:(fun x  y  -> `And (_loc, x, y))
                    ~f:(fun (_name,ty)  -> mk_binding ty) xs in
                `Value (_loc, (`Recursive _loc), binding))))
     | `Single (name,tydcl) ->
         (Hashset.add cxt name;
          (let rec_flag =
             if Ctyp.is_recursive tydcl then `Recursive _loc else `ReNil _loc
           and binding = mk_binding tydcl in `Value (_loc, rec_flag, binding))) : 
    stru ) in
  let item =
    match lst with
    | [] -> `StExp (_loc, (`Id (_loc, (`Uid (_loc, "()")))))
    | _ -> sem_of_list (List.map fs lst) in
  match module_name with
  | None  -> item
  | Some m -> `Module (_loc, (`Uid (_loc, m)), (`Struct (_loc, item)))
let obj_of_module_types ?cons_transform  ?module_name  ?(arity= 1)  ?(names=
  [])  ~trail 
  ~left_type_variable:(left_type_variable : FSig.basic_id_transform) 
  ~mk_record  ~mk_variant  base class_name simple_exp_of_ctyp (k : kind)
  (lst : module_types) =
  (let tbl = Hashtbl.create 50 in
   let f tydcl result_type =
     fun_of_tydcl ~names ~destination:(Obj k) ~arity ~left_type_variable
       ~mk_record simple_exp_of_ctyp
       (exp_of_ctyp ?cons_transform ~arity ~names ~trail ~mk_variant
          simple_exp_of_ctyp)
       (exp_of_variant ?cons_transform ~destination:(Obj k) ~arity ~names
          ~trail ~mk_variant simple_exp_of_ctyp) ~result_type tydcl in
   let mk_type tydcl =
     let (name,len) = Ctyp.name_length_of_tydcl tydcl in
     let (ty,result_type) =
       Ctyp.mk_method_type ~number:arity ~prefix:names
         ((`Lid (_loc, name)), len) (Obj k) in
     (ty, result_type) in
   let mk_cstru (name,tydcl) =
     (let (ty,result_type) = mk_type tydcl in
      `CrMth
        (_loc, (`Lid (_loc, name)), (`OvNil _loc), (`PrNil _loc),
          (f tydcl result_type), ty) : cstru ) in
   let fs (ty : types) =
     (match ty with
      | `Mutual named_types -> sem_of_list (List.map mk_cstru named_types)
      | `Single ((name,tydcl) as named_type) ->
          (match Ctyp.abstract_list tydcl with
           | Some n ->
               let ty_str = "" in
               let () = Hashtbl.add tbl ty_str (Abstract ty_str) in
               let (ty,_) = mk_type tydcl in
               `CrMth
                 (_loc, (`Lid (_loc, name)), (`OvNil _loc), (`PrNil _loc),
                   (unknown n), ty)
           | None  -> mk_cstru named_type) : cstru ) in
   let (extras,lst) = Ctyp.transform_module_types lst in
   let body = List.map fs lst in
   let body: cstru =
     let items =
       List.map
         (fun (dest,src,len)  ->
            let (ty,_dest) =
              Ctyp.mk_method_type ~number:arity ~prefix:names (src, len)
                (Obj k) in
            let () = Hashtbl.add tbl dest (Qualified dest) in
            `CrMth
              (_loc, (`Lid (_loc, dest)), (`OvNil _loc), (`PrNil _loc),
                (unknown len), ty)) extras in
     sem_of_list (body @ items) in
   let v = Ctyp.mk_obj class_name base body in
   Hashtbl.iter
     (fun _  v  -> eprintf "@[%a@]@." FSig.pp_print_warning_type v) tbl;
   (match module_name with
    | None  -> v
    | Some u -> `Module (_loc, (`Uid (_loc, u)), (`Struct (_loc, v)))) : 
  stru )