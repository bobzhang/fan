open Util
open Astn_util
open Astfn
open Ctyp
let check _ = ()
type param =  {
  arity: int;
  names: string list} 
let normal_simple_exp_of_ctyp ?arity  ?names  ~mk_tuple  ~right_type_id 
  ~left_type_id  ~right_type_variable  cxt (ty : ctyp) =
  let right_trans = transform right_type_id in
  let left_trans = basic_transform left_type_id in
  let tyvar = right_transform right_type_variable in
  let rec aux =
    function
    | `Lid id ->
        if Hashset.mem cxt id
        then lid (left_trans id)
        else right_trans (lid id)
    | #ident' as id -> right_trans (Idn_util.to_vid id)
    | `App (t1,t2) ->
        (`App ((aux t1 :>Astfn.exp), (aux t2 :>Astfn.exp)) :>Astfn.exp)
    | `Quote (_,`Lid s) -> tyvar s
    | `Arrow (t1,t2) ->
        aux
          (`App
             ((`App ((`Lid "arrow"), (t1 :>Astfn.ctyp))), (t2 :>Astfn.ctyp)) :>
          Astfn.ctyp)
    | `Par _ as ty -> tuple_exp_of_ctyp ?arity ?names ~mk_tuple ~f:aux ty
    | (ty : ctyp) ->
        failwithf "normal_simple_exp_of_ctyp : %s" (Astfn_print.dump_ctyp ty) in
  aux ty
let mk_prefix (vars : opt_decl_params) (acc : exp) ?(names= []) 
  ~left_type_variable  =
  let varf = basic_transform left_type_variable in
  let f (var : decl_params) acc =
    match var with
    | `Quote (_,`Lid s) ->
        (`Fun (`Case ((`Lid (varf s)), (acc :>Astfn.exp))) :>Astfn.exp)
    | t -> failwithf "mk_prefix: %s" (Astfn_print.dump_decl_params t) in
  match vars with
  | `None -> Expn_util.abstract names acc
  | `Some xs ->
      let vars = Ast_basic.N.list_of_com xs [] in
      List.fold_right f vars (Expn_util.abstract names acc)
let exp_of_ctyp ?cons_transform  ?(arity= 1)  ?(names= [])  ~default 
  ~mk_variant  simple_exp_of_ctyp (ty : or_ctyp) =
  let f (cons : string) (tyargs : ctyp list) =
    (let args_length = List.length tyargs in
     let p: pat =
       (Id_epn.gen_tuple_n ?cons_transform ~arity cons args_length :>
       pat) in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
       mk_variant (Some cons) exps in
     let e = mk (cons, tyargs) in
     (`Case ((p :>Astfn.pat), (e :>Astfn.exp)) :>Astfn.case) : case ) in
  let info = (Sum, (List.length (Ast_basic.N.list_of_bar ty []))) in
  let res: case list = Ctyp.reduce_data_ctors ty [] f ~compose:cons in
  let res =
    let t =
      if ((List.length res) >= 2) && (arity >= 2)
      then match default info with | Some x -> x :: res | None  -> res
      else res in
    List.rev t in
  Expn_util.currying ~arity res
let exp_of_variant ?cons_transform  ?(arity= 1)  ?(names= [])  ~default 
  ~mk_variant  ~destination  simple_exp_of_ctyp ~result  ty =
  let f (cons,tyargs) =
    (let len = List.length tyargs in
     let p = (Id_epn.gen_tuple_n ?cons_transform ~arity cons len :>pat) in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_exp ~arity ~names ~f:simple_exp_of_ctyp) tyargs in
       mk_variant (Some cons) exps in
     let e = mk (cons, tyargs) in
     (`Case ((p :>Astfn.pat), (e :>Astfn.exp)) :>Astfn.case) : case ) in
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
  Expn_util.currying ~arity res
let fun_of_tydcl ?(names= [])  ?(arity= 1)  ~left_type_variable  ~mk_record 
  ~result  simple_exp_of_ctyp exp_of_ctyp exp_of_variant tydcl =
  (match (tydcl : decl ) with
   | `TyDcl (_,tyvars,ctyp,_constraints) ->
       (match ctyp with
        | `TyMan (_,_,repr)|`TyRepr (_,repr) ->
            (match repr with
             | `Record t ->
                 let cols = Ctyp.list_of_record t in
                 let pat = (Ctyp.mk_record ~arity cols :>pat) in
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
                   (Expn_util.currying ~arity
                      [(`Case
                          ((pat :>Astfn.pat), (mk_record info :>Astfn.exp)) :>
                      Astfn.case)])
             | `Sum ctyp ->
                 let funct = exp_of_ctyp ctyp in
                 mk_prefix ~names ~left_type_variable tyvars funct
             | t ->
                 failwithf "fun_of_tydcl outer %s"
                   (Astfn_print.dump_type_repr t))
        | `TyEq (_,ctyp) ->
            (match ctyp with
             | #ident'|`Par _|`Quote _|`Arrow _|`App _ as x ->
                 let exp = simple_exp_of_ctyp x in
                 let funct = Expn_util.eta_expand (exp +> names) arity in
                 mk_prefix ~names ~left_type_variable tyvars funct
             | `PolyEq t|`PolySup t|`PolyInf t|`PolyInfSup (t,_) ->
                 let case = exp_of_variant ~result t in
                 mk_prefix ~names ~left_type_variable tyvars case
             | t ->
                 failwithf "fun_of_tydcl inner %s" (Astfn_print.dump_ctyp t))
        | t ->
            failwithf "fun_of_tydcl middle %s" (Astfn_print.dump_type_info t))
   | t -> failwithf "fun_of_tydcl outer %s" (Astfn_print.dump_decl t) : 
  exp )
let bind_of_tydcl ?cons_transform  simple_exp_of_ctyp ?(arity= 1)  ?(names=
  [])  ?(destination= Str_item)  ?annot  ~default  ~mk_variant 
  ~left_type_id:(left_type_id : basic_id_transform) 
  ~left_type_variable:(left_type_variable : basic_id_transform)  ~mk_record 
  tydcl =
  let tctor_var = basic_transform left_type_id in
  let (name,len) =
    match tydcl with
    | `TyDcl (`Lid name,tyvars,_,_) ->
        (name,
          ((match tyvars with
            | `None -> 0
            | `Some xs -> List.length @@ (Ast_basic.N.list_of_com xs []))))
    | tydcl -> failwith ("bind_of_tydcl" ^ (Astfn_print.dump_decl tydcl)) in
  let fname = tctor_var name in
  let prefix = List.length names in
  let (_ty,result) =
    Ctyp.mk_method_type ~number:arity ~prefix ~id:(lid name) len destination in
  let (annot,result) =
    match annot with
    | None  -> (None, result)
    | Some f -> let (a,b) = f name in ((Some a), b) in
  let fun_exp =
    if not @@ (Ctyp.is_abstract tydcl)
    then
      fun_of_tydcl ~names ~arity ~left_type_variable ~mk_record ~result
        simple_exp_of_ctyp
        (exp_of_ctyp ?cons_transform ~arity ~names ~default ~mk_variant
           simple_exp_of_ctyp)
        (exp_of_variant ?cons_transform ~arity ~names ~default ~mk_variant
           ~destination simple_exp_of_ctyp) tydcl
    else
      ((Format.eprintf
          "Warning: %s as a abstract type no structure generated\n")
         @@ (Astfn_print.dump_decl tydcl);
       (`App ((`Lid "failwith"), (`Str "Abstract data type not implemented")) :>
       Astfn.exp)) in
  match annot with
  | None  -> (`Bind ((`Lid fname), (fun_exp :>Astfn.exp)) :>Astfn.bind)
  | Some x ->
      (`Bind
         ((`Lid fname),
           (`Constraint ((fun_exp :>Astfn.exp), (x :>Astfn.ctyp)))) :>
      Astfn.bind)
let stru_of_mtyps ?cons_transform  ?annot  ?arity  ?names  ~default 
  ~mk_variant  ~left_type_id  ~left_type_variable  ~mk_record 
  simple_exp_of_ctyp_with_cxt (lst : Sigs_util.mtyps) =
  (let cxt = Hashset.create 50 in
   let mk_bind: decl -> bind =
     bind_of_tydcl ?cons_transform ?arity ?annot ?names ~default ~mk_variant
       ~left_type_id ~left_type_variable ~mk_record
       (simple_exp_of_ctyp_with_cxt cxt) in
   let fs (ty : Sigs_util.types) =
     (match ty with
      | `Mutual named_types ->
          (match named_types with
           | [] -> (`StExp `Unit :>Astfn.stru)
           | xs ->
               (List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs;
                (let bind =
                   Listf.reduce_right_with
                     ~compose:(fun x  y  ->
                                 (`And ((x :>Astfn.bind), (y :>Astfn.bind)) :>
                                 Astfn.bind))
                     ~f:(fun (_name,ty)  -> mk_bind ty) xs in
                 (`Value (`Positive, (bind :>Astfn.bind)) :>Astfn.stru))))
      | `Single (name,tydcl) ->
          (Hashset.add cxt name;
           (let flag =
              if Ctyp.is_recursive tydcl then `Positive else `Negative
            and bind = mk_bind tydcl in
            (`Value ((flag :>Astfn.flag), (bind :>Astfn.bind)) :>Astfn.stru))) : 
     stru ) in
   match lst with
   | [] -> (`StExp `Unit :>Astfn.stru)
   | _ -> sem_of_list (List.map fs lst) : stru )
let mk ?(arity= 1)  ?(default=
  (`App ((`Lid "failwith"), (`Str "arity >= 2 in other branches")) :>
  Astfn.exp))  ?cons_transform  ?annot  ~id:(id : basic_id_transform) 
  ?(names= [])  ~mk_record  ~mk_variant  () =
  let left_type_variable = `Pre "mf_" in
  let right_type_variable = `Pre "mf_" in
  let left_type_id = id in
  let right_type_id = (id :>full_id_transform) in
  let default (_,number) =
    if number > 1
    then
      let pat = (Id_epn.tuple_of_number `Any arity :>pat) in
      Some (`Case ((pat :>Astfn.pat), (default :>Astfn.exp)) :>Astfn.case)
    else None in
  let names = names in
  let mk_record = mk_record in
  let cons_transform = cons_transform in
  let () = check names in
  let mk_tuple = mk_variant None in
  stru_of_mtyps ?cons_transform ?annot ~arity ~names ~default ~mk_variant
    ~left_type_id ~left_type_variable ~mk_record
    (normal_simple_exp_of_ctyp ~arity ~names ~mk_tuple ~right_type_id
       ~left_type_id ~right_type_variable)
