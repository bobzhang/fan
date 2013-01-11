open Ast
open Format
open LibUtil
open Lib
open Lib.Basic
open FSig
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
let mapi_expr ?(arity= 1)  ?(names= [])  (simple_expr_of_ctyp : ctyp -> expr)
  (i : int) (y : ctyp) =
  (let name_expr = simple_expr_of_ctyp y in
   let base = name_expr +> names in
   let id_exprs =
     List.init arity (fun index  -> `Id (_loc, (xid ~off:index i))) in
   let exp0 = List.hd id_exprs in
   let id_patts =
     List.init arity (fun index  -> `Id (_loc, (xid ~off:index i))) in
   let pat0 = List.hd id_patts in
   let id_expr = Expr.tuple_of_list id_exprs in
   let id_patt = Patt.tuple_of_list id_patts in
   let expr = apply base id_exprs in
   let ty = y in
   { name_expr; expr; id_expr; id_exprs; id_patt; id_patts; exp0; pat0; ty } : 
  FSig.ty_info )
let tuple_expr_of_ctyp ?(arity= 1)  ?(names= [])  ~mk_tuple 
  simple_expr_of_ctyp (ty : ctyp) =
  (match ty with
   | `Tup (_loc,t) ->
       let ls = FanAst.list_of_ctyp t [] in
       let len = List.length ls in
       let patt = Patt.mk_tuple ~arity ~number:len in
       let tys = List.mapi (mapi_expr ~arity ~names simple_expr_of_ctyp) ls in
       names <+
         (currying [`Case (_loc, patt, (`Nil _loc), (mk_tuple tys))] ~arity)
   | _ -> invalid_arg & (sprintf "tuple_expr_of_ctyp {|%s|}\n" "") : 
  expr )
let rec normal_simple_expr_of_ctyp ?arity  ?names  ~mk_tuple  ~right_type_id 
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
      | `Tup (_loc,_t) as ty ->
          tuple_expr_of_ctyp ?arity ?names ~mk_tuple
            (normal_simple_expr_of_ctyp ?arity ?names ~mk_tuple
               ~right_type_id ~left_type_id ~right_type_variable cxt) ty
      | `TyApp (_loc,t1,t2) -> `ExApp (_loc, (aux t1), (aux t2))
      | `TyQuo (_loc,s) -> tyvar s
      | `TyArr (_loc,t1,t2) ->
          aux
            (`TyApp
               (_loc,
                 (`TyApp (_loc, (`Id (_loc, (`Lid (_loc, "arrow")))), t1)),
                 t2))
      | ty ->
          failwithf "normal_simple_expr_of_ctyp: %s type: \n "
            (Ctyp.to_string ty) in
    aux ty
let rec obj_simple_expr_of_ctyp ~right_type_id  ~left_type_variable 
  ~right_type_variable  ?names  ?arity  ~mk_tuple  ty =
  let open Transform in
    let trans = transform right_type_id in
    let var = basic_transform left_type_variable in
    let tyvar = right_transform right_type_variable in
    let rec aux =
      function
      | `Id (_loc,id) -> trans id
      | `TyQuo (_loc,s) -> tyvar s
      | `TyApp (_loc,_,_) as ty ->
          (match Ctyp.list_of_app ty with
           | (`Id (_loc,tctor))::ls ->
               (ls |>
                  (List.map
                     (function
                      | `TyQuo (_loc,s) -> `Id (_loc, (`Lid (_loc, (var s))))
                      | t ->
                          `Fun
                            (_loc,
                              (`Case
                                 (_loc, (`Id (_loc, (`Lid (_loc, "self")))),
                                   (`Nil _loc), (aux t)))))))
                 |> (apply (trans tctor))
           | _ -> invalid_arg "list_of_app in obj_simple_expr_of_ctyp")
      | `TyArr (_loc,t1,t2) ->
          aux
            (`TyApp
               (_loc,
                 (`TyApp (_loc, (`Id (_loc, (`Lid (_loc, "arrow")))), t1)),
                 t2))
      | `Tup (_loc,_) as ty ->
          tuple_expr_of_ctyp ?arity ?names ~mk_tuple
            (obj_simple_expr_of_ctyp ~right_type_id ~left_type_variable
               ~right_type_variable ?names ?arity ~mk_tuple) ty
      | ty -> failwithf "obj_simple_expr_of_ctyp %s\n" (Ctyp.to_string ty) in
    aux ty
let expr_of_ctyp ?cons_transform  ?(arity= 1)  ?(names= [])  ~trail 
  ~mk_variant  simple_expr_of_ctyp (ty : ctyp) =
  let f cons tyargs acc =
    (let args_length = List.length tyargs in
     let p = Patt.gen_tuple_n ?cons_transform ~arity cons args_length in
     let mk (cons,tyargs) =
       let exprs =
         List.mapi (mapi_expr ~arity ~names simple_expr_of_ctyp) tyargs in
       mk_variant cons exprs in
     let e = mk (cons, tyargs) in (`Case (_loc, p, (`Nil _loc), e)) :: acc : 
    match_case list ) in
  let info = (TyVrn, (List.length (FanAst.list_of_ctyp ty []))) in
  let res = Ctyp.reduce_data_ctors ty [] f in
  let res =
    let t =
      if ((List.length res) >= 2) && (arity >= 2)
      then (trail info) :: res
      else res in
    List.rev t in
  currying ~arity res
let expr_of_variant ?cons_transform  ?(arity= 1)  ?(names= [])  ~trail 
  ~mk_variant  simple_expr_of_ctyp result ty =
  let f (cons,tyargs) =
    (let len = List.length tyargs in
     let p = Patt.gen_tuple_n ?cons_transform ~arity cons len in
     let mk (cons,tyargs) =
       let exps =
         List.mapi (mapi_expr ~arity ~names simple_expr_of_ctyp) tyargs in
       mk_variant cons exps in
     let e = mk (cons, tyargs) in `Case (_loc, p, (`Nil _loc), e) : match_case ) in
  let simple lid =
    (let e = (simple_expr_of_ctyp (`Id (_loc, lid))) +> names in
     MatchCase.gen_tuple_abbrev ~arity result lid e : match_case ) in
  let info = (TyVrnEq, (List.length (FanAst.list_of_ctyp ty []))) in
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
      then (trail info) :: res
      else res in
    List.rev t in
  currying ~arity res
let mk_prefix vars (acc : expr) ?(names= [])  ~left_type_variable  =
  let open Transform in
    let varf = basic_transform left_type_variable in
    let f var acc =
      match var with
      | `TyQuP (_loc,s)|`TyQuM (_loc,s)|`TyQuo (_loc,s) ->
          `Fun
            (_loc,
              (`Case
                 (_loc, (`Id (_loc, (`Lid (_loc, (varf s))))), (`Nil _loc),
                   acc)))
      | _ -> (Ctyp.eprint var; invalid_arg "mk_prefix") in
    List.fold_right f vars (names <+ acc)
let fun_of_tydcl ?(names= [])  ?(arity= 1)  ~left_type_variable  ~mk_record 
  ~destination  simple_expr_of_ctyp expr_of_ctyp expr_of_variant tydcl =
  (let (name,len) = Ctyp.name_length_of_tydcl tydcl in
   let result_type =
     Ctyp.mk_dest_type ~destination ((`Lid (_loc, name)), len) in
   match tydcl with
   | `TyDcl (_,_,tyvars,ctyp,_constraints) ->
       let ctyp =
         match ctyp with
         | `TyMan (_loc,_,ctyp)|`Private (_loc,ctyp) -> ctyp
         | _ -> ctyp in
       (match ctyp with
        | `TyRec (_loc,t) ->
            let cols = Ctyp.list_of_record t in
            let patt = Patt.mk_record ~arity cols in
            let info =
              List.mapi
                (fun i  x  ->
                   match x with
                   | { label; is_mutable; ctyp } ->
                       {
                         info =
                           (mapi_expr ~arity ~names simple_expr_of_ctyp i
                              ctyp);
                         label;
                         is_mutable
                       }) cols in
            mk_prefix ~names ~left_type_variable tyvars
              (currying ~arity
                 [`Case (_loc, patt, (`Nil _loc), (mk_record info))])
        | `Id (_loc,_)|`Tup (_loc,_)|`TyApp (_loc,_,_)|`TyQuo (_loc,_)
          |`TyArr (_loc,_,_) ->
            let expr = simple_expr_of_ctyp ctyp in
            let funct = eta_expand (expr +> names) arity in
            mk_prefix ~names ~left_type_variable tyvars funct
        | `TyVrnEq (_loc,t)|`TyVrnSup (_loc,t)|`TyVrnInf (_loc,t)
          |`TyVrnInfSup (_loc,t,_) ->
            let case = expr_of_variant result_type t in
            mk_prefix ~names ~left_type_variable tyvars case
        | `Sum (_loc,ctyp) ->
            let funct = expr_of_ctyp ctyp in
            mk_prefix ~names ~left_type_variable tyvars funct
        | t -> failwithf "unhandled type %s" (Ctyp.to_string t))
   | _tydcl ->
       failwithf "impossible:fun_of_tydcl <<%s>>\n" (Ctyp.to_string _tydcl) : 
  expr )
let binding_of_tydcl ?cons_transform  simple_expr_of_ctyp tydcl ?(arity= 1) 
  ?(names= [])  ~trail  ~mk_variant  ~left_type_id  ~left_type_variable 
  ~mk_record  =
  (let open Transform in
     let tctor_var = basic_transform left_type_id in
     let (name,len) = Ctyp.name_length_of_tydcl tydcl in
     let ty =
       Ctyp.mk_method_type_of_name ~number:arity ~prefix:names (name, len)
         Str_item in
     if not (Ctyp.is_abstract tydcl)
     then
       let fun_expr =
         fun_of_tydcl ~destination:Str_item ~names ~arity ~left_type_variable
           ~mk_record simple_expr_of_ctyp
           (expr_of_ctyp ?cons_transform ~arity ~names ~trail ~mk_variant
              simple_expr_of_ctyp)
           (expr_of_variant ?cons_transform ~arity ~names ~trail ~mk_variant
              simple_expr_of_ctyp) tydcl in
       `Bind
         (_loc, (`Id (_loc, (`Lid (_loc, (tctor_var name))))),
           (`Constraint_exp (_loc, fun_expr, ty)))
     else
       (eprintf "Warning: %s as a abstract type no structure generated\n"
          (Ctyp.to_string tydcl);
        `Bind
          (_loc, (`Id (_loc, (`Lid (_loc, (tctor_var name))))),
            (`ExApp
               (_loc, (`Id (_loc, (`Lid (_loc, "failwithf")))),
                 (`Str (_loc, "Abstract data type not implemented")))))) : 
  binding )
let str_item_of_module_types ?module_name  ?cons_transform  ?arity  ?names 
  ~trail  ~mk_variant  ~left_type_id  ~left_type_variable  ~mk_record 
  simple_expr_of_ctyp_with_cxt (lst : module_types) =
  let cxt = Hashset.create 50 in
  let mk_binding =
    binding_of_tydcl ?cons_transform ?arity ?names ~trail ~mk_variant
      ~left_type_id ~left_type_variable ~mk_record
      (simple_expr_of_ctyp_with_cxt cxt) in
  let fs (ty : types) =
    (match ty with
     | `Mutual named_types ->
         let binding =
           match named_types with
           | [] -> `Nil _loc
           | xs ->
               (List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs;
                List.reduce_right_with
                  ~compose:(fun x  y  -> `And (_loc, x, y))
                  ~f:(fun (_name,ty)  -> mk_binding ty) xs) in
         `Value (_loc, (`Recursive _loc), binding)
     | `Single (name,tydcl) ->
         (Hashset.add cxt name;
          (let rec_flag =
             if Ctyp.is_recursive tydcl then `Recursive _loc else `ReNil _loc
           and binding = mk_binding tydcl in `Value (_loc, rec_flag, binding))) : 
    str_item ) in
  let item = FanAst.stSem_of_list (List.map fs lst) in
  match module_name with
  | None  -> item
  | Some m -> `Module (_loc, m, (`Struct (_loc, item)))
let obj_of_module_types ?cons_transform  ?module_name  ?(arity= 1)  ?(names=
  [])  ~trail  ~left_type_variable  ~mk_record  ~mk_variant  base class_name
  simple_expr_of_ctyp (k : kind) (lst : module_types) =
  let tbl = Hashtbl.create 50 in
  let f =
    fun_of_tydcl ~names ~destination:(Obj k) ~arity ~left_type_variable
      ~mk_record simple_expr_of_ctyp
      (expr_of_ctyp ?cons_transform ~arity ~names ~trail ~mk_variant
         simple_expr_of_ctyp)
      (expr_of_variant ?cons_transform ~arity ~names ~trail ~mk_variant
         simple_expr_of_ctyp) in
  let mk_type tydcl =
    let (name,len) = Ctyp.name_length_of_tydcl tydcl in
    Ctyp.mk_method_type ~number:arity ~prefix:names
      ((`Lid (_loc, name)), len) (Obj k) in
  let mk_class_str_item (name,tydcl) =
    (let ty = mk_type tydcl in
     `CrMth (_loc, name, (`OvNil _loc), (`PrNil _loc), (f tydcl), ty) : 
    class_str_item ) in
  let fs (ty : types) =
    match ty with
    | `Mutual named_types ->
        FanAst.crSem_of_list (List.map mk_class_str_item named_types)
    | `Single ((name,tydcl) as named_type) ->
        (match Ctyp.abstract_list tydcl with
         | Some n ->
             let ty_str = "" in
             let () = Hashtbl.add tbl ty_str (Abstract ty_str) in
             let ty = mk_type tydcl in
             `CrMth
               (_loc, name, (`OvNil _loc), (`PrNil _loc), (unknown n), ty)
         | None  -> mk_class_str_item named_type) in
  let (extras,lst) = Ctyp.transform_module_types lst in
  let body =
    List.fold_left (fun acc  types  -> `CrSem (_loc, acc, (fs types)))
      (`Nil _loc) lst in
  let body =
    let items =
      List.map
        (fun (dest,src,len)  ->
           let ty =
             Ctyp.mk_method_type ~number:arity ~prefix:names (src, len)
               (Obj k) in
           let () = Hashtbl.add tbl dest (Qualified dest) in
           `CrMth
             (_loc, dest, (`OvNil _loc), (`PrNil _loc), (unknown len), ty))
        extras in
    `CrSem (_loc, body, (FanAst.crSem_of_list items)) in
  let v = Ctyp.mk_obj class_name base body in
  Hashtbl.iter (fun _  v  -> eprintf "%a" FSig.pp_print_warning_type v) tbl;
  (match module_name with
   | None  -> v
   | Some u -> `Module (_loc, u, (`Struct (_loc, v))))