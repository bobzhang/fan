open Ast
open Format
open LibUtil
open Lib
open Lib.Basic
module Ast = FanAst
open FSig
module Make(S:FSig.Config) = struct
  open Expr open Ident
  let _ =
    List.iter
      (fun name  ->
         if List.mem name preserve
         then
           (eprintf "%s is not a valid name\n" name;
            eprintf "preserved keywords:\n";
            List.iter (fun s  -> eprintf "%s\n" s) preserve;
            exit 2)
         else check_valid name) S.names
  let mapi_expr simple_expr_of_ctyp i (y : ctyp) =
    let name_expr = simple_expr_of_ctyp y in
    let base = name_expr +> S.names in
    let id_exprs =
      List.init S.arity (fun index  -> `ExId (_loc, (xid ~off:index i)))
    and id_patts =
      List.init S.arity (fun index  -> `PaId (_loc, (xid ~off:index i))) in
    let id_expr = Expr.tuple_of_list id_exprs in
    let id_patt = Patt.tuple_of_list id_patts in
    let expr = apply base id_exprs in
    { name_expr; expr; id_expr; id_exprs; id_patt; id_patts }
  let tuple_expr_of_ctyp simple_expr_of_ctyp ty =
    let open ErrorMonad in
      let simple_expr_of_ctyp = unwrap simple_expr_of_ctyp in
      match ty with
      | `TyTup (_loc,t) ->
          let ls = FanAst.list_of_ctyp t [] in
          let len = List.length ls in
          let patt = Patt.mk_tuple ~arity:S.arity ~number:len in
          let tys = List.mapi (mapi_expr simple_expr_of_ctyp) ls in
          S.names <+
            (currying [`McArr (_loc, patt, (`ExNil _loc), (S.mk_tuple tys))]
               ~arity:S.arity)
      | _ -> invalid_arg & (sprintf "tuple_expr_of_ctyp {|%s|}\n" "")
  let rec normal_simple_expr_of_ctyp cxt ty =
    let open Transform in
      let open ErrorMonad in
        let right_trans = transform S.right_type_id in
        let left_trans = basic_transform S.left_type_id in
        let tyvar = right_transform S.right_type_variable in
        let rec aux =
          function
          | `TyId (_loc,`Lid (_,id)) ->
              if Hashset.mem cxt id
              then `ExId (_loc, (`Lid (_loc, (left_trans id))))
              else right_trans (`Lid (_loc, id))
          | `TyId (_loc,id) -> right_trans id
          | `TyTup (_loc,_t) as ty ->
              tuple_expr_of_ctyp (normal_simple_expr_of_ctyp cxt) ty
          | `TyApp (_loc,t1,t2) -> `ExApp (_loc, (aux t1), (aux t2))
          | `TyQuo (_loc,s) -> tyvar s
          | `TyArr (_loc,t1,t2) ->
              aux
                (`TyApp
                   (_loc,
                     (`TyApp
                        (_loc, (`TyId (_loc, (`Lid (_loc, "arrow")))), t1)),
                     t2))
          | ty -> raise (Unhandled ty) in
        try return & (aux ty)
        with
        | Unhandled _t ->
            fail &
              (sprintf
                 "normal_simple_expr_of_ctyp inner:{|%s|} outer:{|%s|}\n" ""
                 "")
  let rec obj_simple_expr_of_ctyp ty =
    let open Transform in
      let open ErrorMonad in
        let trans = transform S.right_type_id in
        let var = basic_transform S.left_type_variable in
        let tyvar = right_transform S.right_type_variable in
        let rec aux =
          function
          | `TyId (_loc,id) -> trans id
          | `TyQuo (_loc,s) -> tyvar s
          | `TyApp (_loc,_,_) as ty ->
              (match Ctyp.list_of_app ty with
               | (`TyId (_loc,tctor))::ls ->
                   (ls |>
                      (List.map
                         (function
                          | `TyQuo (_loc,s) ->
                              `ExId (_loc, (`Lid (_loc, (var s))))
                          | t ->
                              `ExFun
                                (_loc,
                                  (`McArr
                                     (_loc,
                                       (`PaId (_loc, (`Lid (_loc, "self")))),
                                       (`ExNil _loc), (aux t)))))))
                     |> (apply (trans tctor))
               | _ -> invalid_arg "list_of_app in obj_simple_expr_of_ctyp")
          | `TyArr (_loc,t1,t2) ->
              aux
                (`TyApp
                   (_loc,
                     (`TyApp
                        (_loc, (`TyId (_loc, (`Lid (_loc, "arrow")))), t1)),
                     t2))
          | `TyTup (_loc,_) as ty ->
              tuple_expr_of_ctyp obj_simple_expr_of_ctyp ty
          | ty -> raise (Unhandled ty) in
        try return & (aux ty)
        with
        | Unhandled _t0 ->
            fail &
              (sprintf "obj_simple_expr_of_ctyp inner:{|%s|} outer:{|%s|}\n"
                 "" "")
  let expr_of_ctyp simple_expr_of_ctyp (ty : ctyp) =
    let open ErrorMonad in
      let f cons tyargs acc =
        let args_length = List.length tyargs in
        let p =
          Patt.gen_tuple_n ?cons_transform:S.cons_transform ~arity:S.arity
            cons args_length in
        let mk (cons,tyargs) =
          let exprs = List.mapi (mapi_expr simple_expr_of_ctyp) tyargs in
          S.mk_variant cons exprs in
        let e = mk (cons, tyargs) in (`McArr (_loc, p, (`ExNil _loc), e)) ::
          acc in
      let info =
        match ty with
        | `TySum (_loc,t) ->
            (TyVrn, (List.length (FanAst.list_of_ctyp t [])))
        | `TyVrnEq (_loc,t) ->
            (TyVrnEq, (List.length (FanAst.list_of_ctyp t [])))
        | `TyVrnSup (_loc,t) ->
            (TyVrnSup, (List.length (FanAst.list_of_ctyp t [])))
        | `TyVrnInf (_loc,t) ->
            (TyVrnInf, (List.length (FanAst.list_of_ctyp t [])))
        | _ -> invalid_arg (sprintf "expr_of_ctyp {|%s|} " "") in
      (Ctyp.reduce_data_ctors ty [] f) >>=
        (fun res  ->
           let res =
             let t =
               if ((List.length res) >= 2) && (S.arity >= 2)
               then (S.trail info) :: res
               else res in
             List.rev t in
           return (currying ~arity:S.arity res))
  let mk_prefix vars (acc : expr) =
    let open Transform in
      let varf = basic_transform S.left_type_variable in
      let f var acc =
        match var with
        | `TyQuP (_loc,s)|`TyQuM (_loc,s)|`TyQuo (_loc,s) ->
            `ExFun
              (_loc,
                (`McArr
                   (_loc, (`PaId (_loc, (`Lid (_loc, (varf s))))),
                     (`ExNil _loc), acc)))
        | _ -> (Ctyp.eprint.contents var; invalid_arg "mk_prefix") in
      List.fold_right f vars (S.names <+ acc)
  let fun_of_tydcl simple_expr_of_ctyp expr_of_ctyp =
    let open ErrorMonad in
      function
      | `TyDcl (_,_,tyvars,ctyp,_constraints) ->
          let ctyp =
            match ctyp with
            | `TyMan (_loc,_,ctyp)|`TyPrv (_loc,ctyp) -> ctyp
            | _ -> ctyp in
          (match ctyp with
           | `TyRec (_loc,t) ->
               let cols = Ctyp.list_of_record t in
               let patt = Patt.mk_record ~arity:S.arity cols in
               let info =
                 List.mapi
                   (fun i  x  ->
                      match x with
                      | { label; is_mutable; ctyp } ->
                          {
                            info =
                              (mapi_expr (unwrap simple_expr_of_ctyp) i ctyp);
                            label;
                            is_mutable
                          }) cols in
               mk_prefix tyvars
                 (currying ~arity:S.arity
                    [`McArr (_loc, patt, (`ExNil _loc), (S.mk_record info))])
           | _ ->
               let process =
                 (fun ctyp  ->
                    (simple_expr_of_ctyp ctyp) >>=
                      (fun expr  ->
                         return & (eta_expand (expr +> S.names) S.arity)))
                   <|> expr_of_ctyp in
               let funct =
                 match process ctyp with
                 | Left result -> result
                 | Right str ->
                     invalid_arg (sprintf "fun_of_tydcl{|%s|}\n%s" "" str) in
               mk_prefix tyvars funct)
      | _tydcl -> invalid_arg (sprintf "fun_of_tydcl <<%s>>\n" "")
  let binding_of_tydcl simple_expr_of_ctyp _name tydcl =
    let open ErrorMonad in
      let open Transform in
        let tctor_var = basic_transform S.left_type_id in
        let (name,len) = Ctyp.name_length_of_tydcl tydcl in
        let ty =
          Ctyp.mk_method_type_of_name ~number:S.arity ~prefix:S.names
            (name, len) Str_item in
        if not & (Ctyp.is_abstract tydcl)
        then
          let fun_expr =
            fun_of_tydcl simple_expr_of_ctyp
              (expr_of_ctyp (unwrap simple_expr_of_ctyp)) tydcl in
          `BiEq
            (_loc, (`PaId (_loc, (`Lid (_loc, (tctor_var name))))),
              (`ExTyc (_loc, fun_expr, ty)))
        else
          (eprintf "Warning: %s as a abstract type no structure generated\n"
             "";
           `BiEq
             (_loc, (`PaId (_loc, (`Lid (_loc, (tctor_var name))))),
               (`ExApp
                  (_loc, (`ExId (_loc, (`Lid (_loc, "failwithf")))),
                    (`ExStr (_loc, "Abstract data type not implemented"))))))
  let str_item_of_module_types ?module_name  simple_expr_of_ctyp_with_cxt
    (lst : module_types) =
    let cxt = Hashset.create 50 in
    let mk_binding = binding_of_tydcl (simple_expr_of_ctyp_with_cxt cxt) in
    let fs (ty : types) =
      match ty with
      | `Mutual named_types ->
          let binding =
            match named_types with
            | [] -> `BiNil _loc
            | xs ->
                (List.iter (fun (name,_ty)  -> Hashset.add cxt name) xs;
                 List.reduce_right_with
                   ~compose:(fun x  y  -> `BiAnd (_loc, x, y))
                   ~f:(fun (name,ty)  -> mk_binding name ty) xs) in
          `StVal (_loc, `ReRecursive, binding)
      | `Single (name,tydcl) ->
          (Hashset.add cxt name;
           (let rec_flag =
              if Ctyp.is_recursive tydcl then `ReRecursive else `ReNil
            and binding = mk_binding name tydcl in
            `StVal (_loc, rec_flag, binding))) in
    let item = FanAst.stSem_of_list (List.map fs lst) in
    match module_name with
    | None  -> item
    | Some m -> `StMod (_loc, m, (`MeStr (_loc, item)))
  let obj_of_module_types ?module_name  base class_name simple_expr_of_ctyp
    (k : FSig.k) (lst : module_types) =
    let open ErrorMonad in
      let tbl = Hashtbl.create 50 in
      let f =
        fun_of_tydcl simple_expr_of_ctyp
          (expr_of_ctyp (unwrap simple_expr_of_ctyp)) in
      let mk_type (_name,tydcl) =
        let (name,len) = Ctyp.name_length_of_tydcl tydcl in
        Ctyp.mk_method_type ~number:S.arity ~prefix:S.names
          ((`Lid (_loc, name)), len) (Obj k) in
      let mk_class_str_item (name,tydcl) =
        let ty = mk_type (name, tydcl) in
        `CrMth (_loc, name, `OvNil _loc, `PrNil, (f tydcl), ty) in
      let fs (ty : types) =
        match ty with
        | `Mutual named_types ->
            FanAst.crSem_of_list (List.map mk_class_str_item named_types)
        | `Single ((name,tydcl) as named_type) ->
            (match Ctyp.abstract_list tydcl with
             | Some n ->
                 let ty_str = "" in
                 let () = Hashtbl.add tbl ty_str (Abstract ty_str) in
                 let ty = mk_type (name, tydcl) in
                 `CrMth (_loc, name, `OvNil _loc, `PrNil, (unknown n), ty)
             | None  -> mk_class_str_item named_type) in
      let (extras,lst) = Ctyp.transform_module_types lst in
      let body =
        List.fold_left (fun acc  types  -> `CrSem (_loc, acc, (fs types)))
          (`CrNil _loc) lst in
      let body =
        let items =
          List.map
            (fun (dest,src,len)  ->
               let ty =
                 Ctyp.mk_method_type ~number:S.arity ~prefix:S.names
                   (src, len) (Obj k) in
               let () = Hashtbl.add tbl dest (Qualified dest) in
               `CrMth (_loc, dest, `OvNil _loc, `PrNil, (unknown len), ty)) extras in
        `CrSem (_loc, body, (FanAst.crSem_of_list items)) in
      let v = Ctyp.mk_obj class_name base body in
      Hashtbl.iter (fun _  v  -> eprintf "%s" (string_of_warning_type v)) tbl;
      (match module_name with
       | None  -> v
       | Some u -> `StMod (_loc, u, (`MeStr (_loc, v))))
  end
