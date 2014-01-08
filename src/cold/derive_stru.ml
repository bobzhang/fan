open Util
open Astn_util
open Astfn
let check _ = ()
type default =  
  | Atom of exp
  | Invalid_argument 
let transform default =
  match default with
  | Atom e -> e
  | Invalid_argument  ->
      (`App
         ((`Lid "invalid_arg"),
           (`App
              ((`App ((`Lid "^"), (`Lid "__MODULE__"))),
                (`App ((`App ((`Lid "^"), (`Str "."))), (`Lid "__BIND__")))))) :>
      Astfn.exp)
type param = 
  {
  arity: int;
  names: string list;
  plugin_name: string;
  id: Ctyp.basic_id_transform;
  default: default option;
  mk_record: (Ctyp.record_col list -> exp) option;
  mk_variant: (string option -> Ctyp.ty_info list -> exp) option;
  annot: (string -> (ctyp* ctyp)) option;
  excludes: string list} 
module type S = sig val p : param end
module Make(U:S) =
  struct
    open U
    let arity = p.arity
    let names = p.names
    let plugin_name = p.plugin_name
    let mk_variant =
      lazy
        (match p.mk_variant with
         | None  ->
             failwithf "Generator  %s can not handle variant" plugin_name
         | Some x -> x)
    let mk_tuple = lazy (match mk_variant with | (lazy x) -> x None)
    let mk_record =
      lazy
        (match p.mk_record with
         | None  ->
             failwithf "Generator %s can not handle record" plugin_name
         | Some x -> x)
    let normal_simple_exp_of_ctyp (ty : ctyp) =
      let right_trans = Ctyp.left_transform p.id in
      let tyvar = Ctyp.right_transform (`Pre "mf_") in
      let rec aux x =
        (match x with
         | `Lid id -> (right_trans id :>exp)
         | #ident' as id ->
             (Idn_util.ident_map_of_ident right_trans (Idn_util.to_vid id) :>
             exp)
         | `App (t1,t2) ->
             (`App ((aux t1 :>Astfn.exp), (aux t2 :>Astfn.exp)) :>Astfn.exp)
         | `Quote (_,`Lid s) -> tyvar s
         | `Arrow (t1,t2) ->
             aux
               (`App
                  ((`App ((`Lid "arrow"), (t1 :>Astfn.ctyp))),
                    (t2 :>Astfn.ctyp)) :>Astfn.ctyp)
         | `Par _ as ty ->
             let mk_tuple = Lazy.force mk_tuple in
             Ctyp.tuple_exp_of_ctyp ~arity ~names:(p.names) ~mk_tuple ~f:aux
               ty
         | (ty : ctyp) ->
             failwithf "normal_simple_exp_of_ctyp : %s"
               (Astfn_print.dump_ctyp ty) : exp ) in
      aux ty
    let mk_prefix (vars : opt_decl_params) (acc : exp) =
      let varf = Ctyp.basic_transform (`Pre "mf_") in
      let f (var : decl_params) acc =
        match var with
        | `Quote (_,`Lid s) ->
            (`Fun (`Case ((`Lid (varf s)), (acc :>Astfn.exp))) :>Astfn.exp)
        | t -> failwithf "mk_prefix: %s" (Astfn_print.dump_decl_params t) in
      match vars with
      | `None -> Expn_util.abstract p.names acc
      | `Some xs ->
          let vars = Ast_basic.N.list_of_com xs [] in
          List.fold_right f vars (Expn_util.abstract p.names acc)
    let exp_of_ctyp (ty : or_ctyp) =
      (let f (cons : string) (tyargs : ctyp list) =
         (let args_length = List.length tyargs in
          let mk (cons,tyargs) =
            let exps =
              List.mapi
                (Ctyp.mapi_exp ~arity ~names ~f:normal_simple_exp_of_ctyp)
                tyargs in
            Lazy.force mk_variant (Some cons) exps in
          let p: pat = (Id_epn.gen_tuple_n ~arity cons args_length :>pat) in
          let e = mk (cons, tyargs) in
          (`Case ((p :>Astfn.pat), (e :>Astfn.exp)) :>Astfn.case) : case ) in
       let reduce_data_ctors (ty : or_ctyp) ~compose 
         (f : string -> ctyp list -> _) =
         let branches = Ast_basic.N.list_of_bar ty [] in
         let aux acc (x : or_ctyp) =
           match x with
           | `Of (`Uid cons,tys) ->
               compose (f cons (Ast_basic.N.list_of_star tys [])) acc
           | `Uid cons -> compose (f cons []) acc
           | t ->
               failwithf "reduce_data_ctors: %s" (Astfn_print.dump_or_ctyp t) in
         match branches with
         | [] -> []
         | x::[] -> aux [] x
         | _ ->
             let res = List.fold_left aux [] branches in
             if arity >= 2
             then
               (match Option.map transform p.default with
                | None  -> res
                | Some x -> (`Case (`Any, (x :>Astfn.exp)) :>Astfn.case) ::
                    res)
             else res in
       ((reduce_data_ctors ty f ~compose:cons) |> List.rev) |>
         (Expn_util.currying ~arity) : exp )
    let exp_of_variant simple_exp_of_ctyp result ty =
      let f (cons,tyargs) =
        (let len = List.length tyargs in
         let mk (cons,tyargs) =
           let exps =
             List.mapi (Ctyp.mapi_exp ~arity ~names ~f:simple_exp_of_ctyp)
               tyargs in
           Lazy.force mk_variant (Some cons) exps in
         let e = mk (cons, tyargs) in
         let p = (Id_epn.gen_tuple_n ~arity cons len :>pat) in
         (`Case ((p :>Astfn.pat), (e :>Astfn.exp)) :>Astfn.case) : case ) in
      let simple (lid : ident) =
        (let e = (simple_exp_of_ctyp (lid :>ctyp)) +> names in
         let (f,a) = Ast_basic.N.view_app [] result in
         let annot = appl_of_list (f :: (List.map (fun _  -> `Any) a)) in
         Ctyp.gen_tuple_abbrev ~arity ~annot ~destination:Ctyp.Str_item lid e : 
        case ) in
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
          then
            match Option.map transform p.default with
            | Some x -> (`Case (`Any, (x :>Astfn.exp)) :>Astfn.case) :: res
            | None  -> res
          else res in
        List.rev t in
      Expn_util.currying ~arity res
    let fun_of_tydcl result simple_exp_of_ctyp exp_of_ctyp exp_of_variant
      tydcl =
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
                                  Ctyp.info =
                                    (Ctyp.mapi_exp ~arity ~names
                                       ~f:simple_exp_of_ctyp i ty);
                                  label;
                                  is_mutable
                                }) cols in
                     mk_prefix tyvars
                       (Expn_util.currying ~arity
                          [(`Case
                              ((pat :>Astfn.pat),
                                (Lazy.force mk_record info :>Astfn.exp)) :>
                          Astfn.case)])
                 | `Sum ctyp ->
                     let funct = exp_of_ctyp ctyp in mk_prefix tyvars funct
                 | t ->
                     failwithf "fun_of_tydcl outer %s"
                       (Astfn_print.dump_type_repr t))
            | `TyEq (_,ctyp) ->
                (match ctyp with
                 | #ident'|`Par _|`Quote _|`Arrow _|`App _ as x ->
                     let exp = simple_exp_of_ctyp x in
                     let funct = Expn_util.eta_expand (exp +> names) arity in
                     mk_prefix tyvars funct
                 | `PolyEq t|`PolySup t|`PolyInf t|`PolyInfSup (t,_) ->
                     let case = exp_of_variant result t in
                     mk_prefix tyvars case
                 | t ->
                     failwithf "fun_of_tydcl inner %s"
                       (Astfn_print.dump_ctyp t))
            | t ->
                failwithf "fun_of_tydcl middle %s"
                  (Astfn_print.dump_type_info t))
       | t -> failwithf "fun_of_tydcl outer %s" (Astfn_print.dump_decl t) : 
      exp )
    let bind_of_tydcl simple_exp_of_ctyp tydcl =
      let tctor_var = Ctyp.left_transform p.id in
      let (name,len) =
        match tydcl with
        | `TyDcl (`Lid name,tyvars,_,_) ->
            (name,
              ((match tyvars with
                | `None -> 0
                | `Some xs -> List.length @@ (Ast_basic.N.list_of_com xs []))))
        | tydcl -> failwith ("bind_of_tydcl" ^ (Astfn_print.dump_decl tydcl)) in
      let fname = tctor_var name in
      let prefix = List.length p.names in
      let (_ty,result) =
        Ctyp.mk_method_type ~number:arity ~prefix ~id:(lid name) len
          Ctyp.Str_item in
      let (annot,result) =
        match p.annot with
        | None  -> (None, result)
        | Some (f : string -> (ctyp* ctyp)) ->
            let (a,b) = f name in ((Some a), b) in
      let fun_exp =
        if not @@ (Ctyp.is_abstract tydcl)
        then
          fun_of_tydcl result simple_exp_of_ctyp exp_of_ctyp
            (exp_of_variant simple_exp_of_ctyp) tydcl
        else
          ((Format.eprintf
              "Warning: %s as a abstract type no structure generated\n")
             @@ (Astfn_print.dump_decl tydcl);
           (`App
              ((`Lid "failwith"),
                (`Str "Abstract data type not implemented")) :>Astfn.exp)) in
      match annot with
      | None  ->
          (`Bind ((fname :>Astfn.pat), (fun_exp :>Astfn.exp)) :>Astfn.bind)
      | Some x ->
          (`Bind
             ((fname :>Astfn.pat),
               (`Constraint ((fun_exp :>Astfn.exp), (x :>Astfn.ctyp)))) :>
          Astfn.bind)
    let stru_of_mtyps simple_exp_of_ctyp_with_cxt (lst : Sigs_util.mtyps) =
      (let mk_bind: decl -> bind = bind_of_tydcl simple_exp_of_ctyp_with_cxt in
       let fs (ty : Sigs_util.types) =
         (match ty with
          | `Mutual named_types ->
              (match named_types with
               | [] -> (`StExp `Unit :>Astfn.stru)
               | xs ->
                   let bind =
                     Listf.reduce_right_with
                       ~compose:(fun x  y  ->
                                   (`And ((x :>Astfn.bind), (y :>Astfn.bind)) :>
                                   Astfn.bind))
                       ~f:(fun (_name,ty)  -> mk_bind ty) xs in
                   (`Value (`Positive, (bind :>Astfn.bind)) :>Astfn.stru))
          | `Single (_,tydcl) ->
              let flag =
                if Ctyp.is_recursive tydcl then `Positive else `Negative
              and bind = mk_bind tydcl in
              (`Value ((flag :>Astfn.flag), (bind :>Astfn.bind)) :>Astfn.stru) : 
         stru ) in
       match lst with
       | [] -> (`StExp `Unit :>Astfn.stru)
       | _ -> sem_of_list (List.map fs lst) : stru )
    let () =
      let () = check p.names in
      let f = stru_of_mtyps normal_simple_exp_of_ctyp in
      Typehook.register ~filter:(fun x  -> not (List.mem x p.excludes))
        ((p.plugin_name), (fun x  -> Some (f x)))
  end
let register p =
  let module M = struct let p = p end in let module N = Make(M) in ()
