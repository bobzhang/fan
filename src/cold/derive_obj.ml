open Util
open Astn_util
open Astfn
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
  mk_record: (Ctyp.record_col list -> exp) option;
  mk_variant: (string option -> Ctyp.ty_info list -> exp) option;
  default: default option;
  excludes: string list;
  kind: Ctyp.kind;
  base: string;
  class_name: string} 
module type S = sig val p : param end
let check _ = ()
module Make(U:S) =
  struct
    open U
    let (arity,names,plugin_name,base,class_name) =
      ((p.arity), (p.names), (p.plugin_name), (p.base), (p.class_name))
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
    let mf_prefix = Gensym.fresh ~prefix:"_mf" ()
    let kind = p.kind
    let left_type_variable = `Pre mf_prefix
    let tyvar_trans x = mf_prefix ^ x
    let rec exp_of_tuple_ctyp (ty : ctyp) =
      (match ty with
       | `Par t ->
           let ls = Ast_basic.N.list_of_star t [] in
           let len = List.length ls in
           Expn_util.abstract names
             (Expn_util.currying ~arity
                [(`Case
                    ((Id_epn.mk_tuple ~arity ~number:len :>Astfn.pat),
                      (Lazy.force mk_tuple (List.mapi mapi_exp ls) :>
                      Astfn.exp)) :>Astfn.case)])
       | _ ->
           failwith
             ("Derive_obj.Make.exp_of_tuple_ctyp" ^
                (Astfn_print.dump_ctyp ty)) : exp )
    and exp_of_ctyp ty =
      (let rec aux (x : ctyp) =
         match x with
         | `Lid x -> (`Send ((`Lid "self"), (`Lid x)) :>Astfn.exp)
         | #ident' as id ->
             let s = Idn_util.map_to_string (Idn_util.to_vid id) in
             (`Send ((`Lid "self"), (`Lid s)) :>Astfn.exp)
         | `Quote (_,`Lid v) ->
             (`App ((`Lid (tyvar_trans v)), (`Lid "self")) :>Astfn.exp)
         | `App _ as ty ->
             (match Ast_basic.N.list_of_app ty [] with
              | (#ident' as tctor)::ls ->
                  appl_of_list ((aux tctor) ::
                    (ls |>
                       (List.map
                          (function
                           | `Quote (_,`Lid s) ->
                               (`Lid (tyvar_trans s) :>Astfn.exp)
                           | t ->
                               (`Fun
                                  (`Case ((`Lid "self"), (aux t :>Astfn.exp))) :>
                               Astfn.exp)))))
              | _ ->
                  failwith
                    ("Derive_obj.Make.exp_of_ctyp.aux" ^
                       (Astfn_print.dump_ctyp ty)))
         | `Arrow (t1,t2) ->
             aux
               (`App
                  ((`App ((`Lid "arrow"), (t1 :>Astfn.ctyp))),
                    (t2 :>Astfn.ctyp)) :>Astfn.ctyp)
         | `Par _ as ty -> exp_of_tuple_ctyp ty
         | ty ->
             failwith
               ("Derive_obj.Make.exp_of_ctyp.aux" ^
                  (Astfn_print.dump_ctyp ty)) in
       aux ty : exp )
    and mapi_exp (i : int) (ty : ctyp) =
      let name_exp = exp_of_ctyp ty in
      let base = apply_args name_exp names in
      let id_eps = (Listf.init arity) @@ (fun index  -> Id.xid ~off:index i) in
      let ep0 = List.hd id_eps in
      let id_ep = tuple_com id_eps in
      let exp = appl_of_list (base :: (id_eps :>exp list)) in
      { Ctyp.name_exp = name_exp; info_exp = exp; id_ep; id_eps; ep0; ty }
    let mk_prefix (vars : opt_decl_params) (acc : exp) =
      let f (var : decl_params) acc =
        match var with
        | `Quote (_,`Lid s) ->
            (`Fun (`Case ((`Lid (tyvar_trans s)), (acc :>Astfn.exp))) :>
            Astfn.exp)
        | t ->
            failwith
              ("Derive_obj.Make.mk_prefix.f" ^
                 (Astfn_print.dump_decl_params t)) in
      match vars with
      | `None -> Expn_util.abstract names acc
      | `Some xs ->
          let vars = Ast_basic.N.list_of_com xs [] in
          List.fold_right f vars (Expn_util.abstract names acc)
    let exp_of_poly_variant ~result  ty =
      let f (cons,tyargs) =
        (let len = List.length tyargs in
         let p = (Id_epn.gen_tuple_n ~arity cons len :>pat) in
         let mk (cons,tyargs) =
           let exps = List.mapi mapi_exp tyargs in
           Lazy.force mk_variant (Some cons) exps in
         let e = mk (cons, tyargs) in
         (`Case ((p :>Astfn.pat), (e :>Astfn.exp)) :>Astfn.case) : case ) in
      let simple (lid : ident) =
        (let e = apply_args (exp_of_ctyp (lid :>ctyp)) names in
         let (f,a) = Ast_basic.N.view_app [] result in
         let annot = appl_of_list (f :: (List.map (fun _  -> `Any) a)) in
         let pat =
           tuple_com
             (Listf.init arity
                (fun i  ->
                   (`Alias
                      ((`ClassPath (lid :>Astfn.ident)),
                        (`Lid (Id.x ~off:i 0))) :>Astfn.pat))) in
         let e =
           appl_of_list (e ::
             (Listf.init arity
                (fun i  -> ((Id.xid ~off:i 0 :>Astfn.vid) :>Astfn.exp)))) in
         match kind with
         | Map  ->
             (`Case
                ((pat :>Astfn.pat),
                  (`Coercion
                     ((e :>Astfn.exp), (lid :>Astfn.ctyp),
                       (annot :>Astfn.ctyp)))) :>Astfn.case)
         | _ ->
             (`Case
                ((pat :>Astfn.pat),
                  (`Subtype ((e :>Astfn.exp), (annot :>Astfn.ctyp)))) :>
             Astfn.case) : case ) in
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
    let exp_of_or_ctyp (ty : or_ctyp) =
      (let mk (cons,tyargs) =
         let args_length = List.length tyargs in
         let exps = List.mapi mapi_exp tyargs in
         (`Case
            ((Id_epn.gen_tuple_n ~arity cons args_length :>Astfn.pat),
              (Lazy.force mk_variant (Some cons) exps :>Astfn.exp)) :>
           Astfn.case) in
       let reduce_data_ctors (ty : or_ctyp) =
         let branches = Ast_basic.N.list_of_bar ty [] in
         let aux acc (x : or_ctyp) =
           (match x with
            | `Of (`Uid cons,tys) ->
                mk (cons, (Ast_basic.N.list_of_star tys []))
            | `Uid cons -> mk (cons, [])
            | t ->
                failwith
                  ("Derive_obj.Make.exp_of_or_ctyp.reduce_data_ctors.aux" ^
                     (Astfn_print.dump_or_ctyp t)))
           :: acc in
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
       ((reduce_data_ctors ty) |> List.rev) |> (Expn_util.currying ~arity) : 
      exp )
    let fun_of_tydcl ~result  tydcl =
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
                                  Ctyp.info = (mapi_exp i ty);
                                  label;
                                  is_mutable
                                }) cols in
                     mk_prefix tyvars
                       (Expn_util.currying ~arity
                          [(`Case
                              ((pat :>Astfn.pat),
                                (Lazy.force mk_record info :>Astfn.exp)) :>
                          Astfn.case)])
                 | `Sum ctyp -> mk_prefix tyvars (exp_of_or_ctyp ctyp)
                 | t ->
                     failwith
                       ("Derive_obj.Make.fun_of_tydcl" ^
                          (Astfn_print.dump_type_repr t)))
            | `TyEq (_,ctyp) ->
                (match ctyp with
                 | #ident'|`Par _|`Quote _|`Arrow _|`App _ as x ->
                     mk_prefix tyvars
                       (Expn_util.eta_expand
                          (Astn_util.apply_args (exp_of_ctyp x) names) arity)
                 | `PolyEq t|`PolySup t|`PolyInf t|`PolyInfSup (t,_) ->
                     let case = exp_of_poly_variant ~result t in
                     mk_prefix tyvars case
                 | t ->
                     failwith
                       ("Derive_obj.Make.fun_of_tydcl" ^
                          (Astfn_print.dump_ctyp t)))
            | t ->
                failwith
                  ("Derive_obj.Make.fun_of_tydcl" ^
                     (Astfn_print.dump_type_info t)))
       | t ->
           failwith
             ("Derive_obj.Make.fun_of_tydcl" ^ (Astfn_print.dump_decl t)) : 
      exp )
    let mk_method_type (id,len) (k : Ctyp.kind) =
      (let prefix = List.length names in
       let a_var_lens =
         Listf.init len
           (fun _  ->
              (`Quote (`Normal, (`Lid (Gensym.fresh ~prefix:"all_" ()))) :>
              Astfn.ctyp)) in
       let b_var_lens =
         Listf.init len
           (fun _  ->
              (`Quote (`Normal, (`Lid (Gensym.fresh ~prefix:"all_" ()))) :>
              Astfn.ctyp)) in
       let a_names = appl_of_list ((id :>ctyp) :: a_var_lens) in
       let b_names = appl_of_list ((id :>ctyp) :: b_var_lens) in
       let prefix = Listf.init prefix (const (`Any :>Astfn.ctyp)) in
       let result_type = (`Any :>Astfn.ctyp) in
       let self_type =
         (`Quote (`Normal, (`Lid "__THIS_OBJ_TYPE__")) :>Astfn.ctyp) in
       let (quant,dst) =
         match k with
         | Map  -> ((a_var_lens @ b_var_lens), b_names)
         | Iter  -> (a_var_lens, result_type)
         | Fold  -> (a_var_lens, self_type)
         | Concrete c -> (a_var_lens, c) in
       let base =
         List.fold_right arrow (prefix @ (Listf.init arity (const a_names)))
           dst in
       if len = 0
       then ((`TyPolEnd base), dst)
       else
         (let quantifiers = appl_of_list quant in
          let params =
            Listf.init len
              (fun i  ->
                 let ith_b = List.nth b_var_lens i in
                 let dst =
                   match k with
                   | Map  -> ith_b
                   | Iter  -> result_type
                   | Concrete c -> c
                   | Fold  -> self_type in
                 List.fold_right arrow (self_type :: prefix) dst) in
          ((`TyPol
              ((quantifiers :>Astfn.ctyp),
                (List.fold_right arrow params base :>Astfn.ctyp)) :>Astfn.ctyp),
            dst)) : (ctyp* ctyp) )
    let obj_of_mtyps (lst : Sigs_util.mtyps) =
      let tbl = Hashtbl.create 50 in
      let f tydcl result = (fun_of_tydcl ~result tydcl : exp ) in
      let mk_type tydcl =
        let (name,len) =
          match tydcl with
          | `TyDcl (`Lid name,tyvars,_,_) ->
              (name,
                ((match tyvars with
                  | `None -> 0
                  | `Some xs ->
                      List.length @@ (Ast_basic.N.list_of_com xs []))))
          | tydcl ->
              failwith
                ("Derive_obj.Make.obj_of_mtyps.mk_type" ^
                   (Astfn_print.dump_decl tydcl)) in
        let (ty,result_type) =
          mk_method_type ((`Lid name :>Astfn.ident), len) kind in
        (ty, result_type) in
      let mk_clfield (name,tydcl) =
        (let (ty,result_type) = mk_type tydcl in
         (`CrMth
            ((`Lid name), `Negative, `Negative,
              (f tydcl result_type :>Astfn.exp), (ty :>Astfn.ctyp)) :>
           Astfn.clfield) : clfield ) in
      let fs (ty : Sigs_util.types) =
        (match ty with
         | Mutual named_types ->
             sem_of_list (List.map mk_clfield named_types)
         | Single ((name,tydcl) as named_type) ->
             (match Ctyp.abstract_list tydcl with
              | Some n ->
                  let ty_str: string = Astfn_print.dump_decl tydcl in
                  let () = Hashtbl.add tbl ty_str (Ctyp.Abstract ty_str) in
                  let (ty,_) = mk_type tydcl in
                  (`CrMth
                     ((`Lid name), `Negative, `Negative,
                       (Expn_util.unknown n :>Astfn.exp), (ty :>Astfn.ctyp)) :>
                    Astfn.clfield)
              | None  -> mk_clfield named_type) : clfield ) in
      let (extras,lst) = Sigs_util.transform_mtyps lst in
      let body = List.map fs lst in
      let prefix = List.length names in
      let body: clfield =
        let items =
          List.map
            (fun (dest,src,len)  ->
               let (ty,_dest) =
                 Ctyp.mk_method_type ~number:arity ~prefix ~id:src len
                   (Obj kind) in
               let () = Hashtbl.add tbl dest (Qualified dest) in
               (`CrMth
                  ((`Lid dest), `Negative, `Negative,
                    (Expn_util.unknown len :>Astfn.exp), (ty :>Astfn.ctyp)) :>
                 Astfn.clfield)) extras in
        sem_of_list (body @ items) in
      let v = Ctyp.mk_obj class_name base body in
      Hashtbl.iter
        (fun _  v  -> Formatf.eprintf "@[%a@]@." Ctyp.pp_print_warning_type v)
        tbl;
      Some v
  end
let register (p : param) =
  let module N = Make(struct let p = p end) in
    Typehook.register ~filter:(fun x  -> not (List.mem x p.excludes))
      ((p.plugin_name), N.obj_of_mtyps)
