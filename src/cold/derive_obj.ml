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
    let left_type_id = `Pre ""
    let right_type_id = `Obj (Ctyp.basic_transform left_type_id)
    let right_type_variable =
      `Exp
        (fun v  ->
           let v = Ctyp.basic_transform left_type_variable v in
           (`App ((`Lid v), (`Lid "self")) :>Astfn.exp))
    let rec mapi_exp (i : int) (ty : ctyp) =
      let name_exp = obj_simple_exp_of_ctyp ty in
      let base = apply_args name_exp names in
      let id_eps = (Listf.init arity) @@ (fun index  -> Id.xid ~off:index i) in
      let ep0 = List.hd id_eps in
      let id_ep = tuple_com id_eps in
      let exp = appl_of_list (base :: (id_eps :>exp list)) in
      { Ctyp.name_exp = name_exp; info_exp = exp; id_ep; id_eps; ep0; ty }
    and tuple_exp_of_ctyp (ty : ctyp) =
      (match ty with
       | `Par t ->
           let ls = Ast_basic.N.list_of_star t [] in
           let len = List.length ls in
           let pat = (Id_epn.mk_tuple ~arity ~number:len :>pat) in
           let tys = Lazy.force mk_tuple (List.mapi mapi_exp ls) in
           Expn_util.abstract names
             (Expn_util.currying
                [(`Case ((pat :>Astfn.pat), (tys :>Astfn.exp)) :>Astfn.case)]
                ~arity)
       | _ ->
           failwith
             ("Derive_obj.Make.tuple_exp_of_ctyp" ^
                (Astfn_print.dump_ctyp ty)) : exp )
    and obj_simple_exp_of_ctyp ty =
      (let trans = Ctyp.transform right_type_id in
       let var = Ctyp.basic_transform left_type_variable in
       let tyvar = Ctyp.right_transform right_type_variable in
       let rec aux: ctyp -> exp =
         function
         | #ident' as id -> trans (Idn_util.to_vid id)
         | `Quote (_,`Lid s) -> tyvar s
         | `App _ as ty ->
             (match Ast_basic.N.list_of_app ty [] with
              | (#ident' as tctor)::ls ->
                  appl_of_list ((trans (Idn_util.to_vid tctor)) ::
                    (ls |>
                       (List.map
                          (function
                           | `Quote (_,`Lid s) -> (`Lid (var s) :>Astfn.exp)
                           | t ->
                               (`Fun
                                  (`Case ((`Lid "self"), (aux t :>Astfn.exp))) :>
                               Astfn.exp)))))
              | _ ->
                  failwithf "list_of_app in obj_simple_exp_of_ctyp: %s"
                    (Astfn_print.dump_ctyp ty))
         | `Arrow (t1,t2) ->
             aux
               (`App
                  ((`App ((`Lid "arrow"), (t1 :>Astfn.ctyp))),
                    (t2 :>Astfn.ctyp)) :>Astfn.ctyp)
         | `Par _ as ty -> tuple_exp_of_ctyp ty
         | ty ->
             failwith
               ("Derive_obj.Make.obj_simple_exp_of_ctyp.aux" ^
                  (Astfn_print.dump_ctyp ty)) in
       aux ty : exp )
    let mk_prefix (vars : opt_decl_params) (acc : exp) =
      let varf = Ctyp.basic_transform left_type_variable in
      let f (var : decl_params) acc =
        match var with
        | `Quote (_,`Lid s) ->
            (`Fun (`Case ((`Lid (varf s)), (acc :>Astfn.exp))) :>Astfn.exp)
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
        (let e = (obj_simple_exp_of_ctyp (lid :>ctyp)) +> names in
         let (f,a) = Ast_basic.N.view_app [] result in
         let annot = appl_of_list (f :: (List.map (fun _  -> `Any) a)) in
         Ctyp.gen_tuple_abbrev ~arity ~annot ~destination:(Obj kind) lid e : 
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
    let exp_of_or_ctyp (ty : or_ctyp) =
      let f (cons : string) (tyargs : ctyp list) =
        (let args_length = List.length tyargs in
         let p: pat = (Id_epn.gen_tuple_n ~arity cons args_length :>pat) in
         let mk (cons,tyargs) =
           let exps = List.mapi mapi_exp tyargs in
           Lazy.force mk_variant (Some cons) exps in
         let e = mk (cons, tyargs) in
         (`Case ((p :>Astfn.pat), (e :>Astfn.exp)) :>Astfn.case) : case ) in
      let res: case list = Ctyp.reduce_data_ctors ty [] f ~compose:cons in
      let res =
        let t =
          if ((List.length res) >= 2) && (arity >= 2)
          then
            match Option.map transform p.default with
            | Some x -> (`Case (`Any, (x :>Astfn.exp)) :>Astfn.case) :: res
            | None  -> res
          else res in
        List.rev t in
      Expn_util.currying ~arity res
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
                 | `Sum ctyp ->
                     let funct = exp_of_or_ctyp ctyp in
                     mk_prefix tyvars funct
                 | t ->
                     failwith
                       ("Derive_obj.Make.fun_of_tydcl" ^
                          (Astfn_print.dump_type_repr t)))
            | `TyEq (_,ctyp) ->
                (match ctyp with
                 | #ident'|`Par _|`Quote _|`Arrow _|`App _ as x ->
                     let exp = obj_simple_exp_of_ctyp x in
                     let funct = Expn_util.eta_expand (exp +> names) arity in
                     mk_prefix tyvars funct
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
        let prefix = List.length names in
        let (ty,result_type) =
          Ctyp.mk_method_type ~number:arity ~prefix
            ~id:(`Lid name :>Astfn.ident) len (Obj kind) in
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
