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
type fn =
  [ `Pre of string | `Post of string | `Fun of string -> string | `Same] 
let pp_print_fn: Format.formatter -> fn -> unit =
  fun fmt  ->
    function
    | `Pre _a0 ->
        Format.fprintf fmt "@[<1>(`Pre@ %a)@]"
          (fun fmt  -> Format.fprintf fmt "%S") _a0
    | `Post _a0 ->
        Format.fprintf fmt "@[<1>(`Post@ %a)@]"
          (fun fmt  -> Format.fprintf fmt "%S") _a0
    | `Fun _a0 ->
        Format.fprintf fmt "@[<1>(`Fun@ %a)@]"
          ((fun _  _  fmt  _  -> Formatf.fprintf fmt "<fun>")
             (fun fmt  -> Format.fprintf fmt "%S")
             (fun fmt  -> Format.fprintf fmt "%S")) _a0
    | `Same -> Format.fprintf fmt "`Same"
let trans (x : fn) =
  match x with
  | `Pre pre -> (fun x  -> pre ^ x)
  | `Post post -> (fun x  -> x ^ post)
  | `Same -> (fun x  -> x)
  | `Fun f -> f
let trans_to_id f x = lid (trans f x)
type param = 
  {
  arity: int;
  names: string list;
  plugin_name: string;
  id: fn;
  default: default option;
  mk_record: (Ctyp.record_col list -> exp) option;
  mk_variant: (string option -> Ctyp.ty_info list -> exp) option;
  annot: (string -> (ctyp* ctyp)) option;
  builtin_tbl: (ctyp* exp) list;
  excludes: string list} 
module type S = sig val p : param end
module Make(U:S) =
  struct
    open U
    let arity = p.arity
    let names = p.names
    let plugin_name = p.plugin_name
    let builtin_tbl = Hashtblf.of_list p.builtin_tbl
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
             ("Derive_stru.Make.exp_of_tuple_ctyp" ^
                (Astfn_print.dump_ctyp ty)) : exp )
    and exp_of_ctyp (ty : ctyp) =
      let tyvar x = (`Lid (mf_prefix ^ x) :>Astfn.exp) in
      let rec aux x =
        (if Hashtbl.mem builtin_tbl x
         then Hashtbl.find builtin_tbl x
         else
           (match x with
            | `Lid id -> (trans_to_id p.id id :>exp)
            | #ident' as id ->
                (Idn_util.ident_map_of_ident (trans_to_id p.id)
                   (Idn_util.to_vid id) :>exp)
            | `App (t1,t2) ->
                (`App ((aux t1 :>Astfn.exp), (aux t2 :>Astfn.exp)) :>
                Astfn.exp)
            | `Quote (_,`Lid s) -> tyvar s
            | `Arrow (t1,t2) ->
                aux
                  (`App
                     ((`App ((`Lid "arrow"), (t1 :>Astfn.ctyp))),
                       (t2 :>Astfn.ctyp)) :>Astfn.ctyp)
            | `Par _ as ty -> exp_of_tuple_ctyp ty
            | (ty : ctyp) ->
                failwith
                  ("Derive_stru.Make.exp_of_ctyp.aux" ^
                     (Astfn_print.dump_ctyp ty))) : exp ) in
      aux ty
    and mapi_exp (i : int) (ty : ctyp) =
      (let name_exp = exp_of_ctyp ty in
       let base = apply_args name_exp names in
       let id_eps = Listf.init arity (fun index  -> Id.xid ~off:index i) in
       let ep0 = List.hd id_eps in
       let id_ep = tuple_com id_eps in
       let exp = appl_of_list (base :: (id_eps :>exp list)) in
       { name_exp; info_exp = exp; id_ep; id_eps; ep0; ty } : Ctyp.ty_info )
    let mk_prefix (vars : opt_decl_params) (acc : exp) =
      let varf x = mf_prefix ^ x in
      let f (var : decl_params) acc =
        match var with
        | `Quote (_,`Lid s) ->
            (`Fun (`Case ((`Lid (varf s)), (acc :>Astfn.exp))) :>Astfn.exp)
        | t ->
            failwith
              ("Derive_stru.Make.mk_prefix.f" ^
                 (Astfn_print.dump_decl_params t)) in
      let v = Expn_util.abstract p.names acc in
      let vars =
        match vars with
        | `None -> []
        | `Some xs -> Ast_basic.N.list_of_com xs [] in
      List.fold_right f vars v
    let exp_of_poly_variant result ty =
      let ls = Ctyp.view_variant ty in
      let aux acc x =
        (match x with
         | `variant (cons,args) ->
             let cons = "`" ^ cons in
             let len = List.length args in
             let exps = List.mapi mapi_exp args in
             (`Case
                ((Id_epn.gen_tuple_n ~arity cons len :>Astfn.pat),
                  (Lazy.force mk_variant (Some cons) exps :>Astfn.exp)) :>
               Astfn.case)
         | `abbrev lid ->
             let e = apply_args (exp_of_ctyp (lid :>ctyp)) names in
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
             (`Case
                ((pat :>Astfn.pat),
                  (`Subtype ((e :>Astfn.exp), (annot :>Astfn.ctyp)))) :>
               Astfn.case))
        :: acc in
      (match ls with
       | [] -> []
       | x::[] -> aux [] x
       | _ ->
           let res = List.fold_left aux [] ls in
           let res =
             match Option.map transform p.default with
             | Some x when arity >= 2 ->
                 (`Case (`Any, (x :>Astfn.exp)) :>Astfn.case) :: res
             | _ -> res in
           List.rev res)
        |> (Expn_util.currying ~arity)
    let exp_of_or_ctyp (ty : or_ctyp) =
      (let mk (cons,tyargs) =
         (let args_length = List.length tyargs in
          let exps = List.mapi mapi_exp tyargs in
          (`Case
             ((Id_epn.gen_tuple_n ~arity cons args_length :>Astfn.pat),
               (Lazy.force mk_variant (Some cons) exps :>Astfn.exp)) :>
            Astfn.case) : case ) in
       let reduce_data_ctors (ty : or_ctyp) =
         let branches = Ast_basic.N.list_of_bar ty [] in
         let aux acc (x : or_ctyp) =
           (match x with
            | `Of (`Uid cons,tys) ->
                mk (cons, (Ast_basic.N.list_of_star tys []))
            | `Uid cons -> mk (cons, [])
            | t ->
                failwith
                  ("Derive_stru.Make.exp_of_or_ctyp.reduce_data_ctors.aux" ^
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
    let bind_of_decl (decl : decl) =
      match (decl : decl ) with
      | `TyDcl (`Lid name,tyvars,ctyp,_constraints) ->
          let fname = trans_to_id p.id name in
          let (annot,result) =
            match p.annot with
            | None  -> (None, (`Any :>Astfn.ctyp))
            | Some (f : string -> (ctyp* ctyp)) ->
                let (a,b) = f name in ((Some a), b) in
          let fun_exp =
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
                      [(`Case
                          ((pat :>Astfn.pat),
                            (Lazy.force mk_record info :>Astfn.exp)) :>
                      Astfn.case)] |> (Expn_util.currying ~arity)
                  | `Sum ctyp -> exp_of_or_ctyp ctyp
                  | t ->
                      failwith
                        ("Derive_stru.Make.bind_of_decl.fun_exp" ^
                           (Astfn_print.dump_type_repr t)))
             | `TyEq (_,ctyp) ->
                 (match ctyp with
                  | #ident'|`Par _|`Quote _|`Arrow _|`App _ as x ->
                      Expn_util.eta_expand (apply_args (exp_of_ctyp x) names)
                        arity
                  | `PolyEq t|`PolySup t|`PolyInf t|`PolyInfSup (t,_) ->
                      exp_of_poly_variant result t
                  | t ->
                      failwith
                        ("Derive_stru.Make.bind_of_decl.fun_exp" ^
                           (Astfn_print.dump_ctyp t)))
             | t ->
                 failwith
                   ("Derive_stru.Make.bind_of_decl.fun_exp" ^
                      (Astfn_print.dump_type_info t)))
              |> (mk_prefix tyvars) in
          (match annot with
           | None  ->
               (`Bind ((fname :>Astfn.pat), (fun_exp :>Astfn.exp)) :>
               Astfn.bind)
           | Some x ->
               (`Bind
                  ((fname :>Astfn.pat),
                    (`Constraint ((fun_exp :>Astfn.exp), (x :>Astfn.ctyp)))) :>
               Astfn.bind))
      | t ->
          failwith
            ("Derive_stru.Make.bind_of_decl" ^ (Astfn_print.dump_decl t))
    let stru_of_mtyps (lst : Sigs_util.mtyps) =
      (let fs (ty : Sigs_util.types) =
         (match ty with
          | Mutual named_types ->
              (match named_types with
               | [] -> None
               | xs ->
                   let bind =
                     Listf.reduce_right_with
                       ~compose:(fun x  y  ->
                                   (`And ((x :>Astfn.bind), (y :>Astfn.bind)) :>
                                   Astfn.bind))
                       ~f:(fun (_name,ty)  -> bind_of_decl ty) xs in
                   Some
                     (`Value (`Positive, (bind :>Astfn.bind)) :>Astfn.stru))
          | Single (_,tydcl) ->
              let flag =
                if Ctyp.is_recursive tydcl then `Positive else `Negative in
              let bind = bind_of_decl tydcl in
              Some
                (`Value ((flag :>Astfn.flag), (bind :>Astfn.bind)) :>
                Astfn.stru) : stru option ) in
       match lst with
       | [] -> None
       | _ -> Some (sem_of_list (Listf.filter_map fs lst)) : stru option )
  end
let register p =
  let module M = struct let p = p end in
    let module N = Make(M) in
      Hashtbl.replace Lang_t.dispatch_tbl p.plugin_name N.exp_of_ctyp;
      Typehook.register ~filter:(fun x  -> not (List.mem x p.excludes))
        ((p.plugin_name), N.stru_of_mtyps)
