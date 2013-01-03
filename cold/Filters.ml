open LibUtil
open Ast
module Ast = Camlp4Ast
module MetaLoc =
  struct
  let meta_loc_patt _loc _ = PaId (_loc, (IdLid (_loc, "loc")))
  let meta_loc_expr _loc _ = ExId (_loc, (IdLid (_loc, "loc")))
  end
module MetaAst = FanAst.Make(MetaLoc)
let _ =
  AstFilters.register_str_item_filter
    ("lift",
      (fun ast  ->
         let _loc = Ast.loc_of_str_item ast in
         StExp
           (_loc,
             (ExLet
                (_loc, ReNil,
                  (BiEq
                     (_loc, (PaId (_loc, (IdLid (_loc, "loc")))),
                       (ExId
                          (_loc,
                            (IdAcc
                               (_loc, (IdUid (_loc, "FanLoc")),
                                 (IdLid (_loc, "ghost")))))))),
                  (MetaAst.Expr.meta_str_item _loc ast))))))
let add_debug_expr e =
  let _loc = Ast.loc_of_expr e in
  let msg = "camlp4-debug: exc: %s at " ^ ((FanLoc.to_string _loc) ^ "@.") in
  ExTry
    (_loc, e,
      (McOr
         (_loc,
           (McArr
              (_loc,
                (PaAli
                   (_loc,
                     (PaOrp
                        (_loc,
                          (PaId
                             (_loc,
                               (IdAcc
                                  (_loc, (IdUid (_loc, "XStream")),
                                    (IdUid (_loc, "Failure")))))),
                          (PaId (_loc, (IdUid (_loc, "Exit")))))),
                     (PaId (_loc, (IdLid (_loc, "exc")))))), (ExNil _loc),
                (ExApp
                   (_loc, (ExId (_loc, (IdLid (_loc, "raise")))),
                     (ExId (_loc, (IdLid (_loc, "exc")))))))),
           (McArr
              (_loc, (PaId (_loc, (IdLid (_loc, "exc")))), (ExNil _loc),
                (ExSeq
                   (_loc,
                     (ExSem
                        (_loc,
                          (ExIfe
                             (_loc,
                               (ExApp
                                  (_loc,
                                    (ExId
                                       (_loc,
                                         (IdAcc
                                            (_loc, (IdUid (_loc, "Debug")),
                                              (IdLid (_loc, "mode")))))),
                                    (ExStr (_loc, "exc")))),
                               (ExApp
                                  (_loc,
                                    (ExApp
                                       (_loc,
                                         (ExId
                                            (_loc,
                                              (IdAcc
                                                 (_loc,
                                                   (IdUid (_loc, "Format")),
                                                   (IdLid (_loc, "eprintf")))))),
                                         (ExStr
                                            (_loc,
                                              (Ast.safe_string_escaped msg))))),
                                    (ExApp
                                       (_loc,
                                         (ExId
                                            (_loc,
                                              (IdAcc
                                                 (_loc,
                                                   (IdUid (_loc, "Printexc")),
                                                   (IdLid (_loc, "to_string")))))),
                                         (ExId (_loc, (IdLid (_loc, "exc")))))))),
                               (ExId (_loc, (IdUid (_loc, "()")))))),
                          (ExApp
                             (_loc, (ExId (_loc, (IdLid (_loc, "raise")))),
                               (ExId (_loc, (IdLid (_loc, "exc")))))))))))))))
let rec map_match_case =
  function
  | McOr (_loc,m1,m2) ->
      McOr (_loc, (map_match_case m1), (map_match_case m2))
  | McArr (_loc,p,w,e) -> McArr (_loc, p, w, (add_debug_expr e))
  | m -> m
let _ =
  AstFilters.register_str_item_filter
    ("exception",
      ((object 
          inherit  Ast.map as super
          method! expr =
            function
            | ExFun (_loc,m) -> ExFun (_loc, (map_match_case m))
            | x -> super#expr x
          method! str_item =
            function
            | StMod (_loc,"Debug",_) as st -> st
            | st -> super#str_item st
        end)#str_item))
let _loc = FanLoc.ghost
let sf = Printf.sprintf
let xik i k =
  let i = if i < 0 then assert false else if i = 0 then "" else sf "_i%d" i in
  let k = if k < 1 then assert false else if k = 1 then "" else sf "_k%d" k in
  sf "_x%s%s" i k
let exik i k = ExId (_loc, (IdLid (_loc, (xik i k))))
let pxik i k = PaId (_loc, (IdLid (_loc, (xik i k))))
let elidk y k = ExId (_loc, (IdLid (_loc, (sf "%s_%d" y k))))
let plidk y k = PaId (_loc, (IdLid (_loc, (sf "%s_%d" y k))))
let xs s = "_x_" ^ s
let xsk = sf "_x_%s_%d"
let exsk s k = ExId (_loc, (IdLid (_loc, (xsk s k))))
let rec apply_expr accu =
  function
  | [] -> accu
  | x::xs ->
      let _loc = Ast.loc_of_expr x in apply_expr (ExApp (_loc, accu, x)) xs
let rec apply_patt accu =
  function
  | [] -> accu
  | x::xs ->
      let _loc = Ast.loc_of_patt x in apply_patt (PaApp (_loc, accu, x)) xs
let rec apply_ctyp accu =
  function
  | [] -> accu
  | x::xs ->
      let _loc = Ast.loc_of_ctyp x in apply_ctyp (TyApp (_loc, accu, x)) xs
let opt_map f = function | Some x -> Some (f x) | None  -> None
let list_init f n =
  let rec self m = if m = n then [] else (f m) :: (self (succ m)) in self 0
let rec lid_of_ident sep =
  function
  | IdLid (_loc,s)|IdUid (_loc,s) -> s
  | IdAcc (_loc,i1,i2) ->
      (lid_of_ident sep i1) ^ (sep ^ (lid_of_ident sep i2))
  | _ -> assert false
type type_decl = (string* Ast.ident* Ast.ctyp list* Ast.ctyp* bool) 
let builtin_types =
  let tyMap = SMap.empty in
  let tyMap =
    let abstr =
      ["string";
      "int";
      "float";
      "int32";
      "int64";
      "nativeint";
      "char";
      "bool"] in
    List.fold_right
      (fun name  ->
         SMap.add name (name, (IdLid (_loc, name)), [], (TyNil _loc), false))
      abstr tyMap in
  let tyMap =
    let concr =
      [("list", (IdLid (_loc, "list")), [TyQuo (_loc, "a")],
         (TySum
            (_loc,
              (TyOr
                 (_loc, (TyId (_loc, (IdUid (_loc, "[]")))),
                   (TyOf
                      (_loc, (TyId (_loc, (IdUid (_loc, "::")))),
                        (TyAnd
                           (_loc, (TyQuo (_loc, "a")),
                             (TyApp
                                (_loc, (TyId (_loc, (IdLid (_loc, "list")))),
                                  (TyQuo (_loc, "a")))))))))))), false);
      ("option", (IdLid (_loc, "option")), [TyQuo (_loc, "a")],
        (TySum
           (_loc,
             (TyOr
                (_loc, (TyId (_loc, (IdUid (_loc, "None")))),
                  (TyOf
                     (_loc, (TyId (_loc, (IdUid (_loc, "Some")))),
                       (TyQuo (_loc, "a")))))))), false);
      ("ref", (IdLid (_loc, "ref")), [TyQuo (_loc, "a")],
        (TyRec
           (_loc,
             (TyCol
                (_loc, (TyId (_loc, (IdLid (_loc, "contents")))),
                  (TyQuo (_loc, "a")))))), false)] in
    List.fold_right (fun ((name,_,_,_,_) as decl)  -> SMap.add name decl)
      concr tyMap in
  tyMap
let used_builtins = ref SMap.empty
let store_if_builtin_type id =
  if SMap.mem id builtin_types
  then
    used_builtins :=
      (SMap.add id (SMap.find id builtin_types) used_builtins.contents)
  else ()
type mode =  
  | Fold
  | Map
  | Fold_map 
let string_of_mode =
  function | Fold  -> "fold" | Map  -> "map" | Fold_map  -> "fold_map"
module Gen(X:sig val size : int val mode : mode end) =
  struct
  let size = X.size let mode = X.mode
  let tuplify_expr f =
    if size <= 0
    then assert false
    else
      if size = 1
      then f 1
      else
        (let rec loop k =
           if k = 2 then f 2 else ExCom (_loc, (loop (k - 1)), (f k)) in
         ExTup (_loc, (ExCom (_loc, (f 1), (loop size)))))
  let tuplify_patt f =
    if size <= 0
    then assert false
    else
      if size = 1
      then f 1
      else
        (let rec loop k =
           if k = 2 then f 2 else PaCom (_loc, (loop (k - 1)), (f k)) in
         PaTup (_loc, (PaCom (_loc, (f 1), (loop size)))))
  let xiks i = tuplify_expr (exik i)
  let tuplify_type typ =
    if size <= 0
    then assert false
    else
      if size = 1
      then typ
      else
        (let rec loop k =
           if k = 2 then typ else TySta (_loc, (loop (k - 1)), typ) in
         TyTup (_loc, (TySta (_loc, typ, (loop size)))))
  let tuplify_tycon tycon = tuplify_type (TyId (_loc, (IdLid (_loc, tycon))))
  let rec patt_of_expr =
    function
    | ExNil _loc -> PaNil _loc
    | ExId (_loc,i) -> PaId (_loc, i)
    | ExCom (_loc,e1,e2) ->
        PaCom (_loc, (patt_of_expr e1), (patt_of_expr e2))
    | ExTup (_loc,e) -> PaTup (_loc, (patt_of_expr e))
    | _ -> assert false
  let bind p e1 e2 =
    match mode with
    | Fold_map  ->
        ExLet
          (_loc, ReNil,
            (BiEq
               (_loc,
                 (PaTup
                    (_loc,
                      (PaCom (_loc, (PaId (_loc, (IdLid (_loc, "o")))), p)))),
                 e1)), e2)
    | Map  -> ExLet (_loc, ReNil, (BiEq (_loc, p, e1)), e2)
    | Fold  ->
        ExLet
          (_loc, ReNil,
            (BiEq (_loc, (PaId (_loc, (IdLid (_loc, "o")))), e1)), e2)
  let return e =
    match mode with
    | Fold_map  ->
        ExTup (_loc, (ExCom (_loc, (ExId (_loc, (IdLid (_loc, "o")))), e)))
    | Map  -> e
    | Fold  -> ExId (_loc, (IdLid (_loc, "o")))
  let rec opt_bind opt_patt e1 mk_e2 =
    match e1 with
    | ExId (_loc,_)|ExSnd (_loc,ExId (_,IdLid (_,_)),_) -> mk_e2 e1
    | ExLet (_loc,ReNil ,BiEq (_,p1,e1),e2) ->
        ExLet (_loc, ReNil, (BiEq (_loc, p1, e1)), (opt_bind None e2 mk_e2))
    | _ ->
        let e2 = mk_e2 (ExId (_loc, (IdLid (_loc, "o")))) in
        (match opt_patt with
         | Some patt -> bind patt e1 e2
         | None  ->
             ExApp
               (_loc,
                 (ExFun
                    (_loc,
                      (McArr
                         (_loc, (PaId (_loc, (IdLid (_loc, "o")))),
                           (ExNil _loc), e1)))), e2))
  let chain_tuple mkp mke expr_of_ty ts =
    let exiks = list_init (fun i  -> tuplify_expr (exik i)) (List.length ts) in
    let exi1s = list_init (fun i  -> exik i 1) (List.length ts) in
    let pxi1s = list_init (fun i  -> pxik i 1) (List.length ts) in
    let ps k = mkp (list_init (fun i  -> pxik i k) (List.length ts)) in
    let p = tuplify_patt ps in
    let e1 = mke exi1s in
    let es = List.map2 (fun x  -> expr_of_ty (Some x)) exiks ts in
    let e =
      List.fold_right2 (fun pxi1  e  acc  -> bind pxi1 e acc) pxi1s es
        (return e1) in
    McArr (_loc, p, (ExNil _loc), e)
  let mk_tuple expr_of_ty t =
    let mc =
      chain_tuple (fun ps  -> PaTup (_loc, (Ast.paCom_of_list ps)))
        (fun es  -> ExTup (_loc, (Ast.exCom_of_list es))) expr_of_ty
        (Ast.list_of_ctyp t []) in
    ExFun (_loc, mc)
  let default_match_case =
    let mk k = if k = 1 then PaId (_loc, (IdLid (_loc, "x"))) else PaAny _loc in
    match mode with
    | Fold_map  ->
        McArr
          (_loc, (tuplify_patt mk), (ExNil _loc),
            (ExTup
               (_loc,
                 (ExCom
                    (_loc, (ExId (_loc, (IdLid (_loc, "o")))),
                      (ExId (_loc, (IdLid (_loc, "x")))))))))
    | Fold  ->
        McArr
          (_loc, (PaAny _loc), (ExNil _loc),
            (ExId (_loc, (IdLid (_loc, "o")))))
    | Map  ->
        McArr
          (_loc, (tuplify_patt mk), (ExNil _loc),
            (ExId (_loc, (IdLid (_loc, "x")))))
  let default_expr = ExFun (_loc, default_match_case)
  let mkfuno e =
    match e with
    | ExApp (_loc,e,ExId (_,IdLid (_,"o"))) -> e
    | _ ->
        ExFun
          (_loc,
            (McArr
               (_loc, (PaId (_loc, (IdLid (_loc, "o")))), (ExNil _loc), e)))
  let is_unknown t =
    let rec loop t =
      match t with
      | TyId (_loc,IdLid (_,_)) -> false
      | TyId (_loc,_) -> true
      | TyApp (_loc,t,_) -> loop t
      | _ -> false in
    match t with | TyId (_loc,IdUid (_,_)) -> false | t -> loop t
  let contains_unknown t =
    try
      let (_ :< .. >)=
        (object 
           inherit  Ast.fold as super
           method! ctyp t = if is_unknown t then raise Exit else super#ctyp t
         end)#ctyp t in
      false
    with | Exit  -> true
  let opt_bind' ox e1 mk_e2 =
    let mk_e2 =
      match ox with
      | Some x -> (fun e1  -> ExApp (_loc, (mk_e2 e1), x))
      | _ -> mk_e2 in
    opt_bind (opt_map patt_of_expr ox) e1 mk_e2
  let opt_app e ox = match ox with | Some x -> ExApp (_loc, e, x) | _ -> e
  let rec expr_of_ty x ty =
    let rec self ?(arity= 0)  ox =
      function
      | t when is_unknown t ->
          self ox (TyId (_loc, (IdLid (_loc, "unknown"))))
      | TyId (_loc,IdLid (_,id)) ->
          let () = store_if_builtin_type id in
          opt_bind' ox (ExId (_loc, (IdLid (_loc, "o"))))
            (fun e1  -> ExSnd (_loc, e1, id))
      | TyApp (_loc,t1,t2) ->
          let e =
            opt_bind None (self ~arity:(arity + 1) None t1)
              (fun e1  -> ExApp (_loc, e1, (mkfuno (self None t2)))) in
          opt_app e ox
      | TyTup (_loc,t) -> opt_app (mk_tuple (self ~arity:0) t) ox
      | TyQuo (_loc,s) ->
          opt_app
            (ExApp
               (_loc, (ExId (_loc, (IdLid (_loc, ("_f_" ^ s))))),
                 (ExId (_loc, (IdLid (_loc, "o")))))) ox
      | _ -> self ox (TyId (_loc, (IdLid (_loc, "unknown")))) in
    self x ty
  and expr_of_ty' e t = expr_of_ty (Some e) t
  and out_constr_patt s = PaId (_loc, (IdUid (_loc, s)))
  and out_constr_expr s = ExId (_loc, (IdUid (_loc, s)))
  and match_case_of_constructor s t =
    chain_tuple (apply_patt (out_constr_patt s))
      (apply_expr (out_constr_expr s)) expr_of_ty (Ast.list_of_ctyp t [])
  and match_case_of_sum_type =
    function
    | TyOr (_loc,t1,t2) ->
        McOr (_loc, (match_case_of_sum_type t1), (match_case_of_sum_type t2))
    | TyOf (_loc,TyId (_,IdUid (_,s)),t) -> match_case_of_constructor s t
    | TyId (_loc,IdUid (_,s)) -> match_case_of_constructor s (TyNil _loc)
    | _ -> assert false
  and match_case_of_poly_constructor s ts =
    chain_tuple
      (function
       | [] -> PaVrn (_loc, s)
       | p::[] -> PaApp (_loc, (PaVrn (_loc, s)), p)
       | ps ->
           PaApp
             (_loc, (PaVrn (_loc, s)),
               (PaTup (_loc, (Ast.paCom_of_list ps)))))
      (function
       | [] -> ExVrn (_loc, s)
       | e::[] -> ExApp (_loc, (ExVrn (_loc, s)), e)
       | es ->
           ExApp
             (_loc, (ExVrn (_loc, s)),
               (ExTup (_loc, (Ast.exCom_of_list es))))) expr_of_ty ts
  and match_case_of_poly_sum_type =
    function
    | TyOr (_loc,t1,t2) ->
        McOr
          (_loc, (match_case_of_poly_sum_type t1),
            (match_case_of_poly_sum_type t2))
    | TyOf (_loc,TyVrn (_,i),TyTup (_,t)) ->
        match_case_of_poly_constructor i (Ast.list_of_ctyp t [])
    | TyOf (_loc,TyVrn (_,i),t) -> match_case_of_poly_constructor i [t]
    | TyVrn (_loc,i) -> match_case_of_poly_constructor i []
    | _ -> assert false
  and record_patt_of_type k =
    function
    | TyCol (_loc,TyId (_,IdLid (_,s)),_) ->
        PaEq
          (_loc, (IdLid (_loc, s)), (PaId (_loc, (IdLid (_loc, (xsk s k))))))
    | TySem (_loc,t1,t2) ->
        PaSem (_loc, (record_patt_of_type k t1), (record_patt_of_type k t2))
    | _ -> assert false
  and type_list_of_record_type t ((acc1,acc2) as acc) =
    match t with
    | TyNil _loc -> acc
    | TyCol (_loc,TyId (_,IdLid (_,s)),TyMut (_,t))|TyCol
        (_loc,TyId (_,IdLid (_,s)),t) -> ((s :: acc1), (t :: acc2))
    | TySem (_loc,t1,t2) ->
        type_list_of_record_type t1 (type_list_of_record_type t2 acc)
    | _ -> assert false
  and expr_of_record_type t =
    let (ls,ts) = type_list_of_record_type t ([], []) in
    let mkp ps =
      PaRec
        (_loc,
          (Ast.paSem_of_list
             (List.map2 (fun l  p  -> PaEq (_loc, (IdLid (_loc, l)), p)) ls
                ps))) in
    let mke es =
      ExRec
        (_loc,
          (Ast.rbSem_of_list
             (List.map2 (fun l  e  -> RbEq (_loc, (IdLid (_loc, l)), e)) ls
                es)), (ExNil _loc)) in
    chain_tuple mkp mke expr_of_ty ts
  and failure_match_case =
    McArr
      (_loc, (tuplify_patt (pxik 0)), (ExNil _loc),
        (ExApp
           (_loc,
             (ExSnd
                (_loc, (ExId (_loc, (IdLid (_loc, "o")))),
                  (sf "%s%d_failure" (string_of_mode mode) size))),
             (tuplify_expr (exik 0)))))
  and complete_match_case mk t =
    match t with
    | TyOr (_loc,_,_) when size > 1 ->
        McOr (_loc, (mk t), failure_match_case)
    | _ -> mk t
  and fun_of_ctyp tyid =
    function
    | TySum (_loc,t) ->
        ExFun (_loc, (complete_match_case match_case_of_sum_type t))
    | TyRec (_loc,t) -> ExFun (_loc, (expr_of_record_type t))
    | TyTup (_loc,t) -> mk_tuple expr_of_ty t
    | TyId (_loc,IdLid (_,i)) when i = tyid -> default_expr
    | TyApp (_loc,_,_)|TyArr (_loc,_,_)|TyQuo (_loc,_)|TyId (_loc,_) as t ->
        expr_of_ty None t
    | TyNil _loc -> expr_of_ty None (TyId (_loc, (IdLid (_loc, "unknown"))))
    | TyVrnEq (_loc,t)|TyVrnInf (_loc,t)|TyPrv (_loc,TyVrnInf (_,t)) ->
        ExFun (_loc, (complete_match_case match_case_of_poly_sum_type t))
    | TyVrnSup (_loc,t)|TyPrv (_loc,TyVrnSup (_,t)) ->
        if size > 1
        then
          ExFun (_loc, (complete_match_case match_case_of_poly_sum_type t))
        else
          ExFun
            (_loc,
              (McOr
                 (_loc, (match_case_of_poly_sum_type t), default_match_case)))
    | _ -> assert false
  and string_of_type_param t =
    match t with
    | TyQuo (_loc,s)|TyQuP (_loc,s)|TyQuM (_loc,s) -> s
    | _ -> assert false
  and method_of_type_decl _ ((id1,_,params,ctyp,priv) as type_decl) acc =
    let rec lambda acc =
      function
      | [] -> acc
      | x::xs ->
          lambda
            (ExFun
               (_loc,
                 (McArr
                    (_loc, (PaId (_loc, (IdLid (_loc, ("_f_" ^ x))))),
                      (ExNil _loc), acc)))) xs in
    let params' = List.map string_of_type_param params in
    let funs = lambda (fun_of_ctyp id1 ctyp) params' in
    let ty = method_type_of_type_decl type_decl in
    let priv = if priv then PrPrivate else PrNil in
    CrSem (_loc, (CrMth (_loc, id1, OvNil, priv, funs, ty)), acc)
  and ctyp_name_of_name_params name params =
    apply_ctyp (TyId (_loc, name)) params
  and method_type_of_type_decl (_,name,params,ctyp,_) =
    let t = ctyp_name_of_name_params name params in
    if (mode = Map) && (not (contains_unknown ctyp))
    then
      let out_params =
        List.map
          (function
           | TyQuo (_loc,i) -> TyQuo (_loc, (i ^ "_out"))
           | _ -> assert false) params in
      let t_out = ctyp_name_of_name_params name out_params in
      method_type_of_type t t_out params out_params
    else method_type_of_type t t params []
  and method_type_of_type t_in t_out params_in params_out =
    let rt t =
      match mode with
      | Fold_map  ->
          TyTup (_loc, (TySta (_loc, (TyQuo (_loc, "self_type")), t)))
      | Fold  -> TyQuo (_loc, "self_type")
      | Map  -> t in
    match (params_in, params_out) with
    | (param_in::[],param_out::[]) ->
        let alphas = tuplify_type param_in in
        TyPol
          (_loc, (TyApp (_loc, param_in, param_out)),
            (TyArr
               (_loc,
                 (TyArr
                    (_loc, (TyQuo (_loc, "self_type")),
                      (TyArr (_loc, alphas, (rt param_out))))),
                 (TyArr (_loc, (tuplify_type t_in), (rt t_out))))))
    | (param::[],[]) ->
        let alphas = tuplify_type param in
        TyPol
          (_loc, param,
            (TyArr
               (_loc,
                 (TyArr
                    (_loc, (TyQuo (_loc, "self_type")),
                      (TyArr (_loc, alphas, (rt param))))),
                 (TyArr (_loc, (tuplify_type t_in), (rt t_out))))))
    | ([],[]) -> TyArr (_loc, (tuplify_type t_in), (rt t_out))
    | _ ->
        let i = List.length params_in in
        failwith
          (Printf.sprintf
             "Camlp4FoldGenerator: FIXME not implemented for types with %d parameters"
             i)
  and class_sig_item_of_type_decl _ ((name,_,_,t,_) as type_decl) acc =
    let (_ :< .. >)=
      (object (self)
         inherit  Ast.fold as super
         method! ctyp =
           function
           | TyId (_loc,IdLid (_,id)) ->
               let () = store_if_builtin_type id in self
           | t -> super#ctyp t
       end)#ctyp t in
    CgSem
      (_loc,
        (CgMth (_loc, name, PrNil, (method_type_of_type_decl type_decl))),
        acc)
  and generate_structure tyMap =
    SMap.fold method_of_type_decl used_builtins.contents
      (SMap.fold method_of_type_decl tyMap (CrNil _loc))
  and generate_signature tyMap =
    SMap.fold class_sig_item_of_type_decl used_builtins.contents
      (SMap.fold class_sig_item_of_type_decl tyMap (CgNil _loc))
  end
let rec tyMap_of_type_decls t acc =
  match t with
  | TyNil _loc -> acc
  | TyAnd (_loc,t1,t2) -> tyMap_of_type_decls t1 (tyMap_of_type_decls t2 acc)
  | Ast.TyDcl (_,name,tl,tk,_) ->
      SMap.add name (name, (IdLid (_loc, name)), tl, tk, false) acc
  | _ -> assert false
let generate_class_implem mode c tydcl n =
  let tyMap = tyMap_of_type_decls tydcl SMap.empty in
  let module M = Gen(struct
    let size = n let mode = mode
    end) in
    let generated = M.generate_structure tyMap in
    let gen_type =
      TyPol
        (_loc, (TyApp (_loc, (TyQuo (_loc, "a")), (TyQuo (_loc, "b")))),
          (M.method_type_of_type (TyQuo (_loc, "a")) (TyQuo (_loc, "b")) []
             [])) in
    let failure =
      if n > 1
      then
        let name = string_of_mode mode in
        CrMth
          (_loc, (sf "%s%d_failure" name n), OvNil, PrNil,
            (ExFun
               (_loc,
                 (McArr
                    (_loc, (M.tuplify_patt (pxik 0)), (ExNil _loc),
                      (ExApp
                         (_loc, (ExId (_loc, (IdLid (_loc, "failwith")))),
                           (ExStr
                              (_loc,
                                (Ast.safe_string_escaped
                                   (sf "%s%d_failure: default implementation"
                                      name n)))))))))), gen_type)
      else CrNil _loc in
    let gen_type =
      TyPol
        (_loc, (TyQuo (_loc, "a")),
          (M.method_type_of_type (TyQuo (_loc, "a")) (TyQuo (_loc, "a")) []
             [])) in
    let unknown =
      CrMth (_loc, "unknown", OvNil, PrNil, M.default_expr, gen_type) in
    StCls
      (_loc,
        (CeEq
           (_loc, (CeCon (_loc, ViNil, (IdLid (_loc, c)), (TyNil _loc))),
             (CeStr
                (_loc,
                  (PaTyc
                     (_loc, (PaId (_loc, (IdLid (_loc, "o")))),
                       (TyQuo (_loc, "self_type")))),
                  (CrSem (_loc, generated, (CrSem (_loc, failure, unknown)))))))))
let generate_class_interf mode c tydcl n =
  let tyMap = tyMap_of_type_decls tydcl SMap.empty in
  let module M = Gen(struct
    let size = n let mode = mode
    end) in
    let generated = M.generate_signature tyMap in
    let gen_type =
      TyPol
        (_loc, (TyApp (_loc, (TyQuo (_loc, "a")), (TyQuo (_loc, "b")))),
          (M.method_type_of_type (TyQuo (_loc, "a")) (TyQuo (_loc, "b")) []
             [])) in
    let failure =
      if n > 1
      then
        let name = string_of_mode mode in
        CgMth (_loc, (sf "%s%d_failure" name n), PrNil, gen_type)
      else CgNil _loc in
    let gen_type =
      TyPol
        (_loc, (TyQuo (_loc, "a")),
          (M.method_type_of_type (TyQuo (_loc, "a")) (TyQuo (_loc, "a")) []
             [])) in
    let unknown = CgMth (_loc, "unknown", PrNil, gen_type) in
    SgCls
      (_loc,
        (CtCol
           (_loc, (CtCon (_loc, ViNil, (IdLid (_loc, c)), (TyNil _loc))),
             (CtSig
                (_loc, (TyQuo (_loc, "self_type")),
                  (CgSem (_loc, generated, (CgSem (_loc, failure, unknown)))))))))
let processor =
  let last = ref (TyNil _loc) in
  let generate_class' generator default c s n =
    match s with
    | "Fold" -> generator Fold c last.contents n
    | "Map" -> generator Map c last.contents n
    | "FoldMap" -> generator Fold_map c last.contents n
    | _ -> default in
  let generate_class_from_module_name generator c default m =
    try
      Scanf.sscanf m "Camlp4%[^G]Generator"
        (fun m'  ->
           try
             Scanf.sscanf m' "%[^0-9]%d"
               (generate_class' generator default c)
           with
           | End_of_file |Scanf.Scan_failure _ ->
               generate_class' generator default c m' 1)
    with | End_of_file |Scanf.Scan_failure _ -> default in
  object (self)
    inherit  Ast.map as super
    method! str_item st =
      match st with
      | StTyp (_loc,t) -> (last := t; st)
      | StCls
          (_loc,CeEq
           (_,CeCon (_,ViNil ,IdLid (_,c),TyNil _),CeCon
            (_,ViNil ,IdAcc
             (_,IdUid (_,"Filters"),IdAcc
              (_,IdUid (_,"GenerateFold"),IdLid (_,"generated"))),TyNil
             _)))
          -> generate_class_implem Fold c last.contents 1
      | StCls
          (_loc,CeEq
           (_,CeCon (_,ViNil ,IdLid (_,c),TyNil _),CeCon
            (_,ViNil ,IdAcc
             (_,IdUid (_,"Filters"),IdAcc
              (_,IdUid (_,"GenerateMap"),IdLid (_,"generated"))),TyNil
             _)))
          -> generate_class_implem Map c last.contents 1
      | StCls
          (_loc,CeEq
           (_,CeCon (_,ViNil ,IdLid (_,c),TyNil _),CeCon
            (_,ViNil ,IdAcc (_,IdUid (_,m),IdLid (_,"generated")),TyNil _)))
          -> generate_class_from_module_name generate_class_implem c st m
      | StSem (_loc,st1,st2) ->
          let st1 = self#str_item st1 in
          StSem (_loc, st1, (self#str_item st2))
      | st -> super#str_item st
    method! sig_item sg =
      match sg with
      | SgTyp (_loc,t) -> (last := t; sg)
      | SgCls
          (_loc,CtCol
           (_,CtCon (_,ViNil ,IdLid (_,c),TyNil _),CtCon
            (_,ViNil ,IdAcc
             (_,IdAcc (_,IdUid (_,"Filters"),IdUid (_,"GenerateFold")),IdLid
              (_,"generated")),TyNil
             _)))
          -> generate_class_interf Fold c last.contents 1
      | SgCls
          (_loc,CtCol
           (_,CtCon (_,ViNil ,IdLid (_,c),TyNil _),CtCon
            (_,ViNil ,IdAcc
             (_,IdAcc (_,IdUid (_,"Filters"),IdUid (_,"GenerateMap")),IdLid
              (_,"generated")),TyNil
             _)))
          -> generate_class_interf Map c last.contents 1
      | SgCls
          (_loc,CtCol
           (_,CtCon (_,ViNil ,IdLid (_,c),TyNil _),CtCon
            (_,ViNil ,IdAcc (_,IdUid (_,m),IdLid (_,"generated")),TyNil _)))
          -> generate_class_from_module_name generate_class_interf c sg m
      | SgSem (_loc,sg1,sg2) ->
          let sg1 = self#sig_item sg1 in
          SgSem (_loc, sg1, (self#sig_item sg2))
      | sg -> super#sig_item sg
  end
let _ = AstFilters.register_str_item_filter ("fold", (processor#str_item))
let _ = AstFilters.register_sig_item_filter ("fold", (processor#sig_item))
let _ =
  AstFilters.register_str_item_filter
    ("strip", (((new Ast.reloc) FanLoc.ghost)#str_item))
let decorate_binding decorate_fun =
  (object 
     inherit  Ast.map as super
     method! binding =
       function
       | BiEq (_loc,PaId (_,IdLid (_,id)),(ExFun (_,_) as e)) ->
           BiEq
             (_loc, (PaId (_loc, (IdLid (_loc, id)))), (decorate_fun id e))
       | b -> super#binding b
   end)#binding
let decorate decorate_fun =
  object (o)
    inherit  Ast.map as super
    method! str_item =
      function
      | StVal (_loc,r,b) ->
          StVal (_loc, r, (decorate_binding decorate_fun b))
      | st -> super#str_item st
    method! expr =
      function
      | ExLet (_loc,r,b,e) ->
          ExLet (_loc, r, (decorate_binding decorate_fun b), (o#expr e))
      | ExFun (_loc,_) as e -> decorate_fun "<fun>" e
      | e -> super#expr e
  end
let decorate_this_expr e id =
  let buf = Buffer.create 42 in
  let _loc = Ast.loc_of_expr e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  ExLet
    (_loc, ReNil,
      (BiEq
         (_loc, (PaId (_loc, (IdUid (_loc, "()")))),
           (ExApp
              (_loc,
                (ExId
                   (_loc,
                     (IdAcc
                        (_loc, (IdUid (_loc, "Camlp4prof")),
                          (IdLid (_loc, "count")))))),
                (ExStr (_loc, (Ast.safe_string_escaped s))))))), e)
let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_expr = decorate#expr in
  let decorate_match_case = decorate#match_case in
  function
  | ExFun (_loc,McArr (_,p,ExNil _,e)) ->
      ExFun (_loc, (McArr (_loc, p, (ExNil _loc), (decorate_fun id e))))
  | ExFun (_loc,m) ->
      decorate_this_expr (ExFun (_loc, (decorate_match_case m))) id
  | e -> decorate_this_expr (decorate_expr e) id
let _ =
  AstFilters.register_str_item_filter
    ("profile", ((decorate decorate_fun)#str_item))
let _ =
  AstFilters.register_str_item_filter
    ("trash",
      ((Ast.map_str_item
          (function | StMod (_loc,"Camlp4Trash",_) -> StNil _loc | st -> st))#str_item))
type t = 
  {
  name: Ast.ident;
  type_decls: Ast.ctyp SMap.t;
  acc: Ast.expr;
  app: Ast.expr;
  id: Ast.expr;
  tup: Ast.expr;
  com: Ast.expr;
  str: Ast.expr;
  int: Ast.expr;
  flo: Ast.expr;
  chr: Ast.expr;
  ant: Ast.ident} 
let _loc = FanLoc.ghost
let x i = IdLid (_loc, ("x" ^ (string_of_int i)))
let meta_ s = IdLid (_loc, ("meta_" ^ s))
let mf_ s = "mf_" ^ s
let rec string_of_ident =
  function
  | IdLid (_loc,s) -> s
  | IdUid (_loc,s) -> s
  | IdAcc (_loc,i1,i2) ->
      "acc_" ^ ((string_of_ident i1) ^ ("_" ^ (string_of_ident i2)))
  | IdApp (_loc,i1,i2) ->
      "app_" ^ ((string_of_ident i1) ^ ("_" ^ (string_of_ident i2)))
  | IdAnt (_loc,_) -> assert false
let fold_args ty f init =
  let (_,res) =
    List.fold_left (fun (i,acc)  ty  -> ((succ i), (f ty i acc))) (0, init)
      ty in
  res
let fold_data_ctors ty f init =
  let rec loop acc t =
    match t with
    | TyOf (_loc,TyId (_,IdUid (_,cons)),ty) ->
        f cons (Ast.list_of_ctyp ty []) acc
    | TyId (_loc,IdUid (_,cons)) -> f cons [] acc
    | TyOr (_loc,t1,t2) -> loop (loop acc t1) t2
    | TyNil _loc -> acc
    | _ -> assert false in
  loop init ty
let fold_type_decls m f init = SMap.fold f m.type_decls init
let patt_of_data_ctor_decl cons tyargs =
  fold_args tyargs
    (fun _  i  acc  -> PaApp (_loc, acc, (PaId (_loc, (x i)))))
    (PaId (_loc, cons))
let expr_of_data_ctor_decl cons tyargs =
  fold_args tyargs
    (fun _  i  acc  -> ExApp (_loc, acc, (ExId (_loc, (x i)))))
    (ExId (_loc, cons))
let is_antiquot_data_ctor s =
  let ls = String.length s in (ls > 3) && ((String.sub s (ls - 3) 3) = "Ant")
let rec meta_ident m =
  function
  | IdAcc (_loc,i1,i2) ->
      ExApp
        (_loc,
          (ExApp
             (_loc,
               (ExApp
                  (_loc,
                    (ExId
                       (_loc,
                         (IdAcc
                            (_loc, (IdUid (_loc, "Ast")),
                              (IdUid (_loc, "IdAcc")))))),
                    (ExId (_loc, (IdLid (_loc, "_loc")))))),
               (meta_ident m i1))), (meta_ident m i2))
  | IdApp (_loc,i1,i2) ->
      ExApp
        (_loc,
          (ExApp
             (_loc,
               (ExApp
                  (_loc,
                    (ExId
                       (_loc,
                         (IdAcc
                            (_loc, (IdUid (_loc, "Ast")),
                              (IdUid (_loc, "IdApp")))))),
                    (ExId (_loc, (IdLid (_loc, "_loc")))))),
               (meta_ident m i1))), (meta_ident m i2))
  | IdAnt (_loc,s) -> ExAnt (_loc, s)
  | IdLid (_loc,s) ->
      ExApp
        (_loc,
          (ExApp
             (_loc,
               (ExId
                  (_loc,
                    (IdAcc
                       (_loc, (IdUid (_loc, "Ast")), (IdUid (_loc, "IdLid")))))),
               (ExId (_loc, (IdLid (_loc, "_loc")))))), (ExStr (_loc, s)))
  | IdUid (_loc,s) ->
      ExApp
        (_loc,
          (ExApp
             (_loc,
               (ExId
                  (_loc,
                    (IdAcc
                       (_loc, (IdUid (_loc, "Ast")), (IdUid (_loc, "IdUid")))))),
               (ExId (_loc, (IdLid (_loc, "_loc")))))), (ExStr (_loc, s)))
let m_app m x y =
  ExApp
    (_loc,
      (ExApp
         (_loc,
           (ExApp (_loc, (m.app), (ExId (_loc, (IdLid (_loc, "_loc")))))), x)),
      y)
let m_id m i =
  ExApp
    (_loc, (ExApp (_loc, (m.id), (ExId (_loc, (IdLid (_loc, "_loc")))))), i)
let m_uid m s = m_id m (meta_ident m (IdUid (_loc, s)))
let m_lid m s = m_id m (meta_ident m (IdLid (_loc, s)))
let failure =
  ExApp
    (_loc, (ExId (_loc, (IdLid (_loc, "raise")))),
      (ExApp
         (_loc, (ExId (_loc, (IdUid (_loc, "Failure")))),
           (ExStr (_loc, "MetaGenerator: cannot handle that kind of types")))))
let mk_meta m =
  let m_name_uid x = IdAcc (_loc, (m.name), (IdUid (_loc, x))) in
  fold_type_decls m
    (fun tyname  tydcl  binding_acc  ->
       match tydcl with
       | Ast.TyDcl (_,_,tyvars,TySum (_loc,ty),_) ->
           let match_case =
             fold_data_ctors ty
               (fun cons  tyargs  acc  ->
                  let m_name_cons = m_name_uid cons in
                  let init = m_id m (meta_ident m m_name_cons) in
                  let p = patt_of_data_ctor_decl m_name_cons tyargs in
                  let e =
                    if
                      List.mem cons
                        ["BAnt";
                        "OAnt";
                        "LAnt";
                        "ReAnt";
                        "DiAnt";
                        "MuAnt";
                        "PrAnt";
                        "ViAnt";
                        "OvAnt";
                        "RvAnt"]
                    then
                      ExApp
                        (_loc,
                          (ExApp
                             (_loc, (ExId (_loc, (m.ant))),
                               (ExId (_loc, (IdLid (_loc, "_loc")))))),
                          (ExId (_loc, (IdLid (_loc, "x0")))))
                    else
                      if is_antiquot_data_ctor cons
                      then expr_of_data_ctor_decl m.ant tyargs
                      else
                        fold_args tyargs
                          (fun ty  i  acc  ->
                             let rec fcall_of_ctyp ty =
                               match ty with
                               | TyId (_loc,id) ->
                                   ExId (_loc, (meta_ (string_of_ident id)))
                               | TyTup (_loc,TySta (_,t1,t2)) ->
                                   ExFun
                                     (_loc,
                                       (McArr
                                          (_loc,
                                            (PaId
                                               (_loc, (IdLid (_loc, "_loc")))),
                                            (ExNil _loc),
                                            (ExFun
                                               (_loc,
                                                 (McArr
                                                    (_loc,
                                                      (PaTup
                                                         (_loc,
                                                           (PaCom
                                                              (_loc,
                                                                (PaId
                                                                   (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "x1")))),
                                                                (PaId
                                                                   (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "x2")))))))),
                                                      (ExNil _loc),
                                                      (ExApp
                                                         (_loc,
                                                           (ExApp
                                                              (_loc, (
                                                                m.tup),
                                                                (ExId
                                                                   (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                           (ExApp
                                                              (_loc,
                                                                (ExApp
                                                                   (_loc,
                                                                    (ExApp
                                                                    (_loc,
                                                                    (m.com),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (ExApp
                                                                    (_loc,
                                                                    (ExApp
                                                                    (_loc,
                                                                    (fcall_of_ctyp
                                                                    t1),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "x1")))))))),
                                                                (ExApp
                                                                   (_loc,
                                                                    (ExApp
                                                                    (_loc,
                                                                    (fcall_of_ctyp
                                                                    t2),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "x2")))))))))))))))))
                               | TyApp (_loc,t1,t2) ->
                                   ExApp
                                     (_loc, (fcall_of_ctyp t1),
                                       (fcall_of_ctyp t2))
                               | TyQuo (_loc,s) ->
                                   ExId (_loc, (IdLid (_loc, (mf_ s))))
                               | _ -> failure in
                             m_app m acc
                               (ExApp
                                  (_loc,
                                    (ExApp
                                       (_loc, (fcall_of_ctyp ty),
                                         (ExId (_loc, (IdLid (_loc, "_loc")))))),
                                    (ExId (_loc, (x i)))))) init in
                  McOr (_loc, (McArr (_loc, p, (ExNil _loc), e)), acc))
               (McNil _loc) in
           let funct =
             List.fold_right
               (fun tyvar  acc  ->
                  match tyvar with
                  | TyQuP (_loc,s)|TyQuM (_loc,s)|TyQuo (_loc,s) ->
                      ExFun
                        (_loc,
                          (McArr
                             (_loc, (PaId (_loc, (IdLid (_loc, (mf_ s))))),
                               (ExNil _loc), acc)))
                  | _ -> assert false) tyvars
               (ExFun
                  (_loc,
                    (McArr
                       (_loc, (PaId (_loc, (IdLid (_loc, "_loc")))),
                         (ExNil _loc), (ExFun (_loc, match_case)))))) in
           BiAnd
             (_loc, binding_acc,
               (BiEq
                  (_loc, (PaId (_loc, (IdLid (_loc, ("meta_" ^ tyname))))),
                    funct)))
       | Ast.TyDcl (_,_,_,_,_) -> binding_acc
       | _ -> assert false) (BiNil _loc)
let find_type_decls =
  object 
    inherit  Ast.fold as super
    val accu = SMap.empty
    method get = accu
    method! ctyp =
      function
      | Ast.TyDcl (_,name,_,_,_) as t -> {<accu = SMap.add name t accu>}
      | t -> super#ctyp t
  end
let filter st =
  let type_decls = lazy ((find_type_decls#str_item st)#get) in
  (object 
     inherit  Ast.map as super
     method! module_expr me =
       let mk_meta_module m =
         let bi = mk_meta m in
         MeStr
           (_loc,
             (StSem
                (_loc,
                  (StVal
                     (_loc, ReNil,
                       (BiEq
                          (_loc,
                            (PaId (_loc, (IdLid (_loc, "meta_string")))),
                            (ExFun
                               (_loc,
                                 (McArr
                                    (_loc,
                                      (PaId (_loc, (IdLid (_loc, "_loc")))),
                                      (ExNil _loc),
                                      (ExFun
                                         (_loc,
                                           (McArr
                                              (_loc,
                                                (PaId
                                                   (_loc,
                                                     (IdLid (_loc, "s")))),
                                                (ExNil _loc),
                                                (ExApp
                                                   (_loc,
                                                     (ExApp
                                                        (_loc, (m.str),
                                                          (ExId
                                                             (_loc,
                                                               (IdLid
                                                                  (_loc,
                                                                    "_loc")))))),
                                                     (ExApp
                                                        (_loc,
                                                          (ExId
                                                             (_loc,
                                                               (IdLid
                                                                  (_loc,
                                                                    "safe_string_escaped")))),
                                                          (ExId
                                                             (_loc,
                                                               (IdLid
                                                                  (_loc, "s")))))))))))))))))))),
                  (StSem
                     (_loc,
                       (StVal
                          (_loc, ReNil,
                            (BiEq
                               (_loc,
                                 (PaId (_loc, (IdLid (_loc, "meta_int")))),
                                 (ExFun
                                    (_loc,
                                      (McArr
                                         (_loc,
                                           (PaId
                                              (_loc, (IdLid (_loc, "_loc")))),
                                           (ExNil _loc),
                                           (ExFun
                                              (_loc,
                                                (McArr
                                                   (_loc,
                                                     (PaId
                                                        (_loc,
                                                          (IdLid (_loc, "s")))),
                                                     (ExNil _loc),
                                                     (ExApp
                                                        (_loc,
                                                          (ExApp
                                                             (_loc, (
                                                               m.int),
                                                               (ExId
                                                                  (_loc,
                                                                    (
                                                                    IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                          (ExId
                                                             (_loc,
                                                               (IdLid
                                                                  (_loc, "s")))))))))))))))))),
                       (StSem
                          (_loc,
                            (StVal
                               (_loc, ReNil,
                                 (BiEq
                                    (_loc,
                                      (PaId
                                         (_loc, (IdLid (_loc, "meta_float")))),
                                      (ExFun
                                         (_loc,
                                           (McArr
                                              (_loc,
                                                (PaId
                                                   (_loc,
                                                     (IdLid (_loc, "_loc")))),
                                                (ExNil _loc),
                                                (ExFun
                                                   (_loc,
                                                     (McArr
                                                        (_loc,
                                                          (PaId
                                                             (_loc,
                                                               (IdLid
                                                                  (_loc, "s")))),
                                                          (ExNil _loc),
                                                          (ExApp
                                                             (_loc,
                                                               (ExApp
                                                                  (_loc,
                                                                    (
                                                                    m.flo),
                                                                    (
                                                                    ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                               (ExId
                                                                  (_loc,
                                                                    (
                                                                    IdLid
                                                                    (_loc,
                                                                    "s")))))))))))))))))),
                            (StSem
                               (_loc,
                                 (StVal
                                    (_loc, ReNil,
                                      (BiEq
                                         (_loc,
                                           (PaId
                                              (_loc,
                                                (IdLid (_loc, "meta_char")))),
                                           (ExFun
                                              (_loc,
                                                (McArr
                                                   (_loc,
                                                     (PaId
                                                        (_loc,
                                                          (IdLid
                                                             (_loc, "_loc")))),
                                                     (ExNil _loc),
                                                     (ExFun
                                                        (_loc,
                                                          (McArr
                                                             (_loc,
                                                               (PaId
                                                                  (_loc,
                                                                    (
                                                                    IdLid
                                                                    (_loc,
                                                                    "s")))),
                                                               (ExNil _loc),
                                                               (ExApp
                                                                  (_loc,
                                                                    (
                                                                    ExApp
                                                                    (_loc,
                                                                    (m.chr),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (
                                                                    ExApp
                                                                    (_loc,
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdAcc
                                                                    (_loc,
                                                                    (IdUid
                                                                    (_loc,
                                                                    "String")),
                                                                    (IdLid
                                                                    (_loc,
                                                                    "escaped")))))),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "s")))))))))))))))))))),
                                 (StSem
                                    (_loc,
                                      (StVal
                                         (_loc, ReNil,
                                           (BiEq
                                              (_loc,
                                                (PaId
                                                   (_loc,
                                                     (IdLid
                                                        (_loc, "meta_bool")))),
                                                (ExFun
                                                   (_loc,
                                                     (McArr
                                                        (_loc,
                                                          (PaId
                                                             (_loc,
                                                               (IdLid
                                                                  (_loc,
                                                                    "_loc")))),
                                                          (ExNil _loc),
                                                          (ExFun
                                                             (_loc,
                                                               (McOr
                                                                  (_loc,
                                                                    (
                                                                    McArr
                                                                    (_loc,
                                                                    (PaId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "false")))),
                                                                    (ExNil
                                                                    _loc),
                                                                    (m_lid m
                                                                    "false"))),
                                                                    (
                                                                    McArr
                                                                    (_loc,
                                                                    (PaId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "true")))),
                                                                    (ExNil
                                                                    _loc),
                                                                    (m_lid m
                                                                    "true"))))))))))))))),
                                      (StSem
                                         (_loc,
                                           (StVal
                                              (_loc, ReRecursive,
                                                (BiEq
                                                   (_loc,
                                                     (PaId
                                                        (_loc,
                                                          (IdLid
                                                             (_loc,
                                                               "meta_list")))),
                                                     (ExFun
                                                        (_loc,
                                                          (McArr
                                                             (_loc,
                                                               (PaId
                                                                  (_loc,
                                                                    (
                                                                    IdLid
                                                                    (_loc,
                                                                    "mf_a")))),
                                                               (ExNil _loc),
                                                               (ExFun
                                                                  (_loc,
                                                                    (
                                                                    McArr
                                                                    (_loc,
                                                                    (PaId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))),
                                                                    (ExNil
                                                                    _loc),
                                                                    (ExFun
                                                                    (_loc,
                                                                    (McOr
                                                                    (_loc,
                                                                    (McArr
                                                                    (_loc,
                                                                    (PaId
                                                                    (_loc,
                                                                    (IdUid
                                                                    (_loc,
                                                                    "[]")))),
                                                                    (ExNil
                                                                    _loc),
                                                                    (m_uid m
                                                                    "[]"))),
                                                                    (McArr
                                                                    (_loc,
                                                                    (PaApp
                                                                    (_loc,
                                                                    (PaApp
                                                                    (_loc,
                                                                    (PaId
                                                                    (_loc,
                                                                    (IdUid
                                                                    (_loc,
                                                                    "::")))),
                                                                    (PaId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "x")))))),
                                                                    (PaId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "xs")))))),
                                                                    (ExNil
                                                                    _loc),
                                                                    (m_app m
                                                                    (m_app m
                                                                    (m_uid m
                                                                    "::")
                                                                    (ExApp
                                                                    (_loc,
                                                                    (ExApp
                                                                    (_loc,
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "mf_a")))),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "x")))))))
                                                                    (ExApp
                                                                    (_loc,
                                                                    (ExApp
                                                                    (_loc,
                                                                    (ExApp
                                                                    (_loc,
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "meta_list")))),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "mf_a")))))),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (ExId
                                                                    (_loc,
                                                                    (IdLid
                                                                    (_loc,
                                                                    "xs"))))))))))))))))))))))))),
                                           (StVal (_loc, ReRecursive, bi))))))))))))))) in
       match super#module_expr me with
       | MeApp
           (_loc,MeId
            (_,IdAcc (_,IdUid (_,"Filters"),IdUid (_,"MetaGeneratorExpr"))),MeId
            (_,i))
           ->
           mk_meta_module
             {
               name = i;
               type_decls = (Lazy.force type_decls);
               app =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExApp"))))));
               acc =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExAcc"))))));
               id =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExId"))))));
               tup =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExTup"))))));
               com =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExCom"))))));
               str =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExStr"))))));
               int =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExInt"))))));
               flo =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExFlo"))))));
               chr =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "ExChr"))))));
               ant =
                 (IdAcc
                    (_loc, (IdUid (_loc, "Ast")), (IdUid (_loc, "ExAnt"))))
             }
       | MeApp
           (_loc,MeId
            (_,IdAcc (_,IdUid (_,"Filters"),IdUid (_,"MetaGeneratorPatt"))),MeId
            (_,i))
           ->
           mk_meta_module
             {
               name = i;
               type_decls = (Lazy.force type_decls);
               app =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaApp"))))));
               acc =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaAcc"))))));
               id =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaId"))))));
               tup =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaTup"))))));
               com =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaCom"))))));
               str =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaStr"))))));
               int =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaInt"))))));
               flo =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaFlo"))))));
               chr =
                 (ExId
                    (_loc,
                      (IdAcc
                         (_loc, (IdUid (_loc, "Ast")),
                           (IdUid (_loc, "PaChr"))))));
               ant =
                 (IdAcc
                    (_loc, (IdUid (_loc, "Ast")), (IdUid (_loc, "PaAnt"))))
             }
       | me -> me
   end)#str_item st
let _ = AstFilters.register_str_item_filter ("meta", filter)
let map_expr =
  function
  | ExApp (_loc,e,ExId (_,IdUid (_,"NOTHING")))|ExFun
      (_loc,McArr (_,PaId (_,IdUid (_,"NOTHING")),ExNil _,e)) -> e
  | ExId (_loc,IdLid (_,"__FILE__")) ->
      ExStr (_loc, (Ast.safe_string_escaped (FanLoc.file_name _loc)))
  | ExId (_loc,IdLid (_,"__PWD__")) ->
      ExStr
        (_loc,
          (Ast.safe_string_escaped (Filename.dirname (FanLoc.file_name _loc))))
  | ExId (_loc,IdLid (_,"__LOCATION__")) ->
      let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple _loc in
      ExApp
        (_loc,
          (ExId
             (_loc,
               (IdAcc
                  (_loc, (IdUid (_loc, "FanLoc")),
                    (IdLid (_loc, "of_tuple")))))),
          (ExTup
             (_loc,
               (ExCom
                  (_loc, (ExStr (_loc, (Ast.safe_string_escaped a))),
                    (ExCom
                       (_loc,
                         (ExCom
                            (_loc,
                              (ExCom
                                 (_loc,
                                   (ExCom
                                      (_loc,
                                        (ExCom
                                           (_loc,
                                             (ExCom
                                                (_loc,
                                                  (ExInt
                                                     (_loc,
                                                       (string_of_int b))),
                                                  (ExInt
                                                     (_loc,
                                                       (string_of_int c))))),
                                             (ExInt (_loc, (string_of_int d))))),
                                        (ExInt (_loc, (string_of_int e))))),
                                   (ExInt (_loc, (string_of_int f))))),
                              (ExInt (_loc, (string_of_int g))))),
                         (if h
                          then ExId (_loc, (IdLid (_loc, "true")))
                          else ExId (_loc, (IdLid (_loc, "false")))))))))))
  | e -> e
let _ =
  AstFilters.register_str_item_filter
    ("trash_nothing", ((Ast.map_expr map_expr)#str_item))
let make_filter (s,code) =
  let f =
    function
    | StExp (_loc,ExId (_,IdLid (_,s'))) when s = s' -> code
    | e -> e in
  (("filter_" ^ s), ((Ast.map_str_item f)#str_item))