open LibUtil
module IdAstLifter = struct
  let name = "Camlp4AstLifter" let version = Sys.ocaml_version
  end
module MakeAstLifter(Syn:Sig.Camlp4Syntax) = struct
  module Ast = Camlp4Ast
  module MetaLoc =
    struct
    let meta_loc_patt _loc _ = Ast.PaId (_loc, (Ast.IdLid (_loc, "loc")))
    let meta_loc_expr _loc _ = Ast.ExId (_loc, (Ast.IdLid (_loc, "loc")))
    end module MetaAst = Ast.Meta.Make(MetaLoc)
  let _ =
    Syn.AstFilters.register_str_item_filter
      (fun ast  ->
         let _loc = Ast.loc_of_str_item ast in
         Ast.StExp
           (_loc,
             (Ast.ExLet
                (_loc, Ast.ReNil,
                  (Ast.BiEq
                     (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "loc")))),
                       (Ast.ExId
                          (_loc,
                            (Ast.IdAcc
                               (_loc, (Ast.IdUid (_loc, "FanLoc")),
                                 (Ast.IdLid (_loc, "ghost")))))))),
                  (MetaAst.Expr.meta_str_item _loc ast)))))
  end
module IdExceptionTracer = struct
  let name = "Camlp4ExceptionTracer" let version = Sys.ocaml_version
  end
module MakeExceptionTracer(Syn:Sig.Camlp4Syntax) =
  struct
  module Ast = Camlp4Ast
  let add_debug_expr e =
    let _loc = Ast.loc_of_expr e in
    let msg = "camlp4-debug: exc: %s at " ^ ((FanLoc.to_string _loc) ^ "@.") in
    Ast.ExTry
      (_loc, e,
        (Ast.McOr
           (_loc,
             (Ast.McArr
                (_loc,
                  (Ast.PaAli
                     (_loc,
                       (Ast.PaOrp
                          (_loc,
                            (Ast.PaId
                               (_loc,
                                 (Ast.IdAcc
                                    (_loc, (Ast.IdUid (_loc, "Stream")),
                                      (Ast.IdUid (_loc, "Failure")))))),
                            (Ast.PaId (_loc, (Ast.IdUid (_loc, "Exit")))))),
                       (Ast.PaId (_loc, (Ast.IdLid (_loc, "exc")))))),
                  (Ast.ExNil _loc),
                  (Ast.ExApp
                     (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
                       (Ast.ExId (_loc, (Ast.IdLid (_loc, "exc")))))))),
             (Ast.McArr
                (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "exc")))),
                  (Ast.ExNil _loc),
                  (Ast.ExSeq
                     (_loc,
                       (Ast.ExSem
                          (_loc,
                            (Ast.ExIfe
                               (_loc,
                                 (Ast.ExApp
                                    (_loc,
                                      (Ast.ExId
                                         (_loc,
                                           (Ast.IdAcc
                                              (_loc,
                                                (Ast.IdUid (_loc, "Debug")),
                                                (Ast.IdLid (_loc, "mode")))))),
                                      (Ast.ExStr (_loc, "exc")))),
                                 (Ast.ExApp
                                    (_loc,
                                      (Ast.ExApp
                                         (_loc,
                                           (Ast.ExId
                                              (_loc,
                                                (Ast.IdAcc
                                                   (_loc,
                                                     (Ast.IdUid
                                                        (_loc, "Format")),
                                                     (Ast.IdLid
                                                        (_loc, "eprintf")))))),
                                           (Ast.ExStr
                                              (_loc,
                                                (Ast.safe_string_escaped msg))))),
                                      (Ast.ExApp
                                         (_loc,
                                           (Ast.ExId
                                              (_loc,
                                                (Ast.IdAcc
                                                   (_loc,
                                                     (Ast.IdUid
                                                        (_loc, "Printexc")),
                                                     (Ast.IdLid
                                                        (_loc, "to_string")))))),
                                           (Ast.ExId
                                              (_loc,
                                                (Ast.IdLid (_loc, "exc")))))))),
                                 (Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))))),
                            (Ast.ExApp
                               (_loc,
                                 (Ast.ExId
                                    (_loc, (Ast.IdLid (_loc, "raise")))),
                                 (Ast.ExId (_loc, (Ast.IdLid (_loc, "exc")))))))))))))))
  let rec map_match_case =
    function
    | Ast.McOr (_loc,m1,m2) ->
        Ast.McOr (_loc, (map_match_case m1), (map_match_case m2))
    | Ast.McArr (_loc,p,w,e) -> Ast.McArr (_loc, p, w, (add_debug_expr e))
    | m -> m
  let filter =
    object 
      inherit  Ast.map as super
      method! expr =
        function
        | Ast.ExFun (_loc,m) -> Ast.ExFun (_loc, (map_match_case m))
        | x -> super#expr x
      method! str_item =
        function
        | Ast.StMod (_,"Debug",_) as st -> st
        | st -> super#str_item st
    end let _ = Syn.AstFilters.register_str_item_filter filter#str_item
  end
module IdFoldGenerator = struct
  let name = "Camlp4FoldGenerator" let version = Sys.ocaml_version
  end
module MakeFoldGenerator(Syn:Sig.Camlp4Syntax) = struct
  module Ast = Camlp4Ast let _loc = FanLoc.ghost let sf = Printf.sprintf
  let xik i k =
    let i = if i < 0 then assert false else if i = 0 then "" else sf "_i%d" i in
    let k = if k < 1 then assert false else if k = 1 then "" else sf "_k%d" k in
    sf "_x%s%s" i k
  let exik i k = Ast.ExId (_loc, (Ast.IdLid (_loc, (xik i k))))
  let pxik i k = Ast.PaId (_loc, (Ast.IdLid (_loc, (xik i k))))
  let elidk y k = Ast.ExId (_loc, (Ast.IdLid (_loc, (sf "%s_%d" y k))))
  let plidk y k = Ast.PaId (_loc, (Ast.IdLid (_loc, (sf "%s_%d" y k))))
  let xs s = "_x_" ^ s let xsk = sf "_x_%s_%d"
  let exsk s k = Ast.ExId (_loc, (Ast.IdLid (_loc, (xsk s k))))
  let rec apply_expr accu =
    function
    | [] -> accu
    | x::xs ->
        let _loc = Ast.loc_of_expr x in
        apply_expr (Ast.ExApp (_loc, accu, x)) xs
  let rec apply_patt accu =
    function
    | [] -> accu
    | x::xs ->
        let _loc = Ast.loc_of_patt x in
        apply_patt (Ast.PaApp (_loc, accu, x)) xs
  let rec apply_ctyp accu =
    function
    | [] -> accu
    | x::xs ->
        let _loc = Ast.loc_of_ctyp x in
        apply_ctyp (Ast.TyApp (_loc, accu, x)) xs
  let opt_map f = function | Some x -> Some (f x) | None  -> None
  let list_init f n =
    let rec self m = if m = n then [] else (f m) :: (self (succ m)) in self 0
  let rec lid_of_ident sep =
    function
    | Ast.IdLid (_,s)|Ast.IdUid (_,s) -> s
    | Ast.IdAcc (_,i1,i2) ->
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
           SMap.add name
             (name, (Ast.IdLid (_loc, name)), [], (Ast.TyNil _loc), false))
        abstr tyMap in
    let tyMap =
      let concr =
        [("list", (Ast.IdLid (_loc, "list")), [Ast.TyQuo (_loc, "a")],
           (Ast.TySum
              (_loc,
                (Ast.TyOr
                   (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, "[]")))),
                     (Ast.TyOf
                        (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, "::")))),
                          (Ast.TyAnd
                             (_loc, (Ast.TyQuo (_loc, "a")),
                               (Ast.TyApp
                                  (_loc,
                                    (Ast.TyId
                                       (_loc, (Ast.IdLid (_loc, "list")))),
                                    (Ast.TyQuo (_loc, "a")))))))))))), false);
        ("option", (Ast.IdLid (_loc, "option")), [Ast.TyQuo (_loc, "a")],
          (Ast.TySum
             (_loc,
               (Ast.TyOr
                  (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, "None")))),
                    (Ast.TyOf
                       (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, "Some")))),
                         (Ast.TyQuo (_loc, "a")))))))), false);
        ("ref", (Ast.IdLid (_loc, "ref")), [Ast.TyQuo (_loc, "a")],
          (Ast.TyRec
             (_loc,
               (Ast.TyCol
                  (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, "contents")))),
                    (Ast.TyQuo (_loc, "a")))))), false)] in
      List.fold_right (fun ((name,_,_,_,_) as decl)  -> SMap.add name decl)
        concr tyMap in
    tyMap let used_builtins = ref SMap.empty
  let store_if_builtin_type id =
    if SMap.mem id builtin_types
    then
      used_builtins :=
        (SMap.add id (SMap.find id builtin_types) used_builtins.contents)
    else () type mode =  
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
             if k = 2 then f 2 else Ast.ExCom (_loc, (loop (k - 1)), (f k)) in
           Ast.ExTup (_loc, (Ast.ExCom (_loc, (f 1), (loop size)))))
    let tuplify_patt f =
      if size <= 0
      then assert false
      else
        if size = 1
        then f 1
        else
          (let rec loop k =
             if k = 2 then f 2 else Ast.PaCom (_loc, (loop (k - 1)), (f k)) in
           Ast.PaTup (_loc, (Ast.PaCom (_loc, (f 1), (loop size)))))
    let xiks i = tuplify_expr (exik i)
    let tuplify_type typ =
      if size <= 0
      then assert false
      else
        if size = 1
        then typ
        else
          (let rec loop k =
             if k = 2 then typ else Ast.TySta (_loc, (loop (k - 1)), typ) in
           Ast.TyTup (_loc, (Ast.TySta (_loc, typ, (loop size)))))
    let tuplify_tycon tycon =
      tuplify_type (Ast.TyId (_loc, (Ast.IdLid (_loc, tycon))))
    let rec patt_of_expr =
      function
      | Ast.ExNil _ -> Ast.PaNil _loc
      | Ast.ExId (_,i) -> Ast.PaId (_loc, i)
      | Ast.ExCom (_,e1,e2) ->
          Ast.PaCom (_loc, (patt_of_expr e1), (patt_of_expr e2))
      | Ast.ExTup (_,e) -> Ast.PaTup (_loc, (patt_of_expr e))
      | _ -> assert false
    let bind p e1 e2 =
      match mode with
      | Fold_map  ->
          Ast.ExLet
            (_loc, Ast.ReNil,
              (Ast.BiEq
                 (_loc,
                   (Ast.PaTup
                      (_loc,
                        (Ast.PaCom
                           (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "o")))),
                             p)))), e1)), e2)
      | Map  -> Ast.ExLet (_loc, Ast.ReNil, (Ast.BiEq (_loc, p, e1)), e2)
      | Fold  ->
          Ast.ExLet
            (_loc, Ast.ReNil,
              (Ast.BiEq
                 (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "o")))), e1)), e2)
    let return e =
      match mode with
      | Fold_map  ->
          Ast.ExTup
            (_loc,
              (Ast.ExCom
                 (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "o")))), e)))
      | Map  -> e
      | Fold  -> Ast.ExId (_loc, (Ast.IdLid (_loc, "o")))
    let rec opt_bind opt_patt e1 mk_e2 =
      match e1 with
      | Ast.ExId (_,_)|Ast.ExSnd (_,Ast.ExId (_,Ast.IdLid (_,_)),_) ->
          mk_e2 e1
      | Ast.ExLet (_,Ast.ReNil ,Ast.BiEq (_,p1,e1),e2) ->
          Ast.ExLet
            (_loc, Ast.ReNil, (Ast.BiEq (_loc, p1, e1)),
              (opt_bind None e2 mk_e2))
      | _ ->
          let e2 = mk_e2 (Ast.ExId (_loc, (Ast.IdLid (_loc, "o")))) in
          (match opt_patt with
           | Some patt -> bind patt e1 e2
           | None  ->
               Ast.ExApp
                 (_loc,
                   (Ast.ExFun
                      (_loc,
                        (Ast.McArr
                           (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "o")))),
                             (Ast.ExNil _loc), e1)))), e2))
    let chain_tuple mkp mke expr_of_ty ts =
      let exiks =
        list_init (fun i  -> tuplify_expr (exik i)) (List.length ts) in
      let exi1s = list_init (fun i  -> exik i 1) (List.length ts) in
      let pxi1s = list_init (fun i  -> pxik i 1) (List.length ts) in
      let ps k = mkp (list_init (fun i  -> pxik i k) (List.length ts)) in
      let p = tuplify_patt ps in
      let e1 = mke exi1s in
      let es = List.map2 (fun x  -> expr_of_ty (Some x)) exiks ts in
      let e =
        List.fold_right2 (fun pxi1  e  acc  -> bind pxi1 e acc) pxi1s es
          (return e1) in
      Ast.McArr (_loc, p, (Ast.ExNil _loc), e)
    let mk_tuple expr_of_ty t =
      let mc =
        chain_tuple (fun ps  -> Ast.PaTup (_loc, (Ast.paCom_of_list ps)))
          (fun es  -> Ast.ExTup (_loc, (Ast.exCom_of_list es))) expr_of_ty
          (Ast.list_of_ctyp t []) in
      Ast.ExFun (_loc, mc)
    let default_match_case =
      let mk k =
        if k = 1
        then Ast.PaId (_loc, (Ast.IdLid (_loc, "x")))
        else Ast.PaAny _loc in
      match mode with
      | Fold_map  ->
          Ast.McArr
            (_loc, (tuplify_patt mk), (Ast.ExNil _loc),
              (Ast.ExTup
                 (_loc,
                   (Ast.ExCom
                      (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "o")))),
                        (Ast.ExId (_loc, (Ast.IdLid (_loc, "x")))))))))
      | Fold  ->
          Ast.McArr
            (_loc, (Ast.PaAny _loc), (Ast.ExNil _loc),
              (Ast.ExId (_loc, (Ast.IdLid (_loc, "o")))))
      | Map  ->
          Ast.McArr
            (_loc, (tuplify_patt mk), (Ast.ExNil _loc),
              (Ast.ExId (_loc, (Ast.IdLid (_loc, "x")))))
    let default_expr = Ast.ExFun (_loc, default_match_case)
    let mkfuno e =
      match e with
      | Ast.ExApp (_,e,Ast.ExId (_,Ast.IdLid (_,"o"))) -> e
      | _ ->
          Ast.ExFun
            (_loc,
              (Ast.McArr
                 (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "o")))),
                   (Ast.ExNil _loc), e)))
    let is_unknown t =
      let rec loop t =
        match t with
        | Ast.TyId (_,Ast.IdLid (_,_)) -> false
        | Ast.TyId (_,_) -> true
        | Ast.TyApp (_,t,_) -> loop t
        | _ -> false in
      match t with | Ast.TyId (_,Ast.IdUid (_,_)) -> false | t -> loop t
    let contains_unknown t =
      try
        let (_ :< .. >)=
          (object 
             inherit  Ast.fold as super
             method! ctyp t =
               if is_unknown t then raise Exit else super#ctyp t
           end)#ctyp t in
        false
      with | Exit  -> true
    let opt_bind' ox e1 mk_e2 =
      let mk_e2 =
        match ox with
        | Some x -> (fun e1  -> Ast.ExApp (_loc, (mk_e2 e1), x))
        | _ -> mk_e2 in
      opt_bind (opt_map patt_of_expr ox) e1 mk_e2
    let opt_app e ox =
      match ox with | Some x -> Ast.ExApp (_loc, e, x) | _ -> e
    let rec expr_of_ty x ty =
      let rec self ?(arity= 0)  ox =
        function
        | t when is_unknown t ->
            self ox (Ast.TyId (_loc, (Ast.IdLid (_loc, "unknown"))))
        | Ast.TyId (_,Ast.IdLid (_,id)) ->
            let () = store_if_builtin_type id in
            opt_bind' ox (Ast.ExId (_loc, (Ast.IdLid (_loc, "o"))))
              (fun e1  -> Ast.ExSnd (_loc, e1, id))
        | Ast.TyApp (_loc,t1,t2) ->
            let e =
              opt_bind None (self ~arity:(arity + 1) None t1)
                (fun e1  -> Ast.ExApp (_loc, e1, (mkfuno (self None t2)))) in
            opt_app e ox
        | Ast.TyTup (_,t) -> opt_app (mk_tuple (self ~arity:0) t) ox
        | Ast.TyQuo (_,s) ->
            opt_app
              (Ast.ExApp
                 (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, ("_f_" ^ s))))),
                   (Ast.ExId (_loc, (Ast.IdLid (_loc, "o")))))) ox
        | _ -> self ox (Ast.TyId (_loc, (Ast.IdLid (_loc, "unknown")))) in
      self x ty
    and expr_of_ty' e t = expr_of_ty (Some e) t
    and out_constr_patt s = Ast.PaId (_loc, (Ast.IdUid (_loc, s)))
    and out_constr_expr s = Ast.ExId (_loc, (Ast.IdUid (_loc, s)))
    and match_case_of_constructor s t =
      chain_tuple (apply_patt (out_constr_patt s))
        (apply_expr (out_constr_expr s)) expr_of_ty (Ast.list_of_ctyp t [])
    and match_case_of_sum_type =
      function
      | Ast.TyOr (_,t1,t2) ->
          Ast.McOr
            (_loc, (match_case_of_sum_type t1), (match_case_of_sum_type t2))
      | Ast.TyOf (_,Ast.TyId (_,Ast.IdUid (_,s)),t) ->
          match_case_of_constructor s t
      | Ast.TyId (_,Ast.IdUid (_,s)) ->
          match_case_of_constructor s (Ast.TyNil _loc)
      | _ -> assert false
    and match_case_of_poly_constructor s ts =
      chain_tuple
        (function
         | [] -> Ast.PaVrn (_loc, s)
         | p::[] -> Ast.PaApp (_loc, (Ast.PaVrn (_loc, s)), p)
         | ps ->
             Ast.PaApp
               (_loc, (Ast.PaVrn (_loc, s)),
                 (Ast.PaTup (_loc, (Ast.paCom_of_list ps)))))
        (function
         | [] -> Ast.ExVrn (_loc, s)
         | e::[] -> Ast.ExApp (_loc, (Ast.ExVrn (_loc, s)), e)
         | es ->
             Ast.ExApp
               (_loc, (Ast.ExVrn (_loc, s)),
                 (Ast.ExTup (_loc, (Ast.exCom_of_list es))))) expr_of_ty ts
    and match_case_of_poly_sum_type =
      function
      | Ast.TyOr (_,t1,t2) ->
          Ast.McOr
            (_loc, (match_case_of_poly_sum_type t1),
              (match_case_of_poly_sum_type t2))
      | Ast.TyOf (_,Ast.TyVrn (_,i),Ast.TyTup (_,t)) ->
          match_case_of_poly_constructor i (Ast.list_of_ctyp t [])
      | Ast.TyOf (_,Ast.TyVrn (_,i),t) ->
          match_case_of_poly_constructor i [t]
      | Ast.TyVrn (_,i) -> match_case_of_poly_constructor i []
      | _ -> assert false
    and record_patt_of_type k =
      function
      | Ast.TyCol (_,Ast.TyId (_,Ast.IdLid (_,s)),_) ->
          Ast.PaEq
            (_loc, (Ast.IdLid (_loc, s)),
              (Ast.PaId (_loc, (Ast.IdLid (_loc, (xsk s k))))))
      | Ast.TySem (_,t1,t2) ->
          Ast.PaSem
            (_loc, (record_patt_of_type k t1), (record_patt_of_type k t2))
      | _ -> assert false
    and type_list_of_record_type t ((acc1,acc2) as acc) =
      match t with
      | Ast.TyNil _ -> acc
      | Ast.TyCol (_,Ast.TyId (_,Ast.IdLid (_,s)),Ast.TyMut (_,t))|Ast.TyCol
          (_,Ast.TyId (_,Ast.IdLid (_,s)),t) -> ((s :: acc1), (t :: acc2))
      | Ast.TySem (_,t1,t2) ->
          type_list_of_record_type t1 (type_list_of_record_type t2 acc)
      | _ -> assert false
    and expr_of_record_type t =
      let (ls,ts) = type_list_of_record_type t ([], []) in
      let mkp ps =
        Ast.PaRec
          (_loc,
            (Ast.paSem_of_list
               (List.map2
                  (fun l  p  -> Ast.PaEq (_loc, (Ast.IdLid (_loc, l)), p)) ls
                  ps))) in
      let mke es =
        Ast.ExRec
          (_loc,
            (Ast.rbSem_of_list
               (List.map2
                  (fun l  e  -> Ast.RbEq (_loc, (Ast.IdLid (_loc, l)), e)) ls
                  es)), (Ast.ExNil _loc)) in
      chain_tuple mkp mke expr_of_ty ts
    and failure_match_case =
      Ast.McArr
        (_loc, (tuplify_patt (pxik 0)), (Ast.ExNil _loc),
          (Ast.ExApp
             (_loc,
               (Ast.ExSnd
                  (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "o")))),
                    (sf "%s%d_failure" (string_of_mode mode) size))),
               (tuplify_expr (exik 0)))))
    and complete_match_case mk t =
      match t with
      | Ast.TyOr (_,_,_) when size > 1 ->
          Ast.McOr (_loc, (mk t), failure_match_case)
      | _ -> mk t
    and fun_of_ctyp tyid =
      function
      | Ast.TySum (_,t) ->
          Ast.ExFun (_loc, (complete_match_case match_case_of_sum_type t))
      | Ast.TyRec (_,t) -> Ast.ExFun (_loc, (expr_of_record_type t))
      | Ast.TyTup (_,t) -> mk_tuple expr_of_ty t
      | Ast.TyId (_,Ast.IdLid (_,i)) when i = tyid -> default_expr
      | Ast.TyApp (_,_,_)|Ast.TyArr (_,_,_)|Ast.TyQuo (_,_)|Ast.TyId 
          (_,_) as t -> expr_of_ty None t
      | Ast.TyNil _ ->
          expr_of_ty None (Ast.TyId (_loc, (Ast.IdLid (_loc, "unknown"))))
      | Ast.TyVrnEq (_,t)|Ast.TyVrnInf (_,t)|Ast.TyPrv (_,Ast.TyVrnInf (_,t))
          ->
          Ast.ExFun
            (_loc, (complete_match_case match_case_of_poly_sum_type t))
      | Ast.TyVrnSup (_,t)|Ast.TyPrv (_,Ast.TyVrnSup (_,t)) ->
          if size > 1
          then
            Ast.ExFun
              (_loc, (complete_match_case match_case_of_poly_sum_type t))
          else
            Ast.ExFun
              (_loc,
                (Ast.McOr
                   (_loc, (match_case_of_poly_sum_type t),
                     default_match_case)))
      | _ -> assert false
    and string_of_type_param t =
      match t with
      | Ast.TyQuo (_,s)|Ast.TyQuP (_,s)|Ast.TyQuM (_,s) -> s
      | _ -> assert false
    and method_of_type_decl _ ((id1,_,params,ctyp,priv) as type_decl) acc =
      let rec lambda acc =
        function
        | [] -> acc
        | x::xs ->
            lambda
              (Ast.ExFun
                 (_loc,
                   (Ast.McArr
                      (_loc,
                        (Ast.PaId (_loc, (Ast.IdLid (_loc, ("_f_" ^ x))))),
                        (Ast.ExNil _loc), acc)))) xs in
      let params' = List.map string_of_type_param params in
      let funs = lambda (fun_of_ctyp id1 ctyp) params' in
      let ty = method_type_of_type_decl type_decl in
      let priv = if priv then Ast.PrPrivate else Ast.PrNil in
      Ast.CrSem
        (_loc, (Ast.CrMth (_loc, id1, Ast.OvNil, priv, funs, ty)), acc)
    and ctyp_name_of_name_params name params =
      apply_ctyp (Ast.TyId (_loc, name)) params
    and method_type_of_type_decl (_,name,params,ctyp,_) =
      let t = ctyp_name_of_name_params name params in
      if (mode = Map) && (not (contains_unknown ctyp))
      then
        let out_params =
          List.map
            (function
             | Ast.TyQuo (_,i) -> Ast.TyQuo (_loc, (i ^ "_out"))
             | _ -> assert false) params in
        let t_out = ctyp_name_of_name_params name out_params in
        method_type_of_type t t_out params out_params
      else method_type_of_type t t params []
    and method_type_of_type t_in t_out params_in params_out =
      let rt t =
        match mode with
        | Fold_map  ->
            Ast.TyTup
              (_loc, (Ast.TySta (_loc, (Ast.TyQuo (_loc, "self_type")), t)))
        | Fold  -> Ast.TyQuo (_loc, "self_type")
        | Map  -> t in
      match (params_in, params_out) with
      | (param_in::[],param_out::[]) ->
          let alphas = tuplify_type param_in in
          Ast.TyPol
            (_loc, (Ast.TyApp (_loc, param_in, param_out)),
              (Ast.TyArr
                 (_loc,
                   (Ast.TyArr
                      (_loc, (Ast.TyQuo (_loc, "self_type")),
                        (Ast.TyArr (_loc, alphas, (rt param_out))))),
                   (Ast.TyArr (_loc, (tuplify_type t_in), (rt t_out))))))
      | (param::[],[]) ->
          let alphas = tuplify_type param in
          Ast.TyPol
            (_loc, param,
              (Ast.TyArr
                 (_loc,
                   (Ast.TyArr
                      (_loc, (Ast.TyQuo (_loc, "self_type")),
                        (Ast.TyArr (_loc, alphas, (rt param))))),
                   (Ast.TyArr (_loc, (tuplify_type t_in), (rt t_out))))))
      | ([],[]) -> Ast.TyArr (_loc, (tuplify_type t_in), (rt t_out))
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
             | Ast.TyId (_,Ast.IdLid (_,id)) ->
                 let () = store_if_builtin_type id in self
             | t -> super#ctyp t
         end)#ctyp t in
      Ast.CgSem
        (_loc,
          (Ast.CgMth
             (_loc, name, Ast.PrNil, (method_type_of_type_decl type_decl))),
          acc)
    and generate_structure tyMap =
      SMap.fold method_of_type_decl used_builtins.contents
        (SMap.fold method_of_type_decl tyMap (Ast.CrNil _loc))
    and generate_signature tyMap =
      SMap.fold class_sig_item_of_type_decl used_builtins.contents
        (SMap.fold class_sig_item_of_type_decl tyMap (Ast.CgNil _loc))
    end
  let rec tyMap_of_type_decls t acc =
    match t with
    | Ast.TyNil _ -> acc
    | Ast.TyAnd (_,t1,t2) ->
        tyMap_of_type_decls t1 (tyMap_of_type_decls t2 acc)
    | Ast.TyDcl (_,name,tl,tk,_) ->
        SMap.add name (name, (Ast.IdLid (_loc, name)), tl, tk, false) acc
    | _ -> assert false
  let generate_class_implem mode c tydcl n =
    let tyMap = tyMap_of_type_decls tydcl SMap.empty in
    let module M = Gen(struct
      let size = n let mode = mode
      end) in
      let generated = M.generate_structure tyMap in
      let gen_type =
        Ast.TyPol
          (_loc,
            (Ast.TyApp
               (_loc, (Ast.TyQuo (_loc, "a")), (Ast.TyQuo (_loc, "b")))),
            (M.method_type_of_type (Ast.TyQuo (_loc, "a"))
               (Ast.TyQuo (_loc, "b")) [] [])) in
      let failure =
        if n > 1
        then
          let name = string_of_mode mode in
          Ast.CrMth
            (_loc, (sf "%s%d_failure" name n), Ast.OvNil, Ast.PrNil,
              (Ast.ExFun
                 (_loc,
                   (Ast.McArr
                      (_loc, (M.tuplify_patt (pxik 0)), (Ast.ExNil _loc),
                        (Ast.ExApp
                           (_loc,
                             (Ast.ExId (_loc, (Ast.IdLid (_loc, "failwith")))),
                             (Ast.ExStr
                                (_loc,
                                  (Ast.safe_string_escaped
                                     (sf
                                        "%s%d_failure: default implementation"
                                        name n)))))))))), gen_type)
        else Ast.CrNil _loc in
      let gen_type =
        Ast.TyPol
          (_loc, (Ast.TyQuo (_loc, "a")),
            (M.method_type_of_type (Ast.TyQuo (_loc, "a"))
               (Ast.TyQuo (_loc, "a")) [] [])) in
      let unknown =
        Ast.CrMth
          (_loc, "unknown", Ast.OvNil, Ast.PrNil, M.default_expr, gen_type) in
      Ast.StCls
        (_loc,
          (Ast.CeEq
             (_loc,
               (Ast.CeCon
                  (_loc, Ast.ViNil, (Ast.IdLid (_loc, c)), (Ast.TyNil _loc))),
               (Ast.CeStr
                  (_loc,
                    (Ast.PaTyc
                       (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "o")))),
                         (Ast.TyQuo (_loc, "self_type")))),
                    (Ast.CrSem
                       (_loc, generated,
                         (Ast.CrSem (_loc, failure, unknown)))))))))
  let generate_class_interf mode c tydcl n =
    let tyMap = tyMap_of_type_decls tydcl SMap.empty in
    let module M = Gen(struct
      let size = n let mode = mode
      end) in
      let generated = M.generate_signature tyMap in
      let gen_type =
        Ast.TyPol
          (_loc,
            (Ast.TyApp
               (_loc, (Ast.TyQuo (_loc, "a")), (Ast.TyQuo (_loc, "b")))),
            (M.method_type_of_type (Ast.TyQuo (_loc, "a"))
               (Ast.TyQuo (_loc, "b")) [] [])) in
      let failure =
        if n > 1
        then
          let name = string_of_mode mode in
          Ast.CgMth (_loc, (sf "%s%d_failure" name n), Ast.PrNil, gen_type)
        else Ast.CgNil _loc in
      let gen_type =
        Ast.TyPol
          (_loc, (Ast.TyQuo (_loc, "a")),
            (M.method_type_of_type (Ast.TyQuo (_loc, "a"))
               (Ast.TyQuo (_loc, "a")) [] [])) in
      let unknown = Ast.CgMth (_loc, "unknown", Ast.PrNil, gen_type) in
      Ast.SgCls
        (_loc,
          (Ast.CtCol
             (_loc,
               (Ast.CtCon
                  (_loc, Ast.ViNil, (Ast.IdLid (_loc, c)), (Ast.TyNil _loc))),
               (Ast.CtSig
                  (_loc, (Ast.TyQuo (_loc, "self_type")),
                    (Ast.CgSem
                       (_loc, generated,
                         (Ast.CgSem (_loc, failure, unknown)))))))))
  let processor =
    let last = ref (Ast.TyNil _loc) in
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
        | Ast.StTyp (_,t) -> (last := t; st)
        | Ast.StCls
            (_loc,Ast.CeEq
             (_,Ast.CeCon
              (_,Ast.ViNil ,Ast.IdLid (_,c),Ast.TyNil _),Ast.CeCon
              (_,Ast.ViNil ,Ast.IdAcc
               (_,Ast.IdUid (_,"Camlp4Filters"),Ast.IdAcc
                (_,Ast.IdUid (_,"GenerateFold"),Ast.IdLid (_,"generated"))),Ast.TyNil
               _)))
            -> generate_class_implem Fold c last.contents 1
        | Ast.StCls
            (_loc,Ast.CeEq
             (_,Ast.CeCon
              (_,Ast.ViNil ,Ast.IdLid (_,c),Ast.TyNil _),Ast.CeCon
              (_,Ast.ViNil ,Ast.IdAcc
               (_,Ast.IdUid (_,"Camlp4Filters"),Ast.IdAcc
                (_,Ast.IdUid (_,"GenerateMap"),Ast.IdLid (_,"generated"))),Ast.TyNil
               _)))
            -> generate_class_implem Map c last.contents 1
        | Ast.StCls
            (_loc,Ast.CeEq
             (_,Ast.CeCon
              (_,Ast.ViNil ,Ast.IdLid (_,c),Ast.TyNil _),Ast.CeCon
              (_,Ast.ViNil ,Ast.IdAcc
               (_,Ast.IdUid (_,m),Ast.IdLid (_,"generated")),Ast.TyNil _)))
            -> generate_class_from_module_name generate_class_implem c st m
        | Ast.StSem (_,st1,st2) ->
            let st1 = self#str_item st1 in
            Ast.StSem (_loc, st1, (self#str_item st2))
        | st -> super#str_item st
      method! sig_item sg =
        match sg with
        | Ast.SgTyp (_,t) -> (last := t; sg)
        | Ast.SgCls
            (_loc,Ast.CtCol
             (_,Ast.CtCon
              (_,Ast.ViNil ,Ast.IdLid (_,c),Ast.TyNil _),Ast.CtCon
              (_,Ast.ViNil ,Ast.IdAcc
               (_,Ast.IdAcc
                (_,Ast.IdUid (_,"Camlp4Filters"),Ast.IdUid
                 (_,"GenerateFold")),Ast.IdLid
                (_,"generated")),Ast.TyNil
               _)))
            -> generate_class_interf Fold c last.contents 1
        | Ast.SgCls
            (_loc,Ast.CtCol
             (_,Ast.CtCon
              (_,Ast.ViNil ,Ast.IdLid (_,c),Ast.TyNil _),Ast.CtCon
              (_,Ast.ViNil ,Ast.IdAcc
               (_,Ast.IdAcc
                (_,Ast.IdUid (_,"Camlp4Filters"),Ast.IdUid (_,"GenerateMap")),Ast.IdLid
                (_,"generated")),Ast.TyNil
               _)))
            -> generate_class_interf Map c last.contents 1
        | Ast.SgCls
            (_loc,Ast.CtCol
             (_,Ast.CtCon
              (_,Ast.ViNil ,Ast.IdLid (_,c),Ast.TyNil _),Ast.CtCon
              (_,Ast.ViNil ,Ast.IdAcc
               (_,Ast.IdUid (_,m),Ast.IdLid (_,"generated")),Ast.TyNil _)))
            -> generate_class_from_module_name generate_class_interf c sg m
        | Ast.SgSem (_,sg1,sg2) ->
            let sg1 = self#sig_item sg1 in
            Ast.SgSem (_loc, sg1, (self#sig_item sg2))
        | sg -> super#sig_item sg
    end let _ = Syn.AstFilters.register_str_item_filter processor#str_item
  let _ = Syn.AstFilters.register_sig_item_filter processor#sig_item
  end
module IdLocationStripper = struct
  let name = "Camlp4LocationStripper" let version = Sys.ocaml_version
  end
module MakeLocationStripper(Syn:Sig.Camlp4Syntax) =
  struct
  module Ast = Camlp4Ast
  let _ =
    Syn.AstFilters.register_str_item_filter
      (Ast.map_loc (fun _  -> FanLoc.ghost))#str_item
  end
module IdProfiler = struct
  let name = "Camlp4Profiler" let version = Sys.ocaml_version
  end
module MakeProfiler(Syn:Sig.Camlp4Syntax) = struct
  module Ast = Camlp4Ast
  let decorate_binding decorate_fun =
    (object 
       inherit  Ast.map as super
       method! binding =
         function
         | Ast.BiEq
             (_loc,Ast.PaId (_,Ast.IdLid (_,id)),(Ast.ExFun (_,_) as e)) ->
             Ast.BiEq
               (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, id)))),
                 (decorate_fun id e))
         | b -> super#binding b
     end)#binding
  let decorate decorate_fun =
    object (o)
      inherit  Ast.map as super
      method! str_item =
        function
        | Ast.StVal (_loc,r,b) ->
            Ast.StVal (_loc, r, (decorate_binding decorate_fun b))
        | st -> super#str_item st
      method! expr =
        function
        | Ast.ExLet (_loc,r,b,e) ->
            Ast.ExLet
              (_loc, r, (decorate_binding decorate_fun b), (o#expr e))
        | Ast.ExFun (_loc,_) as e -> decorate_fun "<fun>" e
        | e -> super#expr e
    end
  let decorate_this_expr e id =
    let buf = Buffer.create 42 in
    let _loc = Ast.loc_of_expr e in
    let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
    let s = Buffer.contents buf in
    Ast.ExLet
      (_loc, Ast.ReNil,
        (Ast.BiEq
           (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "()")))),
             (Ast.ExApp
                (_loc,
                  (Ast.ExId
                     (_loc,
                       (Ast.IdAcc
                          (_loc, (Ast.IdUid (_loc, "Camlp4prof")),
                            (Ast.IdLid (_loc, "count")))))),
                  (Ast.ExStr (_loc, (Ast.safe_string_escaped s))))))), e)
  let rec decorate_fun id =
    let decorate = decorate decorate_fun in
    let decorate_expr = decorate#expr in
    let decorate_match_case = decorate#match_case in
    function
    | Ast.ExFun (_loc,Ast.McArr (_,p,Ast.ExNil _,e)) ->
        Ast.ExFun
          (_loc,
            (Ast.McArr (_loc, p, (Ast.ExNil _loc), (decorate_fun id e))))
    | Ast.ExFun (_loc,m) ->
        decorate_this_expr (Ast.ExFun (_loc, (decorate_match_case m))) id
    | e -> decorate_this_expr (decorate_expr e) id
  let _ =
    Syn.AstFilters.register_str_item_filter (decorate decorate_fun)#str_item
  end
module IdTrashRemover = struct
  let name = "Camlp4TrashRemover" let version = Sys.ocaml_version
  end
module MakeTrashRemover(Syn:Sig.Camlp4Syntax) = struct
  module Ast = Camlp4Ast
  let _ =
    Syn.AstFilters.register_str_item_filter
      (Ast.map_str_item
         (function
          | Ast.StMod (_loc,"Camlp4Trash",_) -> Ast.StNil _loc
          | st -> st))#str_item
  end
module IdMetaGenerator = struct
  let name = "Camlp4MetaGenerator" let version = Sys.ocaml_version
  end
module MakeMetaGenerator(Syn:Sig.Camlp4Syntax) = struct
  module Ast = Camlp4Ast
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
    ant: Ast.ident}  let _loc = FanLoc.ghost
  let x i = Ast.IdLid (_loc, ("x" ^ (string_of_int i)))
  let meta_ s = Ast.IdLid (_loc, ("meta_" ^ s)) let mf_ s = "mf_" ^ s
  let rec string_of_ident =
    function
    | Ast.IdLid (_,s) -> s
    | Ast.IdUid (_,s) -> s
    | Ast.IdAcc (_,i1,i2) ->
        "acc_" ^ ((string_of_ident i1) ^ ("_" ^ (string_of_ident i2)))
    | Ast.IdApp (_,i1,i2) ->
        "app_" ^ ((string_of_ident i1) ^ ("_" ^ (string_of_ident i2)))
    | Ast.IdAnt (_,_) -> assert false
  let fold_args ty f init =
    let (_,res) =
      List.fold_left (fun (i,acc)  ty  -> ((succ i), (f ty i acc))) (0, init)
        ty in
    res
  let fold_data_ctors ty f init =
    let rec loop acc t =
      match t with
      | Ast.TyOf (_,Ast.TyId (_,Ast.IdUid (_,cons)),ty) ->
          f cons (Ast.list_of_ctyp ty []) acc
      | Ast.TyId (_,Ast.IdUid (_,cons)) -> f cons [] acc
      | Ast.TyOr (_,t1,t2) -> loop (loop acc t1) t2
      | Ast.TyNil _ -> acc
      | _ -> assert false in
    loop init ty let fold_type_decls m f init = SMap.fold f m.type_decls init
  let patt_of_data_ctor_decl cons tyargs =
    fold_args tyargs
      (fun _  i  acc  -> Ast.PaApp (_loc, acc, (Ast.PaId (_loc, (x i)))))
      (Ast.PaId (_loc, cons))
  let expr_of_data_ctor_decl cons tyargs =
    fold_args tyargs
      (fun _  i  acc  -> Ast.ExApp (_loc, acc, (Ast.ExId (_loc, (x i)))))
      (Ast.ExId (_loc, cons))
  let is_antiquot_data_ctor s =
    let ls = String.length s in
    (ls > 3) && ((String.sub s (ls - 3) 3) = "Ant")
  let rec meta_ident m =
    function
    | Ast.IdAcc (_,i1,i2) ->
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, "Ast")),
                                (Ast.IdUid (_loc, "IdAcc")))))),
                      (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
                 (meta_ident m i1))), (meta_ident m i2))
    | Ast.IdApp (_,i1,i2) ->
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExId
                         (_loc,
                           (Ast.IdAcc
                              (_loc, (Ast.IdUid (_loc, "Ast")),
                                (Ast.IdUid (_loc, "IdApp")))))),
                      (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
                 (meta_ident m i1))), (meta_ident m i2))
    | Ast.IdAnt (_,s) -> Ast.ExAnt (_loc, s)
    | Ast.IdLid (_,s) ->
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExId
                    (_loc,
                      (Ast.IdAcc
                         (_loc, (Ast.IdUid (_loc, "Ast")),
                           (Ast.IdUid (_loc, "IdLid")))))),
                 (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
            (Ast.ExStr (_loc, s)))
    | Ast.IdUid (_,s) ->
        Ast.ExApp
          (_loc,
            (Ast.ExApp
               (_loc,
                 (Ast.ExId
                    (_loc,
                      (Ast.IdAcc
                         (_loc, (Ast.IdUid (_loc, "Ast")),
                           (Ast.IdUid (_loc, "IdUid")))))),
                 (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
            (Ast.ExStr (_loc, s)))
  let m_app m x y =
    Ast.ExApp
      (_loc,
        (Ast.ExApp
           (_loc,
             (Ast.ExApp
                (_loc, (m.app),
                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), x)), y)
  let m_id m i =
    Ast.ExApp
      (_loc,
        (Ast.ExApp
           (_loc, (m.id), (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))), i)
  let m_uid m s = m_id m (meta_ident m (Ast.IdUid (_loc, s)))
  let m_lid m s = m_id m (meta_ident m (Ast.IdLid (_loc, s)))
  let failure =
    Ast.ExApp
      (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "raise")))),
        (Ast.ExApp
           (_loc, (Ast.ExId (_loc, (Ast.IdUid (_loc, "Failure")))),
             (Ast.ExStr
                (_loc, "MetaGenerator: cannot handle that kind of types")))))
  let mk_meta m =
    let m_name_uid x = Ast.IdAcc (_loc, (m.name), (Ast.IdUid (_loc, x))) in
    fold_type_decls m
      (fun tyname  tydcl  binding_acc  ->
         match tydcl with
         | Ast.TyDcl (_,_,tyvars,Ast.TySum (_,ty),_) ->
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
                        Ast.ExApp
                          (_loc,
                            (Ast.ExApp
                               (_loc, (Ast.ExId (_loc, (m.ant))),
                                 (Ast.ExId (_loc, (Ast.IdLid (_loc, "_loc")))))),
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "x0")))))
                      else
                        if is_antiquot_data_ctor cons
                        then expr_of_data_ctor_decl m.ant tyargs
                        else
                          fold_args tyargs
                            (fun ty  i  acc  ->
                               let rec fcall_of_ctyp ty =
                                 match ty with
                                 | Ast.TyId (_,id) ->
                                     Ast.ExId
                                       (_loc, (meta_ (string_of_ident id)))
                                 | Ast.TyTup (_,Ast.TySta (_,t1,t2)) ->
                                     Ast.ExFun
                                       (_loc,
                                         (Ast.McArr
                                            (_loc,
                                              (Ast.PaId
                                                 (_loc,
                                                   (Ast.IdLid (_loc, "_loc")))),
                                              (Ast.ExNil _loc),
                                              (Ast.ExFun
                                                 (_loc,
                                                   (Ast.McArr
                                                      (_loc,
                                                        (Ast.PaTup
                                                           (_loc,
                                                             (Ast.PaCom
                                                                (_loc,
                                                                  (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x1")))),
                                                                  (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x2")))))))),
                                                        (Ast.ExNil _loc),
                                                        (Ast.ExApp
                                                           (_loc,
                                                             (Ast.ExApp
                                                                (_loc,
                                                                  (m.tup),
                                                                  (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                             (Ast.ExApp
                                                                (_loc,
                                                                  (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (m.com),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (fcall_of_ctyp
                                                                    t1),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x1")))))))),
                                                                  (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (fcall_of_ctyp
                                                                    t2),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x2")))))))))))))))))
                                 | Ast.TyApp (_,t1,t2) ->
                                     Ast.ExApp
                                       (_loc, (fcall_of_ctyp t1),
                                         (fcall_of_ctyp t2))
                                 | Ast.TyQuo (_,s) ->
                                     Ast.ExId
                                       (_loc, (Ast.IdLid (_loc, (mf_ s))))
                                 | _ -> failure in
                               m_app m acc
                                 (Ast.ExApp
                                    (_loc,
                                      (Ast.ExApp
                                         (_loc, (fcall_of_ctyp ty),
                                           (Ast.ExId
                                              (_loc,
                                                (Ast.IdLid (_loc, "_loc")))))),
                                      (Ast.ExId (_loc, (x i)))))) init in
                    Ast.McOr
                      (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), e)), acc))
                 (Ast.McNil _loc) in
             let funct =
               List.fold_right
                 (fun tyvar  acc  ->
                    match tyvar with
                    | Ast.TyQuP (_,s)|Ast.TyQuM (_,s)|Ast.TyQuo (_,s) ->
                        Ast.ExFun
                          (_loc,
                            (Ast.McArr
                               (_loc,
                                 (Ast.PaId
                                    (_loc, (Ast.IdLid (_loc, (mf_ s))))),
                                 (Ast.ExNil _loc), acc)))
                    | _ -> assert false) tyvars
                 (Ast.ExFun
                    (_loc,
                      (Ast.McArr
                         (_loc,
                           (Ast.PaId (_loc, (Ast.IdLid (_loc, "_loc")))),
                           (Ast.ExNil _loc), (Ast.ExFun (_loc, match_case)))))) in
             Ast.BiAnd
               (_loc, binding_acc,
                 (Ast.BiEq
                    (_loc,
                      (Ast.PaId
                         (_loc, (Ast.IdLid (_loc, ("meta_" ^ tyname))))),
                      funct)))
         | Ast.TyDcl (_,_,_,_,_) -> binding_acc
         | _ -> assert false) (Ast.BiNil _loc)
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
           Ast.MeStr
             (_loc,
               (Ast.StSem
                  (_loc,
                    (Ast.StVal
                       (_loc, Ast.ReNil,
                         (Ast.BiEq
                            (_loc,
                              (Ast.PaId
                                 (_loc, (Ast.IdLid (_loc, "meta_string")))),
                              (Ast.ExFun
                                 (_loc,
                                   (Ast.McArr
                                      (_loc,
                                        (Ast.PaId
                                           (_loc, (Ast.IdLid (_loc, "_loc")))),
                                        (Ast.ExNil _loc),
                                        (Ast.ExFun
                                           (_loc,
                                             (Ast.McArr
                                                (_loc,
                                                  (Ast.PaId
                                                     (_loc,
                                                       (Ast.IdLid (_loc, "s")))),
                                                  (Ast.ExNil _loc),
                                                  (Ast.ExApp
                                                     (_loc,
                                                       (Ast.ExApp
                                                          (_loc, (m.str),
                                                            (Ast.ExId
                                                               (_loc,
                                                                 (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                       (Ast.ExApp
                                                          (_loc,
                                                            (Ast.ExId
                                                               (_loc,
                                                                 (Ast.IdLid
                                                                    (_loc,
                                                                    "safe_string_escaped")))),
                                                            (Ast.ExId
                                                               (_loc,
                                                                 (Ast.IdLid
                                                                    (_loc,
                                                                    "s")))))))))))))))))))),
                    (Ast.StSem
                       (_loc,
                         (Ast.StVal
                            (_loc, Ast.ReNil,
                              (Ast.BiEq
                                 (_loc,
                                   (Ast.PaId
                                      (_loc, (Ast.IdLid (_loc, "meta_int")))),
                                   (Ast.ExFun
                                      (_loc,
                                        (Ast.McArr
                                           (_loc,
                                             (Ast.PaId
                                                (_loc,
                                                  (Ast.IdLid (_loc, "_loc")))),
                                             (Ast.ExNil _loc),
                                             (Ast.ExFun
                                                (_loc,
                                                  (Ast.McArr
                                                     (_loc,
                                                       (Ast.PaId
                                                          (_loc,
                                                            (Ast.IdLid
                                                               (_loc, "s")))),
                                                       (Ast.ExNil _loc),
                                                       (Ast.ExApp
                                                          (_loc,
                                                            (Ast.ExApp
                                                               (_loc,
                                                                 (m.int),
                                                                 (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                            (Ast.ExId
                                                               (_loc,
                                                                 (Ast.IdLid
                                                                    (_loc,
                                                                    "s")))))))))))))))))),
                         (Ast.StSem
                            (_loc,
                              (Ast.StVal
                                 (_loc, Ast.ReNil,
                                   (Ast.BiEq
                                      (_loc,
                                        (Ast.PaId
                                           (_loc,
                                             (Ast.IdLid (_loc, "meta_float")))),
                                        (Ast.ExFun
                                           (_loc,
                                             (Ast.McArr
                                                (_loc,
                                                  (Ast.PaId
                                                     (_loc,
                                                       (Ast.IdLid
                                                          (_loc, "_loc")))),
                                                  (Ast.ExNil _loc),
                                                  (Ast.ExFun
                                                     (_loc,
                                                       (Ast.McArr
                                                          (_loc,
                                                            (Ast.PaId
                                                               (_loc,
                                                                 (Ast.IdLid
                                                                    (_loc,
                                                                    "s")))),
                                                            (Ast.ExNil _loc),
                                                            (Ast.ExApp
                                                               (_loc,
                                                                 (Ast.ExApp
                                                                    (_loc,
                                                                    (m.flo),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                 (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "s")))))))))))))))))),
                              (Ast.StSem
                                 (_loc,
                                   (Ast.StVal
                                      (_loc, Ast.ReNil,
                                        (Ast.BiEq
                                           (_loc,
                                             (Ast.PaId
                                                (_loc,
                                                  (Ast.IdLid
                                                     (_loc, "meta_char")))),
                                             (Ast.ExFun
                                                (_loc,
                                                  (Ast.McArr
                                                     (_loc,
                                                       (Ast.PaId
                                                          (_loc,
                                                            (Ast.IdLid
                                                               (_loc, "_loc")))),
                                                       (Ast.ExNil _loc),
                                                       (Ast.ExFun
                                                          (_loc,
                                                            (Ast.McArr
                                                               (_loc,
                                                                 (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "s")))),
                                                                 (Ast.ExNil
                                                                    _loc),
                                                                 (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (m.chr),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdAcc
                                                                    (_loc,
                                                                    (Ast.IdUid
                                                                    (_loc,
                                                                    "String")),
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "escaped")))))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "s")))))))))))))))))))),
                                   (Ast.StSem
                                      (_loc,
                                        (Ast.StVal
                                           (_loc, Ast.ReNil,
                                             (Ast.BiEq
                                                (_loc,
                                                  (Ast.PaId
                                                     (_loc,
                                                       (Ast.IdLid
                                                          (_loc, "meta_bool")))),
                                                  (Ast.ExFun
                                                     (_loc,
                                                       (Ast.McArr
                                                          (_loc,
                                                            (Ast.PaId
                                                               (_loc,
                                                                 (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))),
                                                            (Ast.ExNil _loc),
                                                            (Ast.ExFun
                                                               (_loc,
                                                                 (Ast.McOr
                                                                    (_loc,
                                                                    (Ast.McArr
                                                                    (_loc,
                                                                    (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "false")))),
                                                                    (Ast.ExNil
                                                                    _loc),
                                                                    (m_lid m
                                                                    "false"))),
                                                                    (Ast.McArr
                                                                    (_loc,
                                                                    (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "true")))),
                                                                    (Ast.ExNil
                                                                    _loc),
                                                                    (m_lid m
                                                                    "true"))))))))))))))),
                                        (Ast.StSem
                                           (_loc,
                                             (Ast.StVal
                                                (_loc, Ast.ReRecursive,
                                                  (Ast.BiEq
                                                     (_loc,
                                                       (Ast.PaId
                                                          (_loc,
                                                            (Ast.IdLid
                                                               (_loc,
                                                                 "meta_list")))),
                                                       (Ast.ExFun
                                                          (_loc,
                                                            (Ast.McArr
                                                               (_loc,
                                                                 (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "mf_a")))),
                                                                 (Ast.ExNil
                                                                    _loc),
                                                                 (Ast.ExFun
                                                                    (_loc,
                                                                    (Ast.McArr
                                                                    (_loc,
                                                                    (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))),
                                                                    (Ast.ExNil
                                                                    _loc),
                                                                    (Ast.ExFun
                                                                    (_loc,
                                                                    (Ast.McOr
                                                                    (_loc,
                                                                    (Ast.McArr
                                                                    (_loc,
                                                                    (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdUid
                                                                    (_loc,
                                                                    "[]")))),
                                                                    (Ast.ExNil
                                                                    _loc),
                                                                    (m_uid m
                                                                    "[]"))),
                                                                    (Ast.McArr
                                                                    (_loc,
                                                                    (Ast.PaApp
                                                                    (_loc,
                                                                    (Ast.PaApp
                                                                    (_loc,
                                                                    (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdUid
                                                                    (_loc,
                                                                    "::")))),
                                                                    (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x")))))),
                                                                    (Ast.PaId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "xs")))))),
                                                                    (Ast.ExNil
                                                                    _loc),
                                                                    (m_app m
                                                                    (m_app m
                                                                    (m_uid m
                                                                    "::")
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "mf_a")))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x")))))))
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExApp
                                                                    (_loc,
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "meta_list")))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "mf_a")))))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "_loc")))))),
                                                                    (Ast.ExId
                                                                    (_loc,
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "xs"))))))))))))))))))))))))),
                                             (Ast.StVal
                                                (_loc, Ast.ReRecursive, bi))))))))))))))) in
         match super#module_expr me with
         | Ast.MeApp
             (_,Ast.MeId
              (_,Ast.IdAcc
               (_,Ast.IdUid (_,"Camlp4Filters"),Ast.IdUid
                (_,"MetaGeneratorExpr"))),Ast.MeId
              (_,i))
             ->
             mk_meta_module
               {
                 name = i;
                 type_decls = (Lazy.force type_decls);
                 app =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExApp"))))));
                 acc =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExAcc"))))));
                 id =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExId"))))));
                 tup =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExTup"))))));
                 com =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExCom"))))));
                 str =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExStr"))))));
                 int =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExInt"))))));
                 flo =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExFlo"))))));
                 chr =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "ExChr"))))));
                 ant =
                   (Ast.IdAcc
                      (_loc, (Ast.IdUid (_loc, "Ast")),
                        (Ast.IdUid (_loc, "ExAnt"))))
               }
         | Ast.MeApp
             (_,Ast.MeId
              (_,Ast.IdAcc
               (_,Ast.IdUid (_,"Camlp4Filters"),Ast.IdUid
                (_,"MetaGeneratorPatt"))),Ast.MeId
              (_,i))
             ->
             mk_meta_module
               {
                 name = i;
                 type_decls = (Lazy.force type_decls);
                 app =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaApp"))))));
                 acc =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaAcc"))))));
                 id =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaId"))))));
                 tup =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaTup"))))));
                 com =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaCom"))))));
                 str =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaStr"))))));
                 int =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaInt"))))));
                 flo =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaFlo"))))));
                 chr =
                   (Ast.ExId
                      (_loc,
                        (Ast.IdAcc
                           (_loc, (Ast.IdUid (_loc, "Ast")),
                             (Ast.IdUid (_loc, "PaChr"))))));
                 ant =
                   (Ast.IdAcc
                      (_loc, (Ast.IdUid (_loc, "Ast")),
                        (Ast.IdUid (_loc, "PaAnt"))))
               }
         | me -> me
     end)#str_item st
  let _ = Syn.AstFilters.register_str_item_filter filter
  end
let f_lift ((module P)  : (module Sig.PRECAST)) =
  P.syntax_plugin (module IdAstLifter) (module MakeAstLifter)
let f_exn ((module P)  : (module Sig.PRECAST)) =
  P.syntax_plugin (module IdExceptionTracer) (module MakeExceptionTracer)
let f_prof ((module P)  : (module Sig.PRECAST)) =
  P.syntax_plugin (module IdProfiler) (module MakeProfiler)
let f_fold ((module P)  : (module Sig.PRECAST)) =
  P.syntax_plugin (module IdFoldGenerator) (module MakeFoldGenerator)
let f_striploc ((module P)  : (module Sig.PRECAST)) =
  P.syntax_plugin (module IdLocationStripper) (module MakeLocationStripper)
let f_trash ((module P)  : (module Sig.PRECAST)) =
  P.syntax_plugin (module IdTrashRemover) (module MakeTrashRemover)
let f_meta ((module P)  : (module Sig.PRECAST)) =
  P.syntax_plugin (module IdMetaGenerator) (module MakeMetaGenerator)