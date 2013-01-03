
open LibUtil;
open Ast;
module Ast = Camlp4Ast;
module MetaLoc = struct
   (* this makes sense here, because, for list operation
      you don't care about the location representation here
    *)
  let meta_loc_patt _loc _ = {:patt| loc |};
  let meta_loc_expr _loc _ = {:expr| loc |};
end;
(* module MetaAst = Ast.Meta.Make MetaLoc; *)
module MetaAst = FanAst.Make MetaLoc;
AstFilters.register_str_item_filter ("lift",(fun ast ->
  let _loc = Ast.loc_of_str_item ast in
  {:str_item| let loc = FanLoc.ghost in $(exp:MetaAst.Expr.meta_str_item _loc ast) |})); (* FIXME Loc => FanLoc*)



let add_debug_expr e =
  let _loc = Ast.loc_of_expr e in
  let msg = "camlp4-debug: exc: %s at " ^ FanLoc.to_string _loc ^ "@." in
  {:expr|
      try $e  with
      [ XStream.Failure | Exit as exc -> raise exc
      | exc -> begin
          if Debug.mode "exc" then
            Format.eprintf $`str:msg (Printexc.to_string exc) else ();
          raise exc
        end ] |};

let rec map_match_case =
  fun
  [ {:match_case@_loc| $m1 | $m2 |} ->
      {:match_case| $(map_match_case m1) | $(map_match_case m2) |}
  | {:match_case@_loc| $pat:p when $w -> $e |} ->
      {:match_case@_loc| $pat:p when $w -> $(add_debug_expr e) |}
  | m -> m ];


AstFilters.register_str_item_filter ("exception",object
  inherit Ast.map as super;
  method! expr = fun
  [ {:expr@_loc| fun [ $m ] |}  -> {:expr| fun [ $(map_match_case m) ] |}
  | x -> super#expr x ];
  method! str_item = fun
  [ {:str_item| module Debug = $_ |} as st -> st
  | st -> super#str_item st ];
end#str_item);

let _loc = FanLoc.ghost;
let sf = Printf.sprintf;

let xik i k =
  let i =
    if i < 0 then assert false
    else if i = 0 then ""
    else sf "_i%d" i
  in
  let k =
    if k < 1 then assert false
    else if k = 1 then ""
    else sf "_k%d" k
  in
  sf "_x%s%s" i k;
let exik i k = {:expr| $(lid:xik i k) |};
let pxik i k = {:patt| $(lid:xik i k) |};
let elidk y k = {:expr| $(lid:sf "%s_%d" y k) |};
let plidk y k = {:patt| $(lid:sf "%s_%d" y k) |};

let xs s = "_x_" ^ s;
let xsk = sf "_x_%s_%d";
let exsk s k = {:expr| $(lid:xsk s k)|};

let rec apply_expr accu =
  fun
  [ [] -> accu
  | [x :: xs] ->
      let _loc = Ast.loc_of_expr x
      in apply_expr {:expr| $accu $x |} xs ];

let rec apply_patt accu =
  fun
  [ [] -> accu
  | [x :: xs] ->
      let _loc = Ast.loc_of_patt x
      in apply_patt {:patt| $accu $x |} xs ];

let rec apply_ctyp accu =
  fun
  [ [] -> accu
  | [x :: xs] ->
      let _loc = Ast.loc_of_ctyp x
      in apply_ctyp {:ctyp| $accu $x |} xs ];

let opt_map f = fun [ Some x -> Some (f x) | None -> None ];

let list_init f n =
  let rec self m =
    if m = n then []
    else [f m :: self (succ m)]
  in self 0;

let rec lid_of_ident sep =
  fun
  [ {:ident| $lid:s |} | {:ident| $uid:s |} -> s
  | {:ident| $i1.$i2 |} -> lid_of_ident sep i1 ^ sep ^ lid_of_ident sep i2
  | _ -> assert false ];

type type_decl = (string * Ast.ident * list Ast.ctyp * Ast.ctyp * bool);

let builtin_types =
  let tyMap = SMap.empty in
  let tyMap =
    (* FIXME make [bool] as built-in type, it seems not to affect since,
       [store_if_built_in] [bool], will not bring new types
     *)
    let abstr = ["string"; "int"; "float"; "int32"; "int64"; "nativeint"; "char";"bool"] in
    List.fold_right
      (fun name -> SMap.add name (name, {:ident| $lid:name |}, [], {:ctyp||}, false))
      abstr tyMap in
  let tyMap =
    let concr =
      [(* ("bool", {:ident|bool|}, [], {:ctyp| [ False | True ] |}, false); *)
       ("list", {:ident|list|}, [ {:ctyp| 'a |} ], {:ctyp| [ $(uid:"[]") | $(uid:"::") of 'a and list 'a ] |}, false);
       ("option", {:ident|option|}, [ {:ctyp| 'a |} ], {:ctyp| [ None | Some of 'a ] |}, false);
       ("ref", {:ident|ref|}, [ {:ctyp| 'a |} ], {:ctyp| { contents : 'a } |}, false)]
    in
    List.fold_right (fun ((name, _, _, _, _) as decl) -> SMap.add name decl) concr tyMap
  in
  tyMap;

let used_builtins = ref SMap.empty;

let store_if_builtin_type id =
  if SMap.mem id builtin_types then
    used_builtins := SMap.add id (SMap.find id builtin_types) !used_builtins
  else ();

type mode = [ Fold | Map | Fold_map ];

let string_of_mode = fun [ Fold -> "fold" | Map -> "map" | Fold_map -> "fold_map" ];

module Gen (X :
  sig
    val size : int;
    val mode : mode;
  end) =
  struct

    let size = X.size;
    let mode = X.mode;

    let tuplify_expr f =
      if size <= 0 then assert false
      else if size = 1 then f 1
      else
        let rec loop k =
          if k = 2 then f 2
          else {:expr| $(loop (k - 1)), $(f k) |}
        in {:expr| ($(f 1), $(loop size)) |};

    let tuplify_patt f =
      if size <= 0 then assert false
      else if size = 1 then f 1
      else
        let rec loop k =
          if k = 2 then f 2
          else {:patt| $(loop (k - 1)), $(f k) |}
        in {:patt| ($(f 1), $(loop size)) |};

    let xiks i = tuplify_expr (exik i);

    let tuplify_type typ =
      if size <= 0 then assert false
      else if size = 1 then typ
      else
        let rec loop k =
          if k = 2 then typ
          else {:ctyp| $(loop (k - 1)) * $typ |}
        in {:ctyp| ($typ * $(loop size)) |};

    let tuplify_tycon tycon = tuplify_type {:ctyp| $lid:tycon |};

    let rec patt_of_expr =
      fun
      [ {:expr||} -> {:patt||}
      | {:expr| $id:i |} -> {:patt| $id:i |}
      | {:expr| $e1, $e2 |} -> {:patt| $(patt_of_expr e1), $(patt_of_expr e2) |}
      | {:expr| $tup:e |} -> {:patt| $(tup:patt_of_expr e) |}
      | _ -> assert false ];

    let bind p e1 e2 =
      match mode with
      [ Fold_map -> {:expr| let (o, $p) = $e1 in $e2 |}
      | Map      -> {:expr| let $p = $e1 in $e2 |}
      | Fold     -> {:expr| let o = $e1 in $e2 |} ];

    let return e =
      match mode with
      [ Fold_map -> {:expr| (o, $e) |}
      | Map      -> e
      | Fold     -> {:expr|o|} ];

    let rec opt_bind opt_patt e1 mk_e2 =
      match e1 with
      [ {:expr| $id:_ |} | {:expr| $lid:_#$_ |} -> mk_e2 e1
      | {:expr| let $p1 = $e1 in $e2 |} ->
          {:expr| let $p1 = $e1 in $(opt_bind None e2 mk_e2) |}
      | _ ->
          let e2 = mk_e2 {:expr|o|} in
          match opt_patt with
          [ Some patt -> bind patt e1 e2
          | None -> {:expr| (fun o -> $e1) $e2 |} ] ];

      (* ts = [t1; ...; tN] *)
    let chain_tuple mkp mke expr_of_ty ts =
      (* exiks = [{|(x_i0_k1, ..., x_i0_kM)|}; ...; {|(x_iN_k1, ..., x_iN_kM)|}] *)
      let exiks = list_init (fun i -> tuplify_expr (exik i)) (List.length ts) in
      (* exi1s, pxi1s = [{|x_i0_k1|}; ...; {|x_iN_k1|}] *)
      let exi1s = list_init (fun i -> exik i 1) (List.length ts) in
      let pxi1s = list_init (fun i -> pxik i 1) (List.length ts) in
      let ps k = mkp (list_init (fun i -> pxik i k) (List.length ts)) in
      let p = tuplify_patt ps in
      let e1 = mke exi1s in
      let es = List.map2 (fun x -> expr_of_ty (Some x)) exiks ts in
      let e =
        List.fold_right2 begin fun pxi1 e acc ->
          bind pxi1 e acc
        end pxi1s es (return e1)
      in
      {:match_case| $pat:p -> $e |};

    let mk_tuple expr_of_ty t =
      let mc =
        chain_tuple
          (fun ps -> {:patt| $(tup:Ast.paCom_of_list ps) |})
          (fun es -> {:expr| $(tup:Ast.exCom_of_list es) |})
          expr_of_ty (Ast.list_of_ctyp t [])
      in {:expr| fun [ $mc ] |};

    let default_match_case =
      let mk k = if k = 1 then {:patt| x |} else {:patt| _ |} in
      match mode with
      [ Fold_map -> {:match_case| $(pat:tuplify_patt mk) -> (o, x) |}
      | Fold     -> {:match_case| _ -> o |}
      | Map      -> {:match_case| $(pat:tuplify_patt mk) -> x |} ];

    let default_expr = {:expr| fun [ $default_match_case ] |};

    let mkfuno e =
      match e with
      [ {:expr| $e o |} -> e
      | _ -> {:expr| fun o -> $e |} ];

    let is_unknown t =
      let rec loop t =
        match t with
        [ {:ctyp| $lid:_ |} -> false
        | {:ctyp| $id:_ |} -> true
        | {:ctyp| $t $_ |} -> loop t
        | _ -> false ]
      in
      match t with
      [ {:ctyp| $uid:_ |} -> false
      | t -> loop t ];

    let contains_unknown t =
      try
        let (_ : < .. >) =
          object
            inherit Ast.fold as super;
            method! ctyp t = if is_unknown t then raise Exit else super#ctyp t;
          end#ctyp t
        in false
      with [ Exit -> true ];

    let opt_bind' ox e1 mk_e2 =
      let mk_e2 =
        match ox with
        [ Some x -> fun e1 -> {:expr| $(mk_e2 e1) $x |}
        | _      -> mk_e2 ]
      in
      opt_bind (opt_map patt_of_expr ox) e1 mk_e2;

  (* FIXME finish me
    let rec is_simple =
      fun
      [ {:expr| $id:_$ |} -> true
      | {:expr| $e$#$_$ |} | {:expr| $tup:e$ |} -> is_simple e
      | {:expr| $e1$ $e2$ |} | {:expr| $e1$, $e2$ |} -> is_simple e1 && is_simple e2
      | _ -> false ];

    let app e1 e2 =
      let is_e1_simple = is_simple e1 in
      let is_e2_simple = is_simple e2 in
      if is_e1_simple then
        if is_e2_simple then {:expr| $e1$ $e2$ |}
        else let x = fresh "y" in {:expr| let $lid:y$ = $e2$ in $e1$ $lid:y$ |}
      else
        if is_e2_simple then
          let x = fresh "y" in {:expr| let $lid:y$ = $e1$ in $lid:y$ $e2$ |}
        else ; *)

    let opt_app e ox =
      match ox with
      [ Some x -> {:expr| $e $x |} (* call app *)
      | _ -> e ];

    let rec expr_of_ty x ty =
      let rec self ?(arity=0) ox =
        fun
        [ t when is_unknown t ->
            self ox {:ctyp| unknown |}
        | {:ctyp| $lid:id |} ->
            let () = store_if_builtin_type id in
            opt_bind' ox {:expr|o|} (fun e1 -> {:expr| $e1#$id |})
        | {:ctyp@_loc| $t1 $t2 |} ->
            let e = opt_bind None
                             (self ~arity:(arity+1) None t1)
                             (fun e1 -> {:expr| $e1 $(mkfuno (self None t2)) |}) in
            opt_app e ox
        | {:ctyp| ( $tup:t ) |} ->
            opt_app (mk_tuple (self ~arity:0) t) ox
        | {:ctyp| '$s |} ->
            opt_app {:expr| $(lid:"_f_" ^ s) o |} ox
        | _ ->
            self ox {:ctyp| unknown |} ]
      in self x ty

    and expr_of_ty' e t = expr_of_ty (Some e) t

    and out_constr_patt s =
      {:patt| $uid:s |}
      (* {:patt| `$s |}
      {:patt| M.$uid:s |} *)
    and out_constr_expr s =
      {:expr| $uid:s |}
      (* {:expr| `$s |}
      {:expr| M.$uid:s |} *)

  (* method term t =
      match t with
      | C(x1, ..., xn) ->
          let o, x1 = o#t1 x1 in
          let o, x2 = o#t2 x2 in
          ...
          let o, xn = o#tn xn in
          o, C(x1, ..., xn)
   *)

    (* s = C, t = t1 and ... and tN *)
    and match_case_of_constructor s t =
      chain_tuple
        (apply_patt (out_constr_patt s))
        (apply_expr (out_constr_expr s))
        expr_of_ty (Ast.list_of_ctyp t [])

    and match_case_of_sum_type =
      fun
      [ {:ctyp| $t1 | $t2 |} ->
           {:match_case| $(match_case_of_sum_type t1) | $(match_case_of_sum_type t2) |}
      | {:ctyp| $uid:s of $t |} -> match_case_of_constructor s t
      | {:ctyp| $uid:s |} -> match_case_of_constructor s {:ctyp||}
      | _ -> assert false ]

    and match_case_of_poly_constructor s ts =
      chain_tuple
        (fun [ [] -> {:patt| `$s |} | [p] -> {:patt| `$s $p |} | ps -> {:patt| `$s $(tup:Ast.paCom_of_list ps) |} ])
        (fun [ [] -> {:expr| `$s |} | [e] -> {:expr| `$s $e |} | es -> {:expr| `$s $(tup:Ast.exCom_of_list es) |} ])
        expr_of_ty ts

    and match_case_of_poly_sum_type =
      fun
      [ {:ctyp| $t1 | $t2 |} ->
           {:match_case| $(match_case_of_poly_sum_type t1) | $(match_case_of_poly_sum_type t2) |}
      | {:ctyp| `$i of ($tup:t) |} -> match_case_of_poly_constructor i (Ast.list_of_ctyp t [])
      | {:ctyp| `$i of $t |} -> match_case_of_poly_constructor i [t]
      | {:ctyp| `$i |} -> match_case_of_poly_constructor i []
      | _ -> assert false ]

    and record_patt_of_type k =
      fun
      [ {:ctyp| $lid:s : $_ |} ->
          {:patt| $lid:s = $(lid:xsk s k) |}
      | {:ctyp| $t1 ; $t2 |} ->
          {:patt| $(record_patt_of_type k t1); $(record_patt_of_type k t2) |}
      | _ -> assert false ]

    and type_list_of_record_type t ((acc1, acc2) as acc) =
      match t with
      [ {:ctyp||} -> acc
      | {:ctyp| $lid:s : mutable $t |} | {:ctyp| $lid:s : $t |} ->
            ([s :: acc1], [t :: acc2])
      | {:ctyp| $t1 ; $t2 |} ->
           type_list_of_record_type t1 (type_list_of_record_type t2 acc)
      | _ -> assert false ]

    and expr_of_record_type t =
      let (ls, ts) = type_list_of_record_type t ([], []) in
      let mkp ps = {:patt| { $(list:List.map2 (fun l p -> {:patt| $lid:l = $p |}) ls ps) } |} in
      let mke es = {:expr| { $(list:List.map2 (fun l e -> {:rec_binding| $lid:l = $e |}) ls es) } |} in
      chain_tuple mkp mke expr_of_ty ts

    and failure_match_case =
      {:match_case| $(pat:tuplify_patt (pxik 0)) ->
                      o#$(lid:sf "%s%d_failure" (string_of_mode mode) size) $(tuplify_expr (exik 0)) |}

    and complete_match_case mk t =
      match t with
      [ {:ctyp| $_ | $_ |} when size > 1 ->
          {:match_case| $(mk t) | $failure_match_case |}
      | _ -> mk t ]

    and fun_of_ctyp tyid =
      fun
      [ {:ctyp| [ $t ] |} ->
          {:expr| fun [ $(complete_match_case match_case_of_sum_type t) ] |}
      | {:ctyp| { $t } |} ->
          {:expr| fun [ $(expr_of_record_type t) ] |}
      | {:ctyp| ( $tup:t ) |} -> mk_tuple expr_of_ty t
      | {:ctyp| $lid:i |} when i = tyid -> default_expr
      | {:ctyp| $_ $_ |} | {:ctyp| $_ -> $_ |} | {:ctyp| '$_ |} | {:ctyp| $id:_ |} as t ->
          expr_of_ty None t
      | {:ctyp||} ->
          expr_of_ty None {:ctyp| unknown |}
      | {:ctyp| [ = $t ] |} | {:ctyp| [ < $t ] |} | {:ctyp| private [ < $t ] |} ->
          {:expr| fun [ $(complete_match_case match_case_of_poly_sum_type t) ] |}
      | {:ctyp| [ > $t ] |} | {:ctyp| private [ > $t ] |} ->
          if size > 1 then
            {:expr| fun [ $(complete_match_case match_case_of_poly_sum_type t) ] |}
          else
            {:expr| fun [ $(match_case_of_poly_sum_type t) | $default_match_case ] |}
      | _ -> assert false ]

    and string_of_type_param t =
      match t with
      [ {:ctyp| '$s |} | {:ctyp| +'$s |} | {:ctyp| -'$s |} -> s
      | _ -> assert false ]

    and method_of_type_decl _ ((id1, _, params, ctyp, priv) as type_decl) acc =
      let rec lambda acc =
        fun
        [ [] -> acc
        | [ x :: xs ] -> lambda {:expr| fun $(lid:"_f_" ^ x) -> $acc |} xs ] in
      let params' = List.map string_of_type_param params in
      let funs = lambda (fun_of_ctyp id1 ctyp) params' in
      let ty = method_type_of_type_decl type_decl in
      let priv = if priv then {:private_flag| private |} else {:private_flag||} in
      {:class_str_item| method $private:priv $lid:id1 : $ty = $funs; $acc |}

    and ctyp_name_of_name_params name params =
      apply_ctyp {:ctyp| $id:name |} params

    and method_type_of_type_decl (_, name, params, ctyp, _) =
      let t = ctyp_name_of_name_params name params in
      if mode = Map && not (contains_unknown ctyp) then
        let out_params = List.map (fun [ {:ctyp| '$i |} -> {:ctyp| '$(i^"_out") |} | _ -> assert false ]) params in
        let t_out = ctyp_name_of_name_params name out_params in
        method_type_of_type t t_out params out_params
      else
        method_type_of_type t t params []

    and method_type_of_type t_in t_out params_in params_out =
      let rt t =
        match mode with
        [ Fold_map -> {:ctyp| ('self_type * $t) |}
        | Fold     -> {:ctyp| 'self_type |}
        | Map      -> t ]
      in
      match (params_in, params_out) with
      [ ([param_in], [param_out]) ->
          let alphas = tuplify_type param_in in
          {:ctyp| ! $param_in $param_out . ('self_type -> $alphas -> $(rt param_out)) -> $(tuplify_type t_in) -> $(rt t_out) |}
      | ([param], []) ->
          let alphas = tuplify_type param in
          {:ctyp| ! $param . ('self_type -> $alphas -> $(rt param)) -> $(tuplify_type t_in) -> $(rt t_out) |}
      | ([], []) ->
          {:ctyp| $(tuplify_type t_in) -> $(rt t_out) |}
      | _ ->
          let i = List.length params_in in
          failwith (Printf.sprintf
                "Camlp4FoldGenerator: FIXME not implemented for types with %d parameters" i) ]

    and class_sig_item_of_type_decl _ ((name, _, _, t, _) as type_decl) acc =
      let (_ : < .. >) =
        object (self)
          inherit Ast.fold as super;
          method! ctyp =
            fun
            [ {:ctyp| $lid:id |} -> let () = store_if_builtin_type id in self
            | t -> super#ctyp t ];
        end#ctyp t in
      {:class_sig_item|
         method $lid:name : $(method_type_of_type_decl type_decl);
         $acc |}

    and generate_structure tyMap =
      SMap.fold method_of_type_decl !used_builtins
        (SMap.fold method_of_type_decl tyMap {:class_str_item||})

    and generate_signature tyMap =
      SMap.fold class_sig_item_of_type_decl !used_builtins
        (SMap.fold class_sig_item_of_type_decl tyMap {:class_sig_item||});

end;

let rec tyMap_of_type_decls t acc =
  match t with
  [ {:ctyp||} -> acc
  | {:ctyp| $t1 and $t2 |} ->
      tyMap_of_type_decls t1 (tyMap_of_type_decls t2 acc)
  | Ast.TyDcl (_, name, tl, tk, _) ->
      SMap.add name (name, {:ident| $lid:name |}, tl, tk, false) acc
  | _ -> assert false ];

let generate_class_implem mode c tydcl n =
  let tyMap = tyMap_of_type_decls tydcl SMap.empty in
  let module M = Gen(struct let size = n; let mode = mode; end) in
  let generated = M.generate_structure tyMap in
  let gen_type =
    {:ctyp| ! 'a 'b . $(M.method_type_of_type {:ctyp| 'a |} {:ctyp| 'b |} [] []) |}
  in
  let failure =
    if n > 1 then
      let name = string_of_mode mode in
      {:class_str_item| method $(lid:sf "%s%d_failure" name n) : $gen_type =
                          fun $(M.tuplify_patt (pxik 0)) ->
                            failwith $(`str:sf "%s%d_failure: default implementation" name n) |}
    else {:class_str_item||}
  in
  let gen_type =
    {:ctyp| ! 'a . $(M.method_type_of_type {:ctyp| 'a |} {:ctyp| 'a |} [] []) |}
  in
  let unknown =
    {:class_str_item| method unknown : $gen_type = $(M.default_expr) |}
  in
  {:str_item| class $lid:c = object (o : 'self_type) $generated; $failure; $unknown end |};

let generate_class_interf mode c tydcl n =
  let tyMap = tyMap_of_type_decls tydcl SMap.empty in
  let module M = Gen(struct let size = n; let mode = mode; end) in
  let generated = M.generate_signature tyMap in
  let gen_type =
    {:ctyp| ! 'a 'b . $(M.method_type_of_type {:ctyp| 'a |} {:ctyp| 'b |} [] []) |}
  in
  let failure =
    if n > 1 then
      let name = string_of_mode mode in
      {:class_sig_item| method $(lid:sf "%s%d_failure" name n) : $gen_type |}
    else {:class_sig_item||}
  in
  let gen_type =
    {:ctyp| ! 'a . $(M.method_type_of_type {:ctyp| 'a |} {:ctyp| 'a |} [] []) |}
  in
  let unknown =
    {:class_sig_item| method unknown : $gen_type |}
  in
  {:sig_item| class $lid:c : object ('self_type) $generated; $failure; $unknown end |};

let processor =
  let last = ref {:ctyp||} in
  let generate_class' generator default c s n =
    match s with
    [ "Fold"    -> generator Fold c !last n
    | "Map"     -> generator Map c !last n
    | "FoldMap" -> generator Fold_map c !last n
    | _ -> default ]
  in
  let generate_class_from_module_name generator c default m =
    try Scanf.sscanf m "Camlp4%[^G]Generator" begin fun m' ->
      try Scanf.sscanf m' "%[^0-9]%d" (generate_class' generator default c)
      with [ End_of_file | Scanf.Scan_failure _ -> generate_class' generator default c m' 1 ]
    end with [ End_of_file | Scanf.Scan_failure _ -> default ]
  in
  object (self)
    inherit Ast.map as super;

    method! str_item st =
      match st with
      [ {:str_item| type $t |} -> (last := t; st)

      (* backward compatibility *)
      | {:str_item@_loc| class $lid:c = Filters.GenerateFold.generated |} ->
            generate_class_implem Fold c !last 1
      | {:str_item@_loc| class $lid:c = Filters.GenerateMap.generated |} ->
            generate_class_implem Map c !last 1

      (* Handle Camlp4(Fold|Map|FoldMap)\d*Generator *)
      | {:str_item@_loc| class $lid:c = $uid:m.generated |} ->
            generate_class_from_module_name generate_class_implem c st m

      (* It's a hack to force to recurse on the left to right order *)
      | {:str_item| $st1; $st2 |} ->
           let st1 = self#str_item st1 in
            {:str_item| $st1; $(self#str_item st2) |}

      | st -> super#str_item st ];

    method! sig_item sg =
      match sg with
      [ {:sig_item| type $t |} -> (last := t; sg)

      (* backward compatibility *)
      | {:sig_item@_loc| class $lid:c : Filters.GenerateFold.generated |} ->
           generate_class_interf Fold c !last 1
      | {:sig_item@_loc| class $lid:c : Filters.GenerateMap.generated |} ->
           generate_class_interf Map c !last 1

      (* Handle Camlp4(Fold|Map|FoldMap)\d*Generator *)
      | {:sig_item@_loc| class $lid:c : $uid:m.generated |} ->
          generate_class_from_module_name generate_class_interf c sg m

      (* It's a hack to force to recurse on the left to right order *)
      | {:sig_item| $sg1; $sg2 |} ->
           let sg1 = self#sig_item sg1 in
            {:sig_item| $sg1; $(self#sig_item sg2) |}

      | sg -> super#sig_item sg ];
  end;

AstFilters.register_str_item_filter("fold",processor#str_item);
AstFilters.register_sig_item_filter ("fold",processor#sig_item);
AstFilters.register_str_item_filter ("strip",(new Ast.reloc  FanLoc.ghost)#str_item);

let decorate_binding decorate_fun = object
  inherit Ast.map as super;
  method! binding = fun
    [ {:binding| $lid:id = $( ({:expr@_| fun [ $_ ] |} as e)) |} ->
      {:binding| $lid:id = $(decorate_fun id e) |}
    | b -> super#binding b ];
  end#binding;

let decorate decorate_fun = object (o)
  inherit Ast.map as super;
  method! str_item = fun
    [ {:str_item@_loc| let $rec:r $b |} ->
      {:str_item| let $rec:r $(decorate_binding decorate_fun b) |}
    | st -> super#str_item st ];
  method! expr = fun
    [ {:expr@_loc| let $rec:r $b in $e |} ->
      {:expr| let $rec:r $(decorate_binding decorate_fun b) in $(o#expr e) |}
    | {:expr@_loc| fun [ $_ ] |} as e -> decorate_fun "<fun>" e
    | e -> super#expr e ];
end;

let decorate_this_expr e id =
  let buf = Buffer.create 42 in
  let _loc = Ast.loc_of_expr e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  {:expr| let () = Camlp4prof.count $`str:s in $e |};

let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_expr = decorate#expr in
  let decorate_match_case = decorate#match_case in
  fun
  [ {:expr@_loc| fun $p -> $e |} ->
      {:expr| fun $p -> $(decorate_fun id e) |}
  | {:expr@_loc| fun [ $m ] |} ->
      decorate_this_expr {:expr| fun [ $(decorate_match_case m) ] |} id
  | e -> decorate_this_expr (decorate_expr e) id ];

AstFilters.register_str_item_filter("profile", (decorate decorate_fun)#str_item);
AstFilters.register_str_item_filter
    ("trash",(Ast.map_str_item
      (fun
       [ {:str_item@_loc| module Camlp4Trash = $_ |} ->
            {:str_item||}
       | st -> st ]))#str_item);

type t = { name : Ast.ident;
    type_decls : SMap.t Ast.ctyp;
    acc : Ast.expr;
    app : Ast.expr;
    id  : Ast.expr;
    tup : Ast.expr;
    com : Ast.expr;
    str : Ast.expr;
    int : Ast.expr;
    flo : Ast.expr;
    chr : Ast.expr;
    ant : Ast.ident;
  };

let _loc = FanLoc.ghost;

let x i = {:ident| $(lid:"x"^string_of_int i) |};

let meta_ s = {:ident| $(lid:"meta_"^s) |};

let mf_ s = "mf_" ^ s;

let rec string_of_ident =
  fun
  [ {:ident| $lid:s |} -> s
  | {:ident| $uid:s |} -> s
  | {:ident| $i1.$i2 |} -> "acc_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2)
  | {:ident| ($i1 $i2) |} -> "app_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2)
  | {:ident| $anti:_ |} -> assert false ];

let fold_args ty f init =
  let (_, res) =
    List.fold_left begin fun (i, acc) ty ->
      (succ i, f ty i acc)
    end (0, init) ty
  in res;

let fold_data_ctors ty f init =
  let rec loop acc t =
    match t with
    [ {:ctyp| $uid:cons of $ty |} -> f cons (Ast.list_of_ctyp ty []) acc
    | {:ctyp| $uid:cons |} -> f cons [] acc
    | {:ctyp| $t1 | $t2 |} -> loop (loop acc t1) t2
    | {:ctyp||} -> acc
    | _ -> assert false ] in
  loop init ty;

let fold_type_decls m f init =
  SMap.fold f m.type_decls init;

let patt_of_data_ctor_decl cons tyargs =
  fold_args tyargs begin fun _ i acc ->
    {:patt| $acc $(id:x i) |}
  end {:patt| $id:cons |};

let expr_of_data_ctor_decl cons tyargs =
  fold_args tyargs begin fun _ i acc ->
    {:expr| $acc $(id:x i) |}
  end {:expr| $id:cons |};

let is_antiquot_data_ctor s =
  let ls = String.length s in
  ls > 3 && String.sub s (ls - 3) 3 = "Ant";

let rec meta_ident m =
  fun
  [ {:ident| $i1.$i2 |} -> {:expr| Ast.IdAcc _loc $(meta_ident m i1) $(meta_ident m i2) |}
  | {:ident| ($i1 $i2) |} -> {:expr| Ast.IdApp _loc $(meta_ident m i1) $(meta_ident m i2) |}
  | {:ident| $anti:s |}  -> {:expr| $anti:s |}
  | {:ident| $lid:s |}   -> {:expr| Ast.IdLid _loc $str:s |}
  | {:ident| $uid:s |}   -> {:expr| Ast.IdUid _loc $str:s |} ];
let m_app m x y = {:expr| $(m.app) _loc $x $y |}; (* take care $(m.app) is need*)
let m_id m i = {:expr| $(m.id) _loc $i |};
let m_uid m s = m_id m (meta_ident m {:ident| $uid:s |});
let m_lid m s = m_id m (meta_ident m {:ident| $lid:s |});
let failure = {:expr| raise (Failure "MetaGenerator: cannot handle that kind of types") |};

let mk_meta m =
  let m_name_uid x = {:ident| $(m.name).$uid:x |} in
  fold_type_decls m begin fun tyname tydcl binding_acc ->
    match tydcl with
    [ Ast.TyDcl (_, _, tyvars, {:ctyp| [ $ty] |}, _) ->
      let match_case =
        fold_data_ctors ty begin fun cons tyargs acc ->
          let m_name_cons = m_name_uid cons in
          let init = m_id m (meta_ident m m_name_cons) in
          let p = patt_of_data_ctor_decl m_name_cons tyargs in
          let e =
            if List.mem cons  ["BAnt"; "OAnt"; "LAnt"; "ReAnt"; "DiAnt";
                               "MuAnt"; "PrAnt"; "ViAnt"; "OvAnt"; "RvAnt"] then
              {:expr| $(id:m.ant) _loc x0 |}
            else if is_antiquot_data_ctor cons then
              expr_of_data_ctor_decl m.ant tyargs
            else
              fold_args tyargs begin fun ty i acc ->
                let rec fcall_of_ctyp ty =
                  match ty with
                  [ {:ctyp| $id:id |} ->
                      {:expr| $(id:meta_ (string_of_ident id)) |}
                  | {:ctyp| ($t1 * $t2) |} ->
                      {:expr| fun _loc (x1, x2) ->
                                $(m.tup) _loc
                                  ($(m.com) _loc
                                    ($(fcall_of_ctyp t1) _loc x1)
                                    ($(fcall_of_ctyp t2) _loc x2)) |}
                  | {:ctyp| $t1 $t2 |} ->
                      {:expr| $(fcall_of_ctyp t1) $(fcall_of_ctyp t2) |}
                  | {:ctyp| '$s |} -> {:expr| $(lid:mf_ s) |}
                  | _ -> failure ]
                in m_app m acc {:expr| $(fcall_of_ctyp ty) _loc $(id:x i) |}
              end init
          in {:match_case| $pat:p -> $e | $acc |}
        end {:match_case||} in
        let funct =
          List.fold_right begin fun tyvar acc ->
            match tyvar with
            [ {:ctyp| +'$s |} | {:ctyp| -'$s |} | {:ctyp| '$s |} ->
                {:expr| fun $(lid:mf_ s) -> $acc |}
            | _ -> assert false ]
          end tyvars {:expr| fun _loc -> fun [ $match_case ] |}
        in {:binding| $binding_acc and $(lid:"meta_"^tyname) = $funct |}
    | Ast.TyDcl (_, _, _, _, _) -> binding_acc
    | _ -> assert false ]
  end {:binding||};

let find_type_decls = object
  inherit Ast.fold as super;
  val accu = SMap.empty; 
  method get = accu;
  method! ctyp =
    fun
    [ Ast.TyDcl (_, name, _, _, _) as t -> {< accu = SMap.add name t accu >}
    | t -> super#ctyp t ];
end;

let filter st =
  let type_decls = lazy (find_type_decls#str_item st)#get in
  object
   inherit Ast.map as super;
   method! module_expr me =
     let mk_meta_module m =
       let bi = mk_meta m in
       {:module_expr|
        struct
          let meta_string _loc s = $(m.str) _loc (safe_string_escaped s);
          let meta_int _loc s = $(m.int) _loc s;
          let meta_float _loc s = $(m.flo) _loc s;
          let meta_char _loc s = $(m.chr) _loc (String.escaped s);
          let meta_bool _loc =
            fun
            [ false -> $(m_lid m "false") (* FIXME*)
            | true  -> $(m_lid m "true") ]; (* FIXME*)
          let rec meta_list mf_a _loc =
            fun
            [ [] -> $(m_uid m "[]")
            | [x :: xs] -> $(m_app m (m_app m (m_uid m "::") {:expr| mf_a _loc x |}) {:expr| meta_list mf_a _loc xs |}) ];
          let rec $bi;
        end |} in
     match super#module_expr me with
     [ {:module_expr| Filters.MetaGeneratorExpr $id:i |} ->
         mk_meta_module
           { name = i;
             type_decls = Lazy.force type_decls;
             app = {:expr| Ast.ExApp |};
             acc = {:expr| Ast.ExAcc |};
             id  = {:expr| Ast.ExId  |};
             tup = {:expr| Ast.ExTup |};
             com = {:expr| Ast.ExCom |};
             str = {:expr| Ast.ExStr |};
             int = {:expr| Ast.ExInt |};
             flo = {:expr| Ast.ExFlo |};
             chr = {:expr| Ast.ExChr |};
             ant = {:ident| Ast.ExAnt |}
           }
     | {:module_expr| Filters.MetaGeneratorPatt $id:i |} ->
         mk_meta_module
           { name = i;
             type_decls = Lazy.force type_decls;
             app = {:expr| Ast.PaApp |};
             acc = {:expr| Ast.PaAcc |};
             id  = {:expr| Ast.PaId  |};
             tup = {:expr| Ast.PaTup |};
             com = {:expr| Ast.PaCom |};
             str = {:expr| Ast.PaStr |};
             int = {:expr| Ast.PaInt |};
             flo = {:expr| Ast.PaFlo |};
             chr = {:expr| Ast.PaChr |};
             ant = {:ident| Ast.PaAnt |}
           }
     | me -> me ];
  end#str_item st;

AstFilters.register_str_item_filter ("meta",filter);

let map_expr = with "expr" fun
  [ {| $e NOTHING |} | {| fun [NOTHING  -> $e] |} -> e
  | {| __FILE__ |} -> {| $(`str:FanLoc.file_name _loc) |}
  | {| __PWD__ |} ->
      {|$(`str:Filename.dirname (FanLoc.file_name _loc) ) |}
  | {| __LOCATION__ |} ->
      let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple _loc in
      {| FanLoc.of_tuple
        ($`str:a, $`int:b, $`int:c, $`int:d,
         $`int:e, $`int:f, $`int:g,
         $(if h then {| true |} else {| false |} )) |}
  | e -> e];

AstFilters.register_str_item_filter ("trash_nothing",(Ast.map_expr map_expr)#str_item);
  
(* [s] should starts with "__" *)
let make_filter (s,code) =
  let f = with "str_item" fun
  [ {| $lid:s'|} when s =s' -> code
  | e -> e  ] in
  ("filter_"^s, (Ast.map_str_item f )#str_item);

