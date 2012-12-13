module Ast = Camlp4Ast
open LibUtil
open Format
open Basic
open FSig
let rec fa al =
  function | Ast.TyApp (_loc,f,a) -> fa (a :: al) f | f -> (f, al)
let rec to_var_list =
  function
  | Ast.TyApp (_loc,t1,t2) -> (to_var_list t1) @ (to_var_list t2)
  | Ast.TyQuo (_loc,s) -> [s]
  | _ -> assert false
let list_of_opt ot acc =
  match ot with | Ast.TyNil _loc -> acc | t -> Ast.list_of_ctyp t acc
let rec name_tags =
  function
  | Ast.TyApp (_loc,t1,t2) -> (name_tags t1) @ (name_tags t2)
  | Ast.TyVrn (_loc,s) -> [s]
  | _ -> assert false
let rec to_generalized =
  function
  | Ast.TyArr (_loc,t1,t2) ->
      let (tl,rt) = to_generalized t2 in ((t1 :: tl), rt)
  | t -> ([], t)
let to_string =
  ref
    (fun _  ->
       failwith "Ctyp.to_string foward declaration, not implemented yet")
let eprint: (Ast.ctyp -> unit) ref =
  ref
    (fun _  -> failwith "Ctyp.eprint foward declaration, not implemented yet")
let _loc = FanLoc.ghost
let app a b = Ast.TyApp (_loc, a, b)
let comma a b = Ast.TyCom (_loc, a, b)
let (<$) = app
let rec apply acc = function | [] -> acc | x::xs -> apply (app acc x) xs
let sem a b = Ast.TySem (_loc, a, b)
let list_of_app ty =
  let rec loop t acc =
    match t with
    | Ast.TyApp (_loc,t1,t2) -> loop t1 (t2 :: acc)
    | Ast.TyNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_com ty =
  let rec loop t acc =
    match t with
    | Ast.TyCom (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | Ast.TyNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_sem ty =
  let rec loop t acc =
    match t with
    | Ast.TySem (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | Ast.TyNil _loc -> acc
    | i -> i :: acc in
  loop ty []
let app_of_list =
  function | [] -> Ast.TyNil _loc | l -> List.reduce_left app l
let com_of_list =
  function | [] -> Ast.TyNil _loc | l -> List.reduce_right comma l
let sem_of_list =
  function | [] -> Ast.TyNil _loc | l -> List.reduce_right sem l
let tuple_of_list =
  function
  | [] -> invalid_arg "tuple_of_list while list is empty"
  | x::[] -> x
  | xs -> Ast.TyTup (_loc, (com_of_list xs))
let arrow a b = Ast.TyArr (_loc, a, b)
let (|->) = arrow
let sta a b = Ast.TySta (_loc, a, b)
let sta_of_list = List.reduce_right sta
let arrow_of_list = List.reduce_right arrow
let app_arrow lst acc = List.fold_right arrow lst acc
let tuple_sta_of_list =
  function
  | [] -> invalid_arg "tuple_sta__of_list while list is empty"
  | x::[] -> x
  | xs -> Ast.TyTup (_loc, (sta_of_list xs))
let (<+) names ty =
  List.fold_right
    (fun name  acc  -> Ast.TyArr (_loc, (Ast.TyQuo (_loc, name)), acc)) names
    ty
let (+>) params base = List.fold_right arrow params base
let name_length_of_tydcl =
  function
  | Ast.TyDcl (_,name,tyvars,_,_) -> (name, (List.length tyvars))
  | tydcl ->
      invalid_arg
        ((sprintf "name_length_of_tydcl {|%s|}\n") &
           (to_string.contents tydcl))
let gen_quantifiers ~arity  n =
  ((List.init arity
      (fun i  -> List.init n (fun j  -> Ast.TyQuo (_loc, (allx ~off:i j)))))
     |> List.concat)
    |> app_of_list
let of_id_len ~off  (id,len) =
  apply (Ast.TyId (_loc, id))
    (List.init len (fun i  -> Ast.TyQuo (_loc, (allx ~off i))))
let of_name_len ~off  (name,len) =
  let id = Ast.IdLid (_loc, name) in of_id_len ~off (id, len)
let ty_name_of_tydcl =
  function
  | Ast.TyDcl (_,name,tyvars,_,_) ->
      apply (Ast.TyId (_loc, (Ast.IdLid (_loc, name)))) tyvars
  | tydcl ->
      invalid_arg &
        ((sprintf "ctyp_of_tydcl{|%s|}\n") & (to_string.contents tydcl))
let gen_ty_of_tydcl ~off  tydcl =
  (tydcl |> name_length_of_tydcl) |> (of_name_len ~off)
let list_of_record ty =
  try
    (ty |> list_of_sem) |>
      (List.map
         (function
          | Ast.TyCol
              (_loc,Ast.TyId (_,Ast.IdLid (_,col_label)),Ast.TyMut
               (_,col_ctyp))
              -> { col_label; col_ctyp; col_mutable = true }
          | Ast.TyCol (_loc,Ast.TyId (_,Ast.IdLid (_,col_label)),col_ctyp) ->
              { col_label; col_ctyp; col_mutable = false }
          | t0 -> raise & (Unhandled t0)))
  with
  | Unhandled t0 ->
      invalid_arg &
        (sprintf "list_of_record inner: {|%s|} outer: {|%s|}"
           (to_string.contents t0) (to_string.contents ty))
let gen_tuple_n ty n = (List.init n (fun _  -> ty)) |> tuple_sta_of_list
let repeat_arrow_n ty n = (List.init n (fun _  -> ty)) |> arrow_of_list
let mk_method_type ~number  ~prefix  (id,len) (k : obj_dest) =
  let prefix =
    List.map (fun s  -> String.drop_while (fun c  -> c = '_') s) prefix in
  let app_src =
    app_arrow (List.init number (fun _  -> of_id_len ~off:0 (id, len))) in
  let result_type = Ast.TyQuo (_loc, "result")
  and self_type = Ast.TyQuo (_loc, "self_type") in
  let (quant,dst) =
    match k with
    | Obj (Map ) -> (2, (of_id_len ~off:1 (id, len)))
    | Obj (Iter ) -> (1, result_type)
    | Obj (Fold ) -> (1, self_type)
    | Str_item  -> (1, result_type) in
  let params =
    List.init len
      (fun i  ->
         let app_src =
           app_arrow
             (List.init number (fun _  -> Ast.TyQuo (_loc, (allx ~off:0 i)))) in
         match k with
         | Obj u ->
             let dst =
               match u with
               | Map  -> Ast.TyQuo (_loc, (allx ~off:1 i))
               | Iter  -> result_type
               | Fold  -> self_type in
             self_type |-> (prefix <+ (app_src dst))
         | Str_item  -> prefix <+ (app_src result_type)) in
  let base = prefix <+ (app_src dst) in
  if len = 0
  then base
  else
    (let quantifiers = gen_quantifiers ~arity:quant len in
     Ast.TyPol (_loc, quantifiers, (params +> base)))
let mk_method_type_of_name ~number  ~prefix  (name,len) (k : obj_dest) =
  let id = Ast.IdLid (_loc, name) in
  mk_method_type ~number ~prefix (id, len) k
let mk_obj class_name base body =
  Ast.StCls
    (_loc,
      (Ast.CeEq
         (_loc,
           (Ast.CeCon
              (_loc, Ast.ViNil, (Ast.IdLid (_loc, class_name)),
                (Ast.TyNil _loc))),
           (Ast.CeStr
              (_loc,
                (Ast.PaTyc
                   (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, "self")))),
                     (Ast.TyQuo (_loc, "self_type")))),
                (Ast.CrSem
                   (_loc,
                     (Ast.CrInh
                        (_loc, Ast.OvNil,
                          (Ast.CeCon
                             (_loc, Ast.ViNil, (Ast.IdLid (_loc, base)),
                               (Ast.TyNil _loc))), "")), body)))))))
let is_recursive ty_dcl =
  match ty_dcl with
  | Ast.TyDcl (_,name,_,ctyp,_) ->
      let obj =
        object (self : 'self_type)
          inherit  Camlp4Ast.fold as super
          val mutable is_recursive = false
          method! ctyp =
            function
            | Ast.TyId (_loc,Ast.IdLid (_,i)) when i = name ->
                (is_recursive <- true; self)
            | x -> if is_recursive then self else super#ctyp x
          method is_recursive = is_recursive
        end in
      (obj#ctyp ctyp)#is_recursive
  | Ast.TyAnd (_loc,_,_) -> true
  | _ ->
      invalid_arg
        ("is_recursive not type declartion" ^ (to_string.contents ty_dcl))
let qualified_app_list =
  function
  | Ast.TyApp (_loc,_,_) as x ->
      (match list_of_app x with
       | (Ast.TyId (_loc,Ast.IdLid (_,_)))::_ -> None
       | (Ast.TyId (_loc,i))::ys -> Some (i, ys)
       | _ -> None)
  | Ast.TyId (_loc,Ast.IdLid (_,_))|Ast.TyId (_loc,Ast.IdUid (_,_)) -> None
  | Ast.TyId (_loc,i) -> Some (i, [])
  | _ -> None
let is_abstract =
  function | Ast.TyDcl (_,_,_,Ast.TyNil _loc,_) -> true | _ -> false
let abstract_list =
  function
  | Ast.TyDcl (_,_,lst,Ast.TyNil _loc,_) -> Some (List.length lst)
  | _ -> None
let eq t1 t2 =
  let strip_locs t = (Camlp4Ast.map_loc (fun _  -> FanLoc.ghost))#ctyp t in
  (strip_locs t1) = (strip_locs t2)
let eq_list t1 t2 =
  let rec loop =
    function
    | ([],[]) -> true
    | (x::xs,y::ys) -> (eq x y) && (loop (xs, ys))
    | (_,_) -> false in
  loop (t1, t2)
let mk_transform_type_eq () =
  object (self : 'self_type)
    val transformers = Hashtbl.create 50
    inherit  Camlp4Ast.map as super
    method! str_item =
      function
      | Ast.StTyp (_loc,Ast.TyDcl (_,_name,vars,ctyp,_)) as x ->
          (match qualified_app_list ctyp with
           | Some (i,lst) ->
               if not (eq_list vars lst)
               then super#str_item x
               else
                 (let src = i and dest = Ident.map_to_string i in
                  Hashtbl.replace transformers dest (src, (List.length lst));
                  Ast.StNil _loc)
           | None  -> super#str_item x)
      | x -> super#str_item x
    method! ctyp x =
      match qualified_app_list x with
      | Some (i,lst) ->
          let lst = List.map (fun ctyp  -> self#ctyp ctyp) lst in
          let src = i and dest = Ident.map_to_string i in
          (Hashtbl.replace transformers dest (src, (List.length lst));
           app_of_list ((Ast.TyId (_loc, (Ast.IdLid (_loc, dest)))) :: lst))
      | None  ->
          (match x with
           | Ast.TyMan (_loc,x,ctyp) ->
               Ast.TyMan (_loc, x, (super#ctyp ctyp))
           | _ -> super#ctyp x)
    method type_transformers =
      Hashtbl.fold (fun dest  (src,len)  acc  -> (dest, src, len) :: acc)
        transformers []
  end
let transform_module_types lst =
  let obj = mk_transform_type_eq () in
  let item1 =
    List.map
      (function
       | Mutual ls ->
           Mutual (List.map (fun (s,ty)  -> (s, (obj#ctyp ty))) ls)
       | Single (s,ty) -> Single (s, (obj#ctyp ty))) lst in
  let new_types = obj#type_transformers in (new_types, item1)
let reduce_data_ctors (ty : Ast.ctyp) (init : 'a)
  (f : string -> Ast.ctyp list -> 'e) =
  let open ErrorMonad in
    let rec loop acc t =
      match t with
      | Ast.TyOf (_loc,Ast.TyId (_,Ast.IdUid (_,cons)),tys) ->
          f cons (Ast.list_of_ctyp tys []) acc
      | Ast.TyOf (_loc,Ast.TyVrn (_,cons),tys) ->
          f ("`" ^ cons) (Ast.list_of_ctyp tys []) acc
      | Ast.TyId (_loc,Ast.IdUid (_,cons)) -> f cons [] acc
      | Ast.TyVrn (_loc,cons) -> f ("`" ^ cons) [] acc
      | Ast.TyOr (_loc,t1,t2) -> loop (loop acc t1) t2
      | Ast.TySum (_loc,ty)|Ast.TyVrnEq (_loc,ty)|Ast.TyVrnInf
          (_loc,ty)|Ast.TyVrnSup (_loc,ty) -> loop acc ty
      | Ast.TyNil _loc -> acc
      | t -> raise (Unhandled t) in
    try return & (loop init ty)
    with
    | Unhandled t0 ->
        fail
          (sprintf "reduce_data_ctors inner {|%s|} outer {|%s|}"
             (to_string.contents t0) (to_string.contents ty))