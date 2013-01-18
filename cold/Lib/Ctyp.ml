open FanAst
open LibUtil
open Format
open Basic
open FSig
let rec to_var_list =
  function
  | `TyApp (_loc,t1,t2) -> (to_var_list t1) @ (to_var_list t2)
  | `Quote (_loc,`Normal _,`Some `Lid (_,s))
    |`Quote (_loc,`Positive _,`Some `Lid (_,s))
    |`Quote (_loc,`Negative _,`Some `Lid (_,s)) -> [s]
  | _ -> assert false
let list_of_opt ot acc =
  match ot with | `Nil _loc -> acc | t -> FanAst.list_of_ctyp t acc
let rec name_tags =
  function
  | `TyApp (_loc,t1,t2) -> (name_tags t1) @ (name_tags t2)
  | `TyVrn (_,`C (_,s)) -> [s]
  | _ -> assert false
let rec to_generalized =
  function
  | `Arrow (_loc,t1,t2) ->
      let (tl,rt) = to_generalized t2 in ((t1 :: tl), rt)
  | t -> ([], t)
let to_string = to_string_of_printer FanAst.dump#ctyp
let eprint: ctyp -> unit = fun c  -> eprintf "@[%a@]" FanAst.dump#ctyp c
let _loc = FanLoc.ghost
let app a b = `TyApp (_loc, a, b)
let comma a b = `Com (_loc, a, b)
let rec apply acc = function | [] -> acc | x::xs -> apply (app acc x) xs
let sem a b =
  let _loc = FanLoc.merge (FanAst.loc_of a) (FanAst.loc_of b) in
  `Sem (_loc, a, b)
let list_of_app ty =
  let rec loop t acc =
    match t with
    | `TyApp (_loc,t1,t2) -> loop t1 (t2 :: acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_com ty =
  let rec loop t acc =
    match t with
    | `Com (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let list_of_sem ty =
  let rec loop t acc =
    match t with
    | `Sem (_loc,t1,t2) -> t1 :: (loop t2 acc)
    | `Nil _loc -> acc
    | i -> i :: acc in
  loop ty []
let rec view_app acc =
  function | `TyApp (_loc,f,a) -> view_app (a :: acc) f | f -> (f, acc)
let app_of_list = function | [] -> `Nil _loc | l -> List.reduce_left app l
let com_of_list = function | [] -> `Nil _loc | l -> List.reduce_right comma l
let sem_of_list = function | [] -> `Nil _loc | l -> List.reduce_right sem l
let tuple_of_list =
  function
  | [] -> invalid_arg "tuple_of_list while list is empty"
  | x::[] -> x
  | xs -> `Tup (_loc, (com_of_list xs))
let arrow a b = `Arrow (_loc, a, b)
let (|->) = arrow
let sta a b = `Sta (_loc, a, b)
let sta_of_list = List.reduce_right sta
let arrow_of_list = List.reduce_right arrow
let app_arrow lst acc = List.fold_right arrow lst acc
let tuple_sta_of_list =
  function
  | [] -> invalid_arg "tuple_sta__of_list while list is empty"
  | x::[] -> x
  | xs -> `Tup (_loc, (sta_of_list xs))
let (<+) names ty =
  List.fold_right
    (fun name  acc  ->
       `Arrow
         (_loc, (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, name))))),
           acc)) names ty
let (+>) params base = List.fold_right arrow params base
let name_length_of_tydcl =
  function
  | `TyDcl (_,`Lid (_,name),tyvars,_,_) -> (name, (List.length tyvars))
  | tydcl ->
      invalid_arg
        ((sprintf "name_length_of_tydcl {|%s|}\n") & (to_string tydcl))
let gen_quantifiers ~arity  n =
  ((List.init arity
      (fun i  ->
         List.init n
           (fun j  ->
              `Quote
                (_loc, (`Normal _loc),
                  (`Some (`Lid (_loc, (allx ~off:i j))))))))
     |> List.concat)
    |> app_of_list
let of_id_len ~off  (id,len) =
  apply (`Id (_loc, id))
    (List.init len
       (fun i  ->
          `Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, (allx ~off i)))))))
let of_name_len ~off  (name,len) =
  let id = `Lid (_loc, name) in of_id_len ~off (id, len)
let ty_name_of_tydcl =
  function
  | `TyDcl (_,`Lid (_,name),tyvars,_,_) ->
      apply (`Id (_loc, (`Lid (_loc, name)))) tyvars
  | tydcl ->
      invalid_arg & ((sprintf "ctyp_of_tydcl{|%s|}\n") & (to_string tydcl))
let gen_ty_of_tydcl ~off  tydcl =
  (tydcl |> name_length_of_tydcl) |> (of_name_len ~off)
let list_of_record (ty : ctyp) =
  (list_of_sem' ty []) |>
    (List.map
       (function
        | `TyCol (_,`Id (_,`Lid (_,label)),`Mut (_,ctyp)) ->
            { label; ctyp; is_mutable = true }
        | `TyCol (_,`Id (_,`Lid (_,label)),ctyp) ->
            { label; ctyp; is_mutable = false }
        | t0 -> FanLoc.errorf (loc_of t0) "list_of_record %s" (dump_ctyp t0)))
let gen_tuple_n ty n = (List.init n (fun _  -> ty)) |> tuple_sta_of_list
let repeat_arrow_n ty n = (List.init n (fun _  -> ty)) |> arrow_of_list
let mk_method_type ~number  ~prefix  (id,len) (k : destination) =
  let prefix =
    List.map (fun s  -> String.drop_while (fun c  -> c = '_') s) prefix in
  let app_src =
    app_arrow (List.init number (fun _  -> of_id_len ~off:0 (id, len))) in
  let result_type =
    `Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "result"))))
  and self_type =
    `Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "self_type")))) in
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
             (List.init number
                (fun _  ->
                   `Quote
                     (_loc, (`Normal _loc),
                       (`Some (`Lid (_loc, (allx ~off:0 i))))))) in
         match k with
         | Obj u ->
             let dst =
               match u with
               | Map  ->
                   `Quote
                     (_loc, (`Normal _loc),
                       (`Some (`Lid (_loc, (allx ~off:1 i)))))
               | Iter  -> result_type
               | Fold  -> self_type in
             self_type |-> (prefix <+ (app_src dst))
         | Str_item  -> prefix <+ (app_src result_type)) in
  let base = prefix <+ (app_src dst) in
  if len = 0
  then base
  else
    (let quantifiers = gen_quantifiers ~arity:quant len in
     `TyPol (_loc, quantifiers, (params +> base)))
let mk_dest_type ~destination  (id,len) =
  let result_type =
    `Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "result"))))
  and self_type =
    `Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "self_type")))) in
  let (_quant,dst) =
    match destination with
    | Obj (Map ) -> (2, (of_id_len ~off:1 (id, len)))
    | Obj (Iter ) -> (1, result_type)
    | Obj (Fold ) -> (1, self_type)
    | Str_item  -> (1, result_type) in
  dst
let mk_method_type_of_name ~number  ~prefix  (name,len) (k : destination) =
  let id = `Lid (_loc, name) in mk_method_type ~number ~prefix (id, len) k
let mk_obj class_name base body =
  `Class
    (_loc,
      (`Eq
         (_loc,
           (`CeCon
              (_loc, (`ViNil _loc), (`Lid (_loc, class_name)), (`Nil _loc))),
           (`Obj
              (_loc,
                (`PaTyc
                   (_loc, (`Id (_loc, (`Lid (_loc, "self")))),
                     (`Quote
                        (_loc, (`Normal _loc),
                          (`Some (`Lid (_loc, "self_type"))))))),
                (`CrSem
                   (_loc,
                     (`Inherit
                        (_loc, (`OvNil _loc),
                          (`CeCon
                             (_loc, (`ViNil _loc), (`Lid (_loc, base)),
                               (`Nil _loc))), (`None _loc))), body)))))))
let is_recursive ty_dcl =
  match ty_dcl with
  | `TyDcl (_,`Lid (_,name),_,ctyp,_) ->
      let obj =
        object (self : 'self_type)
          inherit  FanAst.fold as super
          val mutable is_recursive = false
          method! ctyp =
            function
            | `Id (_loc,`Lid (_,i)) when i = name ->
                (is_recursive <- true; self)
            | x -> if is_recursive then self else super#ctyp x
          method is_recursive = is_recursive
        end in
      (obj#ctyp ctyp)#is_recursive
  | `And (_loc,_,_) -> true
  | _ ->
      invalid_arg ("is_recursive not type declartion" ^ (to_string ty_dcl))
let qualified_app_list =
  function
  | `TyApp (_loc,_,_) as x ->
      (match list_of_app x with
       | (`Id (_loc,`Lid (_,_)))::_ -> None
       | (`Id (_loc,i))::ys -> Some (i, ys)
       | _ -> None)
  | `Id (_loc,`Lid (_,_))|`Id (_loc,`Uid (_,_)) -> None
  | `Id (_loc,i) -> Some (i, [])
  | _ -> None
let is_abstract = function | `TyDcl (_,_,_,`Nil _loc,_) -> true | _ -> false
let abstract_list =
  function
  | `TyDcl (_,_,lst,`Nil _loc,_) -> Some (List.length lst)
  | _ -> None
let eq t1 t2 =
  let strip_locs t = (FanAst.map_loc (fun _  -> FanLoc.ghost))#ctyp t in
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
    inherit  FanAst.map as super
    method! str_item =
      function
      | `Type (_loc,`TyDcl (_,_name,vars,ctyp,_)) as x ->
          (match qualified_app_list ctyp with
           | Some (i,lst) ->
               if not (eq_list vars lst)
               then super#str_item x
               else
                 (let src = i and dest = Ident.map_to_string i in
                  Hashtbl.replace transformers dest (src, (List.length lst));
                  `Nil _loc)
           | None  -> super#str_item x)
      | x -> super#str_item x
    method! ctyp x =
      match qualified_app_list x with
      | Some (i,lst) ->
          let lst = List.map (fun ctyp  -> self#ctyp ctyp) lst in
          let src = i and dest = Ident.map_to_string i in
          (Hashtbl.replace transformers dest (src, (List.length lst));
           app_of_list ((`Id (_loc, (`Lid (_loc, dest)))) :: lst))
      | None  ->
          (match x with
           | `TyMan (_loc,x,ctyp) -> `TyMan (_loc, x, (super#ctyp ctyp))
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
       | `Mutual ls ->
           `Mutual (List.map (fun (s,ty)  -> (s, (obj#ctyp ty))) ls)
       | `Single (s,ty) -> `Single (s, (obj#ctyp ty))) lst in
  let new_types = obj#type_transformers in (new_types, item1)
let reduce_data_ctors (ty : ctyp) (init : 'a) ~compose 
  (f : string -> ctyp list -> 'e) =
  let branches = list_of_or' ty [] in
  List.fold_left
    (fun acc  x  ->
       match x with
       | `Of (_loc,`Id (_,`Uid (_,cons)),tys) ->
           compose (f cons (list_of_and' tys [])) acc
       | `Id (_loc,`Uid (_,cons)) -> compose (f cons []) acc
       | t -> FanLoc.errorf (loc_of t) "reduce_data_ctors: %s" (dump_ctyp t))
    init branches
let view_sum (t : ctyp) =
  let bs = FanAst.list_of_ctyp t [] in
  List.map
    (function
     | `Id (_loc,`Uid (_,cons)) -> `branch (cons, [])
     | `Of (_loc,`Id (_,`Uid (_,cons)),t) ->
         `branch (cons, (FanAst.list_of_ctyp t []))
     | _ -> assert false) bs
let view_variant (t : ctyp) =
  (let lst = list_of_or' t [] in
   List.map
     (function
      | `Of (_loc,`TyVrn (_,`C (_,cons)),`Tup (_,t)) ->
          `variant (cons, (list_of_star' t []))
      | `Of (_loc,`TyVrn (_,`C (_,cons)),t) -> `variant (cons, [t])
      | `TyVrn (_loc,`C (_,cons)) -> `variant (cons, [])
      | `Id (_loc,i) -> `abbrev i
      | u ->
          FanLoc.errorf (FanAst.loc_of u) "view_variant %s"
            (FanAst.dump_ctyp u)) lst : vbranch list )
let of_str_item =
  function
  | `Type (_,x) -> x
  | t ->
      FanLoc.errorf (FanAst.loc_of t) "Ctyp.of_str_item %s" (dump_str_item t)