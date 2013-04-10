open Ast

open AstLoc

open LibUtil

open Basic

open FSig

let arrow_of_list = List.reduce_right arrow

let app_arrow lst acc = List.fold_right arrow lst acc

let (<+) (names : string list) (ty : ctyp) =
  List.fold_right
    (fun name  acc  ->
       (`Arrow
          (_loc, (`Quote (_loc, (`Normal _loc), (`Lid (_loc, name)))), acc) : 
       Ast.ctyp )) names ty

let (+>) (params : ctyp list) (base : ctyp) =
  List.fold_right arrow params base

let name_length_of_tydcl (x : typedecl) =
  (match x with
   | `TyDcl (_,`Lid (_,name),tyvars,_,_) ->
       (name,
         ((match tyvars with
           | `None _ -> 0
           | `Some (_,xs) -> List.length & (list_of_com xs []))))
   | tydcl ->
       failwithf "name_length_of_tydcl {|%s|}\n" (Objs.dump_typedecl tydcl) : 
  (string * int) )

let gen_quantifiers1 ~arity  n =
  (((List.init arity
       (fun i  ->
          List.init n
            (fun j  ->
               (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (allx ~off:i j)))) : 
               Ast.ctyp ))))
      |> List.concat)
     |> appl_of_list : ctyp )

let of_id_len ~off  (id,len) =
  appl_of_list ((`Id (_loc, id) : Ast.ctyp ) ::
    (List.init len
       (fun i  ->
          (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (allx ~off i)))) : 
          Ast.ctyp ))))

let of_name_len ~off  (name,len) =
  let id = `Lid (_loc, name) in of_id_len ~off (id, len)

let ty_name_of_tydcl (x : typedecl) =
  match x with
  | `TyDcl (_,`Lid (_,name),tyvars,_,_) ->
      let tyvars =
        match tyvars with
        | `None _ -> []
        | `Some (_,xs) -> (list_of_com xs [] :>ctyp list) in
      appl_of_list ((`Id (_loc, (`Lid (_loc, name))) : Ast.ctyp ) :: tyvars)
  | tydcl -> failwithf "ctyp_of_tydcl{|%s|}\n" (Objs.dump_typedecl tydcl)

let gen_ty_of_tydcl ~off  (tydcl : typedecl) =
  (tydcl |> name_length_of_tydcl) |> (of_name_len ~off)

let list_of_record (ty : name_ctyp) =
  (let (tys :name_ctyp list)= list_of_sem ty [] in
   tys |>
     (List.map
        (function
         | `TyColMut (_,`Lid (_,col_label),col_ctyp) ->
             { col_label; col_ctyp; col_mutable = true }
         | `TyCol (_,`Lid (_,col_label),col_ctyp) ->
             { col_label; col_ctyp; col_mutable = false }
         | t0 ->
             FanLoc.errorf (loc_of t0) "list_of_record %s"
               (Objs.dump_name_ctyp t0))) : FSig.col list )

let gen_tuple_n ty n = (List.init n (fun _  -> ty)) |> tuple_sta

let repeat_arrow_n ty n = (List.init n (fun _  -> ty)) |> arrow_of_list

let result_id = ref 0

let mk_method_type ~number  ~prefix  (id,len) (k : destination) =
  (let prefix =
     List.map (fun s  -> String.drop_while (fun c  -> c = '_') s) prefix in
   let app_src =
     app_arrow (List.init number (fun _  -> of_id_len ~off:0 (id, len))) in
   let result_type: Ast.ctyp =
     `Quote
       (_loc, (`Normal _loc),
         (`Lid (_loc, ("result" ^ (string_of_int result_id.contents))))) in
   let _ = incr result_id in
   let self_type: Ast.ctyp =
     `Quote (_loc, (`Normal _loc), (`Lid (_loc, "self_type"))) in
   let (quant,dst) =
     match k with
     | Obj (Map ) -> (2, (of_id_len ~off:1 (id, len)))
     | Obj (Iter ) -> (1, result_type)
     | Obj (Fold ) -> (1, self_type)
     | Obj (Concrete c) -> (1, c)
     | Str_item  -> (1, result_type) in
   let params =
     List.init len
       (fun i  ->
          let app_src =
            app_arrow
              (List.init number
                 (fun _  ->
                    (`Quote
                       (_loc, (`Normal _loc), (`Lid (_loc, (allx ~off:0 i)))) : 
                    Ast.ctyp ))) in
          match k with
          | Obj u ->
              let dst =
                match u with
                | Map  ->
                    (`Quote
                       (_loc, (`Normal _loc), (`Lid (_loc, (allx ~off:1 i)))) : 
                    Ast.ctyp )
                | Iter  -> result_type
                | Concrete c -> c
                | Fold  -> self_type in
              arrow self_type (prefix <+ (app_src dst))
          | Str_item  -> prefix <+ (app_src result_type)) in
   let base = prefix <+ (app_src dst) in
   if len = 0
   then ((`TyPolEnd (_loc, base)), dst)
   else
     (let quantifiers = gen_quantifiers1 ~arity:quant len in
      ((`TyPol (_loc, quantifiers, (params +> base)) : Ast.ctyp ), dst)) : 
  (ctyp * ctyp) )

let mk_method_type_of_name ~number  ~prefix  (name,len) (k : destination) =
  let id = `Lid (_loc, name) in mk_method_type ~number ~prefix (id, len) k

let mk_obj class_name base body =
  `Class
    (_loc,
      (`Eq
         (_loc,
           (`ClassConS (_loc, (`ViNil _loc), (`Lid (_loc, class_name)))),
           (`ObjPat
              (_loc,
                (`Constraint
                   (_loc, (`Lid (_loc, "self")),
                     (`Quote
                        (_loc, (`Normal _loc), (`Lid (_loc, "self_type")))))),
                (`Sem
                   (_loc,
                     (`Inherit
                        (_loc, (`OvNil _loc),
                          (`ClassConS
                             (_loc, (`ViNil _loc), (`Lid (_loc, base)))))),
                     body)))))))

let is_recursive ty_dcl =
  match ty_dcl with
  | `TyDcl (_,`Lid (_,name),_,ctyp,_) ->
      let obj =
        object (self : 'self_type)
          inherit  Objs.fold as super
          val mutable is_recursive = false
          method! ctyp =
            function
            | (`Id (_loc,`Lid (_,i)) : Ast.ctyp) when i = name ->
                (is_recursive <- true; self)
            | x -> if is_recursive then self else super#ctyp x
          method is_recursive = is_recursive
        end in
      (obj#type_info ctyp)#is_recursive
  | `And (_,_,_) -> true
  | _ ->
      failwithf "is_recursive not type declartion: %s"
        (Objs.dump_typedecl ty_dcl)

let qualified_app_list =
  function
  | (`App (_loc,_,_) : Ast.ctyp) as x ->
      (match list_of_app x [] with
       | (`Id (_loc,`Lid (_,_)) : Ast.ctyp)::_ -> None
       | (`Id (_loc,i) : Ast.ctyp)::ys -> Some (i, ys)
       | _ -> None)
  | (`Id (_loc,`Lid (_,_)) : Ast.ctyp)|(`Id (_loc,`Uid (_,_)) : Ast.ctyp) ->
      None
  | (`Id (_loc,i) : Ast.ctyp) -> Some (i, [])
  | _ -> None

let is_abstract (x : typedecl) =
  match x with | `TyAbstr _ -> true | _ -> false

let abstract_list (x : typedecl) =
  match x with
  | `TyAbstr (_,_,lst,_) ->
      (match lst with
       | `None _ -> Some 0
       | `Some (_,xs) -> Some (List.length & (list_of_com xs [])))
  | _ -> None

let eq t1 t2 =
  let strip_locs t = (Objs.map_loc (fun _  -> FanLoc.ghost))#ctyp t in
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
    inherit  Objs.map as super
    method! stru =
      function
      | (`Type (_loc,`TyDcl (_,_name,vars,ctyp,_)) : Ast.stru) as x ->
          let r =
            match ctyp with
            | `TyEq (_,_,t) -> qualified_app_list t
            | _ -> None in
          (match r with
           | Some (i,lst) ->
               let vars =
                 match vars with
                 | `None _ -> []
                 | `Some (_,x) -> list_of_com x [] in
               if not (eq_list (vars : decl_params list  :>ctyp list) lst)
               then super#stru x
               else
                 (let src = i and dest = Id.to_string i in
                  Hashtbl.replace transformers dest (src, (List.length lst));
                  `StExp (_loc, (`Uid (_loc, "()"))))
           | None  -> super#stru x)
      | x -> super#stru x
    method! ctyp x =
      match qualified_app_list x with
      | Some (i,lst) ->
          let lst = List.map (fun ctyp  -> self#ctyp ctyp) lst in
          let src = i and dest = Id.to_string i in
          (Hashtbl.replace transformers dest (src, (List.length lst));
           appl_of_list ((`Id (_loc, (`Lid (_loc, dest))) : Ast.ctyp ) ::
             lst))
      | None  -> super#ctyp x
    method type_transformers =
      Hashtbl.fold (fun dest  (src,len)  acc  -> (dest, src, len) :: acc)
        transformers []
  end

let transform_module_types (lst : FSig.module_types) =
  let obj = mk_transform_type_eq () in
  let item1 =
    List.map
      (function
       | `Mutual ls ->
           `Mutual (List.map (fun (s,ty)  -> (s, (obj#typedecl ty))) ls)
       | `Single (s,ty) -> `Single (s, (obj#typedecl ty))) lst in
  let new_types = obj#type_transformers in (new_types, item1)

let reduce_data_ctors (ty : or_ctyp) (init : 'a) ~compose 
  (f : string -> ctyp list -> 'e) =
  let branches = list_of_or ty [] in
  List.fold_left
    (fun acc  x  ->
       match (x : or_ctyp ) with
       | `Of (_loc,`Id (_,`Uid (_,cons)),tys) ->
           compose (f cons (list_of_star tys [])) acc
       | `Uid (_,cons) -> compose (f cons []) acc
       | t ->
           FanLoc.errorf (loc_of t) "reduce_data_ctors: %s"
             (Objs.dump_or_ctyp t)) init branches

let view_sum (t : or_ctyp) =
  let bs = list_of_or t [] in
  List.map
    (function
     | `Uid (_,cons) -> `branch (cons, [])
     | `Of (_loc,`Id (_,`Uid (_,cons)),t) ->
         `branch (cons, (list_of_star t []))
     | _ -> assert false) bs

let view_variant (t : row_field) =
  (let lst = list_of_or t [] in
   List.map
     (function
      | `TyVrnOf (_loc,`C (_,cons),`Par (_,t)) ->
          `variant (cons, (list_of_star t []))
      | `TyVrnOf (_loc,`C (_,cons),t) -> `variant (cons, [t])
      | `TyVrn (_loc,`C (_,cons)) -> `variant (cons, [])
      | `Ctyp (_,`Id (_loc,i)) -> `abbrev i
      | u ->
          FanLoc.errorf (loc_of u) "view_variant %s" (Objs.dump_row_field u))
     lst : vbranch list )

let of_stru =
  function
  | `Type (_,x) -> x
  | t -> FanLoc.errorf (loc_of t) "Ctyp.of_stru %s" (Objs.dump_stru t)