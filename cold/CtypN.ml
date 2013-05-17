open AstN

open AstLibN

open LibUtil

open Basic

type col =  {
  col_label: string;
  col_mutable: bool;
  col_ctyp: ctyp} 

type vbranch = [ `variant of (string * ctyp list) | `abbrev of ident] 

type branch = [ `branch of (string * ctyp list)] 

type named_type = (string * typedecl) 
and and_types = named_type list 
and types = [ `Mutual of and_types | `Single of named_type] 
and mtyps = types list 

type destination =  
  | Obj of kind
  | Str_item 
and kind =  
  | Fold
  | Iter
  | Map
  | Concrete of ctyp 

type warning_type =  
  | Abstract of string
  | Qualified of string 

let arrow_of_list f = List.reduce_right arrow f

let app_arrow lst acc = List.fold_right arrow lst acc

let (<+) (names : string list) (ty : ctyp) =
  List.fold_right
    (fun name  acc  ->
       (`Arrow ((`Quote (`Normal, (`Lid name))), acc) : AstN.ctyp )) names ty

let (+>) (params : ctyp list) (base : ctyp) =
  List.fold_right arrow params base

let name_length_of_tydcl (x : typedecl) =
  (match x with
   | `TyDcl (`Lid name,tyvars,_,_) ->
       (name,
         ((match tyvars with
           | `None -> 0
           | `Some xs -> List.length & (list_of_com xs []))))
   | tydcl ->
       failwithf "name_length_of_tydcl {|%s|}\n" (ObjsN.dump_typedecl tydcl) : 
  (string * int) )

let gen_quantifiers1 ~arity  n =
  (((List.init arity
       (fun i  ->
          List.init n
            (fun j  ->
               (`Quote (`Normal, (`Lid (allx ~off:i j))) : AstN.ctyp ))))
      |> List.concat)
     |> appl_of_list : ctyp )

let of_id_len ~off  ((id : ident),len) =
  appl_of_list ((id :>ctyp) ::
    (List.init len
       (fun i  -> (`Quote (`Normal, (`Lid (allx ~off i))) : AstN.ctyp ))))

let of_name_len ~off  (name,len) =
  let id = lid name in of_id_len ~off (id, len)

let gen_ty_of_tydcl ~off  (tydcl : typedecl) =
  (tydcl |> name_length_of_tydcl) |> (of_name_len ~off)

let list_of_record (ty : name_ctyp) =
  (let (tys :name_ctyp list)= list_of_sem ty [] in
   tys |>
     (List.map
        (function
         | `TyColMut (`Lid col_label,col_ctyp) ->
             { col_label; col_ctyp; col_mutable = true }
         | `TyCol (`Lid col_label,col_ctyp) ->
             { col_label; col_ctyp; col_mutable = false }
         | t0 -> failwithf "list_of_record %s" (ObjsN.dump_name_ctyp t0))) : 
  col list )

let gen_tuple_n ty n = (List.init n (fun _  -> ty)) |> tuple_sta

let repeat_arrow_n ty n = (List.init n (fun _  -> ty)) |> arrow_of_list

let result_id = ref 0

let mk_method_type ~number  ~prefix  (id,len) (k : destination) =
  (let prefix =
     List.map (fun s  -> String.drop_while (fun c  -> c = '_') s) prefix in
   let app_src =
     app_arrow (List.init number (fun _  -> of_id_len ~off:0 (id, len))) in
   let result_type: AstN.ctyp =
     `Quote (`Normal, (`Lid ("result" ^ (string_of_int result_id.contents)))) in
   let _ = incr result_id in
   let self_type: AstN.ctyp = `Quote (`Normal, (`Lid "self_type")) in
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
                    (`Quote (`Normal, (`Lid (allx ~off:0 i))) : AstN.ctyp ))) in
          match k with
          | Obj u ->
              let dst =
                match u with
                | Map  ->
                    (`Quote (`Normal, (`Lid (allx ~off:1 i))) : AstN.ctyp )
                | Iter  -> result_type
                | Concrete c -> c
                | Fold  -> self_type in
              arrow self_type (prefix <+ (app_src dst))
          | Str_item  -> prefix <+ (app_src result_type)) in
   let base = prefix <+ (app_src dst) in
   if len = 0
   then ((`TyPolEnd base), dst)
   else
     (let quantifiers = gen_quantifiers1 ~arity:quant len in
      ((`TyPol (quantifiers, (params +> base)) : AstN.ctyp ), dst)) : 
  (ctyp * ctyp) )

let mk_method_type_of_name ~number  ~prefix  (name,len) (k : destination) =
  let id = lid name in mk_method_type ~number ~prefix (id, len) k

let mk_obj class_name base body =
  (`Class
     (`ClDeclS
        (`Negative, (`Lid class_name),
          (`ObjPat
             ((`Constraint
                 ((`Lid "self"), (`Quote (`Normal, (`Lid "self_type"))))),
               (`Sem ((`Inherit (`Negative, (`Lid base))), body)))))) : 
  AstN.stru )

let is_recursive ty_dcl =
  match ty_dcl with
  | `TyDcl (`Lid name,_,ctyp,_) ->
      let obj =
        object (self : 'self_type)
          inherit  ObjsN.fold as super
          val mutable is_recursive = false
          method! ctyp =
            function
            | (`Lid i : AstN.ctyp) when i = name ->
                (is_recursive <- true; self)
            | x -> if is_recursive then self else super#ctyp x
          method is_recursive = is_recursive
        end in
      (obj#type_info ctyp)#is_recursive
  | `And _ -> true
  | _ ->
      failwithf "is_recursive not type declartion: %s"
        (ObjsN.dump_typedecl ty_dcl)

let qualified_app_list (x : ctyp) =
  (match x with
   | (`App (_loc,_) : AstN.ctyp) as x ->
       (match list_of_app x [] with
        | (`Lid _loc : AstN.ctyp)::_ -> None
        | (#ident' as i)::ys -> Some (i, ys)
        | _ -> None)
   | `Lid _|`Uid _ -> None
   | #ident' as i -> Some (i, [])
   | _ -> None : (ident * ctyp list) option )

let is_abstract (x : typedecl) =
  match x with | `TyAbstr _ -> true | _ -> false

let abstract_list (x : typedecl) =
  match x with
  | `TyAbstr (_,lst,_) ->
      (match lst with
       | `None -> Some 0
       | `Some xs -> Some (List.length & (list_of_com xs [])))
  | _ -> None

let mk_transform_type_eq () =
  object (self : 'self_type)
    val transformers = Hashtbl.create 50
    inherit  ObjsN.map as super
    method! stru =
      function
      | (`Type `TyDcl (_name,vars,ctyp,_) : AstN.stru) as x ->
          let r =
            match ctyp with | `TyEq (_,t) -> qualified_app_list t | _ -> None in
          (match r with
           | Some (i,lst) ->
               let vars =
                 match vars with | `None -> [] | `Some x -> list_of_com x [] in
               if not ((vars : decl_params list  :>ctyp list) = lst)
               then super#stru x
               else
                 (let src = i and dest = IdN.to_string i in
                  Hashtbl.replace transformers dest (src, (List.length lst));
                  (`StExp (`Uid "()") : AstN.stru ))
           | None  -> super#stru x)
      | x -> super#stru x
    method! ctyp x =
      match qualified_app_list x with
      | Some (i,lst) ->
          let lst = List.map (fun ctyp  -> self#ctyp ctyp) lst in
          let src = i and dest = IdN.to_string i in
          (Hashtbl.replace transformers dest (src, (List.length lst));
           appl_of_list ((`Lid dest : AstN.ctyp ) :: lst))
      | None  -> super#ctyp x
    method type_transformers =
      Hashtbl.fold (fun dest  (src,len)  acc  -> (dest, src, len) :: acc)
        transformers []
  end

let transform_mtyps (lst : mtyps) =
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
       | `Of (`Uid cons,tys) -> compose (f cons (list_of_star tys [])) acc
       | `Uid cons -> compose (f cons []) acc
       | t -> failwithf "reduce_data_ctors: %s" (ObjsN.dump_or_ctyp t)) init
    branches

let view_sum (t : or_ctyp) =
  let bs = list_of_or t [] in
  List.map
    (function
     | `Uid cons -> `branch (cons, [])
     | `Of (`Uid cons,t) -> `branch (cons, (list_of_star t []))
     | _ -> assert false) bs

let view_variant (t : row_field) =
  (let lst = list_of_or t [] in
   List.map
     (function
      | `TyVrnOf (`C cons,`Par t) -> `variant (cons, (list_of_star t []))
      | `TyVrnOf (`C cons,t) -> `variant (cons, [t])
      | `TyVrn `C cons -> `variant (cons, [])
      | `Ctyp (#ident' as i) -> `abbrev i
      | u -> failwithf "view_variant %s" (ObjsN.dump_row_field u)) lst : 
  vbranch list )