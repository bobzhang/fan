open Astn_util
open FAstN
open StdFan
let pp_print_typedecl = ObjsN.pp_print_typedecl
type named_type = (string* typedecl) 
and and_types = named_type list 
and types = [ `Mutual of and_types | `Single of named_type] 
and mtyps = types list 
let rec pp_print_named_type: Format.formatter -> named_type -> unit =
  fun fmt  _a0  ->
    (fun fmt  (_a0,_a1)  ->
       Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_string _a0
         pp_print_typedecl _a1) fmt _a0
and pp_print_and_types: Format.formatter -> and_types -> unit =
  fun fmt  _a0  -> pp_print_list pp_print_named_type fmt _a0
and pp_print_types: Format.formatter -> types -> unit =
  fun fmt  ->
    function
    | `Mutual _a0 ->
        Format.fprintf fmt "@[<1>(`Mutual@ %a)@]" pp_print_and_types _a0
    | `Single _a0 ->
        Format.fprintf fmt "@[<1>(`Single@ %a)@]" pp_print_named_type _a0
and pp_print_mtyps: Format.formatter -> mtyps -> unit =
  fun fmt  _a0  -> pp_print_list pp_print_types fmt _a0
type plugin_name = string 
type plugin = 
  {
  transform: mtyps -> stru option;
  position: string option;
  filter: (string -> bool) option} 
let apply_filter f (m : mtyps) =
  (let f =
     function
     | `Single (s,_) as x -> if f s then Some x else None
     | `Mutual ls ->
         let x =
           Flist.filter_map
             (fun ((s,_) as x)  -> if f s then Some x else None) ls in
         (match x with
          | [] -> None
          | x::[] -> Some (`Single x)
          | y -> Some (`Mutual y)) in
   Flist.filter_map f m : mtyps )
let stru_from_mtyps ~f:(aux : named_type -> typedecl)  (x : mtyps) =
  (match x with
   | [] -> None
   | _ ->
       let xs: stru list =
         List.map
           (function
            | `Mutual tys ->
                let v = and_of_list (List.map aux tys) in
                (`Type v : FAstN.stru )
            | `Single ty -> let v = aux ty in (`Type v : FAstN.stru )) x in
       Some (sem_of_list xs) : stru option )
let stru_from_ty ~f:(f : string -> stru)  (x : mtyps) =
  (let tys: string list =
     Flist.concat_map
       (function
        | `Mutual tys -> List.map (fun ((x,_) : named_type)  -> x) tys
        | `Single (x,_) -> [x]) x in
   sem_of_list (List.map f tys) : stru )
let mk_transform_type_eq () =
  object (self : 'self_type)
    val transformers = Hashtbl.create 50
    inherit  ObjsN.map as super
    method! stru =
      function
      | (`Type `TyDcl (_name,vars,ctyp,_) : FAstN.stru) as x ->
          let r =
            match ctyp with
            | `TyEq (_,t) -> CtypN.qualified_app_list t
            | _ -> None in
          (match r with
           | Some (i,lst) ->
               let vars =
                 match vars with
                 | `None -> []
                 | `Some x -> Ast_basic.N.list_of_com x [] in
               if not ((vars : decl_params list  :>ctyp list) = lst)
               then super#stru x
               else
                 (let src = i and dest = IdN.to_string i in
                  Hashtbl.replace transformers dest (src, (List.length lst));
                  (`StExp (`Uid "()") : FAstN.stru ))
           | None  -> super#stru x)
      | x -> super#stru x
    method! ctyp x =
      match CtypN.qualified_app_list x with
      | Some (i,lst) ->
          let lst = List.map (fun ctyp  -> self#ctyp ctyp) lst in
          let src = i and dest = IdN.to_string i in
          (Hashtbl.replace transformers dest (src, (List.length lst));
           appl_of_list ((`Lid dest : FAstN.ctyp ) :: lst))
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