
open Astn_util
open Astfn

open StdFan (* for [pp_print_list] [pp_print_string] *)

let pp_print_typedecl = ObjsN.pp_print_typedecl;;  

type named_type = (string* typedecl) 
and and_types = named_type list
and types =
    [ `Mutual of and_types
    | `Single of named_type ]
and mtyps =  types list
with ("Print")

type plugin_name = string 

type plugin = {
    transform:(mtyps -> stru option);
    position: string option ;
    filter: (string->bool) option ;
}

let apply_filter f (m:mtyps) : mtyps = 
  let f  = (function
    | (`Single (s,_) as x) ->
        if f s then Some  x else None
    | `Mutual ls ->
        let x = Listf.filter_map (fun ((s,_) as x) -> if f s then Some x  else None) ls in
        match x with
        | [] -> None
        | [x] -> Some (`Single  x)
        |  y -> Some (`Mutual y)) in
  Listf.filter_map  f m 

      
let stru_from_mtyps  ~f:(aux:named_type -> typedecl)
    (x:mtyps) : stru option =
  match x with
  | [] -> None
  | _ ->
      let xs : stru list   =
        (List.map
           (function
             |`Mutual tys ->
                 let v = (and_of_list (List.map aux tys)) in
                 %stru-{ type $v }
             |`Single ty ->
                 let v = aux ty in
                 %stru-{ type $v} ) x ) in
      Some (sem_of_list xs )

let stru_from_ty  ~f:(f:string -> stru) (x:mtyps) : stru  =     
  let tys : string list  =
    Listf.concat_map
      (function
        |`Mutual tys -> List.map (fun ((x,_):named_type) -> x ) tys
        |`Single (x,_) -> [x] ) x in
  (sem_of_list (List.map f tys))




(*

  {[

  let f = mk_transform_type_eq ();

  let v =
  (f#stru

  <:stru<
  type a = Loc.t
  and  b 'a  = [ A of LL.t 'a and LL.t 'a and Loc.t];
  let f x = 3
  >> );

  f#type_transformers |>  opr#stru fmt;  

  v |> opr#stru fmt;

  type ll_t 'a0 = LL.t 'a0;
  type loc_t = Loc.t;
  type a = loc_t and b 'a = [ A of ll_t 'a and ll_t 'a and loc_t ];
  let f x = 3;
  
  ]}
  There are two cases:
  The first is [Loc.t => loc_t], and record the relationship to the hashtbl.
  It's reasonalble and sound. But it may bring some unnecessary duplicated code.

  We only consider one duplicated case
  [type u 'a = Loc.t 'a] [type u int = Loc.t int ]
  the type variables are the same as the type definition.
  here we record the relationship [Loc.t => u ]
  ]}
 *)
let mk_transform_type_eq () = object(self:'self_type)
  val transformers = Hashtbl.create 50
  inherit ObjsN.map as super
  method! stru = function
    | %stru-{ type ${`TyDcl ( _name, vars, ctyp, _) } } as x -> (* FIXME why tuple?*)
        let r =
          match ctyp with
          | `TyEq (_,t) -> Ctyp.qualified_app_list t | _ -> None  in
        begin match  r with
        | Some (i,lst)  -> (* [ type u 'a = Loc.t int U.float]*)
            let vars =
              match vars with 
              | `None  -> []
              | `Some x -> Ast_basic.N.list_of_com x [] in
            if  not ( (vars : decl_params list  :>  ctyp list) = lst) then 
              super#stru x
            else
              (* Manual substitution
                 [type u 'a 'b = Loc.t 'a 'b]
                 [type u int = Loc.t int]
                 This case can not happen [type u FanAst.int = Loc.t FanAst.int ]
               *)
              let src = i and dest =             
                IdN.to_string i in begin
                  Hashtbl.replace transformers dest (src,List.length lst);
                  %stru-{ let _ = ()} (* FIXME *)
                end 
        | None ->  super#stru x
        end
    | x -> super#stru x 
  method! ctyp x =
    match Ctyp.qualified_app_list x with
    | Some (i, lst) ->
        let lst = List.map (fun ctyp -> self#ctyp ctyp) lst in 
        let src = i and dest = IdN.to_string i in begin
          Hashtbl.replace transformers dest (src,List.length lst);
          appl_of_list (%ctyp-{ $lid:dest } :: lst )
        end
    | None -> super#ctyp x
          (* dump the type declarations *)  
  method type_transformers = 
    Hashtbl.fold (fun dest (src,len) acc ->
      (dest,src,len)  :: acc) transformers []

end

    

(*
  This is a general tranversal, which could be bootstrapped
  using our pluggin actually
  Preprocess mtyps, generate type equalities

 *)
let transform_mtyps  (lst:mtyps)=
  let obj = mk_transform_type_eq () in
  let item1 =
    List.map (function
      |`Mutual ls ->
          `Mutual (List.map
                     (fun (s,ty) ->
                       (s, obj#typedecl ty)) ls)
      |`Single (s,ty) ->
          `Single (s, obj#typedecl ty)) lst in
  let new_types = obj#type_transformers in
  (new_types,item1)




















(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/sigs_util.cmo" *)
(* end: *)
