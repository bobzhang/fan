open Ast

open AstLoc

open LibUtil

let rec tvar_of_ident: vid -> string =
  function
  | `Lid (_loc,x)|`Uid (_loc,x) -> x
  | `Dot (_loc,`Uid (_,x),xs) -> x ^ ("__" ^ (tvar_of_ident xs))
  | _ -> failwith "internal error in the Grammar extension"

let map_to_string (ident : vid) =
  let rec aux i acc =
    match i with
    | `Dot (_loc,a,b) -> aux a ("_" ^ (aux b acc))
    | `Lid (_loc,x) -> x ^ acc
    | `Uid (_loc,x) -> (String.lowercase x) ^ acc
    | t -> FanLoc.errorf (loc_of t) "map_to_string: %s" (Objs.dump_vid t) in
  aux ident ""

let to_string (ident : ident) =
  let rec aux i acc =
    match i with
    | `Dot (_loc,a,b) -> aux a ("_" ^ (aux b acc))
    | `Apply (_loc,a,b) -> "app_" ^ ((aux a ("_to_" ^ (aux b acc))) ^ "_end")
    | `Lid (_loc,x) -> x ^ acc
    | `Uid (_loc,x) -> (String.lowercase x) ^ acc
    | t -> FanLoc.errorf (loc_of t) "map_to_string: %s" (Objs.dump_ident t) in
  aux ident ""

let rec to_vid (x : ident) =
  (match x with
   | `Apply _ -> failwithf "Id.to_vid"
   | `Dot (_loc,a,b) -> `Dot (_loc, (to_vid a), (to_vid b))
   | `Lid _|`Uid _|`Ant _ as x -> x : vid )

let ident_map f (x : vid) =
  let lst = list_of_dot x [] in
  match lst with
  | [] -> invalid_arg "ident_map identifier [] "
  | (`Lid (_loc,y))::[] -> `Lid (_loc, (f y))
  | ls ->
      let l = List.length ls in
      (match List.drop (l - 2) ls with
       | q::(`Lid (_loc,y))::[] -> `Dot (_loc, q, (`Lid (_loc, (f y))))
       | _ -> FanLoc.errorf (loc_of x) "ident_map: %s" (Objs.dump_vid x))

let ident_map_of_ident f x =
  (let lst = list_of_dot x [] in
   match lst with
   | [] -> invalid_arg "ident_map identifier [] "
   | (`Lid (_loc,y))::[] -> f y
   | ls ->
       let l = List.length ls in
       (match List.drop (l - 2) ls with
        | q::(`Lid (_loc,y))::[] -> `Dot (_loc, q, (f y))
        | _ ->
            FanLoc.errorf (loc_of x) "ident_map_of_ident: %s"
              (Objs.dump_vid x)) : vid )
