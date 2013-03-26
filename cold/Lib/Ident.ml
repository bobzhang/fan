open AstLoc
open LibUtil
let rec to_lid =
  function
  | `Dot (_loc,_,i) -> to_lid i
  | `Lid (_loc,lid) -> lid
  | _ -> assert false
let rec tvar_of_ident =
  function
  | `Lid (_loc,x)|`Uid (_loc,x) -> x
  | `Dot (_loc,`Uid (_,x),xs) -> x ^ ("__" ^ (tvar_of_ident xs))
  | _ -> failwith "internal error in the Grammar extension"
let rec lid_of_ident =
  function
  | `Dot (_loc,_,i) -> lid_of_ident i
  | `Lid (_loc,lid) -> lid
  | x -> FanLoc.errorf (loc_of x) "lid_of_ident %s" (FanObjs.dump_ident x)
let uid_of_ident =
  let rec aux =
    function
    | `Dot (_loc,a,`Lid (_,_)) -> Some a
    | `Dot (_loc,a,b) ->
        (match aux b with
         | None  -> assert false
         | Some v -> Some (`Dot (_loc, a, v)))
    | `Lid (_loc,_) -> None
    | _ -> assert false in
  aux
let rec list_of_acc_ident x acc =
  match x with
  | `Dot (_loc,x,y) -> list_of_acc_ident x (list_of_acc_ident y acc)
  | x -> x :: acc
let map_to_string ident =
  let rec aux i acc =
    match i with
    | `Dot (_loc,a,b) -> aux a ("_" ^ (aux b acc))
    | `App (_loc,a,b) -> "app_" ^ ((aux a ("_to_" ^ (aux b acc))) ^ "_end")
    | `Lid (_loc,x) -> x ^ acc
    | `Uid (_loc,x) -> (String.lowercase x) ^ acc
    | t ->
        FanLoc.errorf (loc_of t) "map_to_string: %s" (FanObjs.dump_ident t) in
  aux ident ""
let ident_map f x =
  let lst = list_of_acc_ident x [] in
  match lst with
  | [] -> invalid_arg "ident_map identifier [] "
  | (`Lid (_loc,y))::[] -> `Lid (_loc, (f y))
  | ls ->
      let l = List.length ls in
      (match List.drop (l - 2) ls with
       | q::(`Lid (_loc,y))::[] -> `Dot (_loc, q, (`Lid (_loc, (f y))))
       | _ -> FanLoc.errorf (loc_of x) "ident_map: %s" (FanObjs.dump_ident x))
let ident_map_of_ident f x =
  let lst = list_of_acc_ident x [] in
  match lst with
  | [] -> invalid_arg "ident_map identifier [] "
  | (`Lid (_loc,y))::[] -> f y
  | ls ->
      let l = List.length ls in
      (match List.drop (l - 2) ls with
       | q::(`Lid (_loc,y))::[] -> `Dot (_loc, q, (f y))
       | _ ->
           FanLoc.errorf (loc_of x) "ident_map_of_ident: %s"
             (FanObjs.dump_ident x))
let ident_map_full f x =
  let _loc = loc_of x in
  match ((uid_of_ident x), (lid_of_ident x)) with
  | (Some pre,s) -> `Dot (_loc, pre, (`Lid (_loc, (f s))))
  | (None ,s) -> `Lid (_loc, (f s))
let eq t1 t2 =
  let strip_locs t = (Objs.map_loc (fun _  -> FanLoc.ghost))#ident t in
  (strip_locs t1) = (strip_locs t2)