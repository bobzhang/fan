module Ast = FanAst
open LibUtil
let rec normalize_acc =
  function
  | `IdAcc (_loc,i1,i2) ->
      `ExAcc (_loc, (normalize_acc i1), (normalize_acc i2))
  | `IdApp (_loc,i1,i2) ->
      `ExApp (_loc, (normalize_acc i1), (normalize_acc i2))
  | `Ant (_loc,_)|`IdUid (_loc,_)|`IdLid (_loc,_) as i -> `ExId (_loc, i)
let rec to_lid =
  function
  | `IdAcc (_loc,_,i) -> to_lid i
  | `IdLid (_loc,lid) -> lid
  | _ -> assert false
let rec tvar_of_ident =
  function
  | `IdLid (_loc,x)|`IdUid (_loc,x) -> x
  | `IdAcc (_loc,`IdUid (_,x),xs) -> x ^ ("__" ^ (tvar_of_ident xs))
  | _ -> failwith "internal error in the Grammar extension"
let to_string =
  ref (fun _  -> failwithf "Ident.to_string not implemented yet")
let rec lid_of_ident =
  function
  | `IdAcc (_loc,_,i) -> lid_of_ident i
  | `IdLid (_loc,lid) -> lid
  | x -> invalid_arg ("lid_of_ident" ^ (to_string.contents x))
let uid_of_ident =
  let rec aux =
    function
    | `IdAcc (_loc,a,`IdLid (_,_)) -> Some a
    | `IdAcc (_loc,a,b) ->
        (match aux b with
         | None  -> assert false
         | Some v -> Some (`IdAcc (_loc, a, v)))
    | `IdLid (_loc,_) -> None
    | _ -> assert false in
  aux
let rec list_of_acc_ident x acc =
  match x with
  | `IdAcc (_loc,x,y) -> list_of_acc_ident x (list_of_acc_ident y acc)
  | x -> x :: acc
let map_to_string ident =
  let rec aux i acc =
    match i with
    | `IdAcc (_loc,a,b) -> aux a ("_" ^ (aux b acc))
    | `IdApp (_loc,a,b) -> "app_" ^ ((aux a ("_to_" ^ (aux b acc))) ^ "_end")
    | `IdLid (_loc,x) -> x ^ acc
    | `IdUid (_loc,x) -> (String.lowercase x) ^ acc
    | _ -> invalid_arg & ("map_to_string: " ^ (to_string.contents ident)) in
  aux ident ""
let ident_map f x =
  let lst = list_of_acc_ident x [] in
  match lst with
  | [] -> invalid_arg "ident_map identifier [] "
  | (`IdLid (_loc,y))::[] -> `IdLid (_loc, (f y))
  | ls ->
      let l = List.length ls in
      (match List.drop (l - 2) ls with
       | q::(`IdLid (_loc,y))::[] -> `IdAcc (_loc, q, (`IdLid (_loc, (f y))))
       | _ -> invalid_arg ("ident_map identifier" ^ (to_string.contents x)))
let ident_map_of_ident f x =
  let lst = list_of_acc_ident x [] in
  match lst with
  | [] -> invalid_arg "ident_map identifier [] "
  | (`IdLid (_loc,y))::[] -> f y
  | ls ->
      let l = List.length ls in
      (match List.drop (l - 2) ls with
       | q::(`IdLid (_loc,y))::[] -> `IdAcc (_loc, q, (f y))
       | _ -> invalid_arg ("ident_map identifier" ^ (to_string.contents x)))
let ident_map_full f x =
  let _loc = FanAst.loc_of_ident x in
  match ((uid_of_ident x), (lid_of_ident x)) with
  | (Some pre,s) -> `IdAcc (_loc, pre, (`IdLid (_loc, (f s))))
  | (None ,s) -> `IdLid (_loc, (f s))
let eq t1 t2 =
  let strip_locs t = (FanAst.map_loc (fun _  -> FanLoc.ghost))#ident t in
  (strip_locs t1) = (strip_locs t2)