



open AstN
open AstLibN
open LibUtil

let rec tvar_of_ident : vid -> string =
  function
  | `Lid x | `Uid x -> x
  | `Dot(`Uid x,xs) -> x ^ "__" ^ tvar_of_ident xs
  | _ -> failwith "internal error in the Grammar extension" 

let map_to_string (ident:vid) =  
  let rec aux i acc =
    match i with 
    | `Dot(a,b) -> aux a ("_" ^ aux b acc)
    | `Lid x -> x ^ acc
    | `Uid x  -> String.lowercase x ^ acc
    | t -> failwithf "map_to_string: %s" (ObjsN.dump_vid t) in 
  aux ident ""

let to_string (ident:ident) = 
  let rec aux i acc = 
    match i with 
    | `Dot(a,b) -> aux a ("_" ^ aux b acc)
    | `Apply (a,b) -> "app_" ^ aux a ( "_to_" ^ aux b acc)  ^ "_end"
    | `Lid x -> x ^ acc
    | `Uid x -> String.lowercase x ^ acc
    | t -> failwithf "map_to_string: %s" (ObjsN.dump_ident t) in 
  aux ident ""

let rec to_vid   (x:ident) : vid =
  match x with
  |`Apply _ -> failwith "IdN.to_vid"
  | `Dot(a,b) -> dot  (to_vid a)  (to_vid b)
  | `Lid _ | `Uid _ | `Ant _ as x -> x


let ident_map f (x:vid) = 
  let lst = list_of_dot x [] in
  match lst with
  | [] ->  invalid_arg "ident_map identifier [] "
  | [`Lid y ]  -> lid (f y)
  | ls ->
      let l = List.length ls in
      match List.drop (l-2) ls with
      | [ q; `Lid y ] ->
          `Dot(q,`Lid (f y))
      | _ ->
          failwithf "ident_map: %s" (ObjsN.dump_vid x)

let ident_map_of_ident f x  : vid =
  let lst = list_of_dot x [] in
  match lst with
  | [] ->  invalid_arg "ident_map identifier [] "
  | [ `Lid y ]  -> f y 
  | ls ->
      let l = List.length ls in
      match List.drop (l-2) ls with
      | [ q; `Lid y ] -> `Dot(q,f y)
      | _ -> failwithf "ident_map_of_ident: %s" (ObjsN.dump_vid x)
    
