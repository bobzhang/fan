



open Astfn
open Astn_util
open Util



let map_to_string (ident:vid) =  
  let rec aux i acc =
    match i with 
    | `Dot(a,b) -> aux a ("_" ^ aux b acc)
    | `Lid x -> x ^ acc
    | `Uid x  -> String.lowercase x ^ acc
    | t -> failwithf "map_to_string: %s" (Astfn_print.dump_vid t) in 
  aux ident ""

let to_string (ident:ident) = 
  let rec aux i acc = 
    match i with 
    | `Dot(a,b) -> aux a ("_" ^ aux b acc)
    | `Apply (a,b) -> "app_" ^ aux a ( "_to_" ^ aux b acc)  ^ "_end"
    | `Lid x -> x ^ acc
    | `Uid x -> String.lowercase x ^ acc
    | t -> failwithf "map_to_string: %s" (Astfn_print.dump_ident t) in 
  aux ident ""

let rec to_vid   (x:ident) : vid =
  match x with
  |`Apply _ -> %invalid_arg{}
  | `Dot(a,b) -> dot  (to_vid a)  (to_vid b)
  | `Lid _ | `Uid _ | `Ant _ as x -> x


let ident_map f (x:vid) = 
  let lst = Ast_basic.N.list_of_dot x [] in
  match lst with
  | [] ->  %invalid_arg{}
  | [`Lid y ]  -> lid (f y)
  | ls ->
      let l = List.length ls in
      match Listf.drop (l-2) ls with
      | [ q; `Lid y ] ->
          `Dot(q,`Lid (f y))
      | _ ->
          failwithf "ident_map: %s" (Astfn_print.dump_vid x)


let ident_map_of_ident f x  : vid =
  let lst = Ast_basic.N.list_of_dot x [] in
  match lst with
  | [] ->  %invalid_arg{}
  | [ `Lid y ]  -> f y 
  | ls ->
      let l = List.length ls in
      match Listf.drop (l-2) ls with
      | [ q; `Lid y ] -> `Dot(q,f y)
      | _ -> failwithf "ident_map_of_ident: %s" (Astfn_print.dump_vid x)
    

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/idN.cmo" *)
(* end: *)
