#default_quotation "ident'";;

open AstLoc;
open LibUtil;

(*
  {[  mapping an ident to  a type variable   ]}
 *)  
let rec tvar_of_ident = fun
  [ {| $lid:x |} | {| $uid:x |} -> x
  | {| $uid:x.$xs |} -> x ^ "__" ^ tvar_of_ident xs
  | _ -> failwith "internal error in the Grammar extension" ];

(*
  {[

  map_to_string <:ident< A.B.f.g>>;
  a_b_f_g

  map_to_string <:ident< u>> |> print_string;
  u

  ]}
  see ident_map 
 *)  
let map_to_string ident =
  let rec aux i acc = match i with 
  [ {| $a.$b  |} -> aux a ("_" ^ aux b acc)
  | {| ($a $b) |} -> ("app_" ^(aux a ( "_to_" ^ aux b acc)) ^ "_end")
  | {| $lid:x |} -> x ^ acc
  | {| $uid:x |} -> String.lowercase x ^ acc
  | t -> FanLoc.errorf (loc_of t) "map_to_string: %s" (Objs.dump_ident t)] in 
  aux ident "";


(*
   For qualified identifier, we only use the last qulifier.
   {[
   ident_map (fun x -> "meta_" ^ x ) <:ident< A.B.g >> ;
   IdAcc  (Uid  "B") (Lid  "meta_g")

   ident_map (fun x -> "meta_" ^ x ) <:ident< B.g >> ;
   IdAcc  (Uid  "B") (Lid  "meta_g")

   ident_map (fun x -> "meta_" ^ x ) <:ident< g >> ;
   ident = Lid  "meta_g"

   ]}
 *)
let ident_map f x =
  let lst = list_of_dot x [] in
  match lst with
  [ [] ->  invalid_arg "ident_map identifier [] "
  | [ {| $lid:y |} ]  -> {| $(lid: f y) |}
  | ls ->
      let l = List.length ls in
      match List.drop (l-2) ls with
      [ [ q; {| $lid:y |} ] ->
        {| $q.$(lid: f y) |}
      | _ ->
          FanLoc.errorf (loc_of x) "ident_map: %s" (Objs.dump_ident x) ]];          

(* the same as [ident_map] except f is of type
   [string -> ident ]
 *)
let ident_map_of_ident f x =
  let lst = list_of_dot x [] in
  match lst with
  [ [] ->  invalid_arg "ident_map identifier [] "
  | [ {|$lid:y|} ]  -> f y 
  | ls ->
      let l = List.length ls in
      match List.drop (l-2) ls with
      [ [ q; {|$lid:y|} ] ->
        {|$q.$(f y) |}
      | _ -> FanLoc.errorf (loc_of x) "ident_map_of_ident: %s" (Objs.dump_ident x)]];          
    
