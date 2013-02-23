#default_quotation "ident";;

open AstLoc;
open LibUtil;

(*
  {[
  
  ]}
 *)
let rec normalize_acc =fun
  [ {| $i1.$i2 |} ->
    {:expr| $(normalize_acc i1).$(normalize_acc i2) |}
  | {| ($i1 $i2) |} ->
      {:expr| $(normalize_acc i1) $(normalize_acc i2) |}
  | {| $anti:_ |} | {@_loc| $uid:_ |} |
    {@_loc| $lid:_ |} as i -> {:expr| $id:i |} ];


let rec to_lid = fun
  [ {| $_ . $i |} -> to_lid i
  | {| $lid:lid |} -> lid
  | _                     -> assert false ];


(*
  {[
  mapping an ident to  a type variable 
  ]}
 *)  
let rec tvar_of_ident = fun
  [ {| $lid:x |} | {| $uid:x |} -> x
  | {| $uid:x.$xs |} -> x ^ "__" ^ tvar_of_ident xs
  | _ -> failwith "internal error in the Grammar extension" ];


  
(* let to_string = ref (fun _ -> failwithf "Ident.to_string not implemented yet"); *)
(*   to_string_of_printer opr#ident *)
(* ; *)
(* let eprint = ref (fun _ -> failwithf "Ident.eprint not implemented yet");   *)
(* let eprint v = eprintf "@[%a@]@." opr#ident v; *)
  
(**
   @return a string 
   {[
   lid_of_ident << A.B.c >>;

   string = "c"
   ]}
   
 *)      
let rec lid_of_ident =
    fun
    [ {| $_.$i |} -> lid_of_ident i
    | {| $lid:lid |} -> lid
    | x  -> FanLoc.errorf (loc_of x) "lid_of_ident %s" (FanObjs.dump_ident x )  ]
;

(**
   {[
   uid_of_ident {|A.(B.G.h)|};

   Some (IdAcc  (Uid  "A") (IdAcc  (Uid  "B") (Uid  "G")))
   ]}
  *)
let uid_of_ident =
  let rec aux = 
    fun
    [ {| $a.$lid:_ |} -> (** left assoc, pattern right assoc *)
      Some a
    | {| $a.$b |} ->
        match aux b with
        [None -> assert false
        |Some v ->
            Some {| $a.$v |} ]
    | {|$lid:_|} -> None
    | _ -> assert false ] in
  aux;

(**
   {[
    list_of_acc_ident  {| A.B.c |} [];
    [Uid  "A"; Uid  "B"; Lid  "c"]
   ]}
 *)    
let rec list_of_acc_ident x acc =
    match x with
    [ {| $x.$y |}  ->
      list_of_acc_ident x (list_of_acc_ident y acc)
    | x -> [x :: acc] ];



(*
  {[

  map_to_string <:ident< A.B.f.g>>;
  a_b_f_g

  map_to_string <:ident< u>> |> print_string;
  u

  map_to_string <:ident<
  Camlp4.Sig.MakeCamlp4Ast(Camlp4.PreCast.Loc).meta_bool >> 

  ]}
  see ident_map 
 *)  
let map_to_string ident =
  let rec aux i acc = match i with 
  [ {| $a.$b  |} -> aux a ("_" ^ aux b acc)
  | {| ($a $b) |} -> ("app_" ^(aux a ( "_to_" ^ aux b acc)) ^ "_end")
  | {| $lid:x |} -> x ^ acc
  | {| $uid:x |} -> String.lowercase x ^ acc
  | t -> FanLoc.errorf (loc_of t) "map_to_string: %s" (FanObjs.dump_ident t)] in 
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
  let lst = list_of_acc_ident x [] in
  match lst with
  [ [] ->  invalid_arg "ident_map identifier [] "
  | [ {| $lid:y |} ]  -> {| $(lid: f y) |}
  | ls ->
      let l = List.length ls in
      match List.drop (l-2) ls with
      [ [ q; {| $lid:y |} ] ->
        {| $q.$(lid: f y) |}
      | _ ->
          FanLoc.errorf (loc_of x) "ident_map: %s" (FanObjs.dump_ident x) ]];          

(* the same as [ident_map] except f is of type
   [string -> ident ]
 *)
let ident_map_of_ident f x =
  let lst = list_of_acc_ident x [] in
  match lst with
  [ [] ->  invalid_arg "ident_map identifier [] "
  | [ {|$lid:y|} ]  -> f y 
  | ls ->
      let l = List.length ls in
      match List.drop (l-2) ls with
      [ [ q; {|$lid:y|} ] ->
        {|$q.$(f y) |}
      | _ -> FanLoc.errorf (loc_of x) "ident_map_of_ident: %s" (FanObjs.dump_ident x)]];          
    
(**
   {[
   eprintf "@[%a@]@." opr#ident (ident_map_full (fun x -> "meta_" ^ f  )
   {| A.B.s|});
   ]}
   This kind of map makes shadowing extremely difficult 
   [A.B.meta_s]
 *)  
let ident_map_full f x =
  let _loc = loc_of x in 
  match (uid_of_ident x ,lid_of_ident x ) with
  [(Some pre, s) ->
    {| $pre.$(lid:f s) |} 
  |(None,s) ->
    {| $(lid:f s ) |}];

let eq t1 t2 =
  let strip_locs t = (FanObjs.map_loc (fun _ -> FanLoc.ghost))#ident t in
  strip_locs t1 = strip_locs t2;

