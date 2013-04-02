  
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
(* let rec lid_of_ident = *)
(*     fun *)
(*     [ {| $_.$i |} -> lid_of_ident i *)
(*     | {| $lid:lid |} -> lid *)
(*     | x  -> FanLoc.errorf (loc_of x) "lid_of_ident %s" (Objs.dump_ident x )  ] *)
(* ; *)

(**
   {[
   uid_of_ident {|A.(B.G.h)|};

   Some (IdAcc  (Uid  "A") (IdAcc  (Uid  "B") (Uid  "G")))
   ]}
  *)
(* let uid_of_ident = *)
(*   let rec aux =  *)
(*     fun *)
(*     [ {| $a.$lid:_ |} -> (\** left assoc, pattern right assoc *\) *)
(*       Some a *)
(*     | {| $a.$b |} -> *)
(*         match aux b with *)
(*         [None -> assert false *)
(*         |Some v -> *)
(*             Some {| $a.$v |} ] *)
(*     | {|$lid:_|} -> None *)
(*     | _ -> assert false ] in *)
(*   aux;; *)

(* (\** *)
(*    {[ *)
(*     list_of_acc_ident  {| A.B.c |} []; *)
(*     [Uid  "A"; Uid  "B"; Lid  "c"] *)
(*    ]} *)
(*  *\)     *)
(* let rec list_of_acc_ident x acc = *)
(*     match x with *)
(*     [ {| $x.$y |}  -> *)
(*       list_of_acc_ident x (list_of_acc_ident y acc) *)
(*     | x -> [x :: acc] ]; *)


(**
   {[
   eprintf "@[%a@]@." opr#ident (ident_map_full (fun x -> "meta_" ^ f  )
   {| A.B.s|});
   ]}
   This kind of map makes shadowing extremely difficult 
   [A.B.meta_s]
 *)  
(* let ident_map_full f x = *)
(*   let _loc = loc_of x in  *)
(*   match (uid_of_ident x ,lid_of_ident x ) with *)
(*   [(Some pre, s) -> *)
(*     {| $pre.$(lid:f s) |}  *)
(*   |(None,s) -> *)
(*     {| $(lid:f s ) |}]; *)

(* let eq t1 t2 = *)
(*   let strip_locs t = (Objs.map_loc (fun _ -> FanLoc.ghost))#ident t in *)
(*   strip_locs t1 = strip_locs t2; *)

