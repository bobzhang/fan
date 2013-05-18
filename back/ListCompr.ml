(* +-----------------------------------------------------------------+
   | utilities for list comprehension                                |
   +-----------------------------------------------------------------+ *)
(* loc -> pat -> exp -> exp -> exp     *)
(* let map loc (p:pat) (e:exp) (l:exp) = with exp *)
(*   match (p, e) with *)
(*   | ({:pat| $lid:x |}, {@_| $lid:y |}) when x = y -> l *)
(*   | _ -> *)
(*       if is_irrefut_pat p then *)
(*         {@loc| List.map (fun $p -> $e) $l |} *)
(*       else *)
(*         {@loc| List.fold_right *)
(*           (function *)
(*             | $pat:p when true -> (fun x xs -> x :: xs ) $e *)
(*             | _ -> (fun l -> l) ) $l [] |}  *)


(* let filter loc p b l = with exp *)
(*     if is_irrefut_pat p then *)
(*       {@loc| List.filter (fun $p -> $b) $l |} *)
(*     else *)
(*       {@loc| List.filter (function | $pat:p when true -> $b | _ -> false ) $l |} *)
  
(* let concat _loc l = with exp {| List.concat $l |} *)

(* only this function needs to be exposed *)
(* let rec compr _loc e =  function *)
(*   | [`gen (p, l)] -> map _loc p e l *)
(*   | `gen (p, l):: `cond b :: items -> *)
(*       compr _loc e (`gen (p, filter _loc p b l) :: items) *)
(*   | `gen (p, l) :: ( `gen (_, _) :: _  as is ) -> *)
(*       concat _loc (map _loc p (compr _loc e is) l) *)
(*   | _ -> raise XStream.Failure  *)


