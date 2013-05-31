(* let fun_args _loc args body = with exp *)
(*   if args = [] then {| fun () -> $body |} *)
(*   else List.fold_right (fun arg body ->	{| fun $arg -> $body |}) args body *)
  


(* let _loc = FLoc.ghost  *)

(* let mk_record label_exps : exp= *)
(*   let rec_exps = *)
(*     List.map *)
(*       (fun (label, exp) -> *)
(*         {:rec_exp| $lid:label = $exp |} ) label_exps in *)
(*   `Record (_loc, (sem_of_list rec_exps)) *)

(* let mkfun names acc  = with exp *)
(*   List.fold_right *)
(*   (fun name acc ->  {| function | $lid:name -> $acc |}) names acc  *)

    
(* let (<+<) pats acc = *)
(*   List.fold_right (fun p acc -> {| function | $pat:p -> $acc |} ) pats acc *)


(* let eta_expand (exp:exp) number : exp = *)
(*   let names = List.init number (fun i -> x ~off:0 i ) in *)
(*   mkfun names (exp +> names ) *)


(* let gen_curry_n (acc:exp) ~arity cons n : exp = *)
(*   let args = List.init arity *)
(*       (fun i -> List.init n (fun j -> {:pat| $(id:xid ~off:i j) |})) in *)
(*   let pat = (EP.of_str cons :> pat) in *)
(*   List.fold_right *)
(*     (fun p acc -> {| function | $pat:p -> $acc  |} ) *)
(*     (List.map (fun lst -> appl_of_list (pat:: lst)) args) acc *)

(* let currying cases ~arity = *)
(*   let cases = bar_of_list cases in *)
(*   if  arity >= 2 then  *)
(*     let names = List.init arity (fun i -> x ~off:i 0) in *)
(*     let exps = List.map (fun s-> {| $lid:s |} ) names in *)
(*     let x = tuple_com exps in *)
(*     mkfun names  {| match $x with | $cases |}  *)
(*   else {| function | $cases |} *)


(* let unknown len = *)
(*   if len = 0 then *)
(*     {| self#unknown  |} *)
(*   else {| failwith "not implemented!" |} *)



(* (\*************************************************************************\) *)
(* (\* Multiple staging code generation     *\) *)
  
  
(* let mee_comma x y = *)
(*   {:exp| {:exp'| $($x), $($y) |} |}(\* BOOTSTRAPPING*\) *)


(* (\** {:exp| {:ep| $($x) $($y) |}|} *)
(*     both work, I did not see obvious performance difference *\) *)
(* let mee_app x y = (\* BOOTSTRAPPING *\) *)
(*   {:exp| {:exp'| $($x) $($y) |}|} *)


(* let mee_of_str s = (\*    BOOTSTRAPING *\)   *)
(*   let len = String.length s in *)
(*   if s.[0]='`' then *)
(*     let s = String.sub s 1 (len - 1) in  *)
(*     {:exp|{:exp'|$(vrn:($str:s))|}|} *)
(*   else *)
(*      {:exp| {:exp'| $(uid:$str:s) |} |}  *)



(* let mk_tuple_ee = function (\* BOOTSTRAPPING *\) *)
(*   | [] -> invalid_arg "mktupee arity is zero " *)
(*   | [x] -> x *)
(*   | xs  -> *)
(*       let v = List.reduce_right mee_comma xs in *)
(*       {:exp| {:exp'| $(par:($v))|}|}   *)

  

(* let mee_record_col label exp = *)
(*   {:exp| {:rec_exp'| $(lid:($str:label)) = $($exp) |}|}  *)


(* let mee_record_semi a b = {:exp| {:rec_exp'| $($a);$($b) |} |} *)


(* let mk_record_ee label_exps = *)
(*   label_exps *)
(*   |> List.map (fun (label,exp) -> mee_record_col label exp) *)
(*   |> (fun es -> *)
(*       {:exp| {:exp'| { $($(List.reduce_right mee_record_semi es)) } |}|} ) *)

      
