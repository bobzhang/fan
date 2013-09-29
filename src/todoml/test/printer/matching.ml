
(* open Misc *)
(* open Asttypes *)
(* open Primitive *)
(* open Types *)
(* open Typedtree *)
(* open Lambda *)
(* open Parmatch *)
(* open Printf *)

(* (\*  See Peyton-Jones, ``The Implementation of functional programming *)
(*     languages'', chapter 5. *\) *)
(* (\* *)
(*   Bon, au commencement du monde c'etait vrai. *)
(*   Now, see Lefessant-Maranget ``Optimizing Pattern-Matching'' ICFP'2001 *)
(* *\) *)


(* (\* *)
(*    Many functions on the various data structures ofthe algorithm : *)
(*      - Pattern matrices. *)
(*      - Default environments: mapping from matrices to exit numbers. *)
(*      - Contexts:  matrices whose column are partitioned into *)
(*        left and right. *)
(*      - Jump sumaries: mapping from exit numbers to contexts *)
(* *\) *)

(* type matrix = pattern list list *)

(* let add_omega_column pss = List.map (fun ps -> omega::ps) pss *)

(* type ctx = {left:pattern list ; right:pattern list} *)

(* let pretty_ctx ctx = *)
(*   List.iter *)
(*     (fun {left=left ; right=right} -> *)
(*       prerr_string "LEFT:" ; *)
(*       pretty_line left ; *)
(*       prerr_string " RIGHT:" ; *)
(*       pretty_line right ; *)
(*       prerr_endline "") *)
(*     ctx *)

(* let le_ctx c1 c2 = *)
(*   le_pats c1.left c2.left && *)
(*   le_pats c1.right c2.right *)

(* let lshift {left=left ; right=right} = match right with *)
(* | x::xs -> {left=x::left ; right=xs} *)
(* | _ ->  assert false *)

(* let lforget {left=left ; right=right} = match right with *)
(* | x::xs -> {left=omega::left ; right=xs} *)
(* |  _ -> assert false *)

(* let rec small_enough n = function *)
(*   | [] -> true *)
(*   | _::rem -> *)
(*       if n <= 0 then false *)
(*       else small_enough (n-1) rem *)

(* let ctx_lshift ctx = *)
(*   if small_enough 31 ctx then *)
(*     List.map lshift ctx *)
(*   else (\* Context pruning *\) begin *)
(*     get_mins le_ctx (List.map lforget ctx) *)
(*   end *)

(* let  rshift {left=left ; right=right} = match left with *)
(* | p::ps -> {left=ps ; right=p::right} *)
(* | _ -> assert false *)

(* let ctx_rshift ctx = List.map rshift ctx *)

(* let rec nchars n ps = *)
(*   if n <= 0 then [],ps *)
(*   else match ps with *)
(*   | p::rem -> *)
(*     let chars, cdrs = nchars (n-1) rem in *)
(*     p::chars,cdrs *)
(*   | _ -> assert false *)

(* let  rshift_num n {left=left ; right=right} = *)
(*   let shifted,left = nchars n left in *)
(*   {left=left ; right = shifted@right} *)

(* let ctx_rshift_num n ctx = List.map (rshift_num n) ctx *)

(* (\* Recombination of contexts (eg: (_,_)::p1::p2::rem ->  (p1,p2)::rem) *)
(*   All mutable fields are replaced by '_', since side-effects in *)
(*   guards can alter these fields *\) *)

(* let combine {left=left ; right=right} = match left with *)
(* | p::ps -> {left=ps ; right=set_args_erase_mutable p right} *)
(* | _ -> assert false *)

(* let ctx_combine ctx = List.map combine ctx *)

(* let ncols = function *)
(*   | [] -> 0 *)
(*   | ps::_ -> List.length ps *)


(* exception NoMatch *)
(* exception OrPat *)


let ctx_matcher p =
  let p = normalize_pat p in
  match p.pat_desc with
  | Tpat_construct (_, _, cstr,omegas,_) ->
      (fun q rem -> match q.pat_desc with
      | Tpat_construct (_, _, cstr',args,_) when cstr.cstr_tag=cstr'.cstr_tag ->
          p,args @ rem
      | Tpat_any -> p,omegas @ rem
      | _ -> raise NoMatch)
  | Tpat_constant cst ->
      (fun q rem -> match q.pat_desc with
      | Tpat_constant cst' when const_compare cst cst' = 0 ->
          p,rem
      | Tpat_any -> p,rem
      | _ -> raise NoMatch)
  | Tpat_variant (lab,Some omega,_) ->
      (fun q rem -> match q.pat_desc with
      | Tpat_variant (lab',Some arg,_) when lab=lab' ->
          p,arg::rem
      | Tpat_any -> p,omega::rem
      | _ -> raise NoMatch)
  | Tpat_variant (lab,None,_) ->
      (fun q rem -> match q.pat_desc with
      | Tpat_variant (lab',None,_) when lab=lab' ->
          p,rem
      | Tpat_any -> p,rem
      | _ -> raise NoMatch)
  | Tpat_array omegas ->
      let len = List.length omegas in
      (fun q rem -> match q.pat_desc with
      | Tpat_array args when List.length args=len ->
          p,args @ rem
      | Tpat_any -> p, omegas @ rem
      | _ -> raise NoMatch)
  | Tpat_tuple omegas ->
      (fun q rem -> match q.pat_desc with
      | Tpat_tuple args -> p,args @ rem
      | _          -> p, omegas @ rem)
  | Tpat_record (l,_) -> (* Records are normalized *)
      (fun q rem -> match q.pat_desc with
      | Tpat_record (l',_) ->
          let l' = all_record_args l' in
          p, List.fold_right (fun (_, _, _,p) r -> p::r) l' rem
      | _ -> p,List.fold_right (fun (_, _, _,p) r -> p::r) l rem)
  | Tpat_lazy omega ->
      (fun q rem -> match q.pat_desc with
      | Tpat_lazy arg -> p, (arg::rem)
      | _          -> p, (omega::rem))
 | _ -> fatal_error "Matching.ctx_matcher"



