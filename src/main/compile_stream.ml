open Astf
open Ast_gen

(*
  identifiers referenced:
  {[
   peek junk sempty NotConsumed count Error  t ising lsing icons lcons iapp lapp slazy sempty
  ]}

*)



type spat_comp =
  | When of loc * pat * exp option  (* pat with predicate *)

  | Match of loc * pat * exp (* pat with pattern match *)
        
  | Str of loc * pat  (* pat as stream *)


(* [exp option] is for the error message *)
type stream_pat = (spat_comp * exp option)
      
type stream_pats = stream_pat list

(* the second case is for [Stream.count] *)      
type stream_case = (stream_pats * pat option * exp)
      
type stream_cases = stream_case list
      
(* default module name ["Stream"] for compatibility *)
let grammar_module_name = ref "Streamf"
(** BOOTSTRAPPING,  associated with module fstream*)

let gm () = !grammar_module_name
    
let strm_n = "__strm"

let peek_fun _loc = %exp{ $uid{gm()}.peek }
    
let junk_fun _loc = %exp{ $uid{gm()}.junk }


let empty _loc =  %exp{ $uid{gm()}.sempty }

(* Predicate whether the expression is a constructor application *)    
let rec  is_constr_apply a =
  match a with 
  | %exp{ $uid:_ } -> true
  | %exp{ $x $_ } -> is_constr_apply x
  | %exp{ $lid:_ } -> false        
  | _ -> false 


(** Approximation
   whether expression  e would raise expression [NotConsumed] *)  
let rec handle_failure e =
  match e with
  | %exp{ try $_ with | $uid:m.NotConsumed -> $e } when m = gm()
    ->  handle_failure e
  | %exp{ match $me with | $a  } ->
      let rec case_handle_failure = function
          | %case{ $a1 | $a2 } ->
            case_handle_failure a1 && case_handle_failure a2
          | %case{ $pat:_ -> $e } -> handle_failure e
          | _ -> false in
      handle_failure me && case_handle_failure a
  | %exp{ let $bi in $e } ->
      let rec bind_handle_failure = function
        | %bind{ $b1 and $b2 } ->
            bind_handle_failure b1 && bind_handle_failure b2
        | %bind{ $_ = $e } -> handle_failure e
        | _ -> false  in
      bind_handle_failure bi && handle_failure e
  | #literal | #vid'-> true
  |  %exp{ function | $_  }  -> true
  | %exp{raise $uid:m.NotConsumed} -> m <> gm()
  | %exp{raise $_ } -> true 
  | %exp{ $f $x } ->
      is_constr_apply f && handle_failure f && handle_failure x
  | _ -> false 


let stream_pattern_component (skont : exp) (ckont : exp) (x:spat_comp) : exp =
  match (x:spat_comp) with 
  | When (_loc, p, None) ->
      %exp{match ${peek_fun _loc} $lid:strm_n with
      | Some $p ->
          (${junk_fun _loc} $lid:strm_n; $skont)
      | _ -> $ckont  }
  | When (_loc, p, Some w) ->
      %exp{match ${peek_fun _loc} $lid:strm_n with
      | Some $p when $w ->
          ( ${junk_fun _loc} $lid:strm_n; $skont)
      | _ -> $ckont  }
  | Match (_loc, p, e) ->
      (* Utilities for [Stream] optimizations  *)
      let rec pat_eq_exp p e =
        match (p, e) with
        | (%pat{ $lid:a }, %exp@_{ $lid:b }) 
        | (%pat{ $uid:a }, %exp@_{ $uid:b }) -> a = b
        | (%pat{ $p1 $p2 }, %exp@_{ $e1 $e2 }) ->
            pat_eq_exp p1 e1 && pat_eq_exp p2 e2
        | _ -> false in
      (* inlining *)
      let e =
        match e with
        | %exp{ function | ($lid:v : _ $uid:m.t ) -> $e  } when v = strm_n && m = gm() -> e
        | _ -> %exp{ $e $lid:strm_n }  in
      (* Simplify it *)
      if pat_eq_exp p skont then
        if %p{%exp{raise $uid:m.NotConsumed} when m = gm() }  ckont || handle_failure e then
          e
        else
          %exp{ try $e with | $uid{gm()}.NotConsumed -> $ckont  }
      else
        if %p{%exp{raise $uid:m.NotConsumed} when m = gm() } ckont then
          %exp{ let $p = $e in $skont }
        else
          if pat_eq_exp %pat{ Some $p } skont then
            %exp{ try Some $e with | $uid{gm()}.NotConsumed -> $ckont  }
          else
            if %p{ %exp{raise $_}}  ckont then
              let tst =
                if handle_failure e then e
                else %exp{ try $e with | $uid{gm()}.NotConsumed -> $ckont  }  in
              %exp{ let $p = $tst in $skont }
            else
              %exp{match (try Some $e with | $uid{gm()}.NotConsumed -> None)  with
              | Some $p -> $skont
              | _ -> $ckont  }
  | Str (_loc, p) ->
      (* the substitution does not change the semantics, it is only
         for optimization. Such Ast-level optimization may not be necessary
       *)
      let rec subst (v:string) (e:exp)  =
        let _loc = loc_of e in
        match e with
        | %exp{ $lid:x } ->
            let x = if x = v then strm_n else x in
            %exp{ $lid:x }
        | ( %exp{ $uid:_ }  | %exp{ $int:_ } 
        | %exp{ $chr:_ }  | %exp{ $str:_ } 
        | %exp{ $_ . $_ } )-> e
        | %exp{ let $rec:rf $bi in $e } ->
            %exp{ let $rec:rf ${subst_bind v bi} in ${subst v e} }
        | %exp{ $e1 $e2 } -> %exp{ ${subst v e1} ${subst v e2} }
        | %exp{ ( $par:e ) } -> %exp{ ( $par{subst v e} ) }
        | %exp{ $e1, $e2 } -> %exp{ ${subst v e1}, ${subst v e2} }
        | _ -> raise Not_found 
      and subst_bind v =  function
        | %bind@_loc{ $b1 and $b2 } ->
            %bind{ ${subst_bind v b1} and ${subst_bind v b2} }
        | %bind@_loc{ $lid:v' = $e } ->
            %bind{ $lid:v' = ${if v = v' then e else subst v e} }
        | _ -> raise Not_found  in

      try
        match p with
        | %pat{ $lid:v } -> subst v skont
        | _ -> raise Not_found 
      with
        Not_found -> %exp{ let $p = $lid:strm_n in $skont } 

let rec stream_pattern _loc
    (x,epo,e)
    (ekont:exp option -> exp) =
  match x with 
  | [] ->
      (match epo with
      | Some ep -> %exp{ let $ep = $uid{gm()}.count $lid:strm_n in $e }
      | _ -> e )
  | (spc, err) :: spcl ->
      let skont =
        let ekont0 err =
          let str =
            match err with
            | Some estr -> estr
            | _ -> %exp{ "" }  in
          %exp{ raise ($uid{gm()}.Error $str) } in
        stream_pattern _loc (spcl,epo, e) ekont0 in
      let ckont = ekont err in
      stream_pattern_component skont ckont spc 

(* split the [stream_cases] *)
let rec group_terms (xs:stream_cases) =
  match xs with
  | ((When (_loc, p, w), None) :: spcl, epo, e) :: spel ->
    let (tspel, spel) = group_terms spel in
    ((p, w, _loc, spcl, epo, e) :: tspel, spel)
  | spel -> ([], spel) 

let stream_patterns_term _loc (ekont:unit -> exp) tspel : exp =
  let pel =
    List.fold_right
      (fun (p, w, _loc, spcl, epo, e) acc ->
        let p = %pat{ Some $p } in
        let e =
          let ekont err =
            let str =
              match err with
              | Some estr -> estr
              | _ -> %exp{ "" }  in
            %exp{ raise ($uid{gm()}.Error $str) } in
          let skont = stream_pattern _loc (spcl, epo, e) ekont  in
          %exp{ begin  ${junk_fun _loc} $lid:strm_n; $skont  end } in
          match w with
          | Some w -> %case{ $pat:p when $w -> $e  | $acc }
          | None -> %case{ $pat:p -> $e  | $acc } )
      tspel %case{ _ -> ${ekont () }} in
  %exp{ match ${peek_fun _loc} $lid:strm_n with | $pel  } 


  
let rec parser_cases _loc (x:stream_cases) =
  match x with 
  | [] -> %exp{ raise $uid{gm()}.NotConsumed }
  | spel ->
      match group_terms spel with
      | ([], x :: spel) ->
          stream_pattern _loc x (fun _ -> parser_cases _loc spel)
      | (tspel, spel) ->
          stream_patterns_term _loc (fun _ -> parser_cases _loc spel) tspel 

(* it call [parser_cases] *)  
let cparser _loc  pc =
  let e = parser_cases _loc pc in
  let p = %pat{ ($lid:strm_n : _ $uid{gm()}.t ) } in
  %exp{ fun $p -> $e } 

(* mainly used in inline (fun __x -> y) e
   let __x = e in y
 *)    
(* let cparser_match _loc me bpo pc = *)
(*   let pc = parser_cases _loc pc in *)
(*   let e = *)
(*     match bpo with *)
(*     | Some bp -> %exp{ let $bp = $(uid:gm()).count $lid:strm_n in $pc } *)
(*     | None -> pc   in *)
(*   match me with *)
(*   | %exp{ $lid:x } when x = strm_n -> e *)
(*   | _ -> %exp{ let ($lid:strm_n : _ $(uid:gm()).t ) = $me in $e }  *)







(********************************************************)
(* Stream expression                                    *)
(********************************************************)
type sexp_comp =
  | Trm of loc * exp
  | Ntr of loc * exp 

(** Approximation algorithm to predicate whether x is a computing expression or not
 *)        
let  not_computing x =
  let rec aux  x =
    match x with
    | #literal -> true
    | #vid' -> true
    | %exp{ $x $y } -> is_cons_apply_not_computing x && aux  y
    | _ -> false 
  and is_cons_apply_not_computing = function
    | %exp{ $uid:_ } -> true
    | %exp{ $lid:_ } -> false
    | %exp{ $x $y } -> is_cons_apply_not_computing x && aux y
    | _ -> false  in
  aux x 

let slazy _loc e =
  match e with
  | %exp{ $f () } ->
      (match f with
      | %exp{ $lid:_ } -> f
      | _ -> %exp{ fun _ -> $e } )
  | _ -> %exp{ fun _ -> $e } 

(* FIXME horrible error message *)
let rec cstream gloc =  function
  | [] -> let _loc = gloc in
      %exp{ $uid{gm()}.sempty }
  | [Trm (_loc, e)] ->
      if not_computing e
      then %exp{ $uid{gm()}.ising $e }
      else %exp{ $uid{gm()}.lsing ${slazy _loc e} }
  | Trm (_loc, e) :: secl ->
      if not_computing e
      then %exp{ $uid{gm()}.icons $e ${cstream gloc secl} }
      else %exp{ $uid{gm()}.lcons ${slazy _loc e} ${cstream gloc secl} }
  | [Ntr (_loc, e)] ->
      if not_computing e then e
      else %exp{ $uid{gm()}.slazy ${slazy _loc e} }
  | Ntr (_loc, e) :: secl ->
      if not_computing e then %exp{ $uid{gm()}.iapp $e ${cstream gloc secl} }
      else %exp{ $uid{gm()}.lapp ${slazy _loc e} ${cstream gloc secl} } 
          

(* local variables: *)
(* compile-command: "cd ../main_annot && pmake compile_stream.cmo" *)
(* end: *)
