open Ast
open AstLoc

(*
  identifiers referenced:
  {[
   peek junk sempty Failure count Error  t ising lsing icons lcons iapp lapp slazy sempty
  ]}

*)


#default_quotation "exp'";;
type spat_comp =
  | SpTrm of FanLoc.t * pat * exp option 
  | SpNtr of FanLoc.t * pat * exp
  | SpStr of FanLoc.t * pat 

type sexp_comp =
  |SeTrm of FanLoc.t * exp
  | SeNtr of FanLoc.t * exp 

(* default module name ["Stream"] for compatibility *)
let grammar_module_name = ref "XStream" (* BOOTSTRAPPING *)

let gm () = !grammar_module_name
    
let strm_n = "__strm"

let peek_fun _loc = {| $(uid:gm()).peek |}
    
let junk_fun _loc = {| $(uid:gm()).junk |}


let empty _loc =
  {| $(uid:gm()).sempty |} 
let is_raise  = function
  | {| raise $_ |} -> true
  | _ -> false
        
let is_raise_failure  = function
  | {| raise $uid:m.Failure |} when m = gm() -> true
  | _ -> false
  
let rec handle_failure e =
  match e with
  | {| try $_ with | $(uid:m).Failure -> $e |}  (* {:case'|$(uid:m).Failure -> $e|} *)
    when m = gm()
    ->  handle_failure e
  | {| match $me with | $a  |} ->
      let rec case_handle_failure = function
          | {:case'| $a1 | $a2 |} ->
            case_handle_failure a1 && case_handle_failure a2
          | {:case'| $pat:_ -> $e |} -> handle_failure e
          | _ -> false 
      in handle_failure me && case_handle_failure a
  | {| let $bi in $e |} ->
      let rec binding_handle_failure = function
        | {:binding'| $b1 and $b2 |} ->
            binding_handle_failure b1 && binding_handle_failure b2
        | {:binding'| $_ = $e |} -> handle_failure e
        | _ -> false  in
      binding_handle_failure bi && handle_failure e
  | {| $lid:_ |} | {| $int:_ |} | {| $str:_ |} |
    {| $chr:_ |} | {| function | $_  |} | {| $uid:_ |} ->
      true
  | {| raise $e |} ->
      begin match e with
      | {| $uid:m.Failure |} when m = gm() -> false
      | _ -> true
      end
  | {| $f $x |} ->
      is_constr_apply f && handle_failure f && handle_failure x
  | _ -> false 
and is_constr_apply = function
  | {| $uid:_ |} -> true
  | {| $lid:_ |} -> false
  | {| $x $_ |} -> is_constr_apply x
  | _ -> false 

let rec subst v e =
  let _loc = loc_of e in
  match e with
  | {| $lid:x |} ->
      let x = if x = v then strm_n else x in
      {| $lid:x |}
  | ( {| $uid:_ |}  | {| $int:_ |} 
    | {| $chr:_ |}  | {| $str:_ |} 
    | {| $_ . $_ |} )-> e
  | {| let $rec:rf $bi in $e |} ->
      {| let $rec:rf $(subst_binding v bi) in $(subst v e) |}
  | {| $e1 $e2 |} -> {| $(subst v e1) $(subst v e2) |}
  | {| ( $par:e ) |} -> {| ( $(par:subst v e) ) |}
  | {| $e1, $e2 |} -> {| $(subst v e1), $(subst v e2) |}
  | _ -> raise Not_found 
and subst_binding v =  function
  | {:binding@_loc| $b1 and $b2 |} ->
      {:binding'| $(subst_binding v b1) and $(subst_binding v b2) |}
  | {:binding@_loc| $lid:v' = $e |} ->
      {:binding'| $lid:v' = $(if v = v' then e else subst v e) |}
  | _ -> raise Not_found 

let stream_pattern_component skont ckont = function
  | SpTrm (_loc, p, None) ->
      {|
      match $(peek_fun _loc) $lid:strm_n with
      | Some $p ->
          begin  $(junk_fun _loc) $lid:strm_n; $skont  end
      | _ -> $ckont  |}
  | SpTrm (_loc, p, (Some w)) ->
      {|
      match $(peek_fun _loc) $lid:strm_n with
      | Some $p when $w ->
           begin  $(junk_fun _loc) $lid:strm_n; $skont  end
      | _ -> $ckont  |}
  | SpNtr (_loc, p, e) ->
      let e =
        match e with
        | {| function | ($lid:v : _ $uid:m.t ) -> $e  |} when v = strm_n && m = gm() -> e
        | _ -> {| $e $lid:strm_n |}  in
      (* Simplify it *)
      if Exp.pattern_eq_expression p skont then
        if is_raise_failure ckont then e
        else if handle_failure e then e
        else {| try $e with | $(uid:gm()).Failure -> $ckont  |}
      else if is_raise_failure ckont then
        {| let $p = $e in $skont |}
      else if Exp.pattern_eq_expression {:pat'| Some $p |} skont then
        {| try Some $e with | $(uid:gm()).Failure -> $ckont  |}
      else if is_raise ckont then
        let tst =
          if handle_failure e then e
          else {| try $e with | $(uid:gm()).Failure -> $ckont  |}  in
        {| let $p = $tst in $skont |}
      else
        {|
        match (try Some $e with | $(uid:gm()).Failure -> None)  with
        | Some $p -> $skont
        | _ -> $ckont  |}
  | SpStr (_loc, p) ->
      try
        match p with
        | {:pat'| $lid:v |} -> subst v skont
        | _ -> raise Not_found 
      with
      | Not_found -> {| let $p = $lid:strm_n in $skont |} 

let rec stream_pattern _loc epo e ekont = function
  | [] ->
      (match epo with
      | Some ep -> {| let $ep = $(uid:gm()).count $lid:strm_n in $e |}
      | _ -> e )
  | [(spc, err) :: spcl] ->
      let skont =
        let ekont err =
          let str =
            match err with
            | Some estr -> estr
            | _ -> {| "" |}  in
          {| raise ($(uid:gm()).Error $str) |} in
        stream_pattern _loc epo e ekont spcl in
      let ckont = ekont err in stream_pattern_component skont ckont spc 

let stream_patterns_term _loc ekont tspel : exp =
  let pel =
    List.fold_right
      (fun (p, w, _loc, spcl, epo, e) acc ->
        let p = {:pat'| Some $p |} in
        let e =
          let ekont err =
            let str =
              match err with
              | Some estr -> estr
              | _ -> {| "" |}  in
            {| raise ($(uid:gm()).Error $str) |} in
          let skont = stream_pattern _loc epo e ekont spcl in
          {| begin  $(junk_fun _loc) $lid:strm_n; $skont  end |} in
          match w with
          | Some w -> {:case'| $pat:p when $w -> $e  | $acc |}
          | None -> {:case'| $pat:p -> $e  | $acc |} )
      tspel {:case'| _ -> $(ekont () )|} in
  {| match $(peek_fun _loc) $lid:strm_n with | $pel  |} 

let rec group_terms = function
  | [([(SpTrm (_loc, p, w), None) :: spcl], epo, e) :: spel] ->
    let (tspel, spel) = group_terms spel in
    ([(p, w, _loc, spcl, epo, e) :: tspel], spel)
  | spel -> ([], spel) 


  
let rec parser_cases _loc = function
  | [] -> {| raise $(uid:gm()).Failure |}
  | spel ->
      match group_terms spel with
      | ([], [(spcl, epo, e) :: spel]) ->
          stream_pattern _loc epo e (fun _ -> parser_cases _loc spel) spcl
      | (tspel, spel) ->
          stream_patterns_term _loc (fun _ -> parser_cases _loc spel) tspel 

(* it call [parser_cases] *)  
let cparser _loc bpo pc =
  let e = parser_cases _loc pc in
  let e =
    match bpo with
    | Some bp -> {| let $bp = $(uid:gm()).count $lid:strm_n in $e |}
    | None -> e  in
  let p = {:pat'| ($lid:strm_n : _ $(uid:gm()).t ) |} in
  {| fun $p -> $e |} 

let cparser_match _loc me bpo pc =
  let pc = parser_cases _loc pc in
  let e =
    match bpo with
    | Some bp -> {| let $bp = $(uid:gm()).count $lid:strm_n in $pc |}
    | None -> pc   in
  match me with
  | {| $lid:x |} when x = strm_n -> e
  | _ -> {| let ($lid:strm_n : _ $(uid:gm()).t ) = $me in $e |} 

(* streams *)

let rec not_computing = function
  | {| $lid:_ |} | {| $uid:_ |} | {| $int:_ |} |
    {| $flo:_ |} | {| $chr:_ |} | {| $str:_ |} -> true
  | {| $x $y |} -> is_cons_apply_not_computing x && not_computing y
  | _ -> false 
and is_cons_apply_not_computing = function
  | {| $uid:_ |} -> true
  | {| $lid:_ |} -> false
  | {| $x $y |} -> is_cons_apply_not_computing x && not_computing y
  | _ -> false 

let slazy _loc e =
  match e with
  | {| $f () |} ->
      (match f with
      | {| $lid:_ |} -> f
      | _ -> {| fun _ -> $e |} )
  | _ -> {| fun _ -> $e |} 

let rec cstream gloc =  function
  | [] -> let _loc = gloc in {| [< >] |}
  | [SeTrm (_loc, e)] ->
      if not_computing e
      then {| $(uid:gm()).ising $e |}
      else {| $(uid:gm()).lsing $(slazy _loc e) |}
  | [SeTrm (_loc, e) :: secl] ->
      if not_computing e
      then {| $(uid:gm()).icons $e $(cstream gloc secl) |}
      else {| $(uid:gm()).lcons $(slazy _loc e) $(cstream gloc secl) |}
  | [SeNtr (_loc, e)] ->
      if not_computing e then e
      else {| $(uid:gm()).slazy $(slazy _loc e) |}
  | [SeNtr (_loc, e) :: secl] ->
      if not_computing e then {| $(uid:gm()).iapp $e $(cstream gloc secl) |}
      else {| $(uid:gm()).lapp $(slazy _loc e) $(cstream gloc secl) |} 
    
