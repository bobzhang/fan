open PlAst;
open LibUtil;
module Ast = Camlp4Ast;
open Lib;

(** General aux functions **)

let format_loc _loc =	FanLoc.to_string _loc;
(** Open/closed versions **)

type version = (int * int);

let versions_fold f acc k =
  let n = 1 lsl k in
  let rec aux acc i =
    if i = n then
      acc
    else aux (f acc (i,k)) (i+1)
  in
  aux acc 0;

let version_opened (i,_k) j =
  (i lsr j) land 1 = 1;

let version_reconstruct bs =
  List.fold_left
    (fun (o,k) b ->
      (((* let o = o lsl 1 in *) if b then 2 * o + 1 else 2 * o), k+1)) (0,0) bs;

let version_fold f acc (i,k) =
  let rec aux acc j =
    if j = k then acc else aux (f acc (version_opened (i,k) j)) (j+1) in
  aux acc 0;

let version_string (i,k) =
  String.init k (fun j -> if version_opened (i,k) j then 'o' else 'c');

let version_neg (i,k) =
  ((1 lsl k) - 1 - i, k) ;

(** Name functions **)
let pred_name n v = n ^ "_" ^ (version_string v) ;
  
let atom_name n = String.capitalize n ;



let pred_var_name n = "_arg" ^ (string_of_int n) ;

let f_name = "_f" ;

(** Atom translation **)

let atoms_type _loc atoms =
  let wrap =
    SSet.fold
      (fun atom l ->
        [ {:ctyp| $(uid:atom_name atom) |}::l]) atoms [] in
  {:str_item| type atom = [ $list:wrap ] |};

let atoms_repr _loc atoms =
  let cases = SSet.fold
      (fun atom l ->
	[{:match_case| $(uid:atom_name atom) -> $str:atom |} ::l] ) atoms [] in
  {:str_item| let string_of_atom = fun [ $list:cases ] |};

(** Env management **)

let empty = SMap.empty ;

let bound map = fun
  [ Var (v,_) -> SMap.mem v map
  | Atom (_,_) -> true
  | Anon _ -> false];

let lookup map v = SMap.find v map;

let bind map v id =
  try
    let id' = lookup map v in
    (map, Some id')
  with Not_found -> (SMap.add v id map, None);

(**
	Rules translation:
	pred -> pred_version -> rule (-> terms) -> goal -> terms
**)

(** Decorate a version with identifiers for the closed arguments **)
let extend_version c v =
  let (c,ev) = version_fold
      (fun (c,ev) o ->
        if not o then (c+1, [Some (pred_var_name c)::ev])
        else (c, [None::ev]) ) (c,[]) v
  in (c,List.rev ev);

(** Split terms according to closed (by id) and open **)
let recombine_terms ts ev =
  let (t1,t2) =
    List.fold_left
      (fun (t1,t2) ->
        fun
          [(t,Some id) -> ([(t,id)::t1],t2)
          | (t,None) -> (t1,[t::t2])] )
      ([],[]) (List.combine ts ev) in
  (List.rev t1, List.rev t2);

(** Prepare patterns from extended version (identifiers) **)
let goal_ins _loc ev =
  List.rev
    (List.fold_left
       (fun acc ->
         fun
	   [ Some id -> [{:patt|$lid:id|}::acc]
           | None -> acc]) [] ev) ;

exception UnboundVar of string and FanLoc.t;

(** Prepare exprs from open terms **)
let goal_outs env t2 =
  List.map
    (fun
     [ Atom (a, _loc) -> {:expr| $(uid:atom_name a)|}
     | Var (v, _loc) ->
	 try  {:expr| $(lid:lookup env v) |}
	  with [Not_found -> raise (UnboundVar (v,_loc))]
     | Anon _loc -> raise (UnboundVar ("_",_loc))]) t2;


type test_subject =
    [Test_uid of string | Test_lid of string ];


let test_expr _loc l =
  let rec aux e = fun
    [ [] -> e
    | [(a,b)::l] -> aux {:expr| $e && $a = $b |} l] in
  match l with
  [ [] -> None
  | [(a,b)::l] -> Some (aux {:expr| $a = $b |} l)];		
  
let expand_tests _loc l =
  if l = [] then
    None
  else
    let (e,p,t) =
      List.fold_left
        (fun (e,p,t) ->
          fun
	    [( id, Test_uid uid) ->
	      ([ {:expr|$lid:id|} ::e], [{:patt| $uid:uid |}::p], t)
            |(id, Test_lid lid) ->
		(e, p, [({:expr|$lid:id|}, {:expr|$lid:lid|})::t]) ])
        ([],[],[]) l in
    Some (Expr.tuple _loc e, Patt.tuple _loc p, test_expr _loc t);

let apply_test _loc tst body  =
  match tst with
  [ Some (e,p,None) ->
    {:expr| match $e with [ $p -> $body | _ -> () ] |}
  | Some (e,p,Some t) ->
    {:expr| match $e with [ $p when $t -> $body | _ -> () ] |}
  | None -> body];
    
(** Visit terms of a goal, update env and tests **)
let terms _loc env t1 =
  let (env,tsts) =
    List.fold_left
      (fun (env,tsts) ->
        fun
        [ (Atom (a, _loc), id)
          -> (env, [(id, Test_uid (atom_name a))::tsts])
        | (Var (v, _loc), id)
          ->
	    let (env,id') = bind env v id in
	    (env,
             match id' with
	     [ None -> tsts
	     | Some id' -> [(id, Test_lid id')::tsts] ])
	| (Anon _loc, _id) -> (env, tsts)]) (env,[]) t1 in
  (env, expand_tests _loc tsts);

(** Visit goals of a rule **)
let goal (env,c,f) ((n,_),ts,_loc) =
  let v = version_reconstruct (List.rev_map (fun t -> not (bound env t)) ts) in
  let call = {:expr|$(lid:pred_name n v)|} in
  let (c,ev) = extend_version c (version_neg v) in
  let ins = goal_ins _loc ev
  and (t1,t2) = recombine_terms ts ev in
  let (env,tst) = terms _loc env t1 in
  let outs =
    try goal_outs env t2
	(* we select a version with only bound vars *)
    with UnboundVar _ -> assert false
  in
  (env, c, (fun body ->
    let body = apply_test _loc tst body in
    let body = Expr.fun_args _loc ins body in
    let body = Expr.fun_apply _loc call [body::outs] in
    f body));

(** Visit rules of a predicate version **)
let rule ev c (ts,goals,_loc) =
  let (t1,t2) = recombine_terms ts ev in
  let (env,tst) = terms _loc empty t1 in
  let (env,_c,f) = List.fold_left goal (env,c,(fun body -> body)) goals in
  let outs = goal_outs env t2 in
  let body = Expr.fun_apply _loc {:expr| $(lid:f_name) |} outs in
  let body = f body in
  apply_test _loc tst body;

(** Visit a predicate to produce a certain version **)
let pred_version _loc n rs v =
  let name = {:patt| $(lid:pred_name n v) |} in
  let (c,ev) = extend_version 0 v in
  let args = goal_ins _loc ev in
  let rs = List.map (rule ev c) rs in
  let body = {:expr| begin  $list:rs end |} in
  let f = Expr.fun_args _loc [( {:patt|$lid:f_name|})::args] body in
  {:binding| $name = $f |};

(** Visit a predicate to produce all possible versions **)
let pred _loc (name,n) rs = versions_fold (fun a v ->
  try [(pred_version _loc name rs v)::a]
  with [UnboundVar(var,_loc) ->
    (Printf.eprintf
       "%s:\nWarning: skipping %s, unbound %s\n"
       (format_loc _loc) (pred_name name v) var;
     a)]) [] n ;

(** Starters **)

let prog_atoms _loc prog =
  let atoms = atoms prog in
  [atoms_type _loc atoms; atoms_repr _loc atoms];

let prog_rules _loc (prog:PlAst.prog 'a) =
  let defs = PredMap.fold
      (fun p rs acc ->
	List.append (pred _loc p (List.rev rs)) acc) prog [] in
  [ {:str_item| let rec $list:defs |} ];

