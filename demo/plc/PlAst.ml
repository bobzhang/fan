
module StringSet = Set.Make(String) ;
module StringMap = Map.Make(String) ;

(* e.g. parent/2 *)
type pred = (string * int) ;

module Pred =struct
  type t = pred ;
  let compare : t -> t -> int = compare ;
end ;

module PredMap = Map.Make Pred ;

(* terms are integers, (anonymous) variables and compound (possibly atoms) *)
type term 'loc =
  [ Integer of int and 'loc
  | Var of string and 'loc
  | Anon of 'loc
  | Comp of string and  list (term 'loc) and 'loc]
;

(* e.g. for sibling/2: (X,Y) :- parent(Z,X), parent(Z,Y). *)
type rule 'loc = (list (term 'loc)  * list (term 'loc)   * 'loc);

(* e.g. +X or -Y *)
type arg_mask 'loc = [ArgOpen of 'loc | ArgClosed of 'loc | ArgAny of 'loc] ;

(* e.g. for same/2: +X, ?Y *)
type mask 'loc = (list (arg_mask 'loc) * 'loc );

(* Complete program: map from pred to rule list + mask list *)
type prog 'loc =  PredMap.t (list (rule 'loc)  * list (mask 'loc)) ;

let rec statics_of_terms acc terms =
  List.fold_left (fun comps -> fun
    [ Comp (c,ts,_) ->
	let comps =
	  let n = List.length ts in
	  try
	    let n' = StringMap.find c comps in
	    if n = n' then comps
	    else failwith ("Contradictory arities for " ^ c)
				with Not_found -> StringMap.add c n comps
	in
	statics_of_terms comps ts
    | _ -> comps] ) acc terms;

let rec statics_of_goal_terms acc terms =
  List.fold_left (fun comps -> fun
    [ Comp ("is",[t;_],_loc) -> statics_of_terms comps [t]
    | Comp ("eq",[_;_],_loc) | Comp ("ne",[_;_],_loc)
    | Comp ("lt",[_;_],_loc) | Comp ("lte",[_;_],_loc)
    | Comp ("gt",[_;_],_loc) | Comp ("gte",[_;_],_loc) -> comps
    | Comp ("not",([_t] as ts),_loc) -> statics_of_goal_terms comps ts
    | Comp (_c,ts,_) ->
	(* same and diff, cut, true and fail, etc. will also match here *)
			statics_of_terms comps ts
    | _ -> comps]) acc terms;

let statics (prog : prog 'a) =
  PredMap.fold (fun _pred (rules,_) acc ->
    List.fold_left (fun acc (terms,goals,_) ->
      statics_of_goal_terms (statics_of_terms acc terms) goals) acc rules) prog StringMap.empty ;

(* open LibUtil; *)


(* (\* atoms and integers are constants *\) *)
(* type term 'loc = *)
(*     [ Atom of string and 'loc *)
(*     |  Var of string and 'loc *)
(*     | Anon of 'loc ]; *)

(* (\* e.g. parent/2 *\) *)
(* type pred = (string * int) ; *)

(* module Pred = struct *)
(*   type t = pred ; *)
(*   let compare : t -> t -> int = compare ; *)
(* end; *)

(* module PredMap = Map.Make Pred ; *)

(* (\* e.g. parent(X,maja) *\) *)
(* type goal 'loc = (pred * list (term 'loc) * 'loc) ; *)

(* (\* e.g. voor sibling/2: (X,Y) :- parent(Z,X), parent(Z,Y). *\) *)
(* type rule 'loc = (list (term 'loc)   * list (goal 'loc)   * 'loc); *)

(* (\* Complete program: map from pred to rule list *\) *)
(* type prog 'loc = PredMap.t  (list (rule 'loc))   ; *)

(* let (\* atoms_of_terms *\) (+>) acc terms = *)
(*   List.fold_left *)
(*     (fun acc term -> *)
(*       match term with *)
(*       [ Atom (a,_) -> SSet.add a acc *)
(*       | _ -> acc]) acc terms ; *)

(* let atoms (prog : prog 'a) = *)
(*   PredMap.fold *)
(*     (fun _pred rules acc -> *)
(*       List.fold_left *)
(*         (fun acc (terms,goals,_) -> *)
(* 	  let acc = (\* atoms_of_terms *\) acc +> terms in *)
(* 	  List.fold_left *)
(*             (fun acc (_,terms,_) -> *)
(* 	      (\* atoms_of_terms *\) acc +> terms) acc goals) acc rules) prog SSet.empty ; *)
