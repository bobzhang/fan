

(* e.g. parent/2 *)
type pred = (string * int)

module Pred =struct
  type t = pred
  let compare : t -> t -> int = compare
end

module PredMap = Map.Make(Pred)

(* terms are integers, (anonymous) variables and compound (possibly atoms) *)
type  term =
  |  Integer of int * Locf.t
  | Var of string * Locf.t
  | Anon of Locf.t
  | Comp of string *  term list * Locf.t

(* e.g. for sibling/2: (X,Y) :- parent(Z,X), parent(Z,Y). *)        
type rule =
    (term list *  term list * Locf.t)


(* e.g. +X or -Y *)
type arg_mask =
  | ArgOpen of Locf.t
  | ArgClosed of Locf.t
  | ArgAny of Locf.t

(* e.g. for same/2: +X, ?Y *)
type  mask = ( arg_mask list * Locf.t)



(* Complete program: map from pred to rule list + mask list *)
type  prog = (rule list *  mask list) PredMap.t


let rec statics_of_terms (acc : int Mapf.String.t) (terms : term list) : int Mapf.String.t =
  List.fold_left (fun comps -> fun
    | Comp (c,ts,_) ->
	let comps =
	  let n = List.length ts in
	  try
	    let n' = Mapf.String.find c comps in
	    if n = n' then comps
	    else failwith ("Contradictory arities for " ^ c)
				with Not_found -> Mapf.String.add c n comps
	in
	statics_of_terms comps ts
    | _ -> comps ) acc terms

let rec statics_of_goal_terms acc terms =
  List.fold_left (fun comps -> fun
    | Comp ("is",[t;_],_loc) -> statics_of_terms comps [t]
    | Comp ("eq",[_;_],_loc) | Comp ("ne",[_;_],_loc)
    | Comp ("lt",[_;_],_loc) | Comp ("lte",[_;_],_loc)
    | Comp ("gt",[_;_],_loc) | Comp ("gte",[_;_],_loc) -> comps
    | Comp ("not",([_t] as ts),_loc) -> statics_of_goal_terms comps ts
    | Comp (_c,ts,_) ->
	(* same and diff, cut, true and fail, etc. will also match here *)
			statics_of_terms comps ts
    | _ -> comps) acc terms

let statics (prog : prog) : (int Mapf.String.t) =
  PredMap.fold (fun _pred (rules,_) acc ->
    List.fold_left (fun acc (terms,goals,_) ->
      statics_of_goal_terms (statics_of_terms acc terms) goals) acc rules) prog Mapf.String.empty

