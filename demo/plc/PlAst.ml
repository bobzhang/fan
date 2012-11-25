
module StringSet = Set.Make String;

module StringMap = Map.Make(String);

(* atoms and integers are constants *)
type term 'loc =
    [ Atom of string and 'loc
    |  Var of string and 'loc
    | Anon of 'loc ];

(* e.g. parent/2 *)
type pred = (string * int) ;

module Pred = struct
  type t = pred ;
  let compare : t -> t -> int = compare ;
end;

module PredMap = Map.Make Pred ;

(* e.g. parent(X,maja) *)
type goal 'loc = (pred * list (term 'loc) * 'loc) ;

(* e.g. voor sibling/2: (X,Y) :- parent(Z,X), parent(Z,Y). *)
type rule 'loc = (list (term 'loc)   * list (goal 'loc)   * 'loc);

(* Complete program: map from pred to rule list *)
type prog 'loc = PredMap.t  (list (rule 'loc))   ;

let atoms_of_terms acc terms =
  List.fold_left
    (fun acc term ->
      match term with
      [ Atom (a,_) -> StringSet.add a acc
      | _ -> acc]) acc terms ;

let atoms (prog : prog 'a) =
  PredMap.fold
    (fun _pred rules acc ->
      List.fold_left
        (fun acc (terms,goals,_) ->
	  let acc = atoms_of_terms acc terms in
	  List.fold_left
            (fun acc (_,terms,_) ->
	      atoms_of_terms acc terms) acc goals) acc rules) prog StringSet.empty ;
