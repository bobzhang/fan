type pred = string * int

module Pred : sig type t = pred val compare : t -> t -> int end

module PredMap : Map.S with type key = pred 

type  term =
  |  Integer of int * Locf.t
  | Var of string * Locf.t
  | Anon of Locf.t
  | Comp of string *  term list * Locf.t
        
type rule =
     term list *  term list * Locf.t

type arg_mask =
  | ArgOpen of Locf.t
  | ArgClosed of Locf.t
  | ArgAny of Locf.t

type  mask =  (arg_mask list * Locf.t)

type  prog = ( rule list *  mask list) PredMap.t

val statics_of_terms : int Mapf.String.t ->  term list -> int Mapf.String.t

val statics_of_goal_terms :
  int Mapf.String.t ->  term list -> int Mapf.String.t

val statics :  prog -> int Mapf.String.t
