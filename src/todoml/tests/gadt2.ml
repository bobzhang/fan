
(* PASSED *)
type 'a u =
  |A : int ->  int u
  |B : bool -> bool u    

(** FAILED *)
type _  u =
  |A : int ->  int u
  |B : bool -> bool u    
(** add a quote to automatically transform into Parsetree,
   then compare two parsetree? *)
(* {:eq| *)
(* {type _ u =  *)
(* |} *)
