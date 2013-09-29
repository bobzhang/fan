(* DEFINE A; *)
(* DEFINE B; *)

(* IFDEF A THEN *)
val a_should_be_present : int;
(* ENDIF; *)

(* IFNDEF C THEN *)
(*   let b_should_be_present : int; *)
(* ENDIF; *)

(* IFNDEF C THEN *)
(*   let c_should_be_present : int; *)
(* ELSE *)
(*   let a_should_NOT_be_present : int; *)
(* END; *)

(* IFDEF C THEN *)
(*   let b_should_NOT_be_present : int; *)
(* ELSE *)
(*   let d_should_be_present : int; *)
(*   let e_should_be_present : int; *)
(* ENDIF; *)

val a_should_be_present : int;
