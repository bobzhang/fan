
(* open Format;; *)

(* 3 *)

(* let a = 3;; *)

(* class type a  = object('a) *)
(* end *)


(* type 'a u constraint 'a = int *)
(* type  u constraint 'a = int     *)
(* type 'a u constraint 'a = int*)
(* let f = ()           (\* Pexp_construct "()" *\) *)
(* let f  x = !x *)
  
                (* Pexp_apply *)
                (* expression (test/a.ml[15,208+11]..test/a.ml[15,208+12]) *)
                (*   Pexp_ident "!" *)
                (* [ *)
                (*   <label> "" *)
                (*     expression (test/a.ml[15,208+12]..test/a.ml[15,208+13]) *)
                (*       Pexp_ident "x" *)
                (* ] *)
    


(* let x = object *)
(*   val x =3 *)
(*   method y = *)
(*     {<  x =100 >} *)
(*  end;; *)
(* let x = ref 32;; *)
(* x<-3;; *)

(* let u = ref 3;; *)
(* u.contents<- 32;; (\* Pexp_setfield *\) *)

let x = object
  val mutable x = 3 (*Pcf_val *)
  method z = x <- 3
  (* Pexp_setinstvar *)
end














