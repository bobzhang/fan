type t =
  | Foo of int list
  | Bar of int * int 


class virtual ovisit_t =
    object (self)
      method virtual list : 'a1. ('a1 -> unit) -> 'a1 list -> unit
      method virtual int : int -> unit
      method t : t -> unit =
        fun __value ->
          match __value with
          | Foo __x1 -> (self#list self#int __x1; ())
          | Bar (__x1, __x2) -> (self#int __x1; self#int __x2; ())
    end
  

(* local variables: *)
(* compile-command: "ocamlc -c t.ml" *)
(* end: *)
