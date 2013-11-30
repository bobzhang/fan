type t = A of int

(* module X = struct      *)
(* type nonrec t = t = A of int  *)

(* and u = t *)
(* end *)
(* nonrec and ---> simplified as type a .. type b .. type c ..*)    
module X =
  struct
    include
      (struct
         type __pa_nonrec_1 = t = A of int
         
         type t = __pa_nonrec_1 = | A of int
         type u = __pa_nonrec_1
         
       end :
       sig
         type __pa_nonrec_1 = t = A of int
         
         type t = __pa_nonrec_1 = | A of int
         type  u = __pa_nonrec_1
         
       end with type __pa_nonrec_1 := t)
      
  end
      
      
(* include *)
(*   (struct type __pa_nonrec_0 = t *)
(*            type t = __pa_nonrec_0 *)
(*             end : *)
(*    sig type __pa_nonrec_0 = t *)
(*         type t = __pa_nonrec_0 *)
(*          end *)
(*      with type __pa_nonrec_0 := t) *)


(* local variables: *)
(* compile-command: "ocamlc -pp 'camlp4o -I /Users/hongbozhang/.opam/system/lib/type_conv pa_type_conv.cma' -i -c a_nonrec.ml" *)
(* end: *)
