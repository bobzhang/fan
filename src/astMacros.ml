open FAst
open AstLib
open LibUtil

  
type key = string

type expander =  exp -> exp

let macro_expanders: (key,expander) Hashtbl.t = Hashtbl.create 40 


let register_macro (k,f) =  Hashtbl.replace macro_expanders k f

let rec fib = function
  | 0 | 1 ->  1
  | n when n > 0 -> fib (n-1) + fib (n-2)
  | _ -> invalid_arg "fib" 

let fibm  y =
  match y with
  | {:exp|$int:x|}  -> {:exp| $(`int:fib (int_of_string x))|}
  |  x -> let _loc = loc_of x in {:exp| fib $x |} ;;

register_macro ("FIB",fibm);;      


  
(* let generate_fibs = with exp fun *)
(*   [ {:exp|$int:x|} -> *)
(*     let j = int_of_string x in *)
(*     let res = zfold_left ~until:j ~acc:{||} (fun acc i -> {| $acc; print_int (FIB $`int:i) |}) in *)
(*     {:exp| $seq:res |} *)
(*     (\* Array.map (fun i -> {|print_int (FIB $`int:i) |} ) *\) *)
(*     (\* {:exp| for _j = 0 to $int:x do print_int (FIB _j) done |} *\) *)
(*   | e -> e ]; *)

(* register_macro ("GFIB", generate_fibs);     *)

(*
  #filter "macro";;
  GFIB 10;
(* let u x = *)
  [FIB 13;
  FIB 13;
  FIB x]
  ;
(*
  {:exp| [FIB 13; FIB 13; FIB x ] |}
 *)

 *)
  

let macro_expander = object(self)
  inherit Objs.map as super
  method! exp = with exp function
    |{| $uid:a $y |} ->
        (let try f = Hashtbl.find macro_expanders a in
        self#exp (f y)
        with Not_found -> {| $uid:a $(self#exp y)|})
    | e -> super#exp e 
end

(* AstFilters.register_stru_filter ("macro", macro_expander#stru);   *)
