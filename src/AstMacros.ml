(* open Format; *)

(*
  {:macro|M a b c|}

  {:macro|fib 32 |}

  {:defmacro|fib a b =
  
  |}

  challegens lie in how to extract the
  macro name, and apply

  fib 32 -->
     fib 31 + fib 30

     -- ExApp
     --
  {:str_item| g |};

  -- macro.expr
     

  -- macro.str_item
     StExp ..  
 *)
type key = string;

type expander =  Ast.expr -> Ast.expr;

(*
   Ast.expr -> Ast.str_item
*)
  
let macro_expanders: Hashtbl.t key expander = Hashtbl.create 40 ;

let register_macro (k,f) =
  Hashtbl.replace macro_expanders k f;

let rec fib = fun
  [ 0 | 1 ->  1
  | n when n > 0 -> fib (n-1) + fib (n-2)
  | _ -> invalid_arg "fib" ];

(* {:expr| f a b c|}
   don't support currying
   always
   f a
   or f (a,b,c)
   {:expr| f (a,b,c) |}
 *)
  
let fibm  y =
  match y with
  [ {:expr|$int:x|}  -> {:expr| $(`int:fib (int_of_string x))|}
  |  x -> let _loc = Camlp4Ast.loc_of_expr x in {:expr| fib $x |} ];

register_macro ("FIB",fibm);      

open LibUtil;
    
let generate_fibs = with "expr" fun
  [ {:expr|$int:x|} ->
    let j = int_of_string x in
    let res = zfold_left ~until:j ~acc:{||} (fun acc i -> {| $acc; print_int (FIB $`int:i) |}) in
    {:expr| $seq:res |}
    (* Array.map (fun i -> {|print_int (FIB $`int:i) |} ) *)
    (* {:expr| for _j = 0 to $int:x do print_int (FIB _j) done |} *)
  | e -> e ];

register_macro ("GFIB", generate_fibs);    
(*
#filter "macro";;
GFIB 10;
(* let u x = *)
  [FIB 13;
   FIB 13;
   FIB x]
   ;
(*
  {:expr| [FIB 13; FIB 13; FIB x ] |}
 *)

*)
    
    















