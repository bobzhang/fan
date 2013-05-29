
open AstLib
(*************************************************************************)
(* save DDSL *)
{:create|Gram  save_quot|};;
(* {:save| a b c -> begin *)
(*   print_int a; *)
(*   print_int b ; *)
(*   print_int c; *)
(* end *)
(* |} *)

    
{:extend|save_quot:
  [L1 [`Lid x -> x] {ls} ; "->"; Fsyntax.exp{b} ->
    let symbs = List.map (fun x -> FState.gensym x) ls in
    let res = FState.gensym "res" in
    let exc = FState.gensym "e" in
    let binds = and_of_list
        (List.map2 (fun x y -> {:bind| $lid:x = ! $lid:y |} ) symbs ls ) in
    let restore =
       seq_sem (List.map2 (fun x y -> {:exp| $lid:x := $lid:y |}) ls symbs) in
    {:exp|
    let $binds in
    try begin 
      let $lid:res = $b in
      let _ = $restore in 
      $lid:res    
    end with
    | $lid:exc -> (begin $restore ; raise $lid:exc end)
  |}

 ]
|};;
