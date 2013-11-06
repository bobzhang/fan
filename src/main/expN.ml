

%%control{ default "exp-";}
(* FIXME more  precise location for [resolve_name]*)

open FAstN
open Astn_util
open Fid



let mkfun names acc  =
  List.fold_right  (fun name acc ->  %{ function | $lid:name -> $acc }) names acc 


  
let currying cases ~arity =
  let cases = bar_of_list cases in 
  if  arity >= 2 then 
    let names = Listf.init arity (fun i -> x ~off:i 0) in
    let exps = Listf.map (fun s-> %{ $lid:s } ) names in
    let x = tuple_com exps in
    mkfun names  %{ match $x with | $cases } 
  else %{ function | $cases }


let eta_expand (exp:exp) number : exp =
  let names = Listf.init number (fun i -> x ~off:0 i ) in
  mkfun names (exp +> names )
      

let unknown len =
  if len = 0 then %{self#unknown }
  else %{ failwith  "not implemented!" }



let mk_record label_exps : exp=
  let rec_exps =
    List.map
      (fun (label, exp) ->
        %rec_exp-{ $lid:label = $exp } ) label_exps in
  `Record (sem_of_list rec_exps)


      
(*************************************************************************)
(* Multiple stage code *)
let mee_comma x y =

  (* (`App ((`App ((`App ((`Vrn "Com"), (`Lid "_loc"))), x)), y) : FAstN.exp ) *)
  %exp-{`Com(_loc,$x,$y)}
  (* %exp-{%exp'{${$x}, ${$y}} } *)(** BOOTSTRAPPING*)


(** %exp{ %ep{ $($x) $($y) }}
    both work, I did not see obvious performance difference *)
let mee_app x y = (* BOOTSTRAPPING *)
  %exp-{`App (_loc, $x,$y)}
  (* (`App ((`App ((`App ((`Vrn "App"), (`Lid "_loc"))), x)), y) : FAstN.exp ) *)

  (* %exp-{%exp'{${$x} ${$y}}} *)


let mee_of_str s = (*    BOOTSTRAPING *)  
  let len = String.length s in
  if s.[0]='`' then
    let s = String.sub s 1 (len - 1) in
    (* (`App ((`Vrn "Vrn"), (`Par (`Com ((`Lid "_loc"), (`Str s))))) : FAstN.exp )     *)
    %exp-{%exp'{$vrn{($str:s)}}}
  else
     %exp-{ %exp'{ $uid{$str:s} } } 



let mk_tuple_ee = function (* BOOTSTRAPPING *)
  | [] -> invalid_arg "mktupee arity is zero "
  | [x] -> x
  | xs  ->
      let v = Listf.reduce_right mee_comma xs in
      %exp-{ %exp'{ $par{($v)}}}


  

let mee_record_col label exp =
  %exp-{ %rec_exp'{ $lid{($str:label)} = ${$exp} }} 


let mee_record_semi a b = %exp-{ %rec_exp'{ ${$a};${$b} } }


let mk_record_ee label_exps =
  label_exps
  |> List.map (fun (label,exp) -> mee_record_col label exp)
  |> (fun es ->
      let x = Listf.reduce_right mee_record_semi es in
      %exp-{ %exp'{ { ${$x} } }} )

      

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/expN.cmo" *)
(* end: *)
