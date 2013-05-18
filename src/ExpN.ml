
open LibUtil
open AstN
open AstLibN
open BasicN

#default_quotation "exp-";;

let (<+) names acc  =
  List.fold_right  (fun name acc ->  {| function | $lid:name -> $acc |}) names acc 

let mkfun = (<+)
  
let currying cases ~arity =
  let cases = bar_of_list cases in 
  if  arity >= 2 then 
    let names = List.init arity (fun i -> x ~off:i 0) in
    let exps = List.map (fun s-> {| $lid:s |} ) names in
    let x = tuple_com exps in
    names <+ {| match $x with | $cases |} 
  else {| function | $cases |}
      (* {| fun [ $list:cases ] |} *)


let eta_expand (exp:exp) number : exp =
  let names = List.init number (fun i -> x ~off:0 i ) in
  names <+ (exp +> names )
      

let unknown len =
  if len = 0 then {|self#unknown |}
  else {| failwith  "not implemented!" |}
