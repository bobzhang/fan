open FanUtil;
open LibUtil;
let _loc = FanLoc.ghost ;
  
let gen_quantifiers ~arity n  = with "ctyp"
  List.init arity (fun i -> List.init n (fun j -> {|  '$(lid:allx ~off:i j) |} ))
  |> List.concat |> app_of_list;

(* {:ctyp| $lid:x|}
   {:ctyp| $uid:x |}
   {:expr|$uid:y|}
 *)

















