

(* open Syntax; *)
(* {:create|Gram pred|};; *)
(* {:extend| *)
(* pred: *)
(*   [pat{p};"when"; exp{e} -> {:exp'| function | $pat:p when $e -> true |_ -> false |} *)
(*   |pat{p} -> {:exp'| function | $pat:p -> true | _ -> false |} ] *)
(* |};; *)
(* of_exp ~name:(d,"pred") ~entry:Pred.pred; *)

