


#{:eval|
open  Fsyntax
{:create| Fgram p|};;

{:extend|
  p:
  [pat{p};"when"; exp{e} -> {:exp| function | $pat:p when $e -> true |_ -> false |}
  |pat{p} -> {:exp| function | $pat:p -> true | _ -> false |} ]
|};;
let d = `Absolute["Tutorial"];;
AstQuotation.of_exp ~name:(d,"q") ~entry:p;;
|}  


#{:control| import Tutorial ; |}
if {:q| {:exp-| $a+$b|}|} {:exp-| 3 + 4|} then
    print_endline "yes!"
else print_endline "no"


  
(* local variables: *)
(* compile-command: "ocamlc -pp 'fanX.byte '  langQ.ml && ./a.out" *)
(* end: *)
