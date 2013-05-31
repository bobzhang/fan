
{:create| p|};;

{:extend|
  p:
  [pat{p};"when"; exp{e} -> {:exp| function | $pat:p when $e -> true |_ -> false |}
  |pat{p} -> {:exp| function | $pat:p -> true | _ -> false |} ]
|};;

let d = `Absolute["Tutorial"];;
AstQuotation.of_exp ~name:(d,"q") ~entry:p;;

(* local variables: *)
(* compile-command: "ocamlc -pp 'fan.native'" testq.ml *)
(* end: *)
