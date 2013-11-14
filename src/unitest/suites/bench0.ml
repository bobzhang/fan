let _ =
  function
  | "functor"|"private"|"sig"|"include"|"exception"|"inherit"|"and"|"when"
    |"then"|"initializer"|"in"|"downto"|"as"|"function"|"begin"|"class"|"do"
    |"end"|"assert"|"external"|"virtual"|"to"|"try"|"struct"|"else"|"val"
    |"constraint"|"type"|"new"|"of"|"done"|"for"|"fun"|"method"|"mutable"
    |"lazy"|"with"|"if"|"while"|"rec"|"object"|"or"|"match"|"open"|"module"
    |"let" -> true
  | _ -> false

(* local variables: *)
(* compile-command: "ocamlopt.opt -dcmm -c bench0.ml" *)
(* end: *)
