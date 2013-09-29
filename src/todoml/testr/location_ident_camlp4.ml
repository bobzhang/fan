module X = struct
  let x = 1
end

let _ = X.x

(* local variables: *)
(* compile-command: "ocamlc -dparsetree -pp camlp4o location_ident_camlp4.ml" *)
(* end: *)
