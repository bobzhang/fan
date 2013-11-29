(* List.iter l ~f:(fun x -> *)
(*     <:sexp_of<int * string>> x *)
(*     |> Sexp.to_string *)
(*     |> print_endline) *)

List.iter l
  ~f:
    (fun x ->
       (((fun (v1, v2) ->
            let v1 = sexp_of_int v1
            and v2 = sexp_of_string v2
            in Sexplib.Sexp.List [ v1; v2 ])
           x)
          |> Sexp.to_string)
         |> print_endline)
  
