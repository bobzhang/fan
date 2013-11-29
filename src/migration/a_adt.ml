type u =
  | A of string
  | B of string * int
  | C of (u * string) with sexp

type u = | A of string | B of string * int | C of (u * string)

let _ = fun (_ : u) -> ()
  
let rec __u_of_sexp__ =
  let _tp_loc = "a_adt.ml.u"
  in
    function
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("a" | "A" as _tag)) :: sexp_args)
       as _sexp) ->
        (match sexp_args with
         | [ v1 ] -> let v1 = string_of_sexp v1 in A v1
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("b" | "B" as _tag)) :: sexp_args)
       as _sexp) ->
        (match sexp_args with
         | [ v1; v2 ] ->
             let v1 = string_of_sexp v1
             and v2 = int_of_sexp v2
             in B ((v1, v2))
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | (Sexplib.Sexp.List
         (Sexplib.Sexp.Atom (("c" | "C" as _tag)) :: sexp_args)
       as _sexp) ->
        (match sexp_args with
         | [ v1 ] ->
             let v1 =
               (match v1 with
                | Sexplib.Sexp.List ([ v1; v2 ]) ->
                    let v1 = u_of_sexp v1
                    and v2 = string_of_sexp v2
                    in (v1, v2)
                | sexp ->
                    Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc 2
                      sexp)
             in C v1
         | _ -> Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
    | (Sexplib.Sexp.Atom ("a" | "A") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _tp_loc sexp
    | (Sexplib.Sexp.Atom ("b" | "B") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _tp_loc sexp
    | (Sexplib.Sexp.Atom ("c" | "C") as sexp) ->
        Sexplib.Conv_error.stag_takes_args _tp_loc sexp
    | (Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp) ->
        Sexplib.Conv_error.nested_list_invalid_sum _tp_loc sexp
    | (Sexplib.Sexp.List [] as sexp) ->
        Sexplib.Conv_error.empty_list_invalid_sum _tp_loc sexp
    | sexp -> Sexplib.Conv_error.unexpected_stag _tp_loc sexp
and u_of_sexp sexp = __u_of_sexp__ sexp
  
let _ = __u_of_sexp__
and _ = u_of_sexp
  
let rec sexp_of_u =
  function
  | A v1 ->
      let v1 = sexp_of_string v1
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "A"; v1 ]
  | B ((v1, v2)) ->
      let v1 = sexp_of_string v1
      and v2 = sexp_of_int v2
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "B"; v1; v2 ]
  | C v1 ->
      let v1 =
        (match v1 with
         | (v1, v2) ->
             let v1 = sexp_of_u v1
             and v2 = sexp_of_string v2
             in Sexplib.Sexp.List [ v1; v2 ])
      in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "C"; v1 ]
  
let _ = sexp_of_u
  

  
