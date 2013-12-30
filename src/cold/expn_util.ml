open Astfn
open Astn_util
let mkfun names acc =
  List.fold_right
    (fun name  acc  ->
       (`Fun (`Case ((`Lid name), (acc :>Astfn.exp))) :>Astfn.exp)) names acc
let unknown len =
  if len = 0
  then (`Send ((`Lid "self"), (`Lid "unknown")) :>Astfn.exp)
  else
    (`App
       ((`Lid "ref"),
         (`Fun
            (`Case
               (`Any,
                 (`App
                    ((`App
                        ((`App
                            ((`App
                                ((`Dot ((`Uid "Format"), (`Lid "ksprintf"))),
                                  (`Lid "failwith"))),
                              (`Str "%s.%s not implemented "))),
                          (`Lid "__MODULE__"))), (`Lid "__BIND__"))))))) :>
    Astfn.exp)
let mk_record label_exps =
  (let rec_exps =
     List.map
       (fun (label,exp)  ->
          (`RecBind ((`Lid label), (exp :>Astfn.exp)) :>Astfn.rec_exp))
       label_exps in
   `Record (sem_of_list rec_exps) : exp )
let mee_comma x y =
  (`App
     ((`Vrn "Com"),
       (`Par
          (`Com ((`Lid "_loc"), (`Com ((x :>Astfn.exp), (y :>Astfn.exp))))))) :>
  Astfn.exp)
let mee_app x y =
  (`App
     ((`Vrn "App"),
       (`Par
          (`Com ((`Lid "_loc"), (`Com ((x :>Astfn.exp), (y :>Astfn.exp))))))) :>
  Astfn.exp)
let mee_of_str s =
  let len = String.length s in
  if (s.[0]) = '`'
  then
    let s = String.sub s 1 (len - 1) in
    (`App ((`Vrn "Vrn"), (`Par (`Com ((`Lid "_loc"), (`Str s))))) :>Astfn.exp)
  else
    (`App ((`Vrn "Uid"), (`Par (`Com ((`Lid "_loc"), (`Str s))))) :>Astfn.exp)
let mk_tuple_ee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      let v = Listf.reduce_right mee_comma xs in
      (`App ((`Vrn "Par"), (`Par (`Com ((`Lid "_loc"), (v :>Astfn.exp))))) :>
        Astfn.exp)
let mee_record_col label exp =
  (`App
     ((`Vrn "RecBind"),
       (`Par
          (`Com
             ((`Lid "_loc"),
               (`Com
                  ((`App
                      ((`Vrn "Lid"),
                        (`Par (`Com ((`Lid "_loc"), (`Str label)))))),
                    (exp :>Astfn.exp))))))) :>Astfn.exp)
let mee_record_semi a b =
  (`App
     ((`Vrn "Sem"),
       (`Par
          (`Com ((`Lid "_loc"), (`Com ((a :>Astfn.exp), (b :>Astfn.exp))))))) :>
  Astfn.exp)
let mk_record_ee label_exps =
  (label_exps |> (List.map (fun (label,exp)  -> mee_record_col label exp)))
    |>
    (fun es  ->
       let x = Listf.reduce_right mee_record_semi es in
       (`App
          ((`Vrn "Record"), (`Par (`Com ((`Lid "_loc"), (x :>Astfn.exp))))) :>
         Astfn.exp))
