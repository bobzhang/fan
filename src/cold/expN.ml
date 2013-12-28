open Astfn
open Astn_util
open Fid
let mkfun names acc =
  List.fold_right
    (fun name  acc  ->
       (`Fun (`Case ((`Lid name), (acc :>Astfn.exp))) :>Astfn.exp)) names acc
let currying cases ~arity  =
  let cases = bar_of_list cases in
  if arity >= 2
  then
    let names = Listf.init arity (fun i  -> x ~off:i 0) in
    let exps = Listf.map (fun s  -> (`Lid s :>Astfn.exp)) names in
    let x = tuple_com exps in
    mkfun names (`Match ((x :>Astfn.exp), cases) :>Astfn.exp)
  else (`Fun cases :>Astfn.exp)
let eta_expand (exp : exp) number =
  (let names = Listf.init number (fun i  -> x ~off:0 i) in
   mkfun names (exp +> names) : exp )
let unknown len =
  if len = 0
  then (`Send ((`Lid "self"), (`Lid "unknown")) :>Astfn.exp)
  else (`App ((`Lid "failwith"), (`Str "not implemented!")) :>Astfn.exp)
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
