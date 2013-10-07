open FAstN
open Astn_util
open Fid
let mkfun names acc =
  List.fold_right
    (fun name  acc  -> (`Fun (`Case ((`Lid name), acc)) : FAstN.exp )) names
    acc
let currying cases ~arity  =
  let cases = bar_of_list cases in
  if arity >= 2
  then
    let names = Flist.init arity (fun i  -> x ~off:i 0) in
    let exps = Flist.map (fun s  -> (`Lid s : FAstN.exp )) names in
    let x = tuple_com exps in mkfun names (`Match (x, cases) : FAstN.exp )
  else (`Fun cases : FAstN.exp )
let eta_expand (exp : exp) number =
  (let names = Flist.init number (fun i  -> x ~off:0 i) in
   mkfun names (exp +> names) : exp )
let unknown len =
  if len = 0
  then (`Send ((`Lid "self"), (`Lid "unknown")) : FAstN.exp )
  else (`App ((`Lid "failwith"), (`Str "not implemented!")) : FAstN.exp )
let mk_record label_exps =
  (let rec_exps =
     List.map
       (fun (label,exp)  -> (`RecBind ((`Lid label), exp) : FAstN.rec_exp ))
       label_exps in
   `Record (sem_of_list rec_exps) : exp )
let mee_comma x y =
  (`App ((`App ((`App ((`Vrn "Com"), (`Lid "_loc"))), x)), y) : FAstN.exp )
let mee_app x y =
  (`App ((`App ((`App ((`Vrn "App"), (`Lid "_loc"))), x)), y) : FAstN.exp )
let mee_of_str s =
  let len = String.length s in
  if (s.[0]) = '`'
  then
    let s = String.sub s 1 (len - 1) in
    (`App ((`Vrn "Vrn"), (`Par (`Com ((`Lid "_loc"), (`Str s))))) : FAstN.exp )
  else
    (`App ((`Vrn "Uid"), (`Par (`Com ((`Lid "_loc"), (`Str s))))) : FAstN.exp )
let mk_tuple_ee =
  function
  | [] -> invalid_arg "mktupee arity is zero "
  | x::[] -> x
  | xs ->
      let v = Flist.reduce_right mee_comma xs in
      (`App ((`Vrn "Par"), (`Par (`Com ((`Lid "_loc"), v)))) : FAstN.exp )
let mee_record_col label exp =
  (`App
     ((`App
         ((`App ((`Vrn "RecBind"), (`Lid "_loc"))),
           (`App ((`Vrn "Lid"), (`Par (`Com ((`Lid "_loc"), (`Str label)))))))),
       exp) : FAstN.exp )
let mee_record_semi a b =
  (`App ((`App ((`App ((`Vrn "Sem"), (`Lid "_loc"))), a)), b) : FAstN.exp )
let mk_record_ee label_exps =
  (label_exps |> (List.map (fun (label,exp)  -> mee_record_col label exp)))
    |>
    (fun es  ->
       let x = Flist.reduce_right mee_record_semi es in
       (`App ((`App ((`Vrn "Record"), (`Lid "_loc"))), x) : FAstN.exp ))