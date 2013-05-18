open LibUtil

open AstN

open AstLibN

open BasicN

let (<+) names acc =
  List.fold_right
    (fun name  acc  -> (`Fun (`Case ((`Lid name), acc)) : AstN.exp )) names
    acc

let mkfun = ( <+ )

let currying cases ~arity  =
  let cases = bar_of_list cases in
  if arity >= 2
  then
    let names = List.init arity (fun i  -> x ~off:i 0) in
    let exps = List.map (fun s  -> (`Lid s : AstN.exp )) names in
    let x = tuple_com exps in names <+ (`Match (x, cases) : AstN.exp )
  else (`Fun cases : AstN.exp )

let eta_expand (exp : exp) number =
  (let names = List.init number (fun i  -> x ~off:0 i) in
   names <+ (exp +> names) : exp )

let unknown len =
  if len = 0
  then (`Send ((`Lid "self"), (`Lid "unknown")) : AstN.exp )
  else (`App ((`Lid "failwith"), (`Str "not implemented!")) : AstN.exp )