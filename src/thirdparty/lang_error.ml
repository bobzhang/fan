


  

let () =
  let d = Ns.lang in
  let f  = fun (loc:Locf.t) _meta _content ->
    let s = Locf.to_string loc in
    %exp@loc{$str:s} in
  let f2 = fun (loc:Locf.t) _meta _content ->
    let s = Locf.to_string loc in
    %stru@loc{$str:s} in
  begin 
    Ast_quotation.add (d,"here") Dyn_tag.exp f;
    Ast_quotation.add (d,"here") Dyn_tag.stru f2
  end

(* local variables: *)
(* compile-command: "pmake lib" *)
(* end: *)
