


  

let () =
  let d = Ns.lang in
  let f  = fun (loc:Locf.t) _meta content ->
    let s = content ^ "\n" ^ Locf.to_string loc in
    %exp@loc{OUnit.assert_failure $str:s} in
  let f2 = fun (loc:Locf.t) _meta content ->
    (* let s = Locf.to_string loc in *)
    (* %stru@loc{$str:s} in *)
    let e =  f loc _meta content in
    %stru@loc{$exp:e} in
  begin 
    Ast_quotation.add (d,"err") Dyn_tag.exp f;
    Ast_quotation.add (d,"err") Dyn_tag.stru f2
  end

(* local variables: *)
(* compile-command: "pmake lib" *)
(* end: *)
