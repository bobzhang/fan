


(** A cool example to show how to make use of the *lazy* expansion
    of such context sensitive keyworkds [__MODULE__] and [__BIND__]
 *)
let () =
  let d = Ns.lang in
  let f  = fun (loc:Locf.t) _meta _content ->
    %exp@loc{
          ref (fun _ ->
            Format.ksprintf failwith "%s.%s not implemented " __MODULE__ __BIND__)} in
  
  begin 
    Ast_quotation.add {domain = d; name = "undef"} Dyn_tag.exp f;
  end


(* local variables: *)
(* compile-command: "cd .. && pmake  main_annot/lang_not_implement.cmo" *)
(* end: *)
