


let mtypes : Sigs_util.mtyps = 

  let tys = Ast_basic.N.list_of_sem   Astf.code_of_ast [] in
  let f = function
    | (`TyDcl (`Lid s,_,_,_) as y) -> (s,y)
    | _ -> assert false in 
  List.map
    (fun (t:Astfn.stru) ->
      match t with

      | `Type ( (`And _ as c) ) ->
          let ms =  Ast_basic.N.list_of_and c [] in 
          Sigs_util.Mutual (List.map f ms)

      | `Type x ->
          Sigs_util.Single (f x )
      | _ -> assert false) tys 
;;



module Print = Derive_stru.Make (struct let p = Gen_print.default end);;

let buitin_dispatch =
  [ ("print",
     Option.get (Print.stru_of_mtyps mtypes)
    )
 ];;

%create{builtin};;

%extend{
builtin:
  [ L1 Lid x SEP ";" as xs  %{
    Ast_gen.sem_of_list
      (List.map (fun (x:Tokenf.txt) ->
        Fill.stru x.loc (List.assoc x.txt buitin_dispatch)) xs) 
   }
      
  ]
};;


let _ =
  begin
    Ast_quotation.of_stru
      ~lexer:Lex_fan.from_stream
      ~name:({domain =Ns.lang;name="builtin"})
      ~entry:builtin
      ()
      ;

  end
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/builtin.cmo" *)
(* end: *)
