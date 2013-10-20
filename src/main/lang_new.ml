%import{
Ast_gen:
  sem_of_list
  ;
}


open FAst

%create{qualid nonterminals newterminals t_qualid qualuid}

  
%extend{
let str : [Str y  %{y}]
let type_entry :
      [ Lid x  %{ (_loc,x,None,None)}
      | "("; Lid x ; Str y; ")" %{(_loc,x,Some y,None)}
      | "("; Lid x ; Str y; Syntaxf.ctyp{t};  ")" %{ (_loc,x,Some y,Some t)}
      | "("; Lid x; ":"; Syntaxf.ctyp{t}; OPT str {y};  ")" %{ (_loc,x,y,Some t)}
      ]      

  let ty :
  [ "("; qualid{x} ; ":"; t_qualid{t};")" %{ `Dyn(x,t)}
  |  qualuid{t} %{ `Static t}
  | %{ `Static (`Uid(_loc,"Fgram")) (** BOOTSTRAP, associated with module [Fgram]*)}
  ]
      
qualuid :
  [ Uid x; ".";  S{xs}  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}
  ] 

qualid :
  [ Uid x ; "."; S{xs} %{ `Dot(_loc,`Uid(_loc,x),xs)}
  | Lid i %{ `Lid(_loc,i)}
  ]
      
t_qualid :
  [ Uid x; ".";  S{xs} %{ %ident'{$uid:x.$xs}}
  | Uid x; "."; Lid "t" %{ `Uid(_loc,x)}
  ] 
      
nonterminals : (* when [ty] is nullable, it should take care of the following *)
  [ ty {t}; L1 type_entry {ls} %{
    let mk =
      match t with
      |`Static t -> let t = (t : vid :> exp ) in %exp{ $t.mk }
      |`Dyn(x,t) ->
          let x = (x : vid :> exp) in
          %exp{$id:t.mk_dynamic $x }  in   
    sem_of_list
      ( List.map
      (fun (_loc,x,descr,ty) ->
        match (descr,ty) with
        |(Some d,None) ->
            %stru{ let $lid:x = $mk $str:d }
        | (Some d,Some typ) ->
            %stru{ let $lid:x : $typ = $mk $str:d }
        |(None,None) ->
            %stru{ let $lid:x = $mk $str:x  }
        | (None,Some typ) ->
            %stru{ let $lid:x : $typ = $mk $str:x  }  ) ls)} ]
newterminals :
  [ "("; qualid{x}; ":";t_qualid{t};")"; L1 type_entry {ls}
    %{
      let mk  =
        let x = (x : vid :> exp) in
        %exp{$id:t.mk_dynamic $x }  in
      sem_of_list (* FIXME improve *)
        (%stru{ let $(x :>pat) = $id:t.create_lexer ~annot:"" ~keywords:[] ()} ::
         ( List.map
            (fun (_loc,x,descr,ty) ->
              match (descr,ty) with
              |(Some d,None) ->
                  %stru{ let $lid:x = $mk $str:d }
              | (Some d,Some typ) ->
                  %stru{ let $lid:x : $typ = $mk $str:d }
              |(None,None) ->
                  %stru{ let $lid:x = $mk $str:x  }
              | (None,Some typ) ->
                  %stru{ let $lid:x : $typ = $mk $str:x  }  ) ls)) }]
}  
let _ =
  let d = Ns.lang in
  begin
    Ast_quotation.of_stru
      ~name:(d,"create") ~entry:nonterminals ();
    Ast_quotation.of_stru
      ~name:(d,"new") ~entry:newterminals ();
  end
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_new.cmo" *)
(* end: *)
