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
      | "("; Lid x ; Str y; Syntaxf.ctyp as t;  ")" %{ (_loc,x,Some y,Some t)}
      | "("; Lid x; ":"; Syntaxf.ctyp as t; ? str as y;  ")" %{ (_loc,x,y,Some t)}
      ]      

  let ty :
  [ "("; qualid as x ; ":"; t_qualid as t;")" %{ `Dyn(x,t)}
  |  qualuid as t %{ `Static t}
  | %{ `Static (`Uid(_loc,"Gramf")) (** BOOTSTRAP, associated with module [Gramf]*)}
  ]
      
qualuid :
  [ Uid x; ".";  S as xs  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}
  ] 

qualid :
  [ Uid x ; "."; S as xs %{ `Dot(_loc,`Uid(_loc,x),xs)}
  | Lid i %{ `Lid(_loc,i)}
  ]
      
t_qualid :
  [ Uid x; ".";  S as xs %{ %ident'{$uid:x.$xs}}
  | Uid x; "."; Lid "t" %{ `Uid(_loc,x)}
  ] 
      
nonterminals : (* when [ty] is nullable, it should take care of the following *)
  [ ty as t; L1 type_entry as ls %{
    let mk =
      match t with
      |`Static t ->  %exp{ $id:t.mk }
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
  [ "("; qualid as x; ":";t_qualid as t;")"; L1 type_entry as ls
    %{
      let mk  =
        %exp{$id:t.mk_dynamic ${(x:vid:>exp)} }  in
      sem_of_list (* FIXME improve *)
        (%stru{ let ${(x :>pat)} = $id:t.create_lexer ~annot:"" ~keywords:[] ()} ::
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
