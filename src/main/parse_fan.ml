%import{
Tokenf:
  mk_ant;
Fan_ops:
  ident_of_ctyp
  ;
Gramlib:
  setup_op_parser
  symbolchar
  ;
Ast_gen:
  (<+>)
  apply
  dot
  bar_of_list
  and_of_list
  com_of_list
  appl_of_list
  ;
}
open FAst
open! Syntaxf


%create{Gramf pos_exps};;

(* Gramf.extend_single prefixop *)
(*  (None, *)
(*       ((None, None, *)
(*          [([`Keyword "x"], *)
(*             ("", *)
(*               (Gramf.mk_action *)
(*                  (fun ~__fan_0:_  (_loc : Locf.t)  -> (() : 'a )))))]) :  *)
(*       Gramf.olevel )) *)
let apply () = begin 
  begin
    setup_op_parser infixop2
      (fun x -> List.mem x ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] ||
      (not (List.mem x ["<-"; "||"; "&&"]) && String.length x >= 2 &&
       List.mem x.[0] ['='; '<'; '>'; '|'; '&'; '$'; '!'] && symbolchar x 1));
    setup_op_parser infixop3
      (fun x -> String.length x >= 1 && List.mem x.[0] ['@'; '^'] &&
              symbolchar x 1);
    setup_op_parser infixop4
      (fun x -> x <> "->" && String.length x >= 1 && List.mem x.[0] ['+'; '-'] &&
      symbolchar x 1);
    (* setup_op_parser infixop5 *)
    (*   (fun x -> String.length x >= 1 && List.mem x.[0] ['*'; '/'; '%'; '\\'] && *)
    (*   (x.[0] <> '*' || String.length x < 2 || x.[1] <> '*') && *)
    (*   symbolchar x 1 ); *)
  end;

  (* with mexp *)
  %extend{
      mexp_quot:
      [ mexp as x %{ x}]
      mbind0:
      { RA
        [ "("; a_uident as m; ":"; mtyp as mt; ")"; S as mb %{ `Functor (_loc, m, mt, mb)}
        | ":"; mtyp as mt; "="; mexp as me   %{ `Constraint (_loc, me, mt) }
        | "="; mexp as me  %{ me}  ] }
      mexp:
      { "top"
        [ "functor"; "("; a_uident as i; ":"; mtyp as t; ")"; "->"; S as me %{
             `Functor (_loc, i, t, me)}
        | "struct"; strus as st; "end" %{ `Struct(_loc,st)}
        | "struct"; "end" %{`StructEnd(_loc)}]
       "apply"
        [ S as me1; S as me2 %{ `App (_loc, me1, me2)} ]
       "simple"
        [ Ant (""|"mexp", s) %{  mk_ant ~c:"mexp" s}
        | Quot x %{Ast_quotation.expand x Dyn_tag.mexp}
        | module_longident as i  %{ (i:>mexp)}
        | "("; S as me; ":"; mtyp as mt; ")" %{ `Constraint (_loc, me, mt)}
        | "("; S as me; ")" %{  me}
        | "("; "val"; exp as e; ")" %{ `PackageModule (_loc, e)}
        | "("; "val"; exp as e; ":"; mtyp as p; ")"
            %{ `PackageModule (_loc, `Constraint (_loc, e, `Package (_loc, p)))}] } };

  with mbind
      %extend{
        mbind_quot:
        [ S as b1; "and"; S as b2 %{  `And(_loc,b1,b2)}
        | Ant ("mbind"|"",s) %{mk_ant ~c:"mbind" s}
        | a_uident as m; ":"; mtyp as mt %{ `Constraint(_loc,m,mt)}
        | a_uident as m; ":"; mtyp as mt; "="; mexp as me %{ `ModuleBind(_loc,m,mt,me)}]
        mbind:
        [ S as b1; "and"; S as b2 %{ `And(_loc,b1,b2)}
        | Ant ("mbind" |"" ,s) %{mk_ant  ~c:"mbind" s}
        | Quot x  %{Ast_quotation.expand  x Dyn_tag.mbind}
        | a_uident as m; ":"; mtyp as mt; "="; mexp as me %{`ModuleBind (_loc, m, mt, me)}]
        module_rec_declaration:
        [ S as m1; "and"; S as m2 %{`And(_loc,m1,m2)}
        | Ant (""|"mbind",s) %{mk_ant ~c:"mbind"  s}
        | Quot x %{Ast_quotation.expand  x Dyn_tag.mbind}
        | a_uident as m; ":"; mtyp as mt %{`Constraint(_loc,m,mt)} ] };
  (* with constr *)
     %extend{
        constr_quot:
        [ constr as x %{x}   ]
        constr: 
        [ S as wc1; "and"; S as wc2 %{`And(_loc,wc1,wc2)}
        | Ant (""|"constr",s) %{mk_ant ~c:"constr" s}
        | Quot x  %{Ast_quotation.expand  x Dyn_tag.constr}
        | "type"; type_longident_and_parameters as t1; "="; ctyp as t2 %{`TypeEq (_loc, t1, t2)}
        | "type"; type_longident_and_parameters as t1; "="; "private"; ctyp as t2 %{
            `TypeEqPriv(_loc,t1,t2)}
        | "type"; type_longident_and_parameters as t1; ":="; ctyp as t2 %{
            `TypeSubst (_loc, t1, t2)}
        | "module"; module_longident as i1; "="; module_longident_with_app as i2 %{
            `ModuleEq (_loc, (i1:vid :> ident) , i2)}
        | "module"; module_longident as i1; ":="; module_longident_with_app as i2 %{
            `ModuleSubst (_loc, (i1: vid :> ident), i2)}] };




    %extend{
      sigis:
      [ Ant (""|"sigi",s) %{ mk_ant  ~c:"sigi" s}

      | Ant (""|"sigi",s); ";;"; S as sg %{`Sem (_loc,  mk_ant  ~c:"sigi" s, sg)}
      | Ant (""|"sigi",s);  S as sg %{`Sem (_loc,  mk_ant ~c:"sigi" s, sg)}
            
      | sigi as sg;";;" ; S as s %{ `Sem(_loc,sg,s)}
      | sigi as sg;";;" %{sg}
      | sigi as sg; S as s %{`Sem(_loc,sg,s)}
      | sigi as sg %{ sg} ]
      mtyp:
      { "top"
        [ "functor"; "("; a_uident as i; ":"; S as t; ")"; "->"; S as mt %{`Functor(_loc,i,t,mt)}]
        "with"
        [ S as mt; "with"; constr as wc %{`With(_loc,mt,wc)}]
        "apply"
        [ S as mt1; S as mt2 %{
            match (mt1, mt2) with
            | ((#ident as i1), (#ident as i2)) -> apply i1 i2 
            | _ -> raise Streamf.NotConsumed }] (* FIXME *)
        "."
        [ S as mt1; "."; S as mt2 %{
          let acc0 mt1 mt2 =
            match (mt1, mt2) with
            | ((#ident as i1), (#ident as i2)) ->
              dot i1 i2 
            | _ -> raise Streamf.NotConsumed  in
          acc0 mt1 mt2 }] (*FIXME*)
        "sig"
        [ "sig"; sigis as sg; "end" %{ `Sig(_loc,sg)}
        | "sig";"end" %{`SigEnd(_loc)}]
       "simple"
        [ Ant (""|"mtyp",s) %{mk_ant ~c:"mtyp" s}
        | Quot x %{ Ast_quotation.expand  x Dyn_tag.mtyp}
        | module_longident_with_app as i %{(i:ident:>mtyp)}
        | "("; S as mt; ")" %{mt}
        | "module"; "type"; "of"; mexp as me %{ `ModuleTypeOf(_loc,me)}] }
      module_declaration: (* syntax sugar *)
      [ ":"; mtyp as mt %{ mt}
      | "("; a_uident as i; ":"; mtyp as t; ")"; S as mt %{`Functor(_loc,i,t,mt)}]
      mtyp_quot:
      [ mtyp as x %{ x}  ]  };

  %extend{
  Inline stru_sigi :
  [  "open"; ? "!" as bang; module_longident as i
       %{ `Open(_loc,
                match bang with
                 | Some _ -> `Positive _loc
                 | None -> `Negative _loc , (i:vid :> ident))}
  | "type"; type_declaration as t  %{`Type(_loc,t)}
  | "module"; "type"; a_uident as i; "="; mtyp as mt %{`ModuleType(_loc,i,mt)}
  | "class"; "type"; cltyp_declaration as ctd  %{ `ClassType(_loc,ctd)}
  | "exception"; constructor_declaration as t %{ `Exception(_loc,t)}
  | "external"; a_lident as i;":";ctyp as t;"=" ;string_list as sl %{`External (_loc, i, t, sl)}
   (* | "exception"; constructor_declaration as t; "="; type_longident as i -> *)
   (*     %{ exception $t = $i } *)
       
  ]};
  %extend{
    sigi_quot:
    [ "#"; a_lident as s %{ `DirectiveSimple(_loc,s)}
    | "#"; a_lident as s; exp as dp %{ `Directive(_loc,s,dp)}
    | sigi as sg1; ";"; S as sg2 %{ `Sem(_loc,sg1,sg2)}
    | sigi as sg %{sg}] 
    sigi:
    [ Ant (""|"sigi",s)   %{mk_ant ~c:"sigi" s}
    | Quot x %{ Ast_quotation.expand  x Dyn_tag.sigi}
    | "include"; mtyp as mt %{ `Include(_loc,mt)}
    | "module"; a_uident as i; module_declaration as mt %{ `Module(_loc,i,mt)}
    | "module"; "rec"; module_rec_declaration as mb %{  `RecModule (_loc, mb)}
    | "module"; "type"; a_uident as i  %{`ModuleTypeEnd(_loc,i)}
    | @stru_sigi
    | "val"; a_lident as i; ":"; ctyp as t %{ `Val(_loc,i,t)}
    | "class"; class_description as cd  %{ `Class(_loc,cd)}
    ]
    (* mli entrance *)    
    interf:
    [(*  "#"; a_lident as n;  ";;" -> *)
    (*   ([ `DirectiveSimple(_loc,n) ],  Some _loc) *)
    (* | "#"; a_lident as n; exp as dp; ";;" -> ([ `Directive(_loc,n,dp)], Some _loc)  *)
     sigi as si; ";;";  S as rest %{
         let (sil,stopped) = rest in (si :: sil, stopped)}
    |sigi as si; S as rest %{
        let (sil,stopped) = rest in (si :: sil, stopped)}
    | EOI %{ ([], None)} ]  };

    with exp
    %extend{
      exp_quot:
      [ exp as e1; ","; comma_exp as e2 %{ `Com(_loc,e1,e2)}
      | exp as e1; ";"; sem_exp as e2 %{ `Sem(_loc,e1,e2)}
      | exp as e  %{e}]
     sem_exp:
      [ exp as e1 ; ?";" %{e1}
      | exp as e1 ; ";"; S as e2 %{`Sem(_loc,e1,e2)}]
       (* {:stru|
       let f (type t) () =
          let module M = struct exception E of t ; end in
          ((fun x -> M.E x), (function [M.E x -> Some x | _ -> None]))}
       %stru{ let f : ! 'a . 'a -> 'a = fun x -> x } *)
      cvalue_bind:
      [ "="; exp as e %{ e}
      | ":"; "type"; unquoted_typevars as t1; "." ; ctyp as t2 ; "="; exp as e %{
          let u = %ctyp{ ! $t1 . $t2 } in  %{ ($e : $u) }}
      | ":"; ctyp as t; "="; exp as e %exp{ ($e : $t) }
      | ":"; ctyp as t; ":>"; ctyp as t2; "="; exp as e %{
        match t with
        | %ctyp{ ! $_ . $_ } ->
            raise (Streamf.Error "unexpected polytype here")
        | _ -> %{ ($e : $t :> $t2) } }
      | ":>"; ctyp as t; "="; exp as e %{ `Subtype(_loc,e,t)} ]
      fun_bind:
      { RA
          [ "("; "type"; a_lident as i; ")"; S as e %{
            `LocalTypeFun(_loc,i,e)}
          | ipat as p; S as e  %{ `Fun(_loc,`Case(_loc,p,e))}
          | cvalue_bind as bi %{ bi}  ] }
       lang:
       [ dot_lstrings as ls %{
         let old = !Ast_quotation.default in
         match  Ast_quotation.resolve_name  ls
         with
         | Some x -> (Ast_quotation.default := Some x; old)
         | None ->
             Locf.failf _loc "DDSL `%s' can not be resolved"
               (Tokenf.string_of_name ls)
         }]
       pos_exps:
       [ L1 name_space SEP ";" as xys %{
                    let old = !Ast_quotation.map in
                    (Ast_quotation.map := Mapf.String.add_list xys old;
                     old)}]
      let name_space:
       [ Lid x;":";dot_lstrings as y %{
             (x,
              match Ast_quotation.resolve_name  y with
              |None ->
                  Locf.failf _loc "DDSL `%s' can not be resolved"
                    (Tokenf.string_of_name y)
              | Some x -> x
             )}
       | Lid x %{
           (x,
            match Ast_quotation.resolve_name (`Sub [],x)
            with 
            |None ->
                Locf.failf _loc "DDSL `%s' can not be resolved" x
            | Some  x -> x)} ]  
       let fun_def_pat:
       ["(";"type";a_lident as i;")" %{fun e ->  `LocalTypeFun (_loc, i, e)}
       | ipat as p %{ fun e -> `Fun(_loc,`Case(_loc,p,e))}(* %{ fun $p -> $e } *)
       | ipat as p; "when"; exp as w %{fun e -> `Fun(_loc,`CaseWhen(_loc,p,w,e))} ]
       fun_def:
       {RA
          [ fun_def_pat as f; "->"; exp as e %{  f e}
          | fun_def_pat as f; S as e  %{f e}] }
           
       Inline primitve :
       [ Int s   %{`Int(_loc,s)}
       | Int32 s %{ `Int32(_loc,s)}
       | Int64 s %{ `Int64(_loc,s)}
       | Nativeint s %{ `Nativeint (_loc, s)}
       | Flo s %{  `Flo (_loc, s)}
       | Chr s %{ `Chr (_loc, s)}
       | Str s %{ `Str (_loc, s)}]

       (************************)
       (*  How to handle S     *)    
       (************************)               
       Inline let_stru_exp:
       [ "let"; opt_rec as r; bind as bi; "in"; exp as x %{`LetIn(_loc,r,bi,x)}
       | "let"; "module"; a_uident as m; mbind0 as mb; "in"; exp as e
           %{ `LetModule (_loc, m, mb, e)}
       | "let"; "open"; ? "!" as bang ; module_longident as i; "in"; exp as e %{
            `LetOpen (_loc,
                      match bang with | Some _   -> `Positive _loc | None -> `Negative _loc,
                        (i:vid :> ident), e)}
       | "let"; "try"; opt_rec as r; bind as bi; "in"; exp as x; "with"; case as a %{
              `LetTryInWith(_loc,r,bi,x,a)}]
       exp :
       {
        "top" RA
        [ @let_stru_exp
        | "match"; S as e; "with"; case as a  %{`Match (_loc, e, a)}
        | "try"; S as e; "with"; case as a %{ `Try (_loc, e, a)}
        | "if"; S as e1; "then"; S as e2; "else"; S as e3 %{`IfThenElse (_loc, e1, e2, e3)}
        | "if"; S as e1; "then"; S as e2  %{`IfThen (_loc, e1, e2)}
        | "do"; sequence as seq; "done" %{ `Seq(_loc,seq)}
        | "with"; lang as old; S as x %{ begin  Ast_quotation.default := old; x  end}
        | "with";"{"; pos_exps as old ;"}"; S as x %{ begin Ast_quotation.map := old; x end}
        | "for"; a_lident as i; "="; S as e1; flag as df; S as e2; "do";
            sequence as seq; "done" %{
              `For (_loc, i, e1, e2, df, seq)}
        | "while"; S as e; "do"; sequence as seq; "done" %{
            `While (_loc, e, seq)}]  
       ":=" NA
        [ S as e1; ":="@xloc; S as e2 %{
          `App(_loc, `App (_loc, `Lid(xloc,":="), e1),e2)
          (* (`Assign (_loc,`Field(_loc,e1,`Lid(_loc,"contents")),e2):exp) *)
          (* %{ $e1 := $e2 } *)}
        | S as e1; "<-"; S as e2 %{ (* FIXME should be deleted in original syntax later? *)
            match Fan_ops.bigarray_set _loc e1 e2 with
            | Some e -> e
            | None -> `Assign(_loc,e1,e2)}  ]

       "||" RA
        [ S as e1; ("or"|"||" as op); S as e2  %{
          Ast_gen.appl_of_list [ %exp{$lid:op}; e1 ;e2]}  ]
       "&&" RA
        [ S as e1; ("&"|"&&" as op) ; S as e2  %{
          Ast_gen.appl_of_list [ %exp{$lid:op}; e1 ;e2]}  ]
       "<" LA
        [ S as e1; infixop2 as op; S as e2 %exp{ $op $e1 $e2 }
          (* S as e1; Inf@xloc (2,x); S as e2 %{`App(_loc,`App(_loc,`Lid(xloc,x),e1),e2)}       *)
        ]
       "^" RA
        [ S as e1; infixop3 as op; S as e2 %exp{ $op $e1 $e2 }
        (* | S as e1; Inf@xloc (3,x); S as e2 %{`App(_loc,`App(_loc,`Lid(xloc,x),e1),e2)}             *)
        ]
        "::" RA
        [ S as e1; "::"; S as e2  %exp{  $e1 :: $e2  } ]  
       "+" LA
        [ S as e1; infixop4 as op; S as e2 %exp{ $op $e1 $e2 }
          (* S as e1; Inf@xloc (4,x); S as e2 %{`App(_loc,`App(_loc,`Lid(xloc,x),e1),e2)} *)
        ]
       "*" LA
        [ S as e1; ("land"|"lor"|"lxor"|"mod" as op) ; S as e2
            %{Ast_gen.appl_of_list [ %exp{$lid:op}; e1; e2] }
        (* | S as e1; infixop5 as op; S as e2  %exp{ $op $e1 $e2 } *)
        | S as e1; Inf@xloc (5,x); S as e2 %{
          let op = %exp@xloc{$lid:x} in
          %exp{$op $e1 $e2}}
        ]
       "**" RA
        [ S as e1; ("asr"|"lsl"|"lsr" as op) ; S as e2
            %{Ast_gen.appl_of_list [%exp{$lid:op}; e1;e2] }
        | S as e1; Inf@xloc (4,x); S as e2 %{
          let op = %exp@xloc{$lid:x} in
          %exp{$op $e1 $e2}}]

          
       "obj" RA
        [("fun"|"function"); "|";  L1 case0 SEP "|" as a  %{
           let cases = bar_of_list a in `Fun (_loc,cases)}
        | ("fun"|"function"); fun_def as e %{ e}

        | "object"; "(";pat as p; ")"; class_structure as cst;"end" %{ `ObjPat(_loc,p,cst)}
        | "object"; "(";pat as p; ")"; "end"  %{`ObjPatEnd(_loc,p)}
        | "object"; "(";pat as p;":";ctyp as t;")";class_structure as cst;"end" %{
            `ObjPat(_loc,`Constraint(_loc,p,t),cst)}
        | "object"; "(";pat as p;":";ctyp as t;")" ; "end" %{
            `ObjPatEnd(_loc,`Constraint(_loc,p,t))}
        | "object"; class_structure as cst;"end" %{ `Obj(_loc,cst)}
        | "object";"end" %{ `ObjEnd(_loc)}]
       "unary minus" NA
        [ "-"; S as e %{ Fan_ops.mkumin _loc "-" e} (* Delayed into Dump *)
        | "-."; S as e %{ Fan_ops.mkumin _loc "-." e} ]
       "apply" LA
        [ S as e1; S as e2 %{ `App(_loc,e1,e2)}
        | "assert"; S as e %{ `Assert(_loc,e)}
        | "new"; class_longident as i %{ `New (_loc,i)} 
        | "lazy"; S as e %{ `Lazy(_loc,e)} ]
       "label" NA
        [ "~"; a_lident as i; ":"; S as e %{ `Label (_loc, i, e)}
        | "~"; a_lident as i %{ `LabelS(_loc,i)}
        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | Label i; S as e %{ %{ ~ $lid:i : $e }}
        (* Same remark for ?a:b *)
        | Optlabel i; S as e %{  `OptLabl(_loc,`Lid(_loc,i),e)}
        | "?"; a_lident as i; ":"; S as e %{ `OptLabl(_loc,i,e)}
        | "?"; a_lident as i %{ `OptLablS(_loc,i)} ] 
       "." LA
        [ S as e1; "."; "("; S as e2; ")" %{ `ArrayDot (_loc, e1, e2)}
        | S as e1; "."; "["; S as e2; "]" %{ `StringDot (_loc, e1, e2)}
        | S as e1; "."; "{"; comma_exp as e2; "}" %{ Fan_ops.bigarray_get _loc e1 e2}
        | S as e1; "."; label_longident as e2 %{ `Field(_loc,e1,e2)}
        | S as e; "#"; a_lident as lab %{ `Send (_loc, e, lab)} ]
       "~-" NA
        [ "!"@xloc; S as e %{`App(_loc, `Lid(xloc,"!"),e )}
        | Pre@xloc x; S as e %{`App(_loc,`Lid(xloc,x),e )}]
       "simple"
        [ Quot x  %{Ast_quotation.expand  x Dyn_tag.exp}
        | Ant ("exp"
                |""
                |"par"
                |"seq"
                |"chr"
                |"int"
                |"int32"
                |"str"
                |"int64"
                |"flo"
                |"nativeint"
                | "vrn"

                |"chr'"
                |"int64'"
                |"nativeint'"
                |"bool'"
                |"int'"
                |"int32'"
                |"flo'"
                |"str'"
                |"`chr"
                |"`int64"
                |"`nativeint"
                |"`bool"
                |"`int"
                |"`int32"
                |"`flo"
                |"`str"
                , s) %{
                    mk_ant ~c:"exp" s}
        | @primitve 
        | TRY module_longident_dot_lparen as i;S as e; ")" %{
            `LetOpen (_loc,`Negative _loc, i, e)}
        (* | TRY val_longident as i -> %{ $id:i } *)
        (* | ident as i -> i  (\* FIXME logic was splitted here *\) *)
        | vid as i %{(i :vid :>exp) }
        | "`"; luident as s %{ `Vrn(_loc,s)}
        | "["; "]" %{ %{ [] }} (* FIXME *)
        | "["; sem_exp_for_list as mk_list; "]" %{mk_list %{ [] }}
        | "[|"; "|]" %{ `ArrayEmpty(_loc)}
        | "[|"; sem_exp as el; "|]" %{ `Array (_loc, el)}
        | "{"; Lid x ; "with"; label_exp_list as el; "}" %{
            %{ { ($lid:x) with $el }}} (* FIXME add antiquot support *)
        | "{"; label_exp_list as el; "}" %{ `Record (_loc, el)}
        | "{"; "("; S as e; ")"; "with"; label_exp_list as el; "}" %{
            `RecordWith (_loc, el, e)}
        | "{<"; ">}" %{ `OvrInstEmpty(_loc)}
        | "{<"; field_exp_list as fel; ">}" %{ `OvrInst(_loc,fel) }
        | "("; ")" %{ %{ () }}
        | "("; S as e; ":"; ctyp as t; ")" %{ `Constraint (_loc, e, t)}
        | "("; S as e; ","; comma_exp as el; ")" %{`Par (_loc, `Com (_loc, e, el))}
        | "("; S as e; ";"; sequence as seq; ")"  %{`Seq(_loc,`Sem(_loc,e,seq))}
        | "("; S as e; ";"; ")" %{ `Seq(_loc,e)} (* FIXME Seq or not?*)
        | "("; S as e; ":"; ctyp as t; ":>"; ctyp as t2; ")" %{`Coercion (_loc, e, t, t2)}
        | "("; S as e; ":>"; ctyp as t; ")" %{ `Subtype(_loc,e,t)}
        | "("; S as e; ")"  %{ e}
        | "begin"; sequence as seq; "end" %{ `Seq(_loc,seq)}
        | "begin"; "end" %{ %{ () }}
        | "("; "module"; mexp as me; ")" %{`Package_exp (_loc, me)}
        | "("; "module"; mexp as me; ":"; mtyp as pt; ")" %{
            `Package_exp (_loc, `Constraint (_loc, me, pt))}  ] }
           
       sem_exp_for_list:
       [ exp as e; ";"; S as el %{fun acc -> %exp{ $e :: ${el acc}}}
       | exp as e; ?";" %{fun acc -> %exp{ $e :: $acc }}
       ]

       (* Inline let_in : *)
       (* [ "let"; opt_rec as rf; bind as bi; "in"; exp as e; sequence'{k} %{ *)
       (*   k  (`LetIn (_loc, rf, bi, e))} ] *)
           
       sequence: (*FIXME*)
       [
        "let"; opt_rec as rf; bind as bi; "in"; exp as e; sequence' as k %{
         k  (`LetIn (_loc, rf, bi, e))}
       | "let"; "try"; opt_rec as r; bind as bi; "in"; S as x; "with"; case as a; sequence' as k
         %{k (`LetTryInWith(_loc,r,bi,x,a))}
       | "let"; "module"; a_uident as m; mbind0 as mb; "in";
           exp as e; sequence' as k %{ k  (`LetModule (_loc, m, mb, e))}
       | "let"; "open"; ?"!" as bang; module_longident as i; "in"; S as e %{
           `LetOpen (_loc,
                     match bang with
                     | Some _ -> `Positive _loc
                     | None -> `Negative _loc  , (i: vid :> ident), e)}
       | exp as e; sequence' as k %{ k e}
       (* FIXME Ant should be able to be followed *)      
       ]
       sequence':
       [ ?";" %{ fun e -> e}
       | ";"; sequence as el %{ fun e -> `Sem(_loc,e,el)} ]


           
       comma_exp:
       [ S as e1; ","; S as e2  %{`Com(_loc,e1,e2)}
       | exp as e %{e} ]
      };
  %extend{ with_exp_lang: [ lang as old; ":"; exp as x %{ (Ast_quotation.default := old; x)}] } ;
  %extend{ with_stru_lang: [lang as old;":"; stru as x %{ (Ast_quotation.default:=old;x)}]  };
  with bind
      %extend{
        bind_quot:
        [ bind as x  %{x}  ] 
        bind:
        [ Ant ("bind"|"",s)  %{mk_ant  ~c:"bind" s}
        | Ant ("" ,s); "="; exp as e %{
            %{ ${mk_ant  ~c:"pat" s} = $e }}
        | S as b1; "and"; S as b2 %{ `And (_loc, b1, b2)}
        | let_bind as b %{b} ] 
        let_bind:
        [ pat as p; fun_bind as e %{ `Bind (_loc, p, e)} ] };

  with case
    %extend{
      case:
      [ "|"; L1 case0 SEP "|" as l %{ bar_of_list l }
      | pat as p; "->"; exp as e %{ `Case(_loc,p,e)}]
      case0:
      [ Ant ("case" | "", s) %{ mk_ant  ~c:"case" s}
      | Ant ("",s) ;"when";exp as w;"->"; exp as e %{
          `CaseWhen(_loc,mk_ant  ~c:"case"  s, w,e )}
      | Ant ("",s); "->"; exp as e %{
          `Case(_loc,mk_ant  ~c:"case" s ,e)}
            
      | pat_as_pat_opt as p; "when"; exp as w;  "->"; exp as e %{
           `CaseWhen (_loc, p, w, e)}
      | pat_as_pat_opt as p; "->";exp as e %{ `Case(_loc,p,e)}]
      case_quot:
      [ L1 case0 SEP "|" as x %{ bar_of_list x}]  };
  with rec_exp
      %extend{
        rec_exp_quot:
        [ label_exp_list as x %{x}  ]
        label_exp:
        [ Ant ("rec_exp" |"" ,s) %{mk_ant  ~c:"rec_exp"  s}
        | label_longident as i; fun_bind as e
            %rec_exp{ $id:i = $e }
        | label_longident as i %{  (*FIXME*)
            `RecBind (_loc, i, `Lid (_loc, Fan_ops.to_lid i))}]
        field_exp :
        [ Ant (""|"bi",s) %{ mk_ant  ~c:"rec_exp" s}
        | a_lident as l; "=";  exp  as e %{`RecBind (_loc, (l:>vid), e)} (* %{ $lid:l = $e } *) ]
        label_exp_list:
        [ label_exp as b1; ";"; S as b2 %{`Sem (_loc, b1, b2)}
        | label_exp as b1; ?";"        %{b1}
        ]
        field_exp_list:
        [ field_exp as b1; ";"; S as b2 %{ `Sem (_loc, b1, b2)}
        | field_exp as b1; ?";"        %{ b1}
        ] };
  with pat
    %extend{
       pat_quot:
       [ pat as x; ","; comma_pat as y %{ `Com(_loc,x,y)}
       | pat as x; ";"; sem_pat as y %{ `Sem(_loc,x,y)}
       | pat as x %{ x}]
       pat_as_pat_opt:
       [ pat as p1; "as"; a_lident as s %{  `Alias (_loc, p1, s)}
       | pat as p %{p} ]
       let pat_constr:
       [module_longident as i %{(i :vid :> pat)}
       |"`"; luident as s  %{ (`Vrn(_loc,s) :pat)}
       |Ant (""|"pat"|"vrn" , s) %{ mk_ant  ~c:"pat" s}]
       pat:
       { "|" LA
        [ S as p1; "|"; S as p2 %{ `Bar(_loc,p1,p2)} ]
       ".." NA
        [ S as p1; ".."; S as p2 %{ `PaRng(_loc,p1,p2)} ]
        "::" RA
        [ S as p1; "::"; S as p2 %{`App (_loc, `App (_loc, `Uid (_loc, "::"), p1), p2)}]   
       "apply" LA
        [ pat_constr as p1; S as p2 %{ (*FIXME *)
          match p2 with
          | %{ ($par:p) } ->
              List.fold_left (fun p1 p2 -> %{ $p1 $p2 }) p1
                (Ast_basic.list_of_com p []) (* precise *)
          | _ -> %{$p1 $p2 }}  
        | pat_constr as p1 %{ p1}
        | "lazy"; S as p %{ `Lazy (_loc, p)}  ]
       "simple"
        [ Ant (""
               |"pat"
               |"par"
               |"int"
               |"int32"
               |"int64"
               |"vrn"
               |"flo"
               |"chr"                   
               |"nativeint"
               |"str"

               |"int'"
               |"int32'"
               |"int64'"
               |"nativeint'"
               |"flo'"
               |"chr'"
               |"str'"
               |"`int"
               |"`int32"
               |"`int64"
               |"`nativeint"
               |"`flo"
               |"`chr"
               |"`str",s)

          %{ mk_ant ~c:"pat" s}
        | vid as i %{ (i : vid :> pat)}
        | @primitve
        | "-"; Int s %{  `Int (_loc, Stringf.neg s)}
        | "-"; Int32 s %{ `Int32(_loc, Stringf.neg s) }
        | "-"; Int64 s %{ `Int64(_loc,Stringf.neg s)}
        | "-"; Nativeint s %{ `Nativeint(_loc,Stringf.neg s)}
        | "-"; Flo s %{ `Flo(_loc,Stringf.neg s)}
        | "["; "]" %{ %{ [] }}
        | "["; sem_pat_for_list as mk_list; "]" %{ mk_list %{ [] }}
              
        | "[|"; "|]" %{ `ArrayEmpty(_loc)}
        | "[|"; sem_pat as pl; "|]" %{ `Array(_loc,pl)}
        | "{"; label_pat_list as pl; "}" %{ `Record(_loc,pl)}
            (* %{ { $((pl : rec_pat :>pat)) } } *)
        | "("; ")" %{ %{ () }}
        | "("; "module"; a_uident as m; ")" %{ `ModuleUnpack(_loc,m)}
            (* %{ (module $m) } *)
        | "("; "module"; a_uident as m; ":"; (* package_type *)mtyp as pt; ")" %{
            `ModuleConstraint(_loc,m, `Package(_loc,pt))}
              (* %{ ( module $m :  $pt )} *)
        | "(" ; "module"; a_uident as m;":"; Ant("opt" ,s ); ")" %{
            `ModuleConstraint (_loc, m, mk_ant s)}
        | "("; S as p; ")" %{ p}
        | "("; S as p; ":"; ctyp as t; ")" %{ %{ ($p : $t) }}
        | "("; S as p; "as";  a_lident as s; ")" %{ %{ ($p as $s )}}
        | "("; S as p; ","; comma_pat as pl; ")" %{ %{ ($p, $pl) }}
        | "`"; luident as s %{ %{$vrn:s}}
          (* duplicated may be removed later with [pat Level "apply"] *)
        | "#"; type_longident as i %{ %{ # $i }}
        | Quot x %{ Ast_quotation.expand  x Dyn_tag.pat}
        | "_" %{ %{ _ }}
        | Label i; S as p %{ %{ ~ $lid:i : $p }}
        | "~"; a_lident as i; ":"; S as p %{ (* CHANGE *) %{ ~$i : $p}}
        | "~"; a_lident as i %{ `LabelS(_loc,i)}
        | Optlabel i; "("; pat_tcon as p; "="; exp as e; ")" %{
            `OptLablExpr(_loc,`Lid(_loc,i),p,e)}
            (* %{ ?$lid:i : ($p=$e)} *)
        | Optlabel i; "("; pat_tcon as p; ")"  %{
            `OptLabl(_loc,`Lid(_loc,i),p)}
            (* %{ ? $lid:i : ($p)} *)
        | "?"; a_lident as i;":"; "("; pat_tcon as p; "="; exp as e; ")" %{
            `OptLablExpr(_loc,i,p,e)}
            (* %{ ?$i:($p=$e)} *)
        | "?"; a_lident as i;":"; "("; pat_tcon as p; "="; Ant("opt",s); ")" %{
            `OptLablExpr (_loc, i, p,mk_ant s)}
            (* %{ ?$i : ($p = $(opt: `Ant(_loc, mk_ant n s )) )} *)
        | "?"; a_lident as i; ":"; "("; pat_tcon as p; ")"  %{
            `OptLabl(_loc,i,p)}
            (* %{ ? $i:($p)} *)
        | "?"; a_lident as i %{ `OptLablS(_loc,i )}
        | "?"; "("; ipat_tcon as p; ")" %{ `OptLabl(_loc,`Lid(_loc,""),p) (* FIXME*)}

        | "?"; "("; ipat_tcon as p; "="; exp as e; ")" %{
            `OptLablExpr(_loc,`Lid(_loc,""),p,e)}
            (* %{ ? ($p = $e) }; *)
        ] }

       ipat:
        [ "{"; label_pat_list as pl; "}" %pat{ { $pl }}
          (* %{ { $((pl: rec_pat :>pat)) } } *)
        | Ant (""|"pat"|"par" ,s) %{ mk_ant  ~c:"pat"  s}
        | "("; ")" %{ %{ () }}
        | "("; "module"; a_uident as m; ")" %{ `ModuleUnpack(_loc,m)}
            (* %{ (module $m) } *)
        | "("; "module"; a_uident as m; ":";  mtyp as pt; ")" %{
             `ModuleConstraint (_loc, m, ( (`Package (_loc, pt))))}
              (* %{ (module $m : $pt )} *)
        | "(" ; "module"; a_uident as m;":"; Ant("opt", s); ")" %{
             `ModuleConstraint (_loc, m, mk_ant  s)}
            (* %{ (module $m : $(opt: `Ant(_loc,mk_ant n s)))} *)

        (* when change [pat], we need to take care of the following terms
           for factorization *)      
        | "("; pat as p; ")" %{ p}
        | "("; pat as p; ":"; ctyp as t; ")" %pat{ ($p : $t) }
        | "("; pat as p; "as"; a_lident as s; ")" %pat{  ($p as $s) }
        | "("; pat as p; ","; comma_ipat as pl; ")"  %pat{ ($p, $pl) }
              
        | a_lident as s %{  (s: alident :> pat)}
              
        | Quot x %{ Ast_quotation.expand  x Dyn_tag.pat}
        | "`"; luident as s  %pat{$vrn:s}
        | "_" %{ %{ _ }}
        | Label i; S as p %pat{ ~ $lid:i : $p }
        | "~"; a_lident as i;":";S as p  %pat{ ~$i : $p}
        | "~"; a_lident as i %{  `LabelS(_loc,i)}
        | Optlabel i; "("; pat_tcon as p; "="; exp as e; ")" %{
            `OptLablExpr(_loc,`Lid(_loc,i),p,e)}
            (* %{ ?$lid:i : ($p=$e)} *)
        | Optlabel i; "("; pat_tcon as p; ")"  %{
            `OptLabl(_loc,`Lid(_loc,i),p)}
            (* %{ ? $lid:i : ($p)} *)
        | "?"; a_lident as i;":"; "("; pat_tcon as p; "="; exp as e; ")" %{
            `OptLablExpr(_loc,i,p,e)}
            (* %{ ?$i:($p=$e)} *)
        | "?"; a_lident as i;":"; "("; pat_tcon as p; "="; Ant("opt",s); ")" %{
            `OptLablExpr (_loc, i, p, mk_ant s)}
            (* %{ ?$i : ($p = $(opt: `Ant(_loc, mk_ant n s )) )} *)
        | "?"; a_lident as i; ":"; "("; pat_tcon as p; ")"  %{
            `OptLabl(_loc,i,p)}
            (* %{ ? $i:($p)} *)
        | "?"; a_lident as i %{ `OptLablS(_loc,i ) }
        | "?"; "("; ipat_tcon as p; ")" %{
            `OptLabl(_loc,`Lid(_loc,""),p)}
            (* %{ ? ($p) } *)
        | "?"; "("; ipat_tcon as p; "="; exp as e; ")" %{
            `OptLablExpr(_loc,`Lid(_loc,""),p,e)}
            (* %{ ? ($p = $e) } *)]
       
       sem_pat:
       [ pat as p1; ";"; S as p2 %{ `Sem(_loc,p1,p2)}
       | pat as p; ? ";" %{ p}
       ] 
       sem_pat_for_list:
       [ pat as p; ";"; S as pl %{ fun acc ->
         `App(_loc, `App(_loc,`Uid(_loc,"::"),p),pl acc)}
         (* %pat{  $p :: $(pl acc)  } *)
       | pat as p; ? ";" %{fun acc ->
           `App(_loc, `App(_loc,`Uid(_loc,"::"),p),acc)}
           (* %pat{  $p :: $acc  } *)
       ]
       pat_tcon:
       [ pat as p; ":"; ctyp as t %{ %{ ($p : $t) }}
       | pat as p %{ p} ]
       ipat_tcon:
       [ Ant("" ,s) %{ mk_ant ~c:"pat" s }
       | a_lident as i %{  (i : alident :> pat)}
       | a_lident as i; ":"; ctyp as t %{(`Constraint (_loc, (i : alident :>  pat), t) : pat)}]
       comma_ipat:
       [ S as p1; ","; S as p2 %{ %{ $p1, $p2 }}
       | ipat as p %{ p} ]
       comma_pat:
       [ S as p1; ","; S as p2 %{ %{ $p1, $p2 }}
       | pat as p %{ p} ]
       label_pat_list:
       [ label_pat as p1; ";"; S as p2 %{ `Sem(_loc,p1,p2)}
       | label_pat as p1; ";"; "_"; ? ";"  %{ `Sem(_loc,p1,`Any _loc)}
       | label_pat as p1; ?";"            %{ p1}
       ] 
       label_pat:
       [ Ant (""|"pat",s) %{ mk_ant  ~c:"pat" s}
       (* | `Quot x -> Ast_quotation.expand _loc x Dyn_tag.pat
        *) (* FIXME restore it later *)
       | label_longident as i; "="; pat as p %{ (* %{ $i = $p } *)
         `RecBind(_loc,i,p)}
       | label_longident as i %{
           (* `RecBind(_loc,i,`Id(_loc,`Lid(_loc,Fan_ops.to_lid i))) *)
           `RecBind(_loc,i,`Lid(_loc,Fan_ops.to_lid i))}
           (* %{ $i = $(lid:Id.to_lid i) } *)
       ] };
    
    with ident
    %extend{
      (* parse [a] [B], depreacated  *)
      luident: [Lid i %{ i} | Uid i %{ i}]
      (* parse [a] [B] *)
      aident:
       [ a_lident as i %{ (i:>ident)}
       | a_uident as i %{ (i:>ident)}]
      astr:
       [Lid i %{ `C (_loc,i)}
       | Uid i %{ `C(_loc,i)}
       | Ant ("" | "vrn" ,s) %{ mk_ant  s}]
      ident_quot:
      { "."
        [ S as i; "."; S as j %{ %{ $i.$j  }} ]
        "simple"
        [ Ant (""|"id" |"uid"|"lid" ,s) %{mk_ant   ~c:"ident"  s}
        | Ant (""|"id"|"uid",s); "."; S as i %{
            `Dot (_loc, mk_ant  ~c:"ident" s, i)}
        | Lid i %{ %{ $lid:i }}
        | Uid i %{ %{ $uid:i }}
        | Uid s ; "." ; S as j %{ %{$uid:s.$j}}
        | "("; S as i;S as j; ")" %{ `Apply _loc i j}  ] }

      (* parse [a] [b], [a.b] [A.b]*)
      ident:
      [ Ant (""|"id"|"uid"|"lid" ,s) %{ mk_ant  ~c:"ident" s}
      | Ant (""|"id"|"uid" ,s); "."; S as i %{
           `Dot (_loc, mk_ant ~c:"ident" s, i)}
      | Lid i %{ `Lid(_loc,i)}
      | Uid i %{ `Uid(_loc,i)}
      | Uid s ; "." ; S as j %{  `Dot (_loc, `Uid (_loc, s), j)}]
      
      vid: (* duplicate ident  FIXME *)
      [ Ant (""|"id" |"uid" |"lid",s) %{ mk_ant ~c:"ident"  s}
      | Ant (""|"id"|"uid",s); "."; S as i %{
           `Dot (_loc, mk_ant  ~c:"ident" s, i)}
      | Lid i %{ `Lid(_loc,i)}
      | Uid i %{ `Uid(_loc,i)}
      | Uid s ; "." ; S as j %{  `Dot (_loc, `Uid (_loc, s), j)}]
      
      uident:
      [Uid s %{ `Uid(_loc,s)}
      | Ant(""|"id"|"uid",s) %{mk_ant   ~c:"uident"  s}
      |Uid s; "."; S as l %{ dot (`Uid (_loc,s)) l}
      |Ant(""|"id"|"uid",s) ;"." ; S as i %{
          dot (mk_ant  ~c:"uident"  s) i}]

      (* parse [a.b.c] no antiquot *)
      dot_lstrings:
      [ Lid i %{ (`Sub[],i)}
      | Uid i ; "." ; S  as xs %{
        match xs with
        |(`Sub xs,v) -> (`Sub (i::xs),v)
        | _ -> raise (Streamf.Error "impossible dot_lstrings")}

      | "."; Uid i; "."; S as xs %{
          match xs with
          |(`Sub xs,v) -> (`Absolute (i::xs),v)
          | _ -> raise (Streamf.Error "impossible dot_lstrings")} ]

      (* parse [A.B.(] *)
      module_longident_dot_lparen:
      [ Ant (""|"id"|"uid" ,s); "."; "(" %{ mk_ant   ~c:"ident"  s }

      | Uid i; "."; S as l %{ %{$uid:i.$l}}
      | Uid i; "."; "(" %{ %{$uid:i}}
      | Ant ("uid"|"" ,s); "."; S as l %{ %{${mk_ant  ~c:"ident"  s}.$l} }]
      (* parse [A.B] *)
      module_longident:
      [ Ant (""|"id"|"uid",s) %{mk_ant ~c:"ident"  s }
      | Uid i; "."; S as l %{  `Dot (_loc, `Uid (_loc, i), l)}
      | Uid i %{ `Uid(_loc,i)}
      | Ant(""|"uid", s); "."; S as l %{`Dot (_loc, mk_ant ~c:"ident" s, l)}]

      module_longident_with_app:
      { "apply"
        [ S as i; S as j %{ `Apply(_loc,i,j)} ]
       "."
        [ S as i; "."; S as j %{ %{ $i.$j }} ]
       "simple"
        [ Ant (""|"id"|"uid",s) %{mk_ant ~c:"ident"  s}
        | Uid i %{ `Uid(_loc,i)}
        | "("; S as i; ")" %{ i} ] }

      (* parse [(A B).c ]*)
      type_longident: (* FIXME *)
      { "apply" (* No parens *)
        [ S as i; S as j %{ `Apply(_loc,i,j)} ]
        "."
        [ S as i; "."; S as j %{ %{ $i.$j }} ]
        "simple"
        [ Ant (""|"id"|"uid"|"lid",s) %{ mk_ant ~c:"ident"  s}
        | Lid i %{ %{$lid:i}}
        | Uid i %{ %{$uid:i}}
        | "("; S as i; ")" %{ i} ] }

      label_longident:
      [ Ant (""|"id"|"lid",s) %{ mk_ant ~c:"ident" s}
      | Lid i %{ `Lid(_loc,i)}
      | Uid@iloc i; "."; S as l %{ `Dot(_loc,`Uid(iloc,i),l)}
      | Ant(""|"uid",s); "."; S as l %{
        `Dot (_loc, mk_ant ~c:"ident" s, l)} ]
      
      cltyp_longident: [ type_longident as x  %{x} ]
      val_longident:[ ident as x %{ x} ]
      class_longident: [ label_longident as x %{(x : vid :>ident)} ]
      
      method_opt_override:
      [ "method"; ? "!" as bang  %{
        match bang with
        | Some _ ->  `Positive _loc
        | None -> `Negative _loc }
      | "method"; Ant (""|"override" ,s) %{mk_ant ~c:"flag" s}] 
      opt_override:
      [ ? "!" as bang %{
        match bang with
        | Some _ ->  `Positive _loc | None -> `Negative _loc}
      | Ant ("!"|"override" ,s) %{ mk_ant ~c:"flag" s}]
      
      value_val_opt_override:
      [ "val"; ? "!" as bang %{
        match bang with
        | Some _ -> `Positive _loc | None -> `Negative _loc}
      | "val"; Ant (""|"override"|"!" ,s) %{ mk_ant ~c:"flag" s}]
      flag:
      [ "to" %{  `Positive _loc}
      | "downto" %{ `Negative _loc }
      | Ant ("to"|"" ,s) %{mk_ant  ~c:"flag" s}]

      opt_private: (* Should be inlined directly in the future *)
      [ "private" %{ `Positive _loc}
      | Ant ("private" ,s) %{ mk_ant  ~c:"flag" s}
      | %{`Negative _loc}   ] 
      opt_mutable:
      [ "mutable" %{  `Positive _loc}
      | Ant ("mutable" ,s) %{ mk_ant ~c:"flag"  s}
      |  %{`Negative _loc}  ] 
      opt_virtual:
      [ "virtual" %{ `Positive _loc }
      | Ant ("virtual" ,s) %{ mk_ant  ~c:"flag"  s}
      | %{`Negative _loc}   ] 
      opt_dot_dot:
      [ ".." %{ `Positive _loc}
      | Ant (".." ,s) %{ mk_ant ~c:"flag"  s}
      |  %{`Negative _loc}   ]

      (*opt_rec@inline *)
      opt_rec:
      [ "rec" %{ `Positive _loc}
      | Ant ("rec" ,s) %{ mk_ant ~c:"flag"  s}
      |  %{`Negative _loc}]
      a_lident:
      [ Ant(""|"lid" ,s) %{ mk_ant  ~c:"a_lident"  s}
      | Lid s  %{ `Lid (_loc, s)} ]
      a_uident:
      [ Ant(""|"uid" ,s) %{ mk_ant  ~c:"a_uident"  s}
      | Uid s  %{ `Uid (_loc, s)} ]
      string_list:
      [ Ant("",s) %{ mk_ant  ~c:"str_list" s}
      | Ant("",s) ; S as xs %{ `App(_loc,mk_ant ~c:"" s, xs)}
      | Str  x %{ `Str(_loc,x)}
      | Str x; S as xs %{ `App(_loc,`Str(_loc,x),xs)}]
      rec_flag_quot:  [ opt_rec as x %{x} ]
      direction_flag_quot:  [ flag as x %{x} ] 
      mutable_flag_quot: [  opt_mutable as x %{x} ] 
      private_flag_quot: [  opt_private as x %{x} ]
      virtual_flag_quot: [  opt_virtual as x %{x} ] 
      row_var_flag_quot: [  opt_dot_dot as x %{x} ] 
      override_flag_quot:[  opt_override as x %{x} ] 
      pat_eoi:  [ pat as x; EOI  %{x} ] 
      exp_eoi:  [ exp as x; EOI %{x} ]  };
  with stru
    %extend{
    (** ml file  entrance *)    
      implem:
      [
        DirQuotation x  %{ (* FIXME (a,b,c) pattern broken *)
          begin
            Fdir.handle_quot x;
            ([], Some _loc)
          end}
      | stru as si; ";;"; S as rest %{ let (sil, stopped) = rest in (si :: sil, stopped)}
      | stru as si;  S as rest %{ let (sil, stopped) = rest in (si :: sil, stopped)}
         (* FIXME merge with the above in the future*)            
      | EOI %{ ([], None)} ]
      (** entrance for toplevel *)
      top_phrase:
      [ "#"; a_lident as n; exp as dp; ";;" %{ Some (`Directive(_loc,n,dp))}
      | "#"; a_lident as n; ";;" %{ Some (`DirectiveSimple(_loc,n))}
      | stru as st; ";;" %{ Some st}
      | EOI %{ None} ]
      (* used by [struct .... end]
         constains at least one element *)
      strus: (* FIXME dump seems to be incorrect *)
      [ Ant (""|"stri" ,s) %{ mk_ant ~c:"stru" s}
      | Ant (""|"stri" ,s) ;";;" %{ mk_ant ~c:"stru" s          }
      | Ant (""|"stri" ,s);  S as st %{ `Sem (_loc, mk_ant ~c:"stru" s, st)}
      | Ant (""|"stri" ,s); ";;"; S as st %{ `Sem (_loc, mk_ant  ~c:"stru" s, st)}
      | stru as st %{ st}
      | stru as st;";;" %{ st}
      | stru as st;";;"; S as xs %{ `Sem(_loc,st,xs)}
      | stru as st; S as xs %{`Sem(_loc,st,xs)}]


      stru_quot:
      [ "#"; a_lident as n; exp as dp %{ `Directive(_loc,n,dp)}
      | "#"; a_lident as n %{ `DirectiveSimple(_loc,n)}
      | strus as x  %{x}]

      stru:
      { "top"
        ["include"; mexp as me %{ `Include(_loc,me)}
        | "module"; a_uident as i; mbind0 as mb %{ `Module(_loc,i,mb)}
        | "module"; "rec"; mbind as mb %{ `RecModule(_loc,mb)}
        | @stru_sigi
        | "type"; type_declaration as t;"with"; "("; string_list as ns;")"
            %{`TypeWith (_loc,t,ns)}
        | @let_stru_exp %{fun x -> %stru{$exp:x}}
        | "let"; opt_rec as r; bind as bi %{
          match bi with
          | `Bind(_loc,`Any _,e) -> `StExp(_loc,e)
          | _ -> `Value(_loc,r,bi)}
        | "class"; class_declaration as cd %{  `Class(_loc,cd)}
        | Ant (""|"stri" ,s) %{mk_ant ~c:"stru" s}
        | Quot x %{ Ast_quotation.expand  x Dyn_tag.stru}
        | exp as e %{ `StExp(_loc,e)}
              (* this entry makes %{ let $rec:r $bi in $x } parsable *)
        ] }   };


    %extend{
      clsigi_quot:
      [ clsigi as x1; ";"; S as x2 %{ `Sem(_loc,x1,x2)}
      | clsigi as x  %{x}]

      class_signature:
      [ Ant (""|"csg" ,s); ? ";" %{ mk_ant  ~c:"clsigi" s}
      | Ant (""|"csg" ,s); ? ";"; S as csg %{
          (`Sem (_loc, mk_ant ~c:"clsigi"  s, csg) : FAst.clsigi )}
      | clsigi as csg; ? ";" %{ csg}
      | clsigi as csg; ? ";"; S as xs %{ `Sem(_loc,csg,xs)}
      ]
      
      clsigi:
      [ Ant (""|"csg" ,s) %{ mk_ant ~c:"clsigi"  s}
      | Quot x %{ Ast_quotation.expand  x Dyn_tag.clsigi}
      | "inherit"; cltyp as cs %{ `SigInherit(_loc,cs)}
      | "val"; opt_mutable as mf; opt_virtual as mv;a_lident as l; ":"; ctyp as t %{
        (`CgVal (_loc, l, mf, mv, t) : FAst.clsigi )}
      | "method"; "virtual"; opt_private as pf; a_lident as l; ":";ctyp as t 
          %{(`VirMeth (_loc, l, pf, t) : FAst.clsigi )}
      | "method"; opt_private as pf; a_lident as l; ":";ctyp as t %{(`Method (_loc, l, pf, t) : FAst.clsigi )}
      | "constraint"; ctyp as t1; "="; ctyp as t2 %{ (`Eq (_loc, t1, t2) : FAst.clsigi )} ] };  

    %extend{
      class_structure:
       [ Ant (""|"cst" ,s); ? ";" %{ mk_ant  ~c:"clfield"  s}
       | Ant (""|"cst" ,s);? ";"; S as st %{ `Sem(_loc, mk_ant ~c:"clfield"  s,st)  }
       | clfield as st; ? ";" %{ st}
       | clfield as st; ? ";";S as xs %{ `Sem(_loc,st,xs)}
       ]


      
      clfield:
        [ Ant (""|"cst" ,s) %{ mk_ant ~c:"clfield"  s}
        | Quot x %{ Ast_quotation.expand  x Dyn_tag.clfield}
        | "inherit"; opt_override as o; clexp as ce %{`Inherit(_loc,o,ce)}
        | "inherit"; opt_override as o; clexp as ce; "as"; a_lident as i %{`InheritAs(_loc,o,ce,i)}
        | value_val_opt_override as o; opt_mutable as mf; a_lident as lab; cvalue_bind as e %{
          (`CrVal (_loc, lab, o, mf, e) : FAst.clfield )}
        | value_val_opt_override as o; "virtual"; opt_mutable as mf; a_lident as l; ":";
                ctyp as t %{
          match o with
          | `Negative _ -> (`VirVal (_loc, l, mf, t) : FAst.clfield )
          | _ -> raise (Streamf.Error "override (!) is incompatible with virtual")}                    
        | method_opt_override as o; "virtual"; opt_private as pf; a_lident as l; ":";
                  ctyp as t %{
          match o with
          | `Negative _ -> `VirMeth (_loc, l, pf, t)
          | _ -> raise (Streamf.Error "override (!) is incompatible with virtual")}  

       | method_opt_override as o; opt_private as pf; a_lident as l; ":"; ctyp as t ;
                fun_bind as e %{
                  `CrMth(_loc,l,o,pf,e,t)}
       | method_opt_override as o; opt_private as pf;a_lident as l; fun_bind as e %{
           `CrMthS(_loc,l,o,pf,e)}
             
       | "constraint"; ctyp as t1; "="; ctyp as t2 %{ `Eq(_loc,t1,t2)}
       | "initializer"; exp as se %{ `Initializer(_loc,se)} ]
      clfield_quot:
        [ clfield as x1; ";"; S as x2 %{ `Sem(_loc,x1,x2)}
        | clfield as x %{x}]
    };
    
  with clexp
    %extend{
      clexp_quot:
      [ clexp as x %{ x}]
      class_declaration:
      [ S as c1; "and"; S as c2 %{ `And(_loc,c1,c2)}
      | Ant (""|"cdcl" ,s) %{ mk_ant ~c:"clexp"  s}
      (* | `Quot x -> Ast_quotation.expand  x Dyn_tag.clexp *)
      | opt_virtual as mv;  a_lident as i; "["; comma_type_parameter as x; "]"; class_fun_bind as ce
        %{ `ClDecl(_loc,mv,(i:>ident),x,ce)}
      | opt_virtual as mv; a_lident as i; class_fun_bind as ce %{
          `ClDeclS(_loc,mv,(i:>ident),ce)}]
      class_fun_bind:
      [ "="; clexp as ce %{ ce}
      | ":"; cltyp_plus as ct; "="; clexp as ce %{`Constraint(_loc,ce,ct)}
      | ipat as p; S as cfb %{ `CeFun (_loc, p, cfb)}  ]
      class_fun_def:
      [ ipat as p; S as ce %{ `CeFun(_loc,p,ce)}
      | "->"; clexp as ce %{ ce} ]
      clexp:
      { "top"
          [ ("fun"|"function"); ipat as p; class_fun_def as ce %{  `CeFun (_loc, p, ce)}
          | "let"; opt_rec as rf; bind as bi; "in"; S as ce %{ `LetIn(_loc,rf,bi,ce)}]
        "apply" NA
          [ S as ce; exp Level "label" as e %{ `CeApp (_loc, ce, e)}]
        "simple"
          [ Ant (""|"cexp" ,s) %{ mk_ant ~c:"clexp"  s}
          | Quot x %{ Ast_quotation.expand  x Dyn_tag.clexp}
          | vid as ci; "["; comma_ctyp as t; "]" %{ `ClApply(_loc,ci,t)}
          | vid as ci %{ (ci :>clexp)}
          | "object"; "("; pat as p; ")" ; class_structure as cst;"end"
            %{ `ObjPat(_loc,p,cst)}
          | "object"; "("; pat as p; ")" ;"end" %{ `ObjPatEnd(_loc,p)}
          | "object" ; "("; pat as p;":";ctyp as t;")"; class_structure as cst;"end" %{
              `ObjPat(_loc,`Constraint(_loc,p,t),cst)}
          | "object" ; "("; pat as p;":";ctyp as t;")"; "end" %{
              `ObjPatEnd(_loc,`Constraint(_loc,p,t))}
          | "object" ; class_structure as cst;"end" %{ `Obj(_loc,cst)}
          | "object" ;"end" %{ `ObjEnd(_loc)}
          | "("; S as ce; ":"; cltyp as ct; ")" %{ `Constraint(_loc,ce,ct)}
          | "("; S as ce; ")" %{ce} ] } };
  with cltyp
    %extend{
      class_description:
      [ S as cd1; "and"; S as cd2 %{ `And(_loc,cd1,cd2)}
      | Ant (""|"typ" ,s) %{ mk_ant ~c:"cltyp"  s}
      (* | `Quot x -> Ast_quotation.expand  x Dyn_tag.cltyp *)
      | opt_virtual as mv;  a_lident as i;"[";
          comma_type_parameter as x; "]"; ":"; cltyp_plus as ct %{
            `CtDecl(_loc,mv,(i:>ident),x,ct)}
      | opt_virtual as mv; a_lident as i ; ":"; cltyp_plus as ct %{
          `CtDeclS(_loc,mv,(i:>ident),ct)}]
      cltyp_declaration:
      [ S as cd1; "and"; S as cd2 %{ `And(_loc,cd1,cd2)}
      | Ant (""|"typ" ,s) %{ mk_ant ~c:"cltyp"  s}
      (* | `Quot x -> Ast_quotation.expand  x Dyn_tag.cltyp *)

      | opt_virtual as mv;  a_lident as i;"["; comma_type_parameter as x; "]"
        ; "="; cltyp as ct %{ `CtDecl(_loc,mv,(i:>ident),x,ct)}
      | opt_virtual as mv; a_lident as i; "="; cltyp as ct %{
          `CtDeclS(_loc,mv,(i:>ident),ct)}]
      cltyp_quot:
      [cltyp as x %{x}]
      cltyp_plus:
      [ "["; ctyp as t; "]"; "->"; S as ct %{ `CtFun(_loc,t,ct)}
      | cltyp as ct %{ ct} ]
      cltyp:
      [ Ant (""|"ctyp" ,s) %{ mk_ant  ~c:"cltyp"  s}
      | Quot x %{ Ast_quotation.expand  x Dyn_tag.cltyp}
      | vid as i; "["; comma_ctyp as t; "]" %{ `ClApply(_loc,i,t)}
      | vid as i %{ (i :> cltyp) }
      | "object" ; "(";ctyp as t;")";class_signature as csg;"end" %{ `ObjTy(_loc,t,csg)}
      | "object" ; class_signature as csg;"end" %{ `Obj(_loc,csg)}
      | "object" ; "(";ctyp as t;")" %{ `ObjTyEnd(_loc,t)}
      | "object"; "end" %{`ObjEnd(_loc)}] } ;
end;;


let apply_ctyp () = begin
  with ctyp
    %extend{
      ctyp_quot:
      [ctyp as x; "*"; star_ctyp as y %{ `Sta (_loc, x, y)}
      |ctyp as x %{x} ]
      unquoted_typevars:
      [ S as t1; S as t2 %{ `App(_loc,t1,t2)(* %{ $t1 $t2 } *) (* FIXME order matters ?*)}
      | Ant (""|"typ" ,s) %{  mk_ant ~c:"ctyp"  s}
      | Quot x %{ Ast_quotation.expand  x Dyn_tag.ctyp}
      | a_lident as i %{ (i:>ctyp)} ]
      type_parameter:
      [ Ant (""|"typ" ,s) %{ mk_ant s}
      (* | `Quot x -> Ast_quotation.expand  x Dyn_tag.ctyp *)
      | "'"; a_lident as i %{ `Quote(_loc,`Normal _loc, i)}
      | ("+"|"-" as p); "'"; a_lident as i %{
        (* FIXME support ?("+"|"-" as p) in the future*)
        `Quote (_loc, if p = "+" then `Positive _loc else `Negative _loc,  i)
      }
      | ("+"|"-" as p); "_" %{ `QuoteAny (_loc, if p = "+" then `Positive _loc else `Negative _loc)}
      | "_" %{  `Any _loc}]
      type_longident_and_parameters:
      [ "("; type_parameters as tpl; ")";type_longident as i %{ tpl (i:>ctyp) }
      | type_parameter as tpl ; type_longident as i %{ `App(_loc, (i:>ctyp),(tpl:>ctyp))}
      | type_longident as i %{ (i:>ctyp)}
      | Ant ("" ,s) %{mk_ant s ~c:"ctyp"}]
      type_parameters:
      [ type_parameter as t1; S as t2 %{ fun acc -> t2 (`App(_loc,acc, (t1:>ctyp)))}
      | type_parameter as t %{ fun acc -> `App(_loc,acc, (t:>ctyp))}
      |  %{fun t -> t}  ]
      meth_list:
      [ meth_decl as m; ";"; S as rest   %{ let (ml, v) = rest in (`Sem(_loc,m,ml), v)}
      | meth_decl as m; ?";"; opt_dot_dot as v %{ (m, v)}]
      meth_decl:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:"ctyp"  s}
      (* | `Quot x                       -> AstQuotation.expand _loc x Dyn_tag.ctyp *)
      | a_lident as lab; ":"; ctyp as t  %{`TyCol(_loc,lab,t)}]
      opt_meth_list:
      [ meth_list as rest  %{let (ml, v) = rest in `TyObj (_loc, ml, v)}
      | opt_dot_dot as v     %{ `TyObjEnd(_loc,v)} ]
      row_field:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:"ctyp"  s}
      | Ant("vrn" , s) %{ `TyVrn(_loc,mk_ant ~c:"ctyp"  s)} (* FIXME*)
      | Ant("vrn" , s) ; "of"; ctyp as t %{`TyVrnOf(_loc,mk_ant ~c:"ctyp"  s,t)}
      | S as t1; "|"; S as t2 %{ `Bar(_loc,t1,t2)}
      | "`"; astr as i %{  `TyVrn(_loc,i)}
      | "`"; astr as i; "of";ctyp as t %{ `TyVrnOf(_loc,i,t)}
      | ctyp as t %{ `Ctyp(_loc,t)}
      (* | "`"; astr as i; "of"; "&"; amp_ctyp as t -> *)
      (*     `TyOfAmp (_loc, (`TyVrn (_loc, i)), t) *)
          (* %{ `$i of & $t } *)]
      (* only used in row_field *)
      (* amp_ctyp: *)
      (* [ S as t1; "&"; S as t2 -> `Amp(_loc,t1,t2) *)
      (* | ctyp as t -> t ] *)

      (* only used in ctyps *)
      name_tags:
      [ Ant (""|"typ" ,s) %{  mk_ant ~c:"ctyp"  s}
      | S as t1; S as t2 %{ `App (_loc, t1, t2)}
      | "`"; astr as i %{ `TyVrn (_loc, i)}  ]
      type_declaration:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:"ctyp"  s}
      (* | `Quot x -> AstQuotation.expand _loc x Dyn_tag.ctyp *)
      | S as t1; "and"; S as t2 %{  `And(_loc,t1,t2)}
      |  type_ident_and_parameters as rest; "="; type_info as tk; L0 constrain as cl
        %{ let (n, tpl) = rest in
        `TyDcl (_loc, n, tpl, tk,
                match cl with
                |[]-> `None _loc
                | _ -> `Some(_loc,and_of_list cl))}
      | type_ident_and_parameters as rest; L0 constrain as cl %{
        let (n,tpl) = rest in
        `TyAbstr(_loc,n,tpl,
                 match cl with
                 |[] -> `None _loc
                 | _ -> `Some(_loc, and_of_list cl))}]
      type_info:
      [ type_repr as t2 %{ `TyRepr(_loc,`Negative _loc,t2)}
      | ctyp as t1; "="; type_repr as t2 %{ `TyMan(_loc, t1, `Negative _loc, t2)}
      | ctyp as t1 %{ `TyEq(_loc,`Negative _loc, t1)}
      | "private"; ctyp as t1 %{ `TyEq(_loc,`Positive _loc,t1)}
      |  ctyp as t1; "=";"private"; type_repr as t2 %{ `TyMan(_loc, t1, `Positive _loc,t2)}
      | "private"; type_repr as t2 %{ `TyRepr(_loc,`Positive _loc, t2)}
      ]

      type_repr:
      [ "|"; constructor_declarations as t %{ `Sum(_loc,t)}
      | "{"; label_declaration_list as t; "}" %{ `Record (_loc, t)}]
      type_ident_and_parameters:
      [ "(";  L1 type_parameter SEP "," as tpl; ")"; a_lident as i %{
        (i, `Some(_loc, com_of_list (tpl :>  decl_params list)))}
      |  type_parameter as t;  a_lident as i %{ (i, `Some (_loc,(t:>decl_params)))}
      |  a_lident as i %{ (i, `None _loc)}]
      constrain:
      [ "constraint"; ctyp as t1; "="; ctyp as t2 %{ `Eq(_loc,t1, t2)} ]
      typevars:
      [ S as t1; S as t2 %{ `App(_loc,t1,t2)}
      | Ant (""|"typ" ,s) %{  mk_ant  ~c:"ctyp"  s}
      | Quot x %{ Ast_quotation.expand  x Dyn_tag.ctyp}
      | "'"; a_lident as i  %{ `Quote (_loc, `Normal _loc, i)}]
      ctyp:
      {
       "alias" LA
        [ S as t1; "as"; "'"; a_lident as i %{`Alias(_loc,t1,i)}]
       "forall" LA
        [ "!"; typevars as t1; "."; ctyp as t2 %{ `TyPol (_loc, t1, t2)} ]
       "arrow" RA
        [ S as t1; "->"; S as t2 %{  `Arrow(_loc,t1,t2)} ]
       "label" NA
        [ "~"; a_lident as i; ":"; S as t %{ `Label (_loc, i, t)}
        | Label s ; ":"; S as t %{ `Label (_loc, (`Lid (_loc, s)), t)} (* FIXME *)
        | Optlabel s ; S as t %{ `OptLabl(_loc,`Lid(_loc,s),t)}
        | "?"; a_lident as i; ":"; S as t %{ `OptLabl(_loc,i,t)}]
         
       "apply" LA
        [ S as t1; S as t2 %{`App (_loc,t2,t1)} ]

       (* [mod_ext_longident] and [type_longident]
          | type_longident
          | simple_core_type2 type_longident
          | LPAREN core_type_comma_list RPAREN type_longident *)  
       (* "." LA *)
       (*  [ S as t1; "."; S as t2 -> *)
       (*      try ( *)
       (*        prerr_endline "used"; *)
       (*        `Dot (_loc, ident_of_ctyp t1, ident_of_ctyp t2) *)
       (*          ) (\* FIXME*\) *)
       (*      with Invalid_argument s -> raise (Streamf.Error s) ] *)
       "simple"
        [ "'"; a_lident as i %{  `Quote (_loc, `Normal _loc,  i)}
        | "_" %{ `Any _loc}
        | Ant (""|"typ"|"par"|"id" ,s) %{ mk_ant ~c:"ctyp"  s}
        | Ant ("id" ,s); "."; S as t %{
            let try id = ident_of_ctyp t  in
              (`Dot(_loc,mk_ant ~c:"ident"  s,id) :ctyp)
            with Invalid_argument s -> raise (Streamf.Error s)}
        | Quot x %{ Ast_quotation.expand  x Dyn_tag.ctyp}
        | a_uident as i; "."; S as t %{
            let try id = ident_of_ctyp t in
              `Dot(_loc,(i:>ident),id)
            with Invalid_argument s -> raise (Streamf.Error s)}
        | a_lident as i %{  (i :> ctyp)}
        | "("; S as t; "*"; star_ctyp as tl; ")" %{
            `Par (_loc, `Sta (_loc, t, tl))}
        | "("; S as t; ")" %{ t}
        | "("; S as t; ","; com_ctyp as tl; ")" ; type_longident as j %{
            appl_of_list  ((j:>ctyp):: t:: Ast_basic.list_of_com tl [])}
        | "["; row_field as rfl; "]" %{ `PolyEq(_loc,rfl)}
        (* | "[>"; "]" -> `PolySup (_loc, (`Nil _loc)) *) (* FIXME add later*)
        | "[>"; row_field as rfl; "]" %{   `PolySup (_loc, rfl)}
        | "[<"; row_field as rfl; "]" %{ `PolyInf(_loc,rfl)}
        | "[<"; row_field as rfl; ">"; name_tags as ntl; "]" %{ `PolyInfSup(_loc,rfl,ntl)}
        | "#"; class_longident as i %{  `ClassPath (_loc, i)}
        | "<"; opt_meth_list as t; ">" %{ t}
        | "("; "module"; mtyp as p; ")" %{ `Package(_loc,p)}
        ] }
      comma_ctyp: (* DUPLICATED removed later *)
      [ S as t1; ","; S as t2 %{ `Com (_loc, t1, t2) }
      | Ant ( "" ,s) %{ mk_ant ~c:"ctyp,"  s}
      | ctyp as t %{ `Ctyp(_loc,t)}
      ]
      com_ctyp:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:"ctyp"  s}
      | S as t1; ","; S as t2 %{ `Com(_loc,t1,t2)}
      | ctyp as t %{ t}
      ]
      star_ctyp:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:"ctyp"  s}
      | S as t1; "*"; S as t2 %{ `Sta(_loc,t1,t2)}
      | ctyp as t %{ t}
      ]
      constructor_declarations:
      [ Ant (""|"typ" ,s) %{ mk_ant  ~c:"ctyp"  s}
      (* | `Quot x -> Ast_quotation.expand  x Dyn_tag.ctyp *)
      | S as t1; "|"; S as t2 %{    `Bar(_loc,t1,t2)}
      | a_uident as s; "of"; constructor_arg_list as t %{ `Of(_loc,s,t)}
      | a_uident as s; ":"; ctyp as t %{ (* GADT  *) `TyCol(_loc,s,t)}
      | a_uident as s %{ (s :> or_ctyp)}
      ]
      constructor_declaration:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:"ctyp"  s}
      (* | `Quot x -> Ast_quotation.expand  x Dyn_tag.ctyp *)
      | a_uident as s; "of"; constructor_arg_list as t %{ `Of(_loc,(s:>vid),t)}
      | a_uident as s %{ (s:>of_ctyp)}
      ]
      constructor_arg_list:
      [ S as t1; "*"; S as t2 %{ `Sta(_loc,t1,t2)}
      | ctyp as t %{ t}
      ]
      label_declaration_list:
      [ label_declaration as t1; ";"; S as t2 %{ `Sem(_loc,t1,t2)}
      | label_declaration as t1; ? ";"            %{ t1}
      ]
  
      label_declaration:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:"ctyp"  s}
      (* | `Quot x -> Ast_quotation.expand  x Dyn_tag.ctyp *)
      | a_lident as s; ":"; ctyp as t %{ `TyCol(_loc,s,t)}
      | "mutable"; a_lident as s; ":";  ctyp as t %{ `TyColMut(_loc,s,t)}
      ]
      comma_type_parameter:
      [ S as t1; ","; S as t2 %{  `Com (_loc, t1, t2)}
      | type_parameter as t %{ `Ctyp(_loc, (t:>ctyp))}
      ]
  };
end;;

let fill_parsers =
  let  applied = ref false in
  fun () ->
    if not !applied then
      begin
        applied := true; 
        apply ();
        apply_ctyp ();
      end ;;
        
let () = 
Ast_parsers.register_parser
    ("revise", fill_parsers);;










(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_fan.cmo" *)
(* end: *)
