open FAst
open AstLib
open FanOps
open Fsyntax
open LibUtil
open FanUtil
open Gramlib

{:create|Fgram pos_exps|};;
let symbolchars =
  ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
   '@'; '^'; '|'; '~'; '\\']
    
let symbolchar s i =
  let len = String.length s in
  try
    (for j = i to len - 1 do
      if not (List.mem s.[j] symbolchars) then
        raise Not_found
    done; true)
  with  Not_found -> false
      
let apply () = begin 
  let list = ['!'; '?'; '~'] in
  let excl = ["!="; "??"] in
  let () = setup_op_parser prefixop
      (fun x -> not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list && symbolchar x 1) in
  let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
  let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
  let excl = ["<-"; "||"; "&&"] in
  let () = setup_op_parser infixop2
      (fun x -> (List.mem x list_ok) ||
      (not (List.mem x excl) && String.length x >= 2 &&
       List.mem x.[0] list_first_char_ok && symbolchar x 1)) in
  let list = ['@'; '^'] in
  let () = setup_op_parser infixop3
      (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1) in
  let list = ['+'; '-'] in
  let ()  = setup_op_parser infixop4
    (fun x -> x <> "->" && String.length x >= 1 && List.mem x.[0] list &&
      symbolchar x 1) in
  let list = ['*'; '/'; '%'; '\\'] in
  let () = setup_op_parser infixop5
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              (x.[0] <> '*' || String.length x < 2 || x.[1] <> '*') &&
              symbolchar x 1) in
  let () = setup_op_parser infixop6
    (fun x -> String.length x >= 2 && x.[0] == '*' && x.[1] == '*' &&
              symbolchar x 2) in
  let () = FanTokenFilter.define_filter (Fgram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm)) in
  Fgram.setup_parser sem_exp begin
    let symb1 = Fgram.parse_origin_tokens exp in
    let symb = parser
      |  (`Ant (("list" as n), s), _loc)  ->
          mk_anti ~c:"exp;" _loc n s
      |  a = symb1  -> a  in
    let rec kont al = parser
      |  (`KEYWORD ";", _); a = symb; 's  ->
          let _loc =  al <+> a  in
          kont {:exp| $al; $a |} s
      |  -> al  in
    parser |  a = symb; 's  -> kont a s
  end;

  (* with mexp *)
  {:extend|
      mexp_quot:
      [ mexp{x} -> x]
      mbind0:
      { RA
        [ "("; a_uident{m}; ":"; mtyp{mt}; ")"; S{mb} -> `Functor (_loc, m, mt, mb)
        | ":"; mtyp{mt}; "="; mexp{me} ->  `Constraint (_loc, me, mt)
        | "="; mexp{me} -> me  ] }
      mexp:
      { "top"
        [ "functor"; "("; a_uident{i}; ":"; mtyp{t}; ")"; "->"; S{me} ->
             `Functor (_loc, i, t, me)
        | "struct"; strus{st}; "end" -> `Struct(_loc,st)
        | "struct"; "end" -> `StructEnd(_loc)]
       "apply"
        [ S{me1}; S{me2} -> `App (_loc, me1, me2) ]
       "simple"
        [ `Ant ((""|"mexp" as n),s) ->  mk_anti ~c:"mexp" _loc n s
        | `QUOTATION x ->
            AstQuotation.expand _loc x FDyn.mexp_tag
        | module_longident{i} ->  (i:>mexp)
        | "("; S{me}; ":"; mtyp{mt}; ")" ->  `Constraint (_loc, me, mt)
        | "("; S{me}; ")" ->  me
        | "("; "val"; exp{e}; ")" -> `PackageModule (_loc, e)
        | "("; "val"; exp{e}; ":"; mtyp{p}; ")" ->
             `PackageModule (_loc, `Constraint (_loc, e, `Package (_loc, p)))] } |};

  with mbind
      {:extend|
        mbind_quot:
        [ S{b1}; "and"; S{b2} ->  `And(_loc,b1,b2)
        | `Ant (("mbind"|"" as n),s) ->
            mk_anti _loc ~c:"mbind" n s
        | a_uident{m}; ":"; mtyp{mt} -> `Constraint(_loc,m,mt)
        | a_uident{m}; ":"; mtyp{mt}; "="; mexp{me} -> `ModuleBind(_loc,m,mt,me)]
        mbind:
        [ S{b1}; "and"; S{b2} -> `And(_loc,b1,b2)
        | `Ant (("mbind" |"" as n),s) ->
            mk_anti _loc ~c:"mbind" n s
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.mbind_tag
        | a_uident{m}; ":"; mtyp{mt}; "="; mexp{me} ->
            `ModuleBind (_loc, m, mt, me)]
        module_rec_declaration:
        [ S{m1}; "and"; S{m2} -> `And(_loc,m1,m2)
        | `Ant ((""|"mbind" as n),s) ->
            mk_anti _loc ~c:"mbind" n s
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.mbind_tag
        | a_uident{m}; ":"; mtyp{mt} -> `Constraint(_loc,m,mt) ] |};
  (* with constr *)
      {:extend|
        constr_quot:
        [ constr{x} -> x   ]
        constr: 
        [ S{wc1}; "and"; S{wc2} -> `And(_loc,wc1,wc2)
        | `Ant ((""|"constr" as n),s) ->
            mk_anti _loc ~c:"constr" n s
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.constr_tag
        | "type"; type_longident_and_parameters{t1}; "="; ctyp{t2} ->
            `TypeEq (_loc, t1, t2)
        | "type"; type_longident_and_parameters{t1}; "="; "private"; ctyp{t2} ->
            `TypeEqPriv(_loc,t1,t2)
        | "type"; type_longident_and_parameters{t1}; ":="; ctyp{t2} ->
            `TypeSubst (_loc, t1, t2)
        | "module"; module_longident{i1}; "="; module_longident_with_app{i2} ->
            `ModuleEq (_loc, (i1:vid :> ident) , i2)
        | "module"; module_longident{i1}; ":="; module_longident_with_app{i2} ->
            `ModuleSubst (_loc, (i1: vid :> ident), i2)] |};




    {:extend|
      sigis:
      [ `Ant ((""|"sigi" as n),s) -> mk_anti _loc  n ~c:"sigi" s

      | `Ant ((""|"sigi" as n),s); ";;"; S{sg} ->
          `Sem (_loc,  mk_anti _loc  n ~c:"sigi" s, sg)
      | `Ant ((""|"sigi" as n),s);  S{sg} ->
          `Sem (_loc,  mk_anti _loc  n ~c:"sigi" s, sg)
            
      | sigi{sg};";;" ; S{s} -> `Sem(_loc,sg,s)
      | sigi{sg};";;" ->sg            
      | sigi{sg}; S{s} -> `Sem(_loc,sg,s)
      | sigi{sg} ->sg ]
      mtyp:
      { "top"
        [ "functor"; "("; a_uident{i}; ":"; S{t}; ")"; "->"; S{mt} ->
          `Functor(_loc,i,t,mt)]
        "with"
        [ S{mt}; "with"; constr{wc} -> `With(_loc,mt,wc)]
        "apply"
        [ S{mt1}; S{mt2} ->
            match (mt1, mt2) with
            | ((#ident as i1), (#ident as i2)) -> apply i1 i2 
            | _ -> raise XStream.Failure ] (* FIXME *)
        "."
        [ S{mt1}; "."; S{mt2} ->
          let acc0 mt1 mt2 =
            match (mt1, mt2) with
            | ((#ident as i1), (#ident as i2)) ->
              dot i1 i2 
            | _ -> raise XStream.Failure  in
          acc0 mt1 mt2 ] (*FIXME*)
        "sig"
        [ "sig"; sigis{sg}; "end" -> `Sig(_loc,sg)
        | "sig";"end" -> `SigEnd(_loc)]
       "simple"
        [ `Ant ((""|"mtyp" as n),s) ->
          mk_anti _loc ~c:"mtyp" n s
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.mtyp_tag
        | module_longident_with_app{i} ->  (i:ident:>mtyp) 
        | "("; S{mt}; ")" -> mt
        | "module"; "type"; "of"; mexp{me} -> `ModuleTypeOf(_loc,me)] }
      module_declaration: (* syntax sugar *)
      [ ":"; mtyp{mt} -> mt
      | "("; a_uident{i}; ":"; mtyp{t}; ")"; S{mt} ->
          `Functor(_loc,i,t,mt)]
      mtyp_quot:
      [ mtyp{x} -> x  ]  |};

  with sigi
  {:extend|
    sigi_quot:
    [ "#"; a_lident{s} -> `DirectiveSimple(_loc,s)
    | "#"; a_lident{s}; exp{dp} -> `Directive(_loc,s,dp)
    | sigi{sg1}; ";"; S{sg2} -> `Sem(_loc,sg1,sg2)
    | sigi{sg} -> sg] 
    sigi:
    [ `Ant ((""|"sigi" as n),s) ->  mk_anti _loc ~c:"sigi" n s
    | `QUOTATION x -> AstQuotation.expand _loc x FDyn.sigi_tag
    | "exception"; constructor_declaration{t} ->  {| exception $t |}
    | "external"; a_lident{i};":";ctyp{t};"=" ;string_list{sl} ->
        `External (_loc, i, t, sl)
    | "include"; mtyp{mt} -> `Include(_loc,mt)
    | "module"; a_uident{i}; module_declaration{mt} -> `Module(_loc,i,mt)
    | "module"; "rec"; module_rec_declaration{mb} ->  `RecModule (_loc, mb)
    | "module"; "type"; a_uident{i}; "="; mtyp{mt} -> `ModuleType(_loc,i,mt)
    | "module"; "type"; a_uident{i} -> `ModuleTypeEnd(_loc,i)
    | "open"; module_longident{i} -> `Open(_loc,(i: vid :> ident))
    | "type"; type_declaration{t} -> `Type(_loc,t)

    | "val"; a_lident{i}; ":"; ctyp{t} -> `Val(_loc,i,t)
    | "class"; class_description{cd} ->    `Class(_loc,cd)
    | "class"; "type"; cltyp_declaration{ctd} ->  `ClassType(_loc,ctd) ]
    (* mli entrance *)    
    interf:
    [(*  "#"; a_lident{n};  ";;" -> *)
    (*   ([ `DirectiveSimple(_loc,n) ],  Some _loc) *)
    (* | "#"; a_lident{n}; exp{dp}; ";;" -> ([ `Directive(_loc,n,dp)], Some _loc)  *)
     sigi{si}; ";;";  S{(sil, stopped)} -> (si :: sil, stopped)
    | sigi{si}; S{(sil,stopped)} -> (si :: sil, stopped)
    | `EOI -> ([], None) ]
 |};

    with exp
    {:extend|
      exp_quot:
      [ exp{e1}; ","; comma_exp{e2} -> `Com(_loc,e1,e2)
      | exp{e1}; ";"; sem_exp{e2} -> `Sem(_loc,e1,e2)
      | exp{e} -> e]
       (* {:stru|
       let f (type t) () =
          let module M = struct exception E of t ; end in
          ((fun x -> M.E x), (function [M.E x -> Some x | _ -> None]))|}
       {:stru| let f : ! 'a . 'a -> 'a = fun x -> x |} *)
      cvalue_bind:
      [ "="; exp{e} -> e
      | ":"; "type"; unquoted_typevars{t1}; "." ; ctyp{t2} ; "="; exp{e} -> 
          let u = {:ctyp| ! $t1 . $t2 |} in  {| ($e : $u) |}
      | ":"; ctyp{t}; "="; exp{e} -> {| ($e : $t) |}
      | ":"; ctyp{t}; ":>"; ctyp{t2}; "="; exp{e} ->
          (match t with
          | {:ctyp| ! $_ . $_ |} ->
              raise (XStream.Error "unexpected polytype here")
          | _ -> {| ($e : $t :> $t2) |} )
      | ":>"; ctyp{t}; "="; exp{e} ->`Subtype(_loc,e,t) ]
      fun_bind:
      { RA
          [ "("; "type"; a_lident{i}; ")"; S{e} ->
            `LocalTypeFun(_loc,i,e)
          | ipat{p}; S{e} -> `Fun(_loc,`Case(_loc,p,e))
          | cvalue_bind{bi} -> bi  ] }
       lang:
       [ dot_lstrings{ls} -> 
         let old = !AstQuotation.default in (
         AstQuotation.default := FToken.resolve_name _loc ls;
         old)]
       pos_exps:
       [ L1 name_space SEP ";"{xys} -> 
                    let old = !AstQuotation.map in
                    (AstQuotation.map := SMap.add_list xys old;
                     old)]
      let name_space:
       [ `Lid x;":";dot_lstrings{y} ->
             ((x:string), FToken.resolve_name _loc y)
           | `Lid x ->
               ((x:string), FToken.resolve_name _loc
                  (`Sub [], x) ) ]  
       let fun_def_pat:
       ["(";"type";a_lident{i};")" ->
         fun e ->  `LocalTypeFun (_loc, i, e)
       | ipat{p} -> fun e -> `Fun(_loc,`Case(_loc,p,e))(* {| fun $p -> $e |} *)
       | ipat{p}; "when"; exp{w} ->
           fun e -> `Fun(_loc,`CaseWhen(_loc,p,w,e)) ]
       fun_def:
       {RA
          [ fun_def_pat{f}; "->"; exp{e} ->  f e
          | fun_def_pat{f}; S{e} -> f e] }    
       exp:
       {
        "top" RA
        [ "let"; opt_rec{r}; bind{bi}; "in"; S{x} ->
          `LetIn(_loc,r,bi,x)
        | "let"; "module"; a_uident{m}; mbind0{mb}; "in"; S{e} ->
            `LetModule (_loc, m, mb, e)
        | "let"; "open"; module_longident{i}; "in"; S{e} ->
            `LetOpen (_loc, (i:vid :> ident), e)
        | "let"; "try"; opt_rec{r}; bind{bi}; "in"; S{x}; "with"; case{a} ->
              `LetTryInWith(_loc,r,bi,x,a)
        | "match"; S{e}; "with"; case{a} -> `Match (_loc, e, a)
        | "try"; S{e}; "with"; case{a} -> `Try (_loc, e, a)
        | "if"; S{e1}; "then"; S{e2}; "else"; S{e3} ->
            `IfThenElse (_loc, e1, e2, e3)
        | "if"; S{e1}; "then"; S{e2} -> `IfThen (_loc, e1, e2)
        | "do"; sequence{seq}; "done" -> `Seq(_loc,seq)
        | "with"; lang{old}; S{x} -> begin  AstQuotation.default := old; x  end
        | "with";"{"; pos_exps{old} ;"}"; S{x} -> begin AstQuotation.map := old; x end
        | "for"; a_lident{i}; "="; S{e1}; flag{df}; S{e2}; "do";
            sequence{seq}; "done" ->
              `For (_loc, i, e1, e2, df, seq)
        | "while"; S{e}; "do"; sequence{seq}; "done" ->
            `While (_loc, e, seq)]  
       ":=" NA
        [ S{e1}; ":="; S{e2} ->
          (* (`Assign (_loc,`Field(_loc,e1,`Lid(_loc,"contents")),e2):exp) *)
          {| $e1 := $e2 |}
        | S{e1}; "<-"; S{e2} -> (* FIXME should be deleted in original syntax later? *)
            match FanOps.bigarray_set _loc e1 e2 with
            | Some e -> e
            | None -> `Assign(_loc,e1,e2)  ]
       "||" RA
        [ S{e1}; infixop0{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "&&" RA
        [ S{e1}; infixop1{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "<" LA
        [ S{e1}; infixop2{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "^" RA
        [ S{e1}; infixop3{op}; S{e2} -> {| $op $e1 $e2 |} ]
        "::" RA
        [ S{e1}; "::"; S{e2} -> {|  $e1 :: $e2  |} ]  
       "+" LA
        [ S{e1}; infixop4{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "*" LA
        [ S{e1}; "land"; S{e2} -> {| $e1 land $e2 |}
        | S{e1}; "lor"; S{e2} -> {| $e1 lor $e2 |}
        | S{e1}; "lxor"; S{e2} -> {| $e1 lxor $e2 |}
        | S{e1}; "mod"; S{e2} -> {| $e1 mod $e2 |}
        | S{e1}; infixop5{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "**" RA
        [ S{e1}; "asr"; S{e2} -> {| $e1 asr $e2 |}
        | S{e1}; "lsl"; S{e2} -> {| $e1 lsl $e2 |}
        | S{e1}; "lsr"; S{e2} -> {| $e1 lsr $e2 |}
        | S{e1}; infixop6{op}; S{e2} -> {| $op $e1 $e2 |} ]
          
       "obj" RA
        ["fun"; "|";  L1 case0 SEP "|"{a}  ->
           let cases = bar_of_list a in `Fun (_loc,cases)
        | "function"; "|"; L1 case0 SEP "|"{a} ->
            let cases = bar_of_list a in `Fun(_loc,cases)
        | "fun"; fun_def{e} -> e
        | "function"; fun_def{e} -> e
        | "object"; "(";pat{p}; ")"; class_structure{cst};"end" -> `ObjPat(_loc,p,cst)
        | "object"; "(";pat{p}; ")"; "end" -> `ObjPatEnd(_loc,p)
        | "object"; "(";pat{p};":";ctyp{t};")";class_structure{cst};"end" ->
            `ObjPat(_loc,`Constraint(_loc,p,t),cst)
        | "object"; "(";pat{p};":";ctyp{t};")";"end" ->
            `ObjPatEnd(_loc,`Constraint(_loc,p,t))
        | "object"; class_structure{cst};"end"-> `Obj(_loc,cst)
        | "object";"end" -> `ObjEnd(_loc)]
       "unary minus" NA
        [ "-"; S{e} -> FanOps.mkumin _loc "-" e (* Delayed into Dump *)
        | "-."; S{e} -> FanOps.mkumin _loc "-." e ]
       "apply" LA
        [ S{e1}; S{e2} -> `App(_loc,e1,e2)
        | "assert"; S{e} -> `Assert(_loc,e)
            (* FanOps.mkassert _loc e *)
        | "new"; class_longident{i} -> `New (_loc,i) (* {| new $i |} *)
        | "lazy"; S{e} -> `Lazy(_loc,e) ]
       "label" NA
        [ "~"; a_lident{i}; ":"; S{e} -> `Label (_loc, i, e)
        | "~"; a_lident{i} -> `LabelS(_loc,i)
        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | `LABEL i; S{e} -> {| ~ $lid:i : $e |}
        (* Same remark for ?a:b *)
        | `OPTLABEL i; S{e} ->  `OptLabl(_loc,`Lid(_loc,i),e)
        | "?"; a_lident{i}; ":"; S{e} -> `OptLabl(_loc,i,e)
        | "?"; a_lident{i} -> `OptLablS(_loc,i) ] 
       "." LA
        [ S{e1}; "."; "("; S{e2}; ")" -> `ArrayDot (_loc, e1, e2)
        | S{e1}; "."; "["; S{e2}; "]" -> `StringDot (_loc, e1, e2)
        | S{e1}; "."; "{"; comma_exp{e2}; "}" -> FanOps.bigarray_get _loc e1 e2
        | S{e1}; "."; S{e2} -> `Field(_loc,e1,e2)
        | S{e}; "#"; a_lident{lab} -> `Send (_loc, e, lab) ]
       "~-" NA
        [ "!"; S{e} ->  (* {| ! $e|} *) (* FIXME *)
          `Field(_loc,e,`Lid(_loc,"contents"))
        | prefixop{f}; S{e} -> `App (_loc, f, e) ]
       "simple"
        [ `QUOTATION x -> AstQuotation.expand _loc x FDyn.exp_tag
        | `Ant (("exp"|""|"`bool" |"par"|"seq"|"int"|"`int"
                |"int32"|"`int32"|"int64"|"`int64"|"nativeint"|"`nativeint"
                |"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" | "vrn" as n),s) ->
                    mk_anti _loc ~c:"exp" n s
        | `INT(_,s) ->  `Int(_loc,s)
        | `INT32(_,s) -> `Int32(_loc,s)
        | `INT64(_,s) -> `Int64(_loc,s)
        | `Flo(_,s) -> `Flo (_loc, s)
        | `CHAR(_,s) -> `Chr (_loc, s)
        | `STR(_,s) -> `Str (_loc, s)
        | `NATIVEINT(_,s) -> `Nativeint (_loc, s)
        | TRY module_longident_dot_lparen{i};S{e}; ")" ->
            `LetOpen (_loc, i, e)
        (* | TRY val_longident{i} -> {| $id:i |} *)
        (* | ident{i} -> i  (\* FIXME logic was splitted here *\) *)
        | vid{i} -> (i :vid :>exp) 
        | "`"; luident{s} -> `Vrn(_loc,s)
        | "["; "]" -> {| [] |} (* FIXME *)
              
        (* | "[";sem_exp_for_list{mk_list}; "::"; exp{last}; "]" -> mk_list last *)
        | "["; sem_exp_for_list{mk_list}; "]" -> mk_list {| [] |}
        | "[|"; "|]" -> `ArrayEmpty(_loc)
        | "[|"; sem_exp{el}; "|]" -> `Array (_loc, el)

        (* | "{"; a_lident{x} ; "with"; label_exp_list{el}; "}" -> *)
        (*     `RecordWith (_loc, el, `Id (_loc,(x:>ident))) *)
        | "{"; `Lid x ; "with"; label_exp_list{el}; "}" ->
            {| { ($lid:x) with $el }|} (* FIXME add antiquot support *)
        | "{"; label_exp_list{el}; "}" -> `Record (_loc, el)
        | "{"; "("; S{e}; ")"; "with"; label_exp_list{el}; "}" ->
            `RecordWith (_loc, el, e)
        | "{<"; ">}" -> `OvrInstEmpty(_loc)
        | "{<"; field_exp_list{fel}; ">}" -> `OvrInst(_loc,fel) 
        | "("; ")" -> {| () |}
        | "("; S{e}; ":"; ctyp{t}; ")" -> `Constraint (_loc, e, t)
        | "("; S{e}; ","; comma_exp{el}; ")" ->
            `Par (_loc, `Com (_loc, e, el))
        | "("; S{e}; ";"; sequence{seq}; ")" -> `Seq(_loc,`Sem(_loc,e,seq))
        | "("; S{e}; ";"; ")" -> `Seq(_loc,e)
        | "("; S{e}; ":"; ctyp{t}; ":>"; ctyp{t2}; ")" ->
            `Coercion (_loc, e, t, t2)
        | "("; S{e}; ":>"; ctyp{t}; ")" -> `Subtype(_loc,e,t)
        | "("; S{e}; ")" -> e
        | "begin"; sequence{seq}; "end" -> `Seq(_loc,seq)
        | "begin"; "end" -> {| () |}
        | "("; "module"; mexp{me}; ")" ->
            `Package_exp (_loc, me)
        | "("; "module"; mexp{me}; ":"; mtyp{pt}; ")" ->
            `Package_exp (_loc, `Constraint (_loc, me, pt))  ] }
       sem_exp_for_list:
       [ exp{e}; ";"; S{el} ->
         fun acc -> {:exp| $e :: $(el acc)|}
       | exp{e}; ";" -> fun acc -> {:exp| $e :: $acc |}
       | exp{e} -> fun acc -> {:exp| $e :: $acc |}]


       sequence: (*FIXME*)
       [ "let"; opt_rec{rf}; bind{bi}; "in"; exp{e}; sequence'{k} ->
         k  (`LetIn (_loc, rf, bi, e))
       | "let"; "try"; opt_rec{r}; bind{bi}; "in"; S{x}; "with"; case{a}; sequence'{k}
         -> k (`LetTryInWith(_loc,r,bi,x,a))
       | "let"; "module"; a_uident{m}; mbind0{mb}; "in";
           exp{e}; sequence'{k} -> k  (`LetModule (_loc, m, mb, e))
       | "let"; "open"; module_longident{i}; "in"; S{e} ->
           `LetOpen (_loc, (i: vid :> ident), e)
       (* FIXME Ant should be able to be followed *)      
       | exp{e}; sequence'{k} -> k e ]
       sequence':
       [ -> fun e -> e
       | ";" -> fun e -> e
       | ";"; sequence{el} -> fun e -> `Sem(_loc,e,el) ]


       (* FIXME: more succinct form *)    
       (* infixop1: *)
       (* [  [ "&" | "&&" ]{x} -> `Lid(_loc,x) ] *)

       infixop1:
       [ "&"  -> `Lid (_loc,"&")
       | "&&" -> `Lid (_loc,"&&")]    

       (* infixop0: *)
       (* [  [ "or" | "||" ]{x} -> `Lid(_loc,x) ] *)
           
       infixop0:
       ["or" -> `Lid(_loc,"or")
       |"||" -> `Lid(_loc,"||") ]
           
       comma_exp:
       [ S{e1}; ","; S{e2} -> `Com(_loc,e1,e2)
       | exp Level "top"{e} -> e ]
       dummy:
       [ -> () ] |};
  {:extend| with_exp_lang:
    [ lang{old}; ":"; exp{x} -> (AstQuotation.default := old; x)] |} ;
  {:extend| with_stru_lang:
    [lang{old};":"; stru{x} -> (AstQuotation.default:=old;x)]
  |};
  with bind
      {:extend|
        bind_quot:
        [ bind{x} -> x  ] 
        bind:
        [ `Ant (("bind" as n),s) -> mk_anti _loc ~c:"bind" n s
        | `Ant (("" as n),s); "="; exp{e} ->
            {| $(mk_anti _loc  ~c:"pat" n s) = $e |}
        | `Ant (("" as n),s) -> mk_anti _loc ~c:"bind" n s
        | S{b1}; "and"; S{b2} -> `And (_loc, b1, b2)
        | let_bind{b} -> b ] 
        let_bind:
        [ pat{p}; fun_bind{e} -> `Bind (_loc, p, e) ] |};

  with case
    {:extend|
      case:
      [ "|"; L1 case0 SEP "|"{l} -> bar_of_list l 
      | pat{p}; "->"; exp{e} -> `Case(_loc,p,e)
      ]
      case0:
      [ `Ant ("case" as n, s) -> mk_anti _loc ~c:"case" n s

      | `Ant ("" as n ,s) -> mk_anti _loc ~c:"case" n s

      | `Ant ("" as n,s) ;"when";exp{w};"->"; exp{e} ->
          `CaseWhen(_loc,mk_anti _loc ~c:"case" n s, w,e )
      | `Ant ("" as n,s); "->"; exp {e} ->
          `Case(_loc,mk_anti _loc ~c:"case" n s ,e)
            
      | pat_as_pat_opt{p}; "when"; exp{w};  "->"; exp{e} ->
           `CaseWhen (_loc, p, w, e)
      | pat_as_pat_opt{p}; "->";exp{e} -> `Case(_loc,p,e)]
      case_quot:
      [ L1 case0 SEP "|"{x} -> bar_of_list x]  |};
  with rec_exp
      {:extend|
        rec_exp_quot:
        [ label_exp_list{x} -> x  ]
        label_exp:
        [ `Ant (("rec_exp" |"" as n),s) -> 
          mk_anti _loc ~c:"rec_exp" n s
        | label_longident{i}; fun_bind{e} -> {| $id:i = $e |}
        | label_longident{i} ->  (*FIXME*)
            `RecBind (_loc, i, `Lid (_loc, FanOps.to_lid i))]
        field_exp:
        [ `Ant ((""|"bi" as n),s) -> mk_anti _loc ~c:"rec_exp" n s
        | a_lident{l}; "=";  exp Level "top"{e} ->
            `RecBind (_loc, (l:>ident), e) (* {| $lid:l = $e |} *) ]
        label_exp_list:
        [ label_exp{b1}; ";"; S{b2} ->`Sem (_loc, b1, b2)
        | label_exp{b1}; ";"            -> b1
        | label_exp{b1}                 -> b1  ]
        field_exp_list:
        [ field_exp{b1}; ";"; S{b2} -> `Sem (_loc, b1, b2)
        | field_exp{b1}; ";"            -> b1
        | field_exp{b1}                 -> b1  ] |};
  with pat
    {:extend| 
       pat_quot:
       [ pat{x}; ","; comma_pat{y} -> `Com(_loc,x,y)
       | pat{x}; ";"; sem_pat{y} -> `Sem(_loc,x,y)
       | pat{x} -> x]
       pat_as_pat_opt:
       [ pat{p1}; "as"; a_lident{s} ->  `Alias (_loc, p1, s)
       | pat{p} -> p ]
       let pat_constr:
       [module_longident{i} -> (* `Id(_loc,i) *) (i :vid :> pat)
       |"`"; luident{s}  -> (`Vrn(_loc,s) :pat)
       |`Ant ((""|"pat"|"vrn" as n), s) -> mk_anti _loc ~c:"pat" n s]
       pat:
       { "|" LA
        [ S{p1}; "|"; S{p2} -> `Bar(_loc,p1,p2) ]
       ".." NA
        [ S{p1}; ".."; S{p2} -> `PaRng(_loc,p1,p2) ]
        "::" RA
        [ S{p1}; "::"; S{p2} ->
          `App (_loc, `App (_loc, `Uid (_loc, "::"), p1), p2)]   
       "apply" LA
        [ pat_constr{p1}; S{p2} -> (*FIXME *)
          (match p2 with
          | {| ($par:p) |} ->
              List.fold_left (fun p1 p2 -> {| $p1 $p2 |}) p1
                (list_of_com p []) (* precise *)
          | _ -> {|$p1 $p2 |})  
        | pat_constr{p1} -> p1
        | "lazy"; S{p} -> `Lazy (_loc, p)  ]
       "simple"
        [ `Ant ((""|"pat"|"par"|"int"|"`int"|"int32"|"`int32"|"int64"|"`int64"
                |"vrn"
                |"nativeint"|"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" as n),s)
          -> mk_anti _loc ~c:"pat" n s
        | vid{i} -> (i : vid :> pat)
        | `INT(_,s) ->  `Int (_loc, s)
        | `INT32(_,s) ->  `Int32 (_loc, s)
        | `INT64(_,s) ->  `Int64 (_loc, s)
        | `Flo(_,s) ->  `Flo (_loc, s)
        | `CHAR(_,s) -> `Chr(_loc,s)
        | `STR(_,s) -> `Str(_loc,s)
        | "-"; `INT(_,s) ->  `Int (_loc, String.neg s)
        | "-"; `INT32(_,s) -> `Int32(_loc, String.neg s) 
        | "-"; `INT64(_,s) -> `Int64(_loc,String.neg s)
        | "-"; `NATIVEINT(_,s) -> `Nativeint(_loc,String.neg s)
        | "-"; `Flo(_,s) -> `Flo(_loc,String.neg s)
        | "["; "]" -> {| [] |}
        | "["; sem_pat_for_list{mk_list}; "]" -> mk_list {| [] |}
              
        | "[|"; "|]" -> `ArrayEmpty(_loc)
        | "[|"; sem_pat{pl}; "|]" -> `Array(_loc,pl)
        | "{"; label_pat_list{pl}; "}" -> `Record(_loc,pl)
            (* {| { $((pl : rec_pat :>pat)) } |} *)
        | "("; ")" -> {| () |}
        | "("; "module"; a_uident{m}; ")" -> `ModuleUnpack(_loc,m)
            (* {| (module $m) |} *)
        | "("; "module"; a_uident{m}; ":"; (* package_type *)mtyp{pt}; ")" ->
            `ModuleConstraint(_loc,m, `Package(_loc,pt))
              (* {| ( module $m :  $pt )|} *)
        | "(";"module"; a_uident{m};":"; `Ant(("opt" as n),s ); ")" ->
            `ModuleConstraint (_loc, m, mk_anti _loc n s)
        | "("; S{p}; ")" -> p
        | "("; S{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
        | "("; S{p}; "as";  a_lident{s}; ")" -> {| ($p as $s )|}
        | "("; S{p}; ","; comma_pat{pl}; ")" -> {| ($p, $pl) |}
        | "`"; luident{s} -> {|$vrn:s|}
          (* duplicated may be removed later with [pat Level "apply"] *)
        | "#"; type_longident{i} -> {| # $i |}
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.pat_tag
        | "_" -> {| _ |}
        | `LABEL i; S{p} -> {| ~ $lid:i : $p |}
        | "~"; a_lident{i}; ":"; S{p} -> (* CHANGE *) {| ~$i : $p|}
        | "~"; a_lident{i} -> `LabelS(_loc,i)
        | `OPTLABEL i; "("; pat_tcon{p}; "="; exp{e}; ")" ->
            `OptLablExpr(_loc,`Lid(_loc,i),p,e)
            (* {| ?$lid:i : ($p=$e)|} *)
        | `OPTLABEL i; "("; pat_tcon{p}; ")"  ->
            `OptLabl(_loc,`Lid(_loc,i),p)
            (* {| ? $lid:i : ($p)|} *)
        | "?"; a_lident{i};":"; "("; pat_tcon{p}; "="; exp{e}; ")" ->
            `OptLablExpr(_loc,i,p,e)
            (* {| ?$i:($p=$e)|} *)
        | "?"; a_lident{i};":"; "("; pat_tcon{p}; "="; `Ant(("opt" as n),s); ")" ->
            `OptLablExpr (_loc, i, p,mk_anti _loc  n s)
            (* {| ?$i : ($p = $(opt: `Ant(_loc, mk_anti n s )) )|} *)
        | "?"; a_lident{i}; ":"; "("; pat_tcon{p}; ")"  ->
            `OptLabl(_loc,i,p)
            (* {| ? $i:($p)|} *)
        | "?"; a_lident{i} -> `OptLablS(_loc,i )
        | "?"; "("; ipat_tcon{p}; ")" -> `OptLabl(_loc,`Lid(_loc,""),p) (* FIXME*)

        | "?"; "("; ipat_tcon{p}; "="; exp{e}; ")" ->
            `OptLablExpr(_loc,`Lid(_loc,""),p,e)
            (* {| ? ($p = $e) |}; *)
        ] }

       ipat:
        [ "{"; label_pat_list{pl}; "}" ->
          {| { $pl }|}
          (* {| { $((pl: rec_pat :>pat)) } |} *)
        | `Ant ((""|"pat"|"par" as n),s) -> mk_anti _loc ~c:"pat" n s
        | "("; ")" -> {| () |}
        | "("; "module"; a_uident{m}; ")" -> `ModuleUnpack(_loc,m)
            (* {| (module $m) |} *)
        | "("; "module"; a_uident{m}; ":";  mtyp{pt}; ")" ->
             `ModuleConstraint (_loc, m, ( (`Package (_loc, pt))))
              (* {| (module $m : $pt )|} *)
        | "(";"module"; a_uident{m};":"; `Ant(("opt" as n),s ); ")" ->
             `ModuleConstraint (_loc, m, mk_anti _loc  n s)
            (* {| (module $m : $(opt: `Ant(_loc,mk_anti n s)))|} *)

        (* when change [pat], we need to take care of the following terms
           for factorization *)      
        | "("; pat{p}; ")" -> p
        | "("; pat{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
        | "("; pat{p}; "as"; a_lident{s}; ")" -> {| ($p as $s) |}
        | "("; pat{p}; ","; comma_ipat{pl}; ")" -> {| ($p, $pl) |}
              
        | a_lident{s} ->  (s: alident :> pat)
              
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.pat_tag
        | "`"; luident{s} -> {|$vrn:s|}              
        | "_" -> {| _ |}
        | `LABEL i; S{p} -> {| ~ $lid:i : $p |}
        | "~"; a_lident{i};":";S{p} -> {| ~$i : $p|}
        | "~"; a_lident{i} ->  `LabelS(_loc,i)
        | `OPTLABEL i; "("; pat_tcon{p}; "="; exp{e}; ")" ->
            `OptLablExpr(_loc,`Lid(_loc,i),p,e)
            (* {| ?$lid:i : ($p=$e)|} *)
        | `OPTLABEL i; "("; pat_tcon{p}; ")"  ->
            `OptLabl(_loc,`Lid(_loc,i),p)
            (* {| ? $lid:i : ($p)|} *)
        | "?"; a_lident{i};":"; "("; pat_tcon{p}; "="; exp{e}; ")" ->
            `OptLablExpr(_loc,i,p,e)
            (* {| ?$i:($p=$e)|} *)
        | "?"; a_lident{i};":"; "("; pat_tcon{p}; "="; `Ant(("opt" as n),s); ")" ->
            `OptLablExpr (_loc, i, p, mk_anti _loc n s)
            (* {| ?$i : ($p = $(opt: `Ant(_loc, mk_anti n s )) )|} *)
        | "?"; a_lident{i}; ":"; "("; pat_tcon{p}; ")"  ->
            `OptLabl(_loc,i,p)
            (* {| ? $i:($p)|} *)
        | "?"; a_lident{i} -> `OptLablS(_loc,i ) 
        | "?"; "("; ipat_tcon{p}; ")" ->
            `OptLabl(_loc,`Lid(_loc,""),p)
            (* {| ? ($p) |} *)
        | "?"; "("; ipat_tcon{p}; "="; exp{e}; ")" ->
            `OptLablExpr(_loc,`Lid(_loc,""),p,e)
            (* {| ? ($p = $e) |} *)]
       
       sem_pat:
       [ pat{p1}; ";"; S{p2} -> `Sem(_loc,p1,p2)
       | pat{p}; ";" -> p
       | pat{p} -> p ] 
       sem_pat_for_list:
       [ pat{p}; ";"; S{pl} -> fun acc ->
         `App(_loc, `App(_loc,`Uid(_loc,"::"),p),pl acc)
         (* {:pat|  $p :: $(pl acc)  |} *)
       | pat{p}; ";" -> fun acc ->
           `App(_loc, `App(_loc,`Uid(_loc,"::"),p),acc)
           (* {:pat|  $p :: $acc  |} *)
       | pat{p} -> fun acc ->
           `App(_loc, `App(_loc,`Uid(_loc,"::"),p),acc)
           (* {:pat|  $p :: $acc  |} *)
       ]
       pat_tcon:
       [ pat{p}; ":"; ctyp{t} -> {| ($p : $t) |}
       | pat{p} -> p ]
       ipat_tcon:
       [ `Ant(("" as n),s) -> mk_anti _loc  ~c:"pat" n s 
       | a_lident{i} ->  (i : alident :> pat)
       | a_lident{i}; ":"; ctyp{t} ->
           (`Constraint (_loc, (i : alident :>  pat), t) : pat)]
       comma_ipat:
       [ S{p1}; ","; S{p2} -> {| $p1, $p2 |}
       | ipat{p} -> p ]
       comma_pat:
       [ S{p1}; ","; S{p2} -> {| $p1, $p2 |}
       | pat{p} -> p ]
       label_pat_list:
       [ label_pat{p1}; ";"; S{p2} -> `Sem(_loc,p1,p2)
       | label_pat{p1}; ";"; "_"       -> `Sem(_loc,p1,`Any _loc)
       | label_pat{p1}; ";"; "_"; ";"  -> `Sem(_loc,p1,`Any _loc)
       | label_pat{p1}; ";"            -> p1
       | label_pat{p1}                 -> p1   ] 
       label_pat:
       [ `Ant ((""|"pat" as n),s) -> mk_anti _loc ~c:"pat" n s
       (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.pat_tag
        *) (* FIXME restore it later *)
       | label_longident{i}; "="; pat{p} -> (* {| $i = $p |} *) `RecBind(_loc,i,p)
       | label_longident{i} ->
           (* `RecBind(_loc,i,`Id(_loc,`Lid(_loc,FanOps.to_lid i))) *)
           `RecBind(_loc,i,`Lid(_loc,FanOps.to_lid i))
           (* {| $i = $(lid:Id.to_lid i) |} *)
       ] |};
    
    with ident
    {:extend|
      (* parse [a] [B], depreacated  *)
      luident: [`Lid i -> i | `Uid i -> i]
      (* parse [a] [B] *)
      aident: [ a_lident{i} -> (i:>ident) | a_uident{i} -> (i:>ident)]
      astr:
      [`Lid i -> `C (_loc,i) | `Uid i -> `C(_loc,i) |
       `Ant (("" | "vrn" as n),s) -> mk_anti _loc  n s]
      ident_quot:
      { "."
        [ S{i}; "."; S{j} -> {| $i.$j  |} ]
        "simple"
        [ `Ant ((""|"id" |"uid" as n),s) ->
          mk_anti _loc  ~c:"ident" n s
        | `Ant (("lid" as n), s) -> mk_anti _loc  ~c:"ident" n s
        | `Ant ((""|"id"|"uid" as n),s); "."; S{i} ->
            `Dot (_loc, mk_anti _loc  ~c:"ident" n s, i)
        | `Lid i -> {| $lid:i |}
        | `Uid i -> {| $uid:i |}
        | `Uid s ; "." ; S{j} -> {|$uid:s.$j|}
        | "("; S{i};S{j}; ")" -> `Apply _loc i j  ] }

      (* parse [a] [b], [a.b] [A.b]*)
      ident:
      [ `Ant ((""|"id"|"uid" as n),s) -> mk_anti _loc ~c:"ident" n s
      | `Ant (("lid" as n), s) -> mk_anti _loc  ~c:"ident" n s
      | `Ant ((""|"id"|"uid" as n),s); "."; S{i} ->
           `Dot (_loc, mk_anti _loc ~c:"ident" n s, i)
      | `Lid i -> `Lid(_loc,i)
      | `Uid i -> `Uid(_loc,i)
      | `Uid s ; "." ; S{j} ->  `Dot (_loc, `Uid (_loc, s), j)]
      
      vid: (* duplicate ident  FIXME *)
      [ `Ant ((""|"id" |"uid" as n),s) -> mk_anti _loc ~c:"ident" n s
      | `Ant (("lid" as n), s) -> mk_anti _loc  ~c:"ident" n s
      | `Ant ((""|"id"|"uid" as n),s); "."; S{i} ->
           `Dot (_loc, mk_anti _loc ~c:"ident" n s, i)
      | `Lid i -> `Lid(_loc,i)
      | `Uid i -> `Uid(_loc,i)
      | `Uid s ; "." ; S{j} ->  `Dot (_loc, `Uid (_loc, s), j)]
      
      uident:
      [`Uid s -> `Uid(_loc,s)
      | `Ant((""|"id"|"uid" as n),s) ->
          mk_anti _loc  ~c:"uident" n s
      |`Uid s; "."; S{l} -> dot (`Uid (_loc,s)) l
      |`Ant((""|"id"|"uid" as n),s) ;"." ; S{i} ->
          dot (mk_anti _loc ~c:"uident" n s) i]

      (* parse [a.b.c] no antiquot *)
      dot_lstrings:
      [ `Lid i -> (`Sub[],i)
      | `Uid i ; "." ; S {xs} ->
          (match xs with
          |(`Sub xs,v) -> (`Sub (i::xs),v)
          | _ -> raise (XStream.Error "impossible dot_lstrings"))

      | "."; `Uid i; "."; S{xs} ->
          match xs with
          |(`Sub xs,v) -> (`Absolute (i::xs),v)
          | _ -> raise (XStream.Error "impossible dot_lstrings") ]

      (* parse [A.B.(] *)
      module_longident_dot_lparen:
      [ `Ant ((""|"id"|"uid" as n),s); "."; "(" ->
        mk_anti _loc  ~c:"ident" n s 

      | `Uid i; "."; S{l} -> {|$uid:i.$l|}
      | `Uid i; "."; "(" -> {|$uid:i|}
      | `Ant (("uid"|"" as n),s); "."; S{l} -> {|$(mk_anti _loc  ~c:"ident" n s).$l|} ]
      (* parse [A.B] *)
      module_longident:
      [ `Ant ((""|"id" as n),s) ->
        mk_anti _loc ~c:"ident" n s 
      | `Uid i; "."; S{l} ->  `Dot (_loc, `Uid (_loc, i), l)
      | `Uid i -> `Uid(_loc,i)
      | `Ant ((""|"uid" as n),s) -> mk_anti _loc ~c:"ident" n s
      | `Ant((""|"uid" as n), s); "."; S{l} ->
          `Dot (_loc, mk_anti _loc ~c:"ident" n s, l)
      ]

      module_longident_with_app:
      { "apply"
        [ S{i}; S{j} -> `Apply(_loc,i,j) ]
       "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
       "simple"
        [ `Ant ((""|"id"|"uid" as n),s) ->
          mk_anti _loc ~c:"ident" n s
        | `Uid i -> `Uid(_loc,i)
        | "("; S{i}; ")" -> i ] }

      (* parse [(A B).c ]*)
      type_longident: (* FIXME *)
      { "apply" (* No parens *)
        [ S{i}; S{j} -> `Apply(_loc,i,j) ]
        "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
        "simple"
        [ `Ant ((""|"id"|"uid"|"lid" as n),s) ->
          mk_anti _loc ~c:"ident" n s
        | `Lid i -> {|$lid:i|}
        | `Uid i -> {|$uid:i|}
        | "("; S{i}; ")" -> i ] }

      label_longident:
      [ `Ant ((""|"id"|"lid" as n),s) ->
        mk_anti _loc ~c:"ident" n s
      | `Lid i -> {|$lid:i|}
      | `Uid i; "."; S{l} -> {|$uid:i.$l|}
      | `Ant((""|"uid" as n),s); "."; S{l} -> {|$(mk_anti _loc ~c:"ident" n s).$l|} ]
      
      cltyp_longident: [ type_longident{x} -> x ]
      val_longident:[ ident{x} -> x ]
      class_longident: [ label_longident{x} -> x ]
      
      method_opt_override:
      [ "method"; "!" -> `Positive _loc 
      | "method"; `Ant (((""|"override") as n),s) -> mk_anti _loc ~c:"flag" n s
      | "method" -> `Negative _loc   ] 
      opt_override:
      [ "!" -> `Positive _loc
      | `Ant ((("!"|"override") as n),s) ->
          mk_anti _loc ~c:"flag" n s
      | -> `Negative _loc  ]
      
      value_val_opt_override:
      [ "val"; "!" -> `Positive _loc
      | "val"; `Ant (((""|"override"|"!") as n),s) ->
            mk_anti _loc ~c:"flag" n s
      | "val" -> `Negative _loc]
      flag:
      [ "to" ->  `Positive _loc
      | "downto" -> `Negative _loc 
      | `Ant (("to"|"" as n),s) ->
          mk_anti _loc  ~c:"flag" n s]

      opt_private:
      [ "private" -> `Positive _loc
      | `Ant (("private" as n),s) ->
          mk_anti _loc  ~c:"flag" n s
      | -> `Negative _loc   ] 
      opt_mutable:
      [ "mutable" ->  `Positive _loc
      | `Ant (("mutable" as n),s) ->
          mk_anti _loc ~c:"flag" n s
      | -> `Negative _loc  ] 
      opt_virtual:
      [ "virtual" -> `Positive _loc 
      | `Ant (("virtual" as n),s) -> mk_anti _loc  ~c:"flag" n s
      | -> `Negative _loc   ] 
      opt_dot_dot:
      [ ".." -> `Positive _loc
      | `Ant ((".." as n),s) -> mk_anti _loc ~c:"flag" n s
      | -> `Negative _loc   ]

      (*opt_rec@inline *)
      opt_rec:
      [ "rec" -> `Positive _loc
      | `Ant (("rec" as n),s) -> mk_anti _loc ~c:"flag" n s
      | -> `Negative _loc]
      a_lident:
      [ `Ant((""|"lid") as n,s) -> mk_anti _loc  ~c:"a_lident" n s
      | `Lid s  -> `Lid (_loc, s) ]
      a_uident:
      [ `Ant((""|"uid") as n,s) -> mk_anti _loc  ~c:"a_uident" n s
      | `Uid s  -> `Uid (_loc, s) ]
      string_list:
      [ `Ant("",s) -> mk_anti _loc  "str_list" s
      | `Ant("",s) ; S{xs} -> `App(_loc,mk_anti _loc "" s, xs)
      | `STR (_, x) -> `Str(_loc,x)
      | `STR (_, x); S{xs} -> `App(_loc,`Str(_loc,x),xs)]
      rec_flag_quot:  [ opt_rec{x} -> x ]
      direction_flag_quot:  [ flag{x} -> x ] 
      mutable_flag_quot: [  opt_mutable{x} -> x ] 
      private_flag_quot: [  opt_private{x} -> x ]
      virtual_flag_quot: [  opt_virtual{x} -> x ] 
      row_var_flag_quot: [  opt_dot_dot{x} -> x ] 
      override_flag_quot:[  opt_override{x} -> x ] 
      pat_eoi:  [ pat{x}; `EOI -> x ] 
      exp_eoi:  [ exp{x}; `EOI -> x ]  |};
  with stru
    {:extend|
    (** ml file  entrance *)    
      implem:
      [
        `DirQuotation (shift,name,contents) -> (* FIXME (a,b,c) pattern broken *)
          let _loc = FLoc.move `start shift _loc in
         begin
           (Fdir.handle_dir _loc (name,contents));
           ([],Some _loc)
         end
      | stru{si}; ";;"; S{(sil, stopped)} -> (si :: sil, stopped)
      | stru{si};  S{(sil, stopped)} -> (si :: sil, stopped)
         (* FIXME merge with the above in the future*)            
      | `EOI -> ([], None) ]
      (** entrance for toplevel *)
      top_phrase:
      [ "#"; a_lident{n}; exp{dp}; ";;" -> Some (`Directive(_loc,n,dp))
      | "#"; a_lident{n}; ";;" -> Some (`DirectiveSimple(_loc,n))
      | stru{st}; ";;" -> Some st
      | `EOI -> None ]
      (* used by [struct .... end]
         constains at least one element *)
      strus: (* FIXME dump seems to be incorrect *)
      [ `Ant ((""|"stri" as n),s) -> mk_anti _loc n ~c:"stru" s
      | `Ant ((""|"stri" as n),s) ;";;" -> mk_anti _loc n ~c:"stru" s          
      | `Ant ((""|"stri" as n),s);  S{st} -> `Sem (_loc, mk_anti _loc n ~c:"stru" s, st)
      | `Ant ((""|"stri" as n),s); ";;"; S{st} -> `Sem (_loc, mk_anti _loc n ~c:"stru" s, st)
      | stru{st} -> st
      | stru{st};";;" -> st
      | stru{st};";;"; S{xs} -> `Sem(_loc,st,xs)            
      | stru{st}; S{xs} -> `Sem(_loc,st,xs)]


      stru_quot:
      [ "#"; a_lident{n}; exp{dp} -> `Directive(_loc,n,dp)
      | "#"; a_lident{n} -> `DirectiveSimple(_loc,n)
      | strus{x} -> x]

      stru:
      { "top"
        [ "exception"; constructor_declaration{t} -> `Exception(_loc,t)
        (* | "exception"; constructor_declaration{t}; "="; type_longident{i} -> *)
        (*     {| exception $t = $i |} *)
        | "external"; a_lident{i};":"; ctyp{t};"="; string_list{sl} ->
            `External (_loc, i, t, sl)
        | "include"; mexp{me} -> `Include(_loc,me)
        | "module"; a_uident{i}; mbind0{mb} -> `Module(_loc,i,mb)
        | "module"; "rec"; mbind{mb} -> `RecModule(_loc,mb)
        | "module"; "type"; a_uident{i}; "="; mtyp{mt} ->
            `ModuleType(_loc,i,mt)
        | "open"; module_longident{i} -> `Open(_loc,(i: vid :> ident))
        | "type"; type_declaration{td} -> `Type(_loc,td)
        | "type"; type_declaration{t};"with"; "("; string_list{ns};")" ->
            `TypeWith (_loc,t,ns)              
        | "let"; opt_rec{r}; bind{bi}; "in"; exp{x} ->
              {| let $rec:r $bi in $x |}
        | "let"; opt_rec{r}; bind{bi} ->
            (match bi with
            | `Bind(_loc,`Any _,e) -> `StExp(_loc,e)
            | _ -> `Value(_loc,r,bi))
        | "let"; "module"; a_uident{m}; mbind0{mb}; "in"; exp{e} ->
              {| let module $m = $mb in $e |}
        | "let"; "open"; module_longident{i}; "in"; exp{e} ->
            let i = (i:vid :> ident) in 
            {| let open $id:i in $e |}
        | "let"; "try"; opt_rec{r}; bind{bi}; "in"; exp{x}; "with"; case{a}
          -> `StExp(_loc ,`LetTryInWith(_loc,r,bi,x,a))
        | "class"; class_declaration{cd} ->  `Class(_loc,cd)
        | "class"; "type"; cltyp_declaration{ctd} ->
            `ClassType (_loc, ctd)
        | `Ant ((""|"stri" as n),s) ->
            mk_anti _loc ~c:"stru" n s
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.stru_tag
        | exp{e} -> `StExp(_loc,e)
              (* this entry makes {| let $rec:r $bi in $x |} parsable *)
        ] }   |};

  with clsigi
    {:extend|
      clsigi_quot:
      [ clsigi{x1}; ";"; S{x2} -> `Sem(_loc,x1,x2)
      | clsigi{x} -> x]

      class_signature:
      [ `Ant ((""|"csg" as n),s) -> mk_anti _loc  ~c:"clsigi" n s
      | `Ant((""|"csg" as n),s);";" -> mk_anti _loc  ~c:"clsigi" n s
      | `Ant ((""|"csg" as n),s); S{csg} ->
          (`Sem (_loc, mk_anti _loc ~c:"clsigi" n s, csg) : FAst.clsigi )            
      | `Ant ((""|"csg" as n),s);";"; S{csg} ->
          (`Sem (_loc, mk_anti _loc ~c:"clsigi" n s, csg) : FAst.clsigi )
      | clsigi{csg} -> csg
      | clsigi{csg};";" -> csg            
      | clsigi{csg};";";S{xs} -> `Sem(_loc,csg,xs)
      | clsigi{csg}; S{xs} -> `Sem(_loc,csg,xs)]
      
      clsigi:
      [ `Ant ((""|"csg" as n),s) -> mk_anti _loc ~c:"clsigi" n s
      | `QUOTATION x -> AstQuotation.expand _loc x FDyn.clsigi_tag
      | "inherit"; cltyp{cs} -> `SigInherit(_loc,cs)

      | "val"; opt_mutable{mf}; opt_virtual{mv};a_lident{l}; ":"; ctyp{t} ->
          {| val $mutable:mf $virtual:mv $l : $t |}
      | "method"; "virtual"; opt_private{pf}; a_lident{l}; ":";ctyp{t} ->
          {| method virtual $private:pf $l : $t |}
      | "method"; opt_private{pf}; a_lident{l}; ":";ctyp{t} ->
          {| method $private:pf $l : $t |}
      | "constraint"; ctyp{t1}; "="; ctyp{t2} -> {|constraint $t1 = $t2|} ] |};  
  with clfield
    {:extend|

      
      class_structure:
       [ `Ant ((""|"cst" as n),s) -> mk_anti _loc ~c:"clfield" n s
       | `Ant ((""|"cst" as n),s); ";" -> mk_anti _loc ~c:"clfield" n s
       | `Ant ((""|"cst" as n),s);S{st} -> `Sem(_loc, mk_anti _loc ~c:"clfield" n s,st)  
       | `Ant ((""|"cst" as n),s); ";"; S{cst} -> {| $(mk_anti _loc ~c:"clfield" n s); $cst |}
       | clfield{st} -> st
       | clfield{st};";" -> st
       | clfield{st};";";S{xs} -> `Sem(_loc,st,xs)
       | clfield{st}; S{xs} -> `Sem(_loc,st,xs)]


      
      clfield:
        [ `Ant ((""|"cst" as n),s) -> mk_anti _loc ~c:"clfield" n s
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.clfield_tag
        | "inherit"; opt_override{o}; clexp{ce}(* ; opt_as_lident{pb} *) ->
            `Inherit(_loc,o,ce)
        | "inherit"; opt_override{o}; clexp{ce}; "as"; a_lident{i} ->
            `InheritAs(_loc,o,ce,i)
        | value_val_opt_override{o}; opt_mutable{mf}; a_lident{lab}; cvalue_bind{e}
          ->
            {| val $override:o $mutable:mf $lab = $e |}
        | value_val_opt_override{o}; "virtual"; opt_mutable{mf}; a_lident{l}; ":";
                ctyp{t} ->
                (match o with
                | `Negative _ ->{| val virtual $mutable:mf $l : $t |}
                | _ -> raise (XStream.Error "override (!) is incompatible with virtual"))                    
        | method_opt_override{o}; "virtual"; opt_private{pf}; a_lident{l}; ":";
                  ctyp{t} ->
                (match o with
                | `Negative _ -> `VirMeth (_loc, l, pf, t)
                | _ -> raise (XStream.Error "override (!) is incompatible with virtual"))  

       | method_opt_override{o}; opt_private{pf}; a_lident{l}; ":"; ctyp{t} (* opt_polyt{topt} *);
                fun_bind{e} ->
                  `CrMth(_loc,l,o,pf,e,t)
            (* {| method $override:o $private:pf $l : $topt = $e |} *)
       | method_opt_override{o}; opt_private{pf};a_lident{l}; fun_bind{e} ->
           `CrMthS(_loc,l,o,pf,e)
             
       | "constraint"; ctyp{t1}; "="; ctyp{t2} ->
          {|constraint $t1 = $t2|}
        | "initializer"; exp{se} -> {| initializer $se |} ]
      clfield_quot:
        [ clfield{x1}; (* semi *)";"; S{x2} -> `Sem(_loc,x1,x2)
        | clfield{x} -> x]
    |};
    
  with clexp
    {:extend|
      clexp_quot:
      [ clexp{x} -> x]
      class_declaration:
      [ S{c1}; "and"; S{c2} -> `And(_loc,c1,c2)
      | `Ant ((""|"cdcl" as n),s) -> mk_anti _loc ~c:"clexp" n s
      (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.clexp_tag *)
      | opt_virtual{mv};  a_lident{i}; "["; comma_type_parameter{x}; "]"; class_fun_bind{ce}
        -> `ClDecl(_loc,mv,(i:>ident),x,ce)
      | opt_virtual{mv}; a_lident{i}; class_fun_bind{ce} ->
          `ClDeclS(_loc,mv,(i:>ident),ce)]
      class_fun_bind:
      [ "="; clexp{ce} -> ce
      | ":"; cltyp_plus{ct}; "="; clexp{ce} ->
          `Constraint(_loc,ce,ct)
      | ipat{p}; S{cfb} -> `CeFun (_loc, p, cfb)  ]
      class_fun_def:
      [ ipat{p}; S{ce} -> `CeFun(_loc,p,ce)
      | "->"; clexp{ce} -> ce ]
      clexp:
      { "top"
          [ "fun"; ipat{p}; class_fun_def{ce} ->  `CeFun (_loc, p, ce)
          | "function"; ipat{p}; class_fun_def{ce} -> `CeFun (_loc, p, ce)
          | "let"; opt_rec{rf}; bind{bi}; "in"; S{ce} -> `LetIn(_loc,rf,bi,ce)]
        "apply" NA
          [ S{ce}; exp Level "label"{e} ->
            `CeApp (_loc, ce, e) ]
        "simple"
          [ `Ant ((""|"cexp" as n),s) -> mk_anti _loc ~c:"clexp" n s
          | `QUOTATION x ->
              AstQuotation.expand _loc x FDyn.clexp_tag
          | vid{ci}; "["; comma_ctyp{t}; "]" ->
              `ClApply(_loc,ci,t)
          | vid {ci} -> (ci :>clexp)
          | "object"; "("; pat{p}; ")" ; class_structure{cst};"end"
            -> `ObjPat(_loc,p,cst)
          | "object"; "("; pat{p}; ")" ;"end" ->
              `ObjPatEnd(_loc,p)
          | "object";"("; pat{p};":";ctyp{t};")"; class_structure{cst};"end" ->
              `ObjPat(_loc,`Constraint(_loc,p,t),cst)
          | "object";"("; pat{p};":";ctyp{t};")"; "end" ->
              `ObjPatEnd(_loc,`Constraint(_loc,p,t))
          | "object"; class_structure{cst};"end" -> `Obj(_loc,cst)
          | "object";"end" -> `ObjEnd(_loc)
          | "("; S{ce}; ":"; cltyp{ct}; ")" -> `Constraint(_loc,ce,ct)
          | "("; S{ce}; ")" -> ce ] } |};
  with cltyp
    {:extend|
      class_description:
      [ S{cd1}; "and"; S{cd2} -> `And(_loc,cd1,cd2)
      | `Ant ((""|"typ" as n),s) ->
          mk_anti _loc ~c:"cltyp" n s
      (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.cltyp_tag *)
      | opt_virtual{mv};  a_lident{i};"[";
          comma_type_parameter{x}; "]"; ":"; cltyp_plus{ct} ->
            `CtDecl(_loc,mv,(i:>ident),x,ct)
      | opt_virtual{mv}; a_lident{i} ; ":"; cltyp_plus{ct}->
          `CtDeclS(_loc,mv,(i:>ident),ct)]
      cltyp_declaration:
      [ S{cd1}; "and"; S{cd2} -> `And(_loc,cd1,cd2)
      | `Ant ((""|"typ" as n),s) -> mk_anti _loc ~c:"cltyp" n s
      (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.cltyp_tag *)

      | opt_virtual{mv};  a_lident{i};"["; comma_type_parameter{x}; "]"
        ; "="; cltyp{ct} ->
          `CtDecl(_loc,mv,(i:>ident),x,ct)
      | opt_virtual{mv}; a_lident{i}; "="; cltyp{ct} ->           
          `CtDeclS(_loc,mv,(i:>ident),ct)]
      cltyp_quot:
      [cltyp{x} -> x]
      cltyp_plus:
      [ "["; ctyp{t}; "]"; "->"; S{ct} -> `CtFun(_loc,t,ct)
      | cltyp{ct} -> ct ]
      cltyp:
      [ `Ant ((""|"ctyp" as n),s) -> mk_anti _loc  ~c:"cltyp" n s
      | `QUOTATION x -> AstQuotation.expand _loc x FDyn.cltyp_tag
      | vid{i}; "["; comma_ctyp{t}; "]" -> `ClApply(_loc,i,t)
      | vid{i} -> (i :> cltyp) 
      | "object";"(";ctyp{t};")";class_signature{csg};"end" -> `ObjTy(_loc,t,csg)
      | "object";class_signature{csg};"end"-> `Obj(_loc,csg)
      | "object"; "(";ctyp{t};")" -> `ObjTyEnd(_loc,t)
      | "object"; "end" -> `ObjEnd(_loc)] |} ;
end;;


let apply_ctyp () = begin
  with ctyp
    {:extend|
      ctyp_quot:
      [ctyp{x}; "*"; star_ctyp{y} -> `Sta (_loc, x, y)
      |ctyp{x} -> x ]
      unquoted_typevars:
      [ S{t1}; S{t2} -> `App(_loc,t1,t2)(* {| $t1 $t2 |} *) (* FIXME order matters ?*)
      | `Ant ((""|"typ" as n),s) ->  mk_anti _loc ~c:"ctyp" n s
      | `QUOTATION x -> AstQuotation.expand _loc x FDyn.ctyp_tag
      | a_lident{i} -> (i:>ctyp) ]
      type_parameter:
      [ `Ant ((""|"typ" as n),s) -> mk_anti _loc n s
      (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.ctyp_tag *)
      | "'"; a_lident{i} -> `Quote(_loc,`Normal _loc, i)
      | "+"; "'"; a_lident{i} ->
          `Quote (_loc, `Positive _loc,  i)
      | "-"; "'"; a_lident{i} -> `Quote (_loc, (`Negative _loc),  i)
      | "+"; "_" -> `QuoteAny (_loc, `Positive _loc)
      | "-"; "_" -> `QuoteAny (_loc, `Negative _loc)
      | "_" ->  `Any _loc]
      type_longident_and_parameters:
      [ "("; type_parameters{tpl}; ")";type_longident{i} -> tpl (i:>ctyp) 
      | type_parameter{tpl} ; type_longident{i} -> `App(_loc, (i:>ctyp),(tpl:>ctyp))
      | type_longident{i} -> (i:>ctyp)
      | `Ant (("" as n),s) -> mk_anti _loc n s ~c:"ctyp" ]
      type_parameters:
      [ type_parameter{t1}; S{t2} -> fun acc -> t2 (`App(_loc,acc, (t1:>ctyp)))
      | type_parameter{t} -> fun acc -> `App(_loc,acc, (t:>ctyp))
      | -> fun t -> t  ]
      meth_list:
      [ meth_decl{m}; ";"; S{(ml, v) }  -> (`Sem(_loc,m,ml), v)
      | meth_decl{m}; ";"; opt_dot_dot{v} -> (m, v)
      | meth_decl{m}; opt_dot_dot{v}      -> (m, v)  ]
      meth_decl:
      [ `Ant ((""|"typ" as n),s)        -> mk_anti _loc ~c:"ctyp" n s
      (* | `QUOTATION x                       -> AstQuotation.expand _loc x FDyn.ctyp_tag *)
      | a_lident{lab}; ":"; ctyp{t} -> `TyCol(_loc,lab,t)]
      opt_meth_list:
      [ meth_list{(ml, v) } -> `TyObj (_loc, ml, v)
      | opt_dot_dot{v}     -> `TyObjEnd(_loc,v) ]
      row_field:
      [ `Ant ((""|"typ" as n),s) -> mk_anti _loc ~c:"ctyp" n s
      | `Ant(("vrn") as n, s) -> `TyVrn(_loc,mk_anti _loc ~c:"ctyp" n s)
      | `Ant(("vrn") as n, s) ; "of"; ctyp{t} ->
          `TyVrnOf(_loc,mk_anti _loc ~c:"ctyp" n s,t)
      | S{t1}; "|"; S{t2} -> `Bar(_loc,t1,t2)
      | "`"; astr{i} ->  `TyVrn(_loc,i)
      | "`"; astr{i}; "of";ctyp{t} -> `TyVrnOf(_loc,i,t)
      | ctyp{t} -> `Ctyp(_loc,t)
      (* | "`"; astr{i}; "of"; "&"; amp_ctyp{t} -> *)
      (*     `TyOfAmp (_loc, (`TyVrn (_loc, i)), t) *)
          (* {| `$i of & $t |} *)]
      (* only used in row_field *)
      (* amp_ctyp: *)
      (* [ S{t1}; "&"; S{t2} -> `Amp(_loc,t1,t2) *)
      (* | ctyp{t} -> t ] *)

      (* only used in ctyps *)
      name_tags:
      [ `Ant ((""|"typ" as n),s) ->  mk_anti _loc ~c:"ctyp" n s
      | S{t1}; S{t2} -> `App (_loc, t1, t2)
      | "`"; astr{i} -> `TyVrn (_loc, i)  ]
      type_declaration:
      [ `Ant ((""|"typ" as n),s) -> mk_anti _loc ~c:"ctyp" n s
      (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.ctyp_tag *)
      | S{t1}; "and"; S{t2} ->  `And(_loc,t1,t2)
      |  type_ident_and_parameters{(n, tpl)}; "="; type_info{tk}; L0 constrain{cl}
        -> `TyDcl (_loc, n, tpl, tk,
                   match cl with
                   |[]-> `None _loc
                   | _ -> `Some(_loc,and_of_list cl))
      | type_ident_and_parameters{(n,tpl)}; L0 constrain{cl} ->
          `TyAbstr(_loc,n,tpl,
                   match cl with
                   |[] -> `None _loc | _ -> `Some(_loc, and_of_list cl))]
      type_info:
      [ type_repr{t2} -> `TyRepr(_loc,`Negative _loc,t2)
      | ctyp{t1}; "="; type_repr{t2} -> `TyMan(_loc, t1, `Negative _loc, t2)
      | ctyp{t1} -> `TyEq(_loc,`Negative _loc, t1)
      | "private"; ctyp{t1} -> `TyEq(_loc,`Positive _loc,t1)
      |  ctyp{t1}; "=";"private"; type_repr{t2} -> `TyMan(_loc, t1, `Positive _loc,t2)
      | "private"; type_repr{t2} -> `TyRepr(_loc,`Positive _loc, t2)]

      type_repr:
      [ "|"; constructor_declarations{t} -> `Sum(_loc,t)
      | "{"; label_declaration_list{t}; "}" -> `Record (_loc, t)]
      type_ident_and_parameters:
      [ "(";  L1 type_parameter SEP ","{tpl}; ")"; a_lident{i} ->
        (i, `Some(_loc, com_of_list (tpl :>  decl_params list)))
      |  type_parameter{t};  a_lident{i} -> (i, `Some (_loc,(t:>decl_params)))
      |  a_lident{i} -> (i, `None _loc)]
      constrain:
      [ "constraint"; ctyp{t1}; "="; ctyp{t2} -> `Eq(_loc,t1, t2) ]
      typevars:
      [ S{t1}; S{t2} -> `App(_loc,t1,t2)(* {| $t1 $t2 |} *) (* FIXME order matters?*)
      | `Ant ((""|"typ" as n),s) ->  mk_anti _loc  ~c:"ctyp" n s
      | `QUOTATION x -> AstQuotation.expand _loc x FDyn.ctyp_tag
      | "'"; a_lident{i} ->  `Quote (_loc, `Normal _loc, i)]
      ctyp:
      {
       "alias" LA
        [ S{t1}; "as"; "'"; a_lident{i} -> `Alias(_loc,t1,i)]
       "forall" LA
        [ "!"; typevars{t1}; "."; ctyp{t2} -> `TyPol (_loc, t1, t2) ]
       "arrow" RA
        [ S{t1}; "->"; S{t2} ->  `Arrow(_loc,t1,t2) ]
       "label" NA
        [ "~"; a_lident{i}; ":"; S{t} -> `Label (_loc, i, t)
        | `LABEL s ; ":"; S{t} -> `Label (_loc, (`Lid (_loc, s)), t) (* FIXME *)
        | `OPTLABEL s ; S{t} -> `OptLabl(_loc,`Lid(_loc,s),t)
        | "?"; a_lident{i}; ":"; S{t} -> `OptLabl(_loc,i,t)]
         
       "apply" LA
        [ S{t1}; S{t2} -> `App (_loc,t2,t1) ]

       (* [mod_ext_longident] and [type_longident]
          | type_longident
          | simple_core_type2 type_longident
          | LPAREN core_type_comma_list RPAREN type_longident *)  
       (* "." LA *)
       (*  [ S{t1}; "."; S{t2} -> *)
       (*      try ( *)
       (*        prerr_endline "used"; *)
       (*        `Dot (_loc, ident_of_ctyp t1, ident_of_ctyp t2) *)
       (*          ) (\* FIXME*\) *)
       (*      with Invalid_argument s -> raise (XStream.Error s) ] *)
       "simple"
        [ "'"; a_lident{i} ->  `Quote (_loc, `Normal _loc,  i)
        | "_" -> `Any _loc
        | `Ant ((""|"typ"|"par" as n),s) -> mk_anti _loc ~c:"ctyp" n s
        | `Ant (("id" as n),s) -> mk_anti _loc ~c:"ident" n s
        | `Ant (("id" as n),s); "."; S{t} ->
            let try id = ident_of_ctyp t  in
              (`Dot(_loc,mk_anti _loc ~c:"ident" n s,id) :ctyp)
            with Invalid_argument s -> raise (XStream.Error s)
        | `QUOTATION x -> AstQuotation.expand _loc x FDyn.ctyp_tag
        | a_uident{i}; "."; S{t} ->
            let try id = ident_of_ctyp t in
              `Dot(_loc,(i:>ident),id)
            with Invalid_argument s -> raise (XStream.Error s)
        | a_lident{i}->  (i :> ctyp)
              
        (* | a_uident{i} -> (i:> ctyp) *)
        | "("; S{t}; "*"; star_ctyp{tl}; ")" ->
            `Par (_loc, `Sta (_loc, t, tl))
        | "("; S{t}; ")" -> t
        | "("; S{t}; ","; com_ctyp{tl}; ")" ; type_longident{j} ->
            appl_of_list  ((j:>ctyp):: t::list_of_com tl [])
        | "["; row_field{rfl}; "]" -> `PolyEq(_loc,rfl)
        (* | "[>"; "]" -> `PolySup (_loc, (`Nil _loc)) *) (* FIXME add later*)
        | "[>"; row_field{rfl}; "]" ->   `PolySup (_loc, rfl)
        | "[<"; row_field{rfl}; "]" -> `PolyInf(_loc,rfl)
        | "[<"; row_field{rfl}; ">"; name_tags{ntl}; "]" -> `PolyInfSup(_loc,rfl,ntl)
        | "#"; class_longident{i} ->  `ClassPath (_loc, i)
        | "<"; opt_meth_list{t}; ">" -> t
        | "("; "module"; mtyp{p}; ")" -> `Package(_loc,p)  ] }
      comma_ctyp: (* DUPLICATED removed later *)
      [ S{t1}; ","; S{t2} -> `Com (_loc, t1, t2) 
      | `Ant (( "" as n),s) -> mk_anti _loc ~c:"ctyp," n s
      | ctyp{t} -> `Ctyp(_loc,t)  ]
      com_ctyp:
      [ `Ant ((""|"typ" as n),s) -> mk_anti _loc ~c:"ctyp" n s
      | S{t1}; ","; S{t2} -> `Com(_loc,t1,t2)
      | ctyp{t} -> t  ]
      star_ctyp:
      [ `Ant ((""|"typ" as n),s) -> mk_anti _loc ~c:"ctyp" n s
      | S{t1}; "*"; S{t2} -> `Sta(_loc,t1,t2)
      | ctyp{t} -> t  ]
      constructor_declarations:
      [ `Ant ((""|"typ" as n),s) -> mk_anti _loc ~c:"ctyp" n s
      (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.ctyp_tag *)
      | S{t1}; "|"; S{t2} ->    `Bar(_loc,t1,t2)
      | a_uident{s}; "of"; constructor_arg_list{t} -> `Of(_loc,s,t)
      | a_uident{s}; ":"; ctyp{t} -> (* GADT  *)      
          `TyCol(_loc,s,t)
      | a_uident{s} -> (s :> or_ctyp) ]
      constructor_declaration:
      [ `Ant ((""|"typ" as n),s) -> mk_anti _loc ~c:"ctyp" n s
      (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.ctyp_tag *)
      | a_uident{s}; "of"; constructor_arg_list{t} -> `Of(_loc,(s:>vid),t)
      | a_uident{s} -> (s:>of_ctyp)  ]
      constructor_arg_list:
      [ S{t1}; "*"; S{t2} -> `Sta(_loc,t1,t2)
      | ctyp{t} -> t  ]
      label_declaration_list:
      [ label_declaration{t1}; ";"; S{t2} -> `Sem(_loc,t1,t2)
      | label_declaration{t1}; ";"            -> t1
      | label_declaration{t1}                 -> t1  ]
      label_declaration:
      [ `Ant ((""|"typ" as n),s) -> mk_anti _loc ~c:"ctyp" n s
      (* | `QUOTATION x -> AstQuotation.expand _loc x FDyn.ctyp_tag *)
      | a_lident{s}; ":"; ctyp{t} -> `TyCol(_loc,s,t)
      | "mutable"; a_lident{s}; ":";  ctyp{t} -> `TyColMut(_loc,s,t)]
      comma_type_parameter:
      [ S{t1}; ","; S{t2} ->  `Com (_loc, t1, t2)
      | type_parameter{t} -> `Ctyp(_loc, (t:>ctyp))  ]
  |};
end;;

  
AstParsers.register_parser
    ("revise",fun () -> begin apply (); apply_ctyp () end);;









