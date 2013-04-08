open Ast;
open AstLoc;
open FanOps;
open Syntax;
open LibUtil;
open FanUtil;
open GramLib;

{:create|Gram pos_exps|};
let apply () = begin 
  let list = ['!'; '?'; '~'] in
  let excl = ["!="; "??"] in
  setup_op_parser prefixop
    (fun x -> not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list && symbolchar x 1);
  let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
  let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
  let excl = ["<-"; "||"; "&&"] in
  setup_op_parser infixop2
    (fun x -> (List.mem x list_ok) ||
              (not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list_first_char_ok && symbolchar x 1));

  let list = ['@'; '^'] in
  setup_op_parser infixop3
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

  let list = ['+'; '-'] in
  setup_op_parser infixop4
    (fun x -> x <> "->" && String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

  let list = ['*'; '/'; '%'; '\\'] in
  setup_op_parser infixop5
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              (x.[0] <> '*' || String.length x < 2 || x.[1] <> '*') &&
              symbolchar x 1);

  setup_op_parser infixop6
    (fun x -> String.length x >= 2 && x.[0] == '*' && x.[1] == '*' &&
              symbolchar x 2);


  FanTokenFilter.define_filter (Gram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm));

  Gram.setup_parser sem_exp begin
    let symb1 = Gram.parse_origin_tokens exp in
    let symb = parser
      [ [< (`Ant (("list" as n), s), _loc) >] ->
        {:exp| $(anti:mk_anti ~c:"exp;" n s) |}
      | [< a = symb1 >] -> a ] in
    let rec kont al =
      parser
      [ [< (`KEYWORD ";", _); a = symb; 's >] ->
        let _loc =  al <+> a  in
        kont {:exp| $al; $a |} s
      | [< >] -> al ] in
    parser [< a = symb; 's >] -> kont a s
  end;

  with module_exp'
  {:extend|
      module_exp_quot:
      [ module_exp{x} -> x]
      module_binding0:
      { RA
        [ "("; a_uident{m}; ":"; module_type{mt}; ")"; S{mb} -> `Functor (_loc, m, mt, mb)
        | ":"; module_type{mt}; "="; module_exp{me} ->  `Constraint (_loc, me, mt)
        | "="; module_exp{me} -> me  ] }
      module_exp:
      { "top"
        [ "functor"; "("; a_uident{i}; ":"; module_type{t}; ")"; "->"; S{me} ->
             `Functor (_loc, i, t, me)
        | "struct"; strus{st}; "end" -> `Struct(_loc,st)
        | "struct"; "end" -> `StructEnd(_loc)]
       "apply"
        [ S{me1}; S{me2} -> `App (_loc, me1, me2) ]
       "simple"
        [ `Ant ((""|"mexp"|"anti"|"list" as n),s) -> `Ant (_loc, mk_anti ~c:"module_exp" n s)
        | `QUOTATION x ->
            AstQuotation.expand _loc x FanDyn.module_exp_tag
        | module_longident{i} -> `Id (_loc, i)
        | "("; S{me}; ":"; module_type{mt}; ")" ->  `Constraint (_loc, me, mt)
        | "("; S{me}; ")" ->  me
        | "("; "val"; exp{e}; ")" -> `PackageModule (_loc, e)
        | "("; "val"; exp{e}; ":"; module_type{p}; ")" ->
             `PackageModule (_loc, `Constraint (_loc, e, `Package (_loc, p)))
             ] } |};

  with module_binding'
      {:extend|
        module_binding_quot:
        [ S{b1}; "and"; S{b2} ->  `And(_loc,b1,b2)
        | `Ant (("module_binding"|"anti"|"" as n),s) ->
             `Ant (_loc, mk_anti ~c:"module_binding" n s)
        | a_uident{m}; ":"; module_type{mt} -> `Constraint(_loc,m,mt)
        | a_uident{m}; ":"; module_type{mt}; "="; module_exp{me} ->
            `ModuleBind(_loc,m,mt,me)]
        module_binding:
        [ S{b1}; "and"; S{b2} -> `And(_loc,b1,b2)
        | `Ant (("module_binding"|"anti"|"list" |"" as n),s) ->
            `Ant (_loc, mk_anti ~c:"module_binding" n s)
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.module_binding_tag
        | a_uident{m}; ":"; module_type{mt}; "="; module_exp{me} ->
            `ModuleBind (_loc, m, mt, me)]
        module_rec_declaration:
        [ S{m1}; "and"; S{m2} -> `And(_loc,m1,m2)
        | `Ant ((""|"module_binding"|"anti"|"list" as n),s) ->
            `Ant (_loc, mk_anti ~c:"module_binding" n s)
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.module_binding_tag
        | a_uident{m}; ":"; module_type{mt} ->
            `Constraint(_loc,m,mt) ] |};

  with with_constr
      {:extend|
        with_constr_quot:
        [ with_constr{x} -> x   ]
        with_constr: 
        [ S{wc1}; "and"; S{wc2} -> `And(_loc,wc1,wc2)
        | `Ant ((""|"with_constr"|"anti"|"list" as n),s) ->
            `Ant (_loc, mk_anti ~c:"with_constr" n s)
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.with_constr_tag
        | "type"; type_longident_and_parameters{t1}; "="; ctyp{t2} ->
            `TypeEq (_loc, t1, t2)
        | "type"; type_longident_and_parameters{t1}; "="; "private"; ctyp{t2} ->
            `TypeEqPriv(_loc,t1,t2)
        | "type"; type_longident_and_parameters{t1}; ":="; ctyp{t2} ->
            `TypeSubst (_loc, t1, t2)
        | "module"; module_longident{i1}; "="; module_longident_with_app{i2} ->
            `ModuleEq (_loc, i1, i2)
        | "module"; module_longident{i1}; ":="; module_longident_with_app{i2} ->
            `ModuleSubst (_loc, i1, i2)] |};

  with module_type
    {:extend|
      module_type:
      { "top"
        [ "functor"; "("; a_uident{i}; ":"; S{t}; ")"; "->"; S{mt} ->
          `Functor(_loc,i,t,mt)]
        "with"
        [ S{mt}; "with"; with_constr{wc} -> `With(_loc,mt,wc)]
        "apply"
        [ S{mt1}; S{mt2} ->
          let app0 mt1 mt2 =
            match (mt1, mt2) with
            [ (`Id(loc1,i1),`Id(loc2,i2)) ->
              let _loc = FanLoc.merge loc1 loc2 in
              `Id(_loc,`App(_loc,i1,i2))
            | _ -> raise XStream.Failure ] in app0 mt1 mt2
          (* ModuleType.app0 mt1 mt2 *) ] (* FIXME *)
        "."
        [ S{mt1}; "."; S{mt2} ->
          let acc0 mt1 mt2 =
            match (mt1, mt2) with
            [ (`Id(loc1,i1),`Id(loc2,i2)) ->
              let _loc = FanLoc.merge loc1 loc2 in
              `Id(_loc,`Dot(_loc,i1,i2))
                (* ({| $id:i1 |}, {@_| $id:i2 |}) ->  {| $(id:{:ident| $i1.$i2 |}) |} *)
            | _ -> raise XStream.Failure ] in
          acc0 mt1 mt2
          (* ModuleType.acc0 mt1 mt2 *) ] (*FIXME*)
        "sig"
        [ "sig"; sig_items{sg}; "end" -> `Sig(_loc,sg)
        | "sig";"end" -> `SigEnd(_loc)]
       "simple"
        [ `Ant ((""|"mtyp"|"anti"|"list" as n),s) ->  {| $(anti:mk_anti ~c:"module_type" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.module_type_tag
        | module_longident_with_app{i} -> `Id(_loc,i)
        | "("; S{mt}; ")" -> mt
        | "module"; "type"; "of"; module_exp{me} -> `ModuleTypeOf(_loc,me)] }
      module_declaration:
      { RA
        [ ":"; module_type{mt} -> mt
        | "("; a_uident{i}; ":"; module_type{t}; ")"; S{mt} ->
            `Functor(_loc,i,t,mt)] }
      module_type_quot:
      [ module_type{x} -> x  ]  |};

  with sig_item
  {:extend|
    sig_item_quot:
    [ "#"; a_lident{s} -> `DirectiveSimple(_loc,s)
    | "#"; a_lident{s}; exp{dp} -> `Directive(_loc,s,dp)
    | sig_item{sg1}; ";"; S{sg2} -> `Sem(_loc,sg1,sg2)
    | sig_item{sg} -> sg] 
    sig_item:
    [ `Ant ((""|"sigi"|"anti"|"list" as n),s) -> `Ant (_loc, mk_anti ~c:"sig_item" n s)
    | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.sig_item_tag
    | "exception"; constructor_declaration{t} ->  {| exception $t |}
    | "external"; a_lident{i};":";ctyp{t};"=" ;string_list{sl} ->
        `External (_loc, i, t, sl)
    | "include"; module_type{mt} -> `Include(_loc,mt)
    | "module"; a_uident{i}; module_declaration{mt} -> `Module(_loc,i,mt)
    | "module"; "rec"; module_rec_declaration{mb} ->  `RecModule (_loc, mb)
    | "module"; "type"; a_uident{i}; "="; module_type{mt} -> `ModuleType(_loc,i,mt)
    | "module"; "type"; a_uident{i} -> `ModuleTypeEnd(_loc,i)
    | "open"; module_longident{i} -> `Open(_loc,i)
    | "type"; type_declaration{t} -> `Type(_loc,t)
    | "val"; a_lident{i}; ":"; ctyp{t} -> `Val(_loc,i,t)
    | "class"; class_description{cd} ->    `Class(_loc,cd)
    | "class"; "type"; class_type_declaration{ctd} ->  `ClassType(_loc,ctd) ]
    (* mli entrance *)    
    interf:
    [ "#"; a_lident{n};  ";;" ->
      ([ `DirectiveSimple(_loc,n) ],  Some _loc)
    | "#"; a_lident{n}; exp{dp}; ";;" -> ([ `Directive(_loc,n,dp)], Some _loc) 
    | sig_item{si}; ";";  S{(sil, stopped)} -> ([si :: sil], stopped)
    | `EOI -> ([], None) ]
    sig_items:
    [ `Ant ((""|"sigi"|"anti"|"list" as n),s) -> `Ant (_loc, mk_anti n ~c:"sig_item" s)
    | `Ant ((""|"sigi"|"anti"|"list" as n),s); ";"; S{sg} ->
         `Sem (_loc, `Ant (_loc, mk_anti n ~c:"sig_item" s), sg)
    | L1 [ sig_item{sg}; ";" -> sg ]{l} -> sem_of_list l  ]
 |};

    with exp
    {:extend|
      local:  fun_def_pat;
      exp_quot:
      [ exp{e1}; ","; comma_exp{e2} -> `Com(_loc,e1,e2)
      | exp{e1}; ";"; sem_exp{e2} -> `Sem(_loc,e1,e2)
      | exp{e} -> e]
       (*
       {:stru|
       let f (type t) () =
          let module M = struct exception E of t ; end in
          ((fun x -> M.E x), (function [M.E x -> Some x | _ -> None]))|}

       {:stru| let f : ! 'a . 'a -> 'a = fun x -> x |}  
        *)
      cvalue_binding:
      [ "="; exp{e} -> e
      | ":"; "type"; unquoted_typevars{t1}; "." ; ctyp{t2} ; "="; exp{e} -> 
          let u = {:ctyp| ! $t1 . $t2 |} in  {| ($e : $u) |}
      | ":"; ctyp{t}; "="; exp{e} -> {| ($e : $t) |}
      | ":"; ctyp{t}; ":>"; ctyp{t2}; "="; exp{e} ->
          match t with
          [ {:ctyp| ! $_ . $_ |} -> raise (XStream.Error "unexpected polytype here")
          | _ -> {| ($e : $t :> $t2) |} ]
      | ":>"; ctyp{t}; "="; exp{e} ->`Subtype(_loc,e,t) ]
      fun_binding:
      { RA
          [ "("; "type"; a_lident{i}; ")"; S{e} -> {| fun (type $i) -> $e |} 
          | ipat{p}; S{e} -> `Fun(_loc,`Case(_loc,p,e))

          | cvalue_binding{bi} -> bi  ] }
       lang:
       [ dot_lstrings{ls} -> 
         let old = !AstQuotation.default in (
         AstQuotation.default := FanToken.resolve_name ls;
         old)]
       pos_exps:
       [ L1
           [ `Lid x;":";dot_lstrings{y} ->
             ((x:string),
              FanToken.resolve_name y
             )
           | `Lid x ->
               ((x:string), FanToken.resolve_name
                  (`Sub [], x) ) ] SEP ";"{xys} -> begin
           let old = !AstQuotation.map;
           AstQuotation.map := SMap.add_list xys old;
           old
       end]
       fun_def_pat:
       ["(";"type";a_lident{i};")" ->
         fun e -> {|fun (type $i) -> $e |} 
       | ipat{p} -> fun e -> `Fun(_loc,`Case(_loc,p,e))(* {| fun $p -> $e |} *)
       | ipat{p}; "when"; exp{w} -> fun e ->
           `Fun(_loc,`CaseWhen(_loc,p,w,e))
           (* {|fun $p when $w -> $e |} *) ]
       fun_def:
       {RA
          [ fun_def_pat{f}; "->"; exp{e} ->  f e
          | fun_def_pat{f}; S{e} -> f e] }    
       exp:
       {
        "top" RA
        [ "let"; opt_rec{r}; binding{bi}; "in"; S{x} ->
            {| let $rec:r $bi in $x |}
        | "let"; "module"; a_uident{m}; module_binding0{mb}; "in"; S{e} ->
            {| let module $m = $mb in $e |}
        | "let"; "open"; module_longident{i}; "in"; S{e} ->
            {| let open $id:i in $e |}
        | "let"; "try"; opt_rec{r}; binding{bi}; "in"; S{x}; "with"; case{a} ->
            {| let try $rec:r $bi in $x with [ $a ] |}
        | "match"; S{e}; "with"; case{a} -> {|match $e with [$a]|}
        | "try"; S{e}; "with"; case{a} -> {|try $e with [$a]|}
        | "if"; S{e1}; "then"; S{e2}; "else"; S{e3} -> {| if $e1 then $e2 else $e3 |}
        | "if"; S{e1}; "then"; S{e2} -> {| if $e1 then $e2 |}
        | "do"; sequence{seq}; "done" -> FanOps.mksequence ~loc:_loc seq
        | "with"; lang{old}; S{x} -> begin  AstQuotation.default := old; x  end
        | "with";"{"; pos_exps{old} ;"}"; S{x} -> begin AstQuotation.map := old; x end
        | "for"; a_lident{i}; "="; S{e1}; direction_flag{df}; S{e2}; "do";
            sequence{seq}; "done" ->
              {| for $i = $e1 $to:df $e2 do $seq done |}
        | "while"; S{e}; "do"; sequence{seq}; "done" ->
            {|while $e do $seq done |}   ]  
       ":=" NA
        [ S{e1}; ":="; S{e2} -> {| $e1 := $e2 |} 
        | S{e1}; "<-"; S{e2} -> (* FIXME should be deleted in original syntax later? *)
            match FanOps.bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> `Assign(_loc,e1,e2) ] ]
       "||" RA
        [ S{e1}; infixop0{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "&&" RA
        [ S{e1}; infixop1{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "<" LA
        [ S{e1}; infixop2{op}; S{e2} -> {| $op $e1 $e2 |} ]
       "^" RA
        [ S{e1}; infixop3{op}; S{e2} -> {| $op $e1 $e2 |} ]
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
        [
        (* FIXME fun and function duplicated *)      
         "fun"; "[";  L1 case0 SEP "|"{a}; "]" ->
           let cases = bar_of_list a in `Fun (_loc,cases)
        | "function"; "[";  L1 case0 SEP "|"{a}; "]" ->
            let cases = bar_of_list a in `Fun(_loc,cases)
        | "fun"; fun_def{e} -> e
        | "function"; fun_def{e} -> e

        | "object"; "(";pat{p}; ")"; class_structure{cst};"end" ->
            `ObjPat(_loc,p,cst)
        | "object"; "(";pat{p}; ")"; "end" ->
            `ObjPatEnd(_loc,p)
        | "object"; "(";pat{p};":";ctyp{t};")";class_structure{cst};"end" ->
            `ObjPat(_loc,`Constraint(_loc,p,t),cst)
        | "object"; "(";pat{p};":";ctyp{t};")";"end" ->
            `ObjPatEnd(_loc,`Constraint(_loc,p,t))
        | "object"; class_structure{cst};"end"->
            `Obj(_loc,cst)
        | "object";"end" -> `ObjEnd(_loc)
      ]
       "unary minus" NA
        [ "-"; S{e} -> FanOps.mkumin _loc "-" e
        | "-."; S{e} -> FanOps.mkumin _loc "-." e ]
       "apply" LA
        [ S{e1}; S{e2} -> {| $e1 $e2 |}
        | "assert"; S{e} -> `Assert(_loc,e)
            (* FanOps.mkassert _loc e *)
        | "new"; class_longident{i} -> `New (_loc,i) (* {| new $i |} *)
        | "lazy"; S{e} -> {| lazy $e |} ]
       "label" NA
        [ "~"; a_lident{i}; ":"; S{e} ->
          {| ~ $i : $e |}
        | "~"; a_lident{i} -> `LabelS(_loc,i)
        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | `LABEL i; S{e} -> {| ~ $lid:i : $e |}
        (* Same remark for ?a:b *)
        | `OPTLABEL i; S{e} ->  `OptLabl(_loc,`Lid(_loc,i),e)
        | "?"; a_lident{i}; ":"; S{e} -> `OptLabl(_loc,i,e)
        | "?"; a_lident{i} -> `OptLablS(_loc,i) ] 
       "." LA
        [ S{e1}; "."; "("; S{e2}; ")" -> {| $e1 .( $e2 ) |}
        | S{e1}; "."; "["; S{e2}; "]" -> {| $e1 .[ $e2 ] |}
        | S{e1}; "."; "{"; comma_exp{e2}; "}" -> FanOps.bigarray_get _loc e1 e2
        | S{e1}; "."; S{e2} -> {| $e1 . $e2 |}
        | S{e}; "#"; a_lident{lab} -> {| $e # $lab |} ]
       "~-" NA
        [ "!"; S{e} ->  {| ! $e|}
        | prefixop{f}; S{e} -> {| $f $e |} ]
       "simple"
        [ `QUOTATION x -> AstQuotation.expand _loc x FanDyn.exp_tag
        | `Ant (("exp"|""|"anti"|"`bool" |"par"|"seq"|"int"|"`int"
                |"int32"|"`int32"|"int64"|"`int64"|"nativeint"|"`nativeint"
                |"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" | "vrn" as n),s) ->
                    {| $(anti:mk_anti ~c:"exp" n s) |}
        | `INT(_,s) -> {|$int:s|}
        | `INT32(_,s) -> {|$int32:s|}
        | `INT64(_,s) -> {|$int64:s|}
        | `Flo(_,s) -> {|$flo:s|}
        | `CHAR(_,s) -> {|$chr:s|}
        | `STR(_,s) -> {|$str:s|}
        | `NATIVEINT(_,s) -> {|$nativeint:s|}
        | TRY module_longident_dot_lparen{i};S{e}; ")" ->
            {| let open $i in $e |}
        (* | TRY val_longident{i} -> {| $id:i |} *)
        | ident{i} -> {|$id:i|} (* FIXME logic was splitted here *)
        | "`"; luident{s} -> {| $vrn:s|}
        | "["; "]" -> {| [] |}
        | "[";sem_exp_for_list{mk_list}; "::"; exp{last}; "]" -> mk_list last
        | "["; sem_exp_for_list{mk_list}; "]" -> mk_list {| [] |}
        | "[|"; "|]" -> `ArrayEmpty(_loc)
        | "[|"; sem_exp{el}; "|]" -> {| [| $el |] |}

        | "{"; `Lid x ; "with"; label_exp_list{el}; "}" ->
            {| { ($lid:x) with $el }|} (* FIXME add antiquot support *)
        | "{"; label_exp_list{el}; "}" -> {| { $el } |}
        | "{"; "("; S{e}; ")"; "with"; label_exp_list{el}; "}" ->
            {| { ($e) with $el } |}
        | "{<"; ">}" -> `OvrInstEmpty(_loc)
        | "{<"; field_exp_list{fel}; ">}" -> `OvrInst(_loc,fel) 
        | "("; ")" -> {| () |}
        | "("; S{e}; ":"; ctyp{t}; ")" -> {| ($e : $t) |}
        | "("; S{e}; ","; comma_exp{el}; ")" -> {| ( $e, $el ) |}
        | "("; S{e}; ";"; sequence{seq}; ")" -> FanOps.mksequence ~loc:_loc {| $e; $seq |}
        | "("; S{e}; ";"; ")" -> FanOps.mksequence ~loc:_loc e
        | "("; S{e}; ":"; ctyp{t}; ":>"; ctyp{t2}; ")" ->
            {| ($e : $t :> $t2 ) |}
        | "("; S{e}; ":>"; ctyp{t}; ")" -> `Subtype(_loc,e,t)(* {| ($e :> $t) |} *)
        | "("; S{e}; ")" -> e
        | "begin"; sequence{seq}; "end" -> FanOps.mksequence ~loc:_loc seq
        | "begin"; "end" -> {| () |}
        | "("; "module"; module_exp{me}; ")" ->
            {| (module $me) |}
        | "("; "module"; module_exp{me}; ":"; (* package_type *)module_type{pt}; ")" ->
            {| (module $me : $pt) |}  ] }
       sequence: (*FIXME*)
       [ "let"; opt_rec{rf}; binding{bi}; "in"; exp{e}; sequence'{k} ->
         k {| let $rec:rf $bi in $e |}
       | "let"; "try"; opt_rec{r}; binding{bi}; "in"; S{x}; "with"; case{a}; sequence'{k}
         -> k {| let try $rec:r $bi in $x with [ $a ] |}
       | "let"; opt_rec{rf}; binding{bi}; ";"; S{el} ->
           {| let $rec:rf $bi in $(FanOps.mksequence ~loc:_loc el) |}
       | "let"; "module"; a_uident{m}; module_binding0{mb}; "in";
           exp{e}; sequence'{k} -> k {| let module $m = $mb in $e |}
       | "let"; "module"; a_uident{m}; module_binding0{mb}; ";"; S{el} ->
           {| let module $m = $mb in $(FanOps.mksequence ~loc:_loc el) |}
       | "let"; "open"; module_longident{i}; "in"; S{e} ->
           {| let open $id:i in $e |}
       (* FIXME Ant should be able to be followed *)      
       | `Ant (("list" as n),s) -> {| $(anti:mk_anti ~c:"exp;" n s) |}
       | exp{e}; sequence'{k} -> k e ]
       sequence':
       [ -> fun e -> e
       | ";" -> fun e -> e
       | ";"; sequence{el} -> fun e -> {| $e; $el |} ]       
       infixop1:
       [  [ "&" | "&&" ]{x} -> {| $lid:x |} ]
       infixop0:
       [  [ "or" | "||" ]{x} -> {| $lid:x |} ]
       sem_exp_for_list:
       [ exp{e}; ";"; S{el} -> fun acc -> {| [ $e :: $(el acc) ] |}
       | exp{e}; ";" -> fun acc -> {| [ $e :: $acc ] |}
       | exp{e} -> fun acc -> {| [ $e :: $acc ] |} ]
       comma_exp:
       [ S{e1}; ","; S{e2} -> {| $e1, $e2 |}
       | `Ant (("list" as n),s) -> {| $(anti:mk_anti ~c:"exp," n s) |}
       | exp Level "top"{e} -> e ]
       dummy:
       [ -> () ] |};
  {:extend| with_exp_lang:
    [ lang{old}; ":"; exp{x} -> (AstQuotation.default := old; x)] |} ;
  {:extend| with_stru_lang:
    [lang{old};":"; stru{x} -> (AstQuotation.default:=old;x)]
  |};
  with binding
      {:extend|
        binding_quot:
        [ binding{x} -> x  ] 
        binding:
        [ `Ant (("binding"|"list" as n),s) ->
          {| $(anti:mk_anti ~c:"binding" n s) |}
        | `Ant ((""|"anti" as n),s); "="; exp{e} ->
            {| $(anti:mk_anti ~c:"pat" n s) = $e |}
        | `Ant ((""|"anti" as n),s) -> {| $(anti:mk_anti ~c:"binding" n s) |}
        | S{b1}; "and"; S{b2} -> {| $b1 and $b2 |}
        | let_binding{b} -> b ] 
        let_binding:
        [ pat{p}; fun_binding{e} -> {| $p = $e |} ] |};

  with case
    {:extend|
      case:
      [ "["; L1 case0 SEP "|"{l}; "]" -> bar_of_list l (* {|  $list:l  |} *) (* FIXME *)
      | pat{p}; "->"; exp{e} -> `Case(_loc,p,e)(* {| $pat:p -> $e |} *) ]
      case0:
      [ `Ant (("case"|"list"| "anti"|"" as n),s) ->
        `Ant (_loc, (mk_anti ~c:"case" n s))
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
        [ `Ant (("rec_exp" |""|"anti"|"list" as n),s) -> 
          `Ant (_loc, (mk_anti ~c:"rec_exp" n s))
        | label_longident{i}; fun_binding{e} -> {| $id:i = $e |}
        | label_longident{i} ->
            `RecBind (_loc, i, (`Id (_loc, (`Lid (_loc, (FanOps.to_lid i))))))]
        field_exp:
        [ `Ant ((""|"bi"|"anti" |"list" as n),s) -> {| $(anti:mk_anti ~c:"rec_exp" n s) |}
        | a_lident{l}; "=";  exp Level "top"{e} ->
            `RecBind (_loc, (l:>ident), e) (* {| $lid:l = $e |} *) ]
        label_exp_list:
        [ label_exp{b1}; ";"; S{b2} -> {| $b1 ; $b2 |}
        | label_exp{b1}; ";"            -> b1
        | label_exp{b1}                 -> b1  ]
        field_exp_list:
        [ field_exp{b1}; ";"; S{b2} -> {| $b1 ; $b2 |}
        | field_exp{b1}; ";"            -> b1
        | field_exp{b1}                 -> b1  ] |};
  with pat
    {:extend| local: pat_constr;
       pat_quot:
       [ pat{x}; ","; comma_pat{y} -> `Com(_loc,x,y)
       | pat{x}; ";"; sem_pat{y} -> `Sem(_loc,x,y)
       | pat{x} -> x]
       pat_as_pat_opt:
       [ pat{p1}; "as"; a_lident{s} -> {| ($p1 as $s) |}
       | pat{p} -> p ]
       pat_constr:
       [module_longident{i} -> {| $id:i |}

       |"`"; luident{s}  -> {| $vrn:s|}
       |`Ant ((""|"pat"|"anti"|"vrn" as n), s) -> {|$(anti:mk_anti ~c:"pat" n s)|} ]
       pat:
       { "|" LA
        [ S{p1}; "|"; S{p2} -> {| $p1 | $p2 |} ]
       ".." NA
        [ S{p1}; ".."; S{p2} -> {| $p1 .. $p2 |} ]
       "apply" LA
        [ pat_constr{p1}; S{p2} ->
          match p2 with
            [ {| ($par:p) |} ->
              List.fold_left (fun p1 p2 -> {| $p1 $p2 |}) p1
                (list_of_com p []) (* precise *)
            | _ -> {|$p1 $p2 |}  ]
        | pat_constr{p1} -> p1
        | "lazy"; S{p} -> {| lazy $p |}  ]
       "simple"
        [ `Ant ((""|"pat"|"anti"|"par"|"int"|"`int"|"int32"|"`int32"|"int64"|"`int64"
                |"vrn"
                |"nativeint"|"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" as n),s)
          -> {| $(anti:mk_anti ~c:"pat" n s) |}
        | ident{i} -> {| $id:i |}
        | `INT(_,s) -> {|$int:s|}
        | `INT32(_,s) -> {|$int32:s|}
        | `INT64(_,s) -> {|$int64:s|}
        | `Flo(_,s) -> {|$flo:s|}
        | `CHAR(_,s) -> {|$chr:s|}
        | `STR(_,s) -> {|$str:s|}
        | "-"; `INT(_,s) -> {|$(int:String.neg s)|}
        | "-"; `INT32(_,s) -> {|$(int32:String.neg s)|}
        | "-"; `INT64(_,s) -> {|$(int64:String.neg s)|}
        | "-"; `NATIVEINT(_,s) -> {|$(int64:String.neg s)|}
        | "-"; `Flo(_,s) -> {|$(flo:String.neg s)|}
        | "["; "]" -> {| [] |}
        | "["; sem_pat_for_list{mk_list}; "::"; pat{last}; "]" -> mk_list last
        | "["; sem_pat_for_list{mk_list}; "]" -> mk_list {| [] |}
        | "[|"; "|]" -> `ArrayEmpty(_loc)
        | "[|"; sem_pat{pl}; "|]" -> `Array(_loc,pl)
        | "{"; label_pat_list{pl}; "}" -> `Record(_loc,pl)
            (* {| { $((pl : rec_pat :>pat)) } |} *)
        | "("; ")" -> {| () |}
        | "("; "module"; a_uident{m}; ")" -> `ModuleUnpack(_loc,m)
            (* {| (module $m) |} *)
        | "("; "module"; a_uident{m}; ":"; (* package_type *)module_type{pt}; ")" ->
            `ModuleConstraint(_loc,m, `Package(_loc,pt))
              (* {| ( module $m :  $pt )|} *)
        | "(";"module"; a_uident{m};":"; `Ant(("opt" as n),s ); ")" ->
            `ModuleConstraint (_loc, m, `Ant (_loc, (mk_anti n s)))
            (* {| (module $m : $(opt: `Ant(_loc,mk_anti n s)))|} *)
        | "("; S{p}; ")" -> p
        | "("; S{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
        | "("; S{p}; "as";  a_lident{s}; ")" -> {| ($p as $s )|}
        | "("; S{p}; ","; comma_pat{pl}; ")" -> {| ($p, $pl) |}
        | "`"; luident{s} -> {|$vrn:s|}
          (* duplicated may be removed later with [pat Level "apply"] *)
        | "#"; type_longident{i} -> {| # $i |}
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.pat_tag
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
            `OptLablExpr (_loc, i, p, (`Ant (_loc, (mk_anti n s))))
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
        | `Ant ((""|"pat"|"anti"|"par" as n),s) -> {| $(anti:mk_anti ~c:"pat" n s) |}
        | "("; ")" -> {| () |}
        | "("; "module"; a_uident{m}; ")" -> `ModuleUnpack(_loc,m)
            (* {| (module $m) |} *)
        | "("; "module"; a_uident{m}; ":"; (* package_type *)module_type{pt}; ")" ->
             `ModuleConstraint (_loc, m, ( (`Package (_loc, pt))))
              (* {| (module $m : $pt )|} *)
        | "(";"module"; a_uident{m};":"; `Ant(("opt" as n),s ); ")" ->
             `ModuleConstraint (_loc, m, (`Ant (_loc, (mk_anti n s))))
            (* {| (module $m : $(opt: `Ant(_loc,mk_anti n s)))|} *)
        | "("; S{p}; ")" -> p
        | "("; S{p}; ":"; ctyp{t}; ")" -> {| ($p : $t) |}
        | "("; S{p}; "as"; a_lident{s}; ")" -> {| ($p as $s) |}
        | "("; S{p}; ","; comma_ipat{pl}; ")" -> {| ($p, $pl) |}
        | a_lident{s} -> {| $(id:(s:>ident)) |}
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.pat_tag                            
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
            `OptLablExpr (_loc, i, p, (`Ant (_loc, (mk_anti n s))))
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
            (* {| ? ($p = $e) |} *)
   ]
       sem_pat:
       [`Ant (("list" as n),s) -> {| $(anti:mk_anti ~c:"pat;" n s) |}
       | pat{p1}; ";"; S{p2} -> `Sem(_loc,p1,p2)
       | pat{p}; ";" -> p
       | pat{p} -> p ] 
       sem_pat_for_list:
       [ pat{p}; ";"; S{pl} -> fun acc -> {| [ $p :: $(pl acc) ] |}
       | pat{p}; ";" -> fun acc -> {| [ $p :: $acc ] |}
       | pat{p} -> fun acc -> {| [ $p :: $acc ] |}  ]
       pat_tcon:
       [ pat{p}; ":"; ctyp{t} -> {| ($p : $t) |}
       | pat{p} -> p ]
       ipat_tcon:
       [ `Ant((""|"anti" as n),s) -> {| $(anti:mk_anti ~c:"pat" n s ) |}
       | a_lident{i} -> {|$(id:(i:>ident))|}
       | a_lident{i}; ":"; ctyp{t} -> {| ($(id:(i:>ident)) : $t) |}]
       comma_ipat:
       [ S{p1}; ","; S{p2} -> {| $p1, $p2 |}
       | `Ant (("list" as n),s) -> {| $(anti:mk_anti ~c:"pat," n s) |}
       | ipat{p} -> p ]
       comma_pat:
       [ S{p1}; ","; S{p2} -> {| $p1, $p2 |}
       | `Ant (("list" as n),s) -> {| $(anti:mk_anti ~c:"pat," n s) |}
       | pat{p} -> p ]
       label_pat_list:
       [ label_pat{p1}; ";"; S{p2} -> {| $p1 ; $p2 |}
       | label_pat{p1}; ";"; "_"       -> {| $p1 ; _ |}
       | label_pat{p1}; ";"; "_"; ";"  -> {| $p1 ; _ |}
       | label_pat{p1}; ";"            -> p1
       | label_pat{p1}                 -> p1   ] 
       label_pat:
       [ `Ant ((""|"pat"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"pat" n s) |}
       (* | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.pat_tag
        *) (* FIXME restore it later *)
       | `Ant (("list" as n),s) -> {| $(anti:mk_anti ~c:"pat;" n s) |}
       | label_longident{i}; "="; pat{p} -> (* {| $i = $p |} *) `RecBind(_loc,i,p)
       | label_longident{i} ->
           `RecBind(_loc,i,`Id(_loc,`Lid(_loc,FanOps.to_lid i)))
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
      `Ant (n,s) -> `Ant(_loc,mk_anti n s)]
      ident_quot:
      { "."
        [ S{i}; "."; S{j} -> {| $i.$j  |} ]
        "simple"
        [ `Ant ((""|"id"|"anti"|"list" |"uid" as n),s) ->
          `Ant (_loc, (mk_anti ~c:"ident" n s))
        | `Ant (("lid" as n), s) -> `Ant (_loc, (mk_anti ~c:"ident" n s))
        | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s); "."; S{i} ->
            `Dot (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s))), i)
        | `Lid i -> {| $lid:i |}
        | `Uid i -> {| $uid:i |}
        | `Uid s ; "." ; S{j} -> {|$uid:s.$j|}
        | "("; S{i};S{j}; ")" -> `App _loc i j  ] }

      (* parse [a] [b], [a.b] [A.b]*)
      ident:
      [ `Ant ((""|"id"|"anti"|"list" |"uid" as n),s) -> `Ant (_loc, mk_anti ~c:"ident" n s)
      | `Ant (("lid" as n), s) -> `Ant (_loc, mk_anti ~c:"ident" n s)
      | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s); "."; S{i} ->
           `Dot (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s))), i)
      | `Lid i -> `Lid(_loc,i)
      | `Uid i -> `Uid(_loc,i)
      | `Uid s ; "." ; S{j} ->  `Dot (_loc, `Uid (_loc, s), j)]

      uident:
      [`Uid s -> `Uid(_loc,s)
      | `Ant((""|"id"|"anti"|"list"|"uid" as n),s) ->
          `Ant(_loc,mk_anti ~c:"uident" n s)
      |`Uid s; "."; S{l} -> dot (`Uid (_loc,s)) l
      |`Ant((""|"id"|"anti"|"list"|"uid" as n),s) ;"." ; S{i} ->
          dot (`Ant(_loc,mk_anti ~c:"uident" n s)) i]


      dot_namespace:
      [ `Uid i; "."; S{xs} -> [i::xs]
      | `Uid i -> [i]]
      (* parse [a.b.c] no antiquot *)
      dot_lstrings:
      [ `Lid i -> (`Sub[],i)
      | `Uid i ; "." ; S {xs} ->
          match xs with
          [(`Sub xs,v) -> (`Sub [i::xs],v)
          | _ -> raise (XStream.Error "impossible dot_lstrings")]  

      | "."; `Uid i; "."; S{xs} ->
          match xs with
          [(`Sub xs,v) -> (`Absolute [i::xs],v)
          | _ -> raise (XStream.Error "impossible dot_lstrings") ]]

      (* parse [A.B.(] *)
      module_longident_dot_lparen:
      [ `Ant ((""|"id"|"anti"|"list"|"uid" as n),s); "."; "(" ->
        {| $(anti:mk_anti ~c:"ident" n s) |}

      | `Uid i; "."; S{l} -> {|$uid:i.$l|}
      | `Uid i; "."; "(" -> {|$uid:i|}
      | `Ant (("uid"|"" as n),s); "."; S{l} -> {| $(anti:mk_anti ~c:"ident" n s).$l|} ]
      (* parse [A.B] *)
      module_longident:
      [ `Ant ((""|"id"|"anti"|"list" as n),s) ->
        {| $(anti:mk_anti ~c:"ident" n s) |}
      | `Uid i; "."; S{l} -> {| $uid:i.$l|}
      | `Uid i -> {|$uid:i|}
      | `Ant ((""|"uid" as n),s) -> {| $(anti:mk_anti ~c:"ident" n s) |}
      | `Ant((""|"uid" as n), s); "."; S{l} -> {| $(anti:mk_anti ~c:"ident" n s).$l |} ]

      module_longident_with_app:
      { "apply"
        [ S{i}; S{j} -> {| ($i $j) |} ]
       "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
       "simple"
        [ `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
          `Ant (_loc, (mk_anti ~c:"ident" n s))
        | `Uid i -> `Uid(_loc,i)
        | "("; S{i}; ")" -> i ] }

      (* parse [(A B).c ]*)
      type_longident: (* FIXME *)
      { "apply" (* No parens *)
        [ S{i}; S{j} -> {| ($i $j) |} ]
        "."
        [ S{i}; "."; S{j} -> {| $i.$j |} ]
        "simple"
        [ `Ant ((""|"id"|"anti"|"list"|"uid"|"lid" as n),s) ->
          {| $(anti:mk_anti ~c:"ident" n s) |}
        | `Lid i -> {|$lid:i|}
        | `Uid i -> {|$uid:i|}
        | "("; S{i}; ")" -> i ] }

      label_longident:
      [ `Ant ((""|"id"|"anti"|"list"|"lid" as n),s) ->
        {| $(anti:mk_anti ~c:"ident" n s) |}
      | `Lid i -> {|$lid:i|}
      | `Uid i; "."; S{l} -> {|$uid:i.$l|}
      | `Ant((""|"uid" as n),s); "."; S{l} -> {|$(anti:mk_anti ~c:"ident" n s).$l|} ]
      
      class_type_longident: [ type_longident{x} -> x ]
      val_longident:[ ident{x} -> x ]
      class_longident: [ label_longident{x} -> x ]
      
      method_opt_override:
      [ "method"; "!" -> {:override_flag| ! |}
      | "method"; `Ant (((""|"override"|"anti") as n),s) ->
          `Ant (_loc,mk_anti ~c:"override_flag" n s)
      | "method" -> {:override_flag||}  ] 
      opt_override:
      [ "!" -> {:override_flag| ! |}
      | `Ant ((("!"|"override"|"anti") as n),s) ->
          `Ant (_loc,mk_anti ~c:"override_flag" n s)
      | -> {:override_flag||} ]
      
      value_val_opt_override:
      [ "val"; "!" -> `Override _loc
      | "val"; `Ant (((""|"override"|"anti"|"!") as n),s) ->
            `Ant (_loc,mk_anti ~c:"override_flag" n s)
      | "val" -> `OvNil _loc]
      direction_flag:
      [ "to" -> {:direction_flag| to |}
      | "downto" -> {:direction_flag| downto |}
      | `Ant (("to"|"anti"|"" as n),s) ->
          `Ant (_loc,mk_anti ~c:"direction_flag" n s)]

      opt_private:
      [ "private" -> {:private_flag| private |}
      | `Ant (("private"|"anti" as n),s) ->
          `Ant (_loc,mk_anti ~c:"private_flag" n s)
      | -> {:private_flag||}  ] 
      opt_mutable:
      [ "mutable" -> {:mutable_flag| mutable |}
      | `Ant (("mutable"|"anti" as n),s) ->
          `Ant (_loc,mk_anti ~c:"mutable_flag" n s)
      | -> {:mutable_flag||}  ] 
      opt_virtual:
      [ "virtual" -> {:virtual_flag| virtual |}
      | `Ant (("virtual"|"anti" as n),s) -> `Ant (_loc,mk_anti ~c:"virtual_flag" n s)
      | -> {:virtual_flag||}  ] 
      opt_dot_dot:
      [ ".." -> `RowVar _loc
      | `Ant ((".."|"anti" as n),s) -> `Ant (_loc,mk_anti ~c:"row_var_flag" n s)
      | -> `RvNil _loc   ]

      (*opt_rec@inline *)
      opt_rec:
      [ "rec" -> `Recursive _loc
      | `Ant (("rec"|"anti" as n),s) -> `Ant (_loc,mk_anti ~c:"rec_flag" n s)
      | -> `ReNil _loc]

      a_string:
      [ `Ant((""|"lid") as n,s) -> `Ant (_loc, mk_anti n s)
      |  `Lid s -> `C (_loc, s )
      |  `Uid s -> `C (_loc,s)]
      a_lident:
      [ `Ant((""|"lid") as n,s) -> `Ant (_loc,mk_anti ~c:"a_lident" n s)
      | `Lid s  -> `Lid (_loc, s) ]
      a_uident:
      [ `Ant((""|"uid") as n,s) -> `Ant (_loc,mk_anti ~c:"a_uident" n s)
      | `Uid s  -> `Uid (_loc, s) ]
      string_list:
      [ `Ant ((""),s) -> `Ant (_loc,mk_anti "str_list" s)
      | `Ant("",s) ; S{xs} -> `App(_loc,`Ant(_loc,mk_anti "" s), xs)
      | `STR (_, x) -> `Str(_loc,x)
      | `STR (_, x); S{xs} -> `App(_loc,`Str(_loc,x),xs)]
      rec_flag_quot:  [ opt_rec{x} -> x ]
      direction_flag_quot:  [ direction_flag{x} -> x ] 
      mutable_flag_quot: [  opt_mutable{x} -> x ] 
      private_flag_quot: [  opt_private{x} -> x ]
      virtual_flag_quot: [  opt_virtual{x} -> x ] 
      row_var_flag_quot: [  opt_dot_dot{x} -> x ] 
      override_flag_quot:[  opt_override{x} -> x ] 
      pat_eoi:  [ pat{x}; `EOI -> x ] 
      exp_eoi:  [ exp{x}; `EOI -> x ]  |};
  with stru
    {:extend|
    (* ml entrance *)    
      implem:
      [ "#"; a_lident{n}; exp{dp}; ";;" ->
        ([ `Directive(_loc,n,dp) ],  Some _loc)
      | "#"; a_lident{n}; ";;" ->
        ([`DirectiveSimple(_loc,n)], Some _loc)
      | "#";"import"; dot_namespace{x};";;" -> 
          (FanToken.paths := [ `Absolute  x :: !FanToken.paths];
            ([`DirectiveSimple(_loc,`Lid(_loc,"import"))],Some _loc))

      | stru{si}; ";"; S{(sil, stopped)} -> ([si :: sil], stopped)
      | stru{si}; ";;"; S{(sil, stopped)} -> ([si :: sil], stopped)
            (* FIXME merge with the above in the future*)            
      | `EOI -> ([], None) ]

      (* used by module .... end *)
      strus: (* FIXME dump seems to be incorrect *)
      [ `Ant ((""|"stri"|"anti"|"list" as n),s) -> {| $(anti:mk_anti n ~c:"stru" s) |}
      | `Ant ((""|"stri"|"anti"|"list" as n),s); ";"; S{st} ->
          {| $(anti:mk_anti n ~c:"stru" s); $st |}
      | L1 [ stru{st}; ";" -> st ]{l} -> sem_of_list l
      | L1 [ stru{st}; ";;" -> st ]{l} -> sem_of_list l ]
      top_phrase:
      [ "#"; a_lident{n}; exp{dp}; ";;" -> Some (`Directive(_loc,n,dp))
      | "#"; a_lident{n}; ";;" -> Some (`DirectiveSimple(_loc,n))
      | "#";"import"; dot_namespace{x} ->
          (FanToken.paths := [ `Absolute  x :: !FanToken.paths];
           None)
      | stru{st}; ";" -> Some st
      | `EOI -> None ]
      stru_quot:
      [ "#"; a_lident{n}; exp{dp} -> `Directive(_loc,n,dp)
      | "#"; a_lident{n} -> `DirectiveSimple(_loc,n)
      | stru{st1}; ";";S{st2} -> `Sem(_loc,st1,st2)
      | stru{st} -> st]

      stru:
      { "top"
        [ "exception"; constructor_declaration{t} -> `Exception(_loc,t)
        (* | "exception"; constructor_declaration{t}; "="; type_longident{i} -> *)
        (*     {| exception $t = $i |} *)
        | "external"; a_lident{i};":"; ctyp{t};"="; string_list{sl} ->
            `External (_loc, i, t, sl)
        | "include"; module_exp{me} -> `Include(_loc,me)
        | "module"; a_uident{i}; module_binding0{mb} -> `Module(_loc,i,mb)
        | "module"; "rec"; module_binding{mb} -> `RecModule(_loc,mb)
        | "module"; "type"; a_uident{i}; "="; module_type{mt} ->
            `ModuleType(_loc,i,mt)
        | "open"; module_longident{i} -> `Open(_loc,i)
        | "type"; type_declaration{td} -> `Type(_loc,td)
        | "let"; opt_rec{r}; binding{bi}; "in"; exp{x} ->
              {| let $rec:r $bi in $x |}
        | "let"; opt_rec{r}; binding{bi} ->
            match bi with
            [ `Bind(_loc,`Any _,e) -> `StExp(_loc,e)
            | _ -> `Value(_loc,r,bi) ]
        | "let"; "module"; a_uident{m}; module_binding0{mb}; "in"; exp{e} ->
              {| let module $m = $mb in $e |}
        | "let"; "open"; module_longident{i}; "in"; exp{e} ->
            {| let open $id:i in $e |}
        | "let"; "try"; opt_rec{r}; binding{bi}; "in"; exp{x}; "with"; case{a}
          -> {| let try $rec:r $bi in $x with [ $a ]|}
        | "class"; class_declaration{cd} ->  `Class(_loc,cd)
        | "class"; "type"; class_type_declaration{ctd} ->
            `ClassType (_loc, ctd)
        | `Ant ((""|"stri"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"stru" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.stru_tag
        | exp{e} -> `StExp(_loc,e)
              (* this entry makes {| let $rec:r $bi in $x |} parsable *)
        ] }   |};

  with class_sig_item
    {:extend|
      class_sig_item_quot:
      [ class_sig_item{x1}; ";"; S{x2} -> `Sem(_loc,x1,x2)
      | class_sig_item{x} -> x]
      class_signature:
      [ `Ant ((""|"csg"|"anti"|"list" as n),s) ->
        {| $(anti:mk_anti ~c:"class_sig_item" n s) |}
      | `Ant ((""|"csg"|"anti"|"list" as n),s);";"; S{csg} ->
          {| $(anti:mk_anti ~c:"class_sig_item" n s); $csg |}
      | L1 [ class_sig_item{csg};";" -> csg ]{l} -> sem_of_list l ]
      class_sig_item:
      [ `Ant ((""|"csg"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"class_sig_item" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.class_sig_item_tag
      | "inherit"; class_type{cs} -> `SigInherit(_loc,cs)

      | "val"; opt_mutable{mf}; opt_virtual{mv};a_lident{l}; ":"; ctyp{t} ->
          {| val $mutable:mf $virtual:mv $l : $t |}
      | "method"; "virtual"; opt_private{pf}; a_lident{l}; ":";ctyp{t} ->
          {| method virtual $private:pf $l : $t |}
      | "method"; opt_private{pf}; a_lident{l}; ":";ctyp{t} ->
          {| method $private:pf $l : $t |}
      | "constraint"; ctyp{t1}; "="; ctyp{t2} -> {|constraint $t1 = $t2|} ] |};  
  with cstru
    {:extend|
      class_structure:
        [ `Ant ((""|"cst"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"cstru" n s) |}
        | `Ant ((""|"cst"|"anti"|"list" as n),s); (* semi *)";"; S{cst} ->
            {| $(anti:mk_anti ~c:"cstru" n s); $cst |}
        | L1 [ cstru{cst}; (* semi *)";" -> cst ]{l} -> sem_of_list l  ]
      cstru:
        [ `Ant ((""|"cst"|"anti"|"list" as n),s) ->
            {| $(anti:mk_anti ~c:"cstru" n s) |}
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.cstru_tag
        | "inherit"; opt_override{o}; class_exp{ce}(* ; opt_as_lident{pb} *) ->
            `Inherit(_loc,o,ce)
        | "inherit"; opt_override{o}; class_exp{ce}; "as"; a_lident{i} ->
            `InheritAs(_loc,o,ce,i)
        | value_val_opt_override{o}; opt_mutable{mf}; a_lident{lab}; cvalue_binding{e}
          ->
            {| val $override:o $mutable:mf $lab = $e |}
        | value_val_opt_override{o}; "virtual"; opt_mutable{mf}; a_lident{l}; ":";
                (* poly_type *)ctyp{t} ->
                match o with
                [ {:override_flag@_||} ->{| val virtual $mutable:mf $l : $t |}
                | _ -> raise (XStream.Error "override (!) is incompatible with virtual")]                    
        | method_opt_override{o}; "virtual"; opt_private{pf}; a_lident{l}; ":";
                (* poly_type *)ctyp{t} ->
                match o with
                [ {:override_flag@_||} -> `CrVir (_loc, l, pf, t)
                | _ -> raise (XStream.Error "override (!) is incompatible with virtual")]  

       | method_opt_override{o}; opt_private{pf}; a_lident{l}; ":"; ctyp{t} (* opt_polyt{topt} *);
                fun_binding{e} ->
                  `CrMth(_loc,l,o,pf,e,t)
            (* {| method $override:o $private:pf $l : $topt = $e |} *)
       | method_opt_override{o}; opt_private{pf};a_lident{l}; fun_binding{e} ->
           `CrMthS(_loc,l,o,pf,e)
             
       | "constraint"; ctyp{t1}; "="; ctyp{t2} ->
          {|constraint $t1 = $t2|}
        | "initializer"; exp{se} -> {| initializer $se |} ]
      cstru_quot:
        [ cstru{x1}; (* semi *)";"; S{x2} -> `Sem(_loc,x1,x2)
        | cstru{x} -> x]
    |};
    
  with class_exp
    {:extend|
      class_exp_quot:
      [ S{ce1}; "and"; S{ce2} -> {| $ce1 and $ce2 |}
      | S{ce1}; "="; S{ce2} -> {| $ce1 = $ce2 |}
      (* | "virtual";   class_name_and_param{(i, ot)} -> *)
      (*     `ClassCon (_loc, `Virtual _loc, (i :>ident), ot) *)
      (* | `Ant (("virtual" as n),s); ident{i}; opt_comma_ctyp{ot} -> *)
      (*     let anti = `Ant (_loc,mk_anti ~c:"class_exp" n s) in *)
      (*     `ClassCon (_loc, anti, i, ot) *)
      | class_exp{x} -> x]
      class_declaration:
      [ S{c1}; "and"; S{c2} -> {| $c1 and $c2 |}
      | `Ant ((""|"cdcl"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"class_exp" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.class_exp_tag
      | class_info_for_class_exp{ci}; class_fun_binding{ce} -> {| $ci = $ce |} ]
      class_fun_binding:
      [ "="; class_exp{ce} -> ce
      | ":"; class_type_plus{ct}; "="; class_exp{ce} -> `Constraint(_loc,ce,ct)
      | ipat{p}; S{cfb} -> {| fun $p -> $cfb |}  ]
      class_info_for_class_exp:
      [ opt_virtual{mv};  a_lident{i}; "["; comma_type_parameter{x}; "]" -> 
        `ClassCon(_loc,mv,(i:>ident),x)
      | opt_virtual{mv}; a_lident{i} -> `ClassConS(_loc,mv, (i:>ident))]
      class_fun_def:
      [ ipat{p}; S{ce} -> {| fun $p -> $ce |}  | "->"; class_exp{ce} -> ce ]
      class_exp:
      { "top"
          [ "fun"; ipat{p}; class_fun_def{ce} -> {| fun $p -> $ce |}
          | "function"; ipat{p}; class_fun_def{ce} -> {| fun $p -> $ce |}
          | "let"; opt_rec{rf}; binding{bi}; "in"; S{ce} -> `LetIn(_loc,rf,bi,ce)]
        "apply" NA
          [ S{ce}; exp Level "label"{e} -> {| $ce $e |} ]
        "simple"
          [ `Ant ((""|"cexp"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"class_exp" n s) |}
          | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.class_exp_tag
          | class_longident_and_param{ce} -> ce

          | "object"; "("; pat{p}; ")" ; class_structure{cst};"end" ->
              `ObjPat(_loc,p,cst)
          | "object"; "("; pat{p}; ")" ;"end" ->
              `ObjPatEnd(_loc,p)
          | "object";"("; pat{p};":";ctyp{t};")"; class_structure{cst};"end" ->
              `ObjPat(_loc,`Constraint(_loc,p,t),cst)
          | "object";"("; pat{p};":";ctyp{t};")"; "end" ->
              `ObjPatEnd(_loc,`Constraint(_loc,p,t))
          | "object"; class_structure{cst};"end" -> `Obj(_loc,cst)
          | "object";"end" -> `ObjEnd(_loc)
          | "("; S{ce}; ":"; class_type{ct}; ")" -> `Constraint(_loc,ce,ct)
          | "("; S{ce}; ")" -> ce ] }
      class_longident_and_param:
      [ class_longident{ci}; "["; comma_ctyp{t}; "]" ->
        `ClassCon (_loc, `ViNil _loc, ci, t)
      | class_longident{ci} ->
          `ClassConS(_loc,`ViNil _loc,ci)
          (* {| $id:ci |} *)  ]  |};
  with class_type
    {:extend|
      class_description:
      [ S{cd1}; "and"; S{cd2} -> {| $cd1 and $cd2 |}
      | `Ant ((""|"typ"|"anti"|"list" as n),s) ->
          {| $(anti:mk_anti ~c:"class_type" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.class_type_tag
      | class_info_for_class_type{ci}; ":"; class_type_plus{ct} -> {| $ci : $ct |}  ]
      class_type_declaration:
      [ S{cd1}; "and"; S{cd2} -> {| $cd1 and $cd2 |}
      | `Ant ((""|"typ"|"anti"|"list" as n),s) -> {| $(anti:mk_anti ~c:"class_type" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.class_type_tag
      | class_info_for_class_type{ci}; "="; class_type{ct} ->
          `Eq(_loc,ci,ct) ]
      class_info_for_class_type:
      [ opt_virtual{mv};  a_lident{i};"["; comma_type_parameter{x}; "]" ->
        `ClassCon(_loc,mv,(i:>ident),x)
      | opt_virtual{mv}; a_lident{i} -> `ClassConS(_loc,mv,(i:>ident))]
      class_type_quot:
      [ S{ct1}; "and"; S{ct2} -> `And(_loc,ct1,ct2)
      | S{ct1}; "="; S{ct2} -> `Eq(_loc,ct1,ct2)
      | S{ct1}; ":"; S{ct2} -> {| $ct1 : $ct2 |}
      (* | "virtual";  class_name_and_param{(i, ot)} -> *)
      (*     `ClassCon (_loc, `Virtual _loc, (i :>ident), ot) *)
          (* {| virtual $((i:>ident)) [ $ot ] |} (\* types *\) *)
      (* | `Ant (("virtual" as n),s); ident{i}; opt_comma_ctyp{ot} -> *)
      (*     let anti = `Ant (_loc,mk_anti ~c:"class_type" n s) in *)
      (*     `ClassCon (_loc, anti, i, ot) *)
      | `Ant (("virtual" as n),s); ident{i}; "["; comma_ctyp{t}; "]" ->
          let anti = `Ant (_loc,mk_anti ~c:"class_type" n s) in
          `ClassCon(_loc,anti,i,t)
      | `Ant (("virtual" as n),s); ident{i} ->
          let anti = `Ant (_loc,mk_anti ~c:"class_type" n s) in
          `ClassConS(_loc,anti,i)
      | class_type_plus{x} -> x]
      class_type_plus:
      [ "["; ctyp{t}; "]"; "->"; S{ct} -> `CtFun(_loc,t,ct)
      | class_type{ct} -> ct ]
      class_type:
      [ `Ant ((""|"ctyp"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"class_type" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.class_type_tag
      | class_type_longident_and_param{ct} -> ct
      | "object";"(";ctyp{t};")";class_signature{csg};"end" -> `ObjTy(_loc,t,csg)
      | "object";class_signature{csg};"end"-> `Obj(_loc,csg)
      | "object"; "(";ctyp{t};")" -> `ObjTyEnd(_loc,t)
      | "object"; "end" -> `ObjEnd(_loc)]
      class_type_longident_and_param:
      [ class_type_longident{i}; "["; comma_ctyp{t}; "]" ->
        `ClassCon (_loc, (`ViNil _loc), i, t)
      | class_type_longident{i} -> `ClassConS(_loc,`ViNil _loc,i)] |} ;
end;


let apply_ctyp () = begin
  with ctyp
    {:extend|
      ctyp_quot:
      [more_ctyp{x}; "*"; star_ctyp{y} -> `Sta (_loc, x, y)
      | more_ctyp{x} -> x ]
      more_ctyp:
      [ctyp{x} -> x (* | type_parameter{x} -> x *)   ]
      unquoted_typevars:
      [ S{t1}; S{t2} -> {| $t1 $t2 |}
      | `Ant ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.ctyp_tag
      | a_lident{i} -> `Id(_loc,(i:>ident))]
      type_parameter:
      [ `Ant ((""|"typ"|"anti" as n),s) -> `Ant (_loc, (mk_anti n s))
      (* | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.ctyp_tag *)
      | "'"; a_lident{i} -> `Quote(_loc,`Normal _loc, i)
      | "+"; "'"; a_lident{i} ->
          `Quote (_loc, `Positive _loc,  i)
      | "-"; "'"; a_lident{i} -> `Quote (_loc, (`Negative _loc),  i)
      | "+"; "_" -> `QuoteAny (_loc, `Positive _loc)
      | "-"; "_" -> `QuoteAny (_loc, `Negative _loc)
      | "_" ->  `Any _loc]
      type_longident_and_parameters:
      [ "("; type_parameters{tpl}; ")";type_longident{i} -> tpl {| $id:i|}
      | type_parameter{tpl} ; type_longident{i} -> `App(_loc,{|$id:i|},(tpl:>ctyp))
      | type_longident{i} -> {|$id:i|} 
      | `Ant ((""|"anti" as n),s) -> {|$(anti:mk_anti n s ~c:"ctyp")|} ]
      type_parameters:
      [ type_parameter{t1}; S{t2} ->
        fun acc -> t2 (`App(_loc,acc, (t1:>ctyp)))
      | type_parameter{t} -> fun acc -> `App(_loc,acc, (t:>ctyp))
      | -> fun t -> t  ]
      meth_list:
      [ meth_decl{m}; ";"; S{(ml, v) }  -> (`Sem(_loc,m,ml)(* {| $m; $ml |} *), v)
      | meth_decl{m}; ";"; opt_dot_dot{v} -> (m, v)
      | meth_decl{m}; opt_dot_dot{v}      -> (m, v)  ]
      meth_decl:
      [ `Ant ((""|"typ" as n),s)        -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `Ant (("list" as n),s)          -> {| $(anti:mk_anti ~c:"ctyp;" n s) |}
      (* | `QUOTATION x                       -> AstQuotation.expand _loc x FanDyn.ctyp_tag *)
      | a_lident{lab}; ":"; ctyp{t} -> `TyCol(_loc,`Id(_loc, (lab :> ident)),t)]
      opt_meth_list:
      [ meth_list{(ml, v) } -> `TyObj (_loc, ml, v)
      | opt_dot_dot{v}     -> `TyObjEnd(_loc,v) ]
      row_field:
      [ `Ant ((""|"typ" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp" n s))
      | `Ant (("list" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp|" n s))
      | S{t1}; "|"; S{t2} -> `Bar(_loc,t1,t2)
      | "`"; astr{i} -> (* {| `$i |} *) `TyVrn(_loc,i)
      (* | "`"; astr{i}; "of"; "&"; amp_ctyp{t} -> *)
      (*     `TyOfAmp (_loc, (`TyVrn (_loc, i)), t) *)
          (* {| `$i of & $t |} *)
      | `Ant(("vrn") as n, s) -> `TyVrn(_loc,`Ant(_loc,mk_anti ~c:"ctyp" n s))
      | `Ant(("vrn") as n, s) ; "of"; ctyp{t} ->
          `TyVrnOf(_loc,`Ant(_loc,(mk_anti ~c:"ctyp" n s)),t)
      | "`"; astr{i}; "of"; (* amp_ctyp *)ctyp{t} -> `TyVrnOf(_loc,i,t)
      | ctyp{t} -> `Ctyp(_loc,t) ]

      (* only used in row_field *)
      (* amp_ctyp: *)
      (* [ S{t1}; "&"; S{t2} -> `Amp(_loc,t1,t2) *)
      (* | `Ant (("list" as n),s) -> {| $(anti:mk_anti ~c:"ctyp&" n s) |} *)
      (* | ctyp{t} -> t ] *)

      (* only used in ctyps *)
      name_tags:
      [ `Ant ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | S{t1}; S{t2} -> `App (_loc, t1, t2)
      | "`"; astr{i} -> `TyVrn (_loc, i)  ]


      
      type_declaration:
      [ `Ant ((""|"typ"|"anti" as n),s) -> {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `Ant (("list" as n),s) ->          {| $(anti:mk_anti ~c:"ctypand" n s) |}
      (* | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.ctyp_tag *)
      | S{t1}; "and"; S{t2} ->  `And(_loc,t1,t2)
      |  type_ident_and_parameters{(n, tpl)}; "="; type_info{tk}; L0 constrain{cl}
        -> `TyDcl (_loc, n, tpl, tk,
                   match cl with [[]-> `None _loc | _ -> `Some(_loc,and_of_list cl)])
      | type_ident_and_parameters{(n,tpl)}; L0 constrain{cl} ->
          `TyAbstr(_loc,n,tpl,
                   match cl with [[] -> `None _loc | _ -> `Some(_loc, and_of_list cl)])]
      type_info:
      [type_repr{t2} -> `TyRepr(_loc,`PrNil _loc,t2)
      | ctyp{t1}; "="; type_repr{t2} -> `TyMan(_loc, t1, `PrNil _loc, t2)
      |  ctyp{t1} -> `TyEq(_loc,`PrNil _loc, t1)
      | "private"; ctyp{t1} -> `TyEq(_loc,`Private _loc,t1)
      |  ctyp{t1}; "=";"private"; type_repr{t2} ->
          `TyMan(_loc, t1, `Private _loc,t2)
      | "private"; type_repr{t2} -> `TyRepr(_loc,`Private _loc, t2)]

      type_repr:
      ["["; constructor_declarations{t}; "]" -> `Sum(_loc,t )
      (* | "["; "]" -> `Sum(_loc,`Nil _loc) *)
      | "{"; label_declaration_list{t}; "}" -> `Record (_loc, t)]
      type_ident_and_parameters:
      [ "(";  L1 type_parameter SEP ","{tpl}; ")"; a_lident{i} ->
        (i, `Some(_loc, com_of_list (tpl :> list decl_params)))
      |  type_parameter{t};  a_lident{i} ->
          (i, `Some (_loc,(t:>decl_params)))
      |  a_lident{i} -> (i, `None _loc)]
      constrain:
      [ "constraint"; ctyp{t1}; "="; ctyp{t2} -> `Eq(_loc,t1, t2) ]
      typevars:
      [ S{t1}; S{t2} -> {| $t1 $t2 |}
      | `Ant ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      | `Ant(("list" as n),s) ->     {| $(anti:mk_anti ~c:"forall" n s)|}
      | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.ctyp_tag
      | "'"; a_lident{i} ->  `Quote (_loc, `Normal _loc, i)
            (* {| '$i |} *)]
      ctyp:
      {
       "alias" LA
        [ S{t1}; "as"; "'"; a_lident{i} -> `Alias(_loc,t1,i)]
       "forall" LA
        [ "!"; typevars{t1}; "."; ctyp{t2} -> {| ! $t1 . $t2 |} ]
       "arrow" RA
        [ S{t1}; "->"; S{t2} ->  {| $t1 -> $t2 |} ]
       "label" NA
        [ "~"; a_lident{i}; ":"; S{t} -> {| ~ $i : $t |}
        | `LABEL s ; ":"; S{t} -> {| ~$lid:s : $t |}
        | `OPTLABEL s ; S{t} -> `OptLabl(_loc,`Lid(_loc,s),t)
        | "?"; a_lident{i}; ":"; S{t} -> `OptLabl(_loc,i,t)]
       "apply" LA
        [ S{t1}; S{t2} ->
          let t = `App(_loc,t1,t2) in
          try `Id(_loc,ident_of_ctyp t)
          with [ Invalid_argument _ -> t ]]
       "." LA
        [ S{t1}; "."; S{t2} ->
            try
              `Id (_loc, (`Dot (_loc, (ident_of_ctyp t1), (ident_of_ctyp t2))))
            with [ Invalid_argument s -> raise (XStream.Error s) ] ]
       "simple"
        [ "'"; a_lident{i} ->  `Quote (_loc, `Normal _loc,  i)
        | "_" -> `Any _loc
        | `Ant ((""|"typ"|"anti"|"par" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp" n s))
        | `Ant (("id" as n),s) -> `Id (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s))))
        | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.ctyp_tag
        | a_lident{i}->  `Id(_loc,(i:>ident))
        | a_uident{i} -> `Id(_loc,(i:>ident))
        | "("; S{t}; "*"; star_ctyp{tl}; ")" -> `Par (_loc, `Sta (_loc, t, tl))
        | "("; S{t}; ")" -> t
        | "[="; row_field{rfl}; "]" -> `PolyEq(_loc,rfl)
        (* | "[>"; "]" -> `PolySup (_loc, (`Nil _loc)) *) (* FIXME add later*)
        | "[>"; row_field{rfl}; "]" ->   `PolySup (_loc, rfl)
        | "[<"; row_field{rfl}; "]" -> `PolyInf(_loc,rfl)
        | "[<"; row_field{rfl}; ">"; name_tags{ntl}; "]" -> `PolyInfSup(_loc,rfl,ntl)
        | "#"; class_longident{i} -> {| #$i |}
        | "<"; opt_meth_list{t}; ">" -> t
        | "("; "module"; module_type{p}; ")" -> `Package(_loc,p)  ] }
      star_ctyp:
      [ `Ant ((""|"typ" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp" n s))
      | `Ant (("list" as n),s) ->  `Ant (_loc, (mk_anti ~c:"ctyp*" n s))
      | S{t1}; "*"; S{t2} -> `Sta(_loc,t1,t2)
      | ctyp{t} -> t  ]
      constructor_declarations:
      [ `Ant ((""|"typ" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp" n s)) 
      | `Ant (("list" as n),s) ->   `Ant (_loc, (mk_anti ~c:"ctyp|" n s))
      (* | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.ctyp_tag *)
      | S{t1}; "|"; S{t2} ->    `Bar(_loc,t1,t2)
      | a_uident{s}; "of"; constructor_arg_list{t} -> `Of(_loc,`Id(_loc,(s:>ident)),t)
      (* GADT to be improved *)      
      | a_uident{s}; ":"; ctyp{t} ->
          let (tl, rt) = FanOps.to_generalized t in
            (* {| $(id:(s:>ident)) : ($(FanAst.and_of_list tl) -> $rt) |} *)
            `TyCol
            (_loc, (`Id (_loc, (s :>ident))),
             match tl with [ [] -> rt | _ -> `Arrow (_loc,sta_of_list tl,rt)]
             (* (`Arrow (_loc, (sta_of_list tl), rt)) *))
      | a_uident{s} -> `Id(_loc,(s:>ident)) ]
      constructor_declaration:
      [ `Ant ((""|"typ" as n),s) ->  {| $(anti:mk_anti ~c:"ctyp" n s) |}
      (* | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.ctyp_tag *)
      | a_uident{s}; "of"; constructor_arg_list{t} -> `Of(_loc,`Id(_loc,(s:>ident)),t)
      | a_uident{s} -> `Id(_loc,(s:>ident))  ]
      constructor_arg_list:
      [ `Ant (("list" as n),s) ->  `Ant(_loc,mk_anti ~c:"ctyp*" n s)
      | S{t1}; "*"; S{t2} -> `Sta(_loc,t1,t2)
      | ctyp{t} -> t  ]
      label_declaration_list:
      [ label_declaration{t1}; ";"; S{t2} -> `Sem(_loc,t1,t2)
      | label_declaration{t1}; ";"            -> t1
      | label_declaration{t1}                 -> t1  ]
      label_declaration:
      [ `Ant ((""|"typ" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp" n s))
      | `Ant (("list" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp;" n s))
      (* | `QUOTATION x -> AstQuotation.expand _loc x FanDyn.ctyp_tag *)
      | a_lident{s}; ":"; ctyp{t} -> `TyCol (_loc, (`Id (_loc, (s :>ident))), t)
      | "mutable"; a_lident{s}; ":";  ctyp{t} -> `TyColMut(_loc,`Id(_loc,(s:>ident)),t)]
      comma_type_parameter:
      [ S{t1}; ","; S{t2} ->  `Com (_loc, t1, t2)
      | `Ant (("list" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp," n s))
      | type_parameter{t} -> `Ctyp(_loc, (t:>ctyp))  ]
      comma_ctyp:
      [ S{t1}; ","; S{t2} -> `Com (_loc, t1, t2) 
      | `Ant (("list" | "" as n),s) -> `Ant (_loc, (mk_anti ~c:"ctyp," n s))
      | ctyp{t} -> `Ctyp(_loc,t)  ]  |};
end;

  
AstParsers.register_parser
    ("revise",fun () -> begin apply (); apply_ctyp () end);









