%import{
Tokenf:
  mk_ant;
Fan_ops:
  ident_of_ctyp
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
};;
open Astf
open! Syntaxf


%create{Gramf pos_exps};;

%extend{primitve@Inline :
       [ Int s   %{`Int(_loc,s)}
       | Int32 s %{ `Int32(_loc,s)}
       | Int64 s %{ `Int64(_loc,s)}
       | Nativeint s %{ `Nativeint (_loc, s)}
       | Flo s  %{  `Flo (_loc, s)}
       | Chr s  %{ `Chr (_loc, s)}
       | Str s  %{ `Str (_loc, s)}
       | "true" %{`Bool (_loc,true)}
       | "false" %{`Bool (_loc,false)} ]};;

let make_infix ?(left=true)  exp f i =
  %extend{
  exp: ${f i} $bool:left
    [ S  as e1 ; Inf@xloc ($i,op); S as e2 %{
      let op = %exp@xloc{$lid:op} in %exp{$op $e1 $e2}}]}

let make_key ?(left=true) ?action exp i op =
  match action with
  | None -> 
      %extend{
      exp: $i $bool:left
        [ S as e1 ; $key:op @xloc; S as e2 %{
          let op = %exp@xloc{$lid:op} in %exp{$op $e1 $e2}}
        ]
    }
  | Some action ->
      %extend{exp: $i $bool:left
      [S ; $key:op ; S  ${action}
      ]}
        
let _ =
  let transform i =
    List.assoc i [(0,50);(1,60);(2,80);(3,90);(4,100)] in
  begin
    make_key exp 20 ~left:true
      ~action:(fun e2 _ e1 _loc -> (* reverse order *)
        match Fan_ops.bigarray_set _loc e1 e2 with
        | Some e -> e
        | None -> %exp{$e1 <- $e2 }) "<-";
    List.iter (make_key exp 20 ~left:true) [":="];
    List.iter (make_key exp 30 ~left:false) ["or"; "||"];
    List.iter (make_key exp 40 ~left:false) ["&";"&&"];
    List.iter (make_key exp 50 ~left:true) ["==";"=";"<";">"];
    List.iter (make_key exp 80 ~left:true) ["+";"-";"-."];
    make_infix exp transform 0;
    make_infix ~left:false exp transform 1;
    make_infix  exp transform 2;
    make_infix  exp transform 3;
    make_infix  exp transform 4;
  end
    
let make_case exp pat =
  %extend{
  pat_as_pat_opt@Local:
  [ pat as p1; "as"; a_lident as s %{  `Alias (_loc, p1, s)}
  | pat as p %{p} ]
  case:
  [ "|"; L1 case0 SEP "|" as l %{ bar_of_list l }
  | pat as p; "->"; exp as e %{ `Case(_loc,p,e)}
  ]
  case0:
  [ Ant ("case" | "", s) %{ mk_ant  s}
  | Ant ("",s) ;"when";exp as w;"->"; exp as e %{
    `CaseWhen(_loc,mk_ant  s, w,e )} (* %case{$p -> $e  }*)
  | Ant ("",s); "->"; exp as e %{
    `Case(_loc,mk_ant  s ,e)}
  | pat_as_pat_opt as p; "when"; exp as w;  "->"; exp as e %{
           `CaseWhen (_loc, p, w, e)}
  | pat_as_pat_opt as p; "->";exp as e %{ `Case(_loc,p,e)}]
  case_quot:
  [ L1 case0 SEP "|" as x %{ bar_of_list x}]  };;


let make_semi  atom nt =
  %extend{
  nt:
    [atom as b1; ";"; S as b2 %{`Sem(_loc,b1,b2)}
    |atom as b1; ?";" %{b1}]};;

let make_comma atom nt =
  %extend{
   nt:
    [ S as p1; ","; S as p2 %{`Com(_loc,p1,p2)}
    | atom as p %{p}]}

    
let make_ant ?(c="") ?(i=10) nt x=
    %extend{
  nt: $i
    [ Ant($x, s) %{mk_ant ~c s }]
  }

let make_ants ?c ?i nt xs =  List.iter (make_ant ?c ?i nt) xs
    
let make_quot tag ?(i=10) nt = (* FIXME *)
  %extend{
  nt: $i
    [Quot x %{Ast_quotation.expand x tag}]}
    
let make_pat exp =
  %extend{
       pat_quot:
       [ pat as x; ","; comma_pat as y %{ `Com(_loc,x,y)}
       | pat as x; ";"; sem_pat as y %{ `Sem(_loc,x,y)}
       | pat as x %{ x}]
       atom_pat@Inline:
        [ Quot x %{ Ast_quotation.expand  x Dyn_tag.pat}
        | "`"; luident as s  %pat{$vrn:s}
        | "_" %{ `Any _loc}
        | "~"; a_lident as i %{  `LabelS(_loc,i)}
        | Optlabel i; "("; pat_tcon as p; ?["="; exp as e]; ")" %{
            match e with
            |Some e -> `OptLablExpr(_loc,`Lid(_loc,i),p,e)
            | None -> `OptLabl(_loc,`Lid(_loc,i),p) }
        | "?"; a_lident as i;":"; "("; pat_tcon as p; ?[ "="; exp as e]; ")" %{
          match e with (* has a boolean binding so compiler could optimize it? *)
          | None -> `OptLabl(_loc,i,p)
          | Some e -> `OptLablExpr(_loc,i,p,e)
        }
        | "?"; a_lident as i;":"; "("; pat_tcon as p; "="; Ant("opt",s); ")" %{
            `OptLablExpr (_loc, i, p, mk_ant s)}

        | "?"; a_lident as i %{ `OptLablS(_loc,i ) }

        | "?"; "("; ipat_tcon as p; ? ["="; exp as e]; ")" %{
          match e with
          | Some e -> `OptLablExpr(_loc,`Lid(_loc,""),p,e)
          | None   -> `OptLabl(_loc,`Lid(_loc,""),p)}
        ]

       pat_constr@Local:
       [module_longident as i %{(i :vid :> pat)}
       |"`"; luident as s  %{ (`Vrn(_loc,s) :pat)}
       |Ant (""|"pat"|"vrn" , s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.pat) s}]
                     
       ep_constr@Local:
       [module_longident as i %{(i :vid :> ep)}
       |"`"; luident as s  %{ (`Vrn(_loc,s) :ep)}
       |Ant (""|"ep"|"vrn" , s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.ep) s}]

       ep: 30 RA
       [ S as p1; "::"; S as p2 %{
          `App(_loc,`Uid(_loc,"::"),`Par(_loc,`Com(_loc,p1,p2)))}]
       ep: 40
       [ ep_constr as p1; S as p2 %ep{$p1 $p2}
       | ep_constr as p1 %{ p1}
       ]
       sem_ep_for_list:
       [ ep as p; ";"; S as pl %{`App(_loc, `Uid(_loc,"::"), `Par(_loc,`Com(_loc,p,pl)))}
       | ep as p; ? ";"%{`App(_loc,`Uid(_loc,"::"),`Par(_loc,`Com(_loc,p,`Uid(_loc,"[]"))))}
       ]


       ep: 50
       [vid as i %{ (i : vid :> ep)}
       | @primitve
       | "-"; Int s %{  `Int (_loc, Stringf.neg s)}
       | "-"; Int32 s %{ `Int32(_loc, Stringf.neg s) }
       | "-"; Int64 s %{ `Int64(_loc,Stringf.neg s)}
       | "-"; Nativeint s %{ `Nativeint(_loc,Stringf.neg s)}
       | "-"; Flo s %{ `Flo(_loc,Stringf.neg s)}
       | "["; "]" %{ `Uid (_loc, "[]")}
       | "["; sem_ep_for_list as s; "]" %{ s}
       | "[|"; "|]" %{ `ArrayEmpty(_loc)}
       | "[|"; sem_ep as pl; "|]" %{ `Array(_loc,pl)}
       | "{"; label_ep_list as pl; "}" %{ `Record(_loc,pl)}
       | "("; ")" %{`Unit _loc}
       | "("; S as p; ")" %{ p}
       | "("; S as p; ":"; ctyp as t; ")" %{ `Constraint(_loc,p,t)}
       | "("; S as p; ","; comma_ep as pl; ")" %{ `Par(_loc,`Com(_loc,p,pl))}
      ]
              
       pat: 10 
       [ S as p1; "|"; S as p2 %{ `Bar(_loc,p1,p2)} ]
       pat: 20 
       [ S as p1; ".."; S as p2 %{ `PaRng(_loc,p1,p2)} ]

       pat: 30 RA
       [ S as p1; "::"; S as p2 %{
          `App(_loc,`Uid(_loc,"::"),`Par(_loc,`Com(_loc,p1,p2)))}]
       pat: 40 
       [ pat_constr as p1; S as p2 %pat{$p1 $p2}
       | pat_constr as p1 %{ p1}
       | "lazy"; S as p %{ `Lazy (_loc, p)}  ]
       pat: 50
       [ vid as i %{ (i : vid :> pat)}
       | @primitve
       | "-"; Int s %{  `Int (_loc, Stringf.neg s)}
       | "-"; Int32 s %{ `Int32(_loc, Stringf.neg s) }
       | "-"; Int64 s %{ `Int64(_loc,Stringf.neg s)}
       | "-"; Nativeint s %{ `Nativeint(_loc,Stringf.neg s)}
       | "-"; Flo s %{ `Flo(_loc,Stringf.neg s)}
       | "["; "]" %{ `Uid (_loc, "[]")}
       | "["; sem_pat_for_list as s; "]" %{ s}
       | "[|"; "|]" %{ `ArrayEmpty(_loc)}
       | "[|"; sem_pat as pl; "|]" %{ `Array(_loc,pl)}
       | "{"; label_pat_list as pl; "}" %{ `Record(_loc,pl)}
       | "("; ")" %{`Unit _loc}
       | "("; "module"; a_uident as m; ")" %{ `ModuleUnpack(_loc,m)}
       | "("; "module"; a_uident as m; ":"; mtyp as pt; ")" %{
            `ModuleConstraint(_loc,m, `Package(_loc,pt))}
       | "(" ; "module"; a_uident as m;":"; Ant("opt" ,s ); ")" %{
           `ModuleConstraint (_loc, m, mk_ant s)}
       | "("; S as p; ")" %{ p}
       | "("; S as p; ":"; ctyp as t; ")" %{ `Constraint(_loc,p,t)}
       | "("; S as p; "as";  a_lident as s; ")" %{ `Alias (_loc, p, s)}
       | "("; S as p; ","; comma_pat as pl; ")" %{ `Par(_loc,`Com(_loc,p,pl))}
       | "#"; type_longident as i %{ `ClassPath(_loc,i)}              

          (* duplicated may be removed later with [pat Level "apply"] *)
        
       | "~"; a_lident as i; ":"; S as p %{`Label (_loc, i, p)}
       | Label i; S as p   %{`Label (_loc, (`Lid (_loc, i)), p) }
       | @atom_pat]

       ipat:
        [ "{"; label_pat_list as pl; "}" %pat{ { $pl }}
        | Ant (""|"pat"|"par" ,s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.pat)  s}
        | "("; ")" %{`Unit _loc}
        | "("; "module"; a_uident as m; ?[":";  mtyp as pt]; ")" %{
          match pt with
          | None -> `ModuleUnpack(_loc,m)
          | Some pt -> `ModuleConstraint (_loc, m, ( (`Package (_loc, pt))))}
        | "(" ; "module"; a_uident as m;":"; Ant("opt", s); ")" %{
             `ModuleConstraint (_loc, m, mk_ant  s)}
        (* when change [pat], we need to take care of the following terms
           for factorization *)      

        | "("; pat as p; ")" %{ p}
        | "("; pat as p; ":"; ctyp as t; ")" %pat{ ($p : $t) }
        | "("; pat as p; "as"; a_lident as s; ")" %pat{  ($p as $s) }
        | "("; pat as p; ","; comma_ipat as pl; ")"  %pat{ ($p, $pl) }
        | a_lident as s %{  (s: alident :> pat)}
        | Label i; S as p %pat{ ~ $lid:i : $p }
        | "~"; a_lident as i;":";S as p  %pat{ ~$i : $p}
        | @atom_pat
        ]
       sem_pat_for_list:
       [ pat as p; ";"; S as pl %{`App(_loc, `Uid(_loc,"::"), `Par(_loc,`Com(_loc,p,pl)))}
       | pat as p; ? ";"%{`App(_loc,`Uid(_loc,"::"),`Par(_loc,`Com(_loc,p,`Uid(_loc,"[]"))))}]
       pat_tcon:
       [ pat as p; ":"; ctyp as t %{ `Constraint(_loc,p,t)}
       | pat as p %{ p} ]
       ipat_tcon:
       [ Ant("" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s }
       | a_lident as i %{  (i : alident :> pat)}
       | a_lident as i; ":"; ctyp as t %{(`Constraint (_loc, (i : alident :>  pat), t) : pat)}]
       label_pat_list:
       [ label_pat as p1; ";"; S as p2 %{ `Sem(_loc,p1,p2)}
       | label_pat as p1; ";"; "_"; ? ";"  %{ `Sem(_loc,p1,`Any _loc)}
       | label_pat as p1; ?";"            %{ p1}
       ]


       label_exp:
       [ Ant ("rec_exp" |"" ,s) %{mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.rec_exp)  s}
       | label_longident as i; fun_bind as e
            %rec_exp{ $id:i = $e }
       | label_longident as i %{  (*FIXME*)
         `RecBind (_loc, i, `Lid (_loc, Fan_ops.to_lid i))}]

       label_ep:
       [ Ant ("rec_exp" |"" ,s) %{mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.rec_bind)  s}
       | label_longident as i; ?["="; ep as p] %{
         let p = match p with
         | None ->   `Lid(_loc,Fan_ops.to_lid i)
         | Some p -> p in `RecBind(_loc,i,p)}
                  
       ]                     
       label_pat:
       [ Ant (""|"pat",s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.rec_pat)  s}
       | label_longident as i; ?["="; pat as p] %{
         let p = match p with
         | None ->   `Lid(_loc,Fan_ops.to_lid i)
         | Some p -> p in `RecBind(_loc,i,p)}
       ]};;


let () = 
  begin
    make_semi  field_exp field_exp_list;
    make_semi  exp sem_exp;
    make_semi  label_exp label_exp_list;
    make_semi  label_ep label_ep_list ;
    make_semi  pat sem_pat ;
    make_semi  ep sem_ep;
    make_semi  clfield clfield_quot;
    make_semi  clsigi clsigi_quot;
    make_comma pat comma_pat;
    make_comma ep comma_ep;
    make_comma ipat comma_ipat;
    make_comma exp comma_exp;
    make_case exp pat ;
    make_pat exp;

    make_ants ~c:"pat" ~i:50 pat 
      ["" ;"pat" ;"par" ;"int" ;
       "int32" ;"int64" ;"vrn" ;"flo" ;
       "chr" ;"nativeint" ;"str" ;"int'" ;
       "int32'" ;"int64'" ;"nativeint'" ;
       "flo'" ;"chr'" ;"str'" ];
    make_ants ~c:(Dyn_tag.to_string Dyn_tag.ep) ~i:50 ep
      ["" ;"pat" ;"par" ;"int" ;
       "int32" ;"int64" ;"vrn" ;"flo" ;
       "chr" ;"nativeint" ;"str" ;"int'" ;
       "int32'" ;"int64'" ;"nativeint'" ;
       "flo'" ;"chr'" ;"str'" ; "bool'"];

    make_quot Dyn_tag.exp ~i:170 exp;
    make_ants ~c:(Dyn_tag.to_string Dyn_tag.exp) ~i:170 exp
      ["exp" ;"" ;"par" ;"seq" ;"chr" ;
       "int" ;"int32" ;"str" ;"int64" ;
       "flo" ;"nativeint" ; "vrn" ;
       "chr'" ;"int64'" ;"nativeint'" ;
       "bool'" ;"int'" ;"int32'" ;
       "flo'" ;"str'" ];

    
    make_ants ~c:"mexp" ~i:30 mexp
      ["";"mexp"];

    make_quot  Dyn_tag.mexp ~i:30 mexp;

    make_quot Dyn_tag.mbind mbind;
    make_ants ~c:"mexp" mbind ["mbind";""];

    make_quot Dyn_tag.mbind module_rec_declaration;
    make_ants ~c:"mbind" module_rec_declaration ["mbind";""];

    make_quot Dyn_tag.constr constr;
    make_ants ~c:"constr" constr ["";"constr"];

    make_quot ~i:60 Dyn_tag.mtyp mtyp;
    make_ants ~c:"mtyp" ~i:60 mtyp ["";"mtyp"];

    make_quot Dyn_tag.sigi sigi;
    make_ants ~c:"sigi" sigi ["";"sigi"];

    make_quot Dyn_tag.stru stru;
    make_ants ~c:"stru" stru ["";"stri"];

    make_quot Dyn_tag.clsigi clsigi;
    make_ants ~c:"clsigi" clsigi ["";"csg"];

    make_quot Dyn_tag.clfield clfield;
    make_ants ~c:"clfield" clfield ["";"cst"];

    make_quot Dyn_tag.clexp ~i:30 clexp;
    make_ants ~c:"clexp" ~i:30 clexp ["";"cexp"];

    make_quot Dyn_tag.cltyp cltyp;
    make_ants ~c:"cltyp" cltyp ["";"ctyp"];

    make_ants ~c:"ctyp" meth_decl ["";"typ"];
  end

let apply () = begin 
  %extend{
      mexp_quot:
      [ mexp as x %{ x}]
      mbind0: RA

        [ "("; a_uident as m; ":"; mtyp as mt; ")"; S as mb %{ `Functor (_loc, m, mt, mb)}
        | ":"; mtyp as mt; "="; mexp as me   %{ `Constraint (_loc, me, mt) }
        | "="; mexp as me  %{ me}  ] 
      mexp: 10 

        [ "functor"; "("; a_uident as i; ":"; mtyp as t; ")"; "->"; S as me %{
             `Functor (_loc, i, t, me)}
        | "struct"; ? strus as st; "end" %{
          match st with
          | Some st -> `Struct(_loc,st)
          | None -> `StructEnd _loc}
        ]
      mexp: 20 
        [ S as me1; S as me2 %{ `App (_loc, me1, me2)} ]
      mexp: 30 
        [ module_longident as i  %{ (i:>mexp)}
        | "("; S as me; ":"; mtyp as mt; ")" %{ `Constraint (_loc, me, mt)}
            (* FIXME improve ?[":"; mtyp as mt ] *)
        | "("; S as me; ")" %{  me}
        | "("; "val"; exp as e; ")" %{ `PackageModule (_loc, e)}
        | "("; "val"; exp as e; ":"; mtyp as p; ")"
            %{ `PackageModule
                 (_loc,
                  `Constraint (_loc, e, `Package (_loc, p)))}]};
      %extend{
        mbind_quot:
        [ S as b1; "and"; S as b2 %{  `And(_loc,b1,b2)}
        | Ant ("mbind"|"",s) %{mk_ant ~c:(Dyn_tag.to_string Dyn_tag.mbind) s}
        | a_uident as m; ":"; mtyp as mt %{ `Constraint(_loc,m,mt)}
        | a_uident as m; ":"; mtyp as mt; "="; mexp as me %{ `ModuleBind(_loc,m,mt,me)}]
        mbind:
        [ S as b1; "and"; S as b2 %{ `And(_loc,b1,b2)}
        | a_uident as m; ":"; mtyp as mt; "="; mexp as me %{`ModuleBind (_loc, m, mt, me)}]
        module_rec_declaration:
        [ S as m1; "and"; S as m2 %{`And(_loc,m1,m2)}
        | a_uident as m; ":"; mtyp as mt %{`Constraint(_loc,m,mt)} ] };

     %extend{
        constr_quot:
        [ constr as x %{x}   ]
        constr: 
        [ S as wc1; "and"; S as wc2 %{`And(_loc,wc1,wc2)}
        | "type"; type_longident_and_parameters as t1; "="; ? "private" as p; ctyp as t2 %{
            match p with | Some _ ->  `TypeEqPriv(_loc,t1,t2) | None -> `TypeEq(_loc,t1,t2)
         }
        | "type"; type_longident_and_parameters as t1; ":="; ctyp as t2 %{
            `TypeSubst (_loc, t1, t2)}
        | "module"; module_longident as i1; ("="|":=" as v); module_longident_with_app as i2 %{
          let i = (i1:vid:>ident) in
          if v = "=" then `ModuleEq (_loc, i, i2)
          else `ModuleSubst(_loc, i,i2)}]};
    %extend{
      sigis:
      [ Ant (""|"sigi",s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.sigi) s}
      | Ant (""|"sigi",s); ? ";;"; S as sg
          %{`Sem (_loc,
                  mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.sigi) s, sg)}
      | sigi as sg ; ? ";;" ; S as s %{ `Sem(_loc,sg,s)}
      | sigi as sg ; ? ";;" %{sg}
      ]
      mtyp: 10
      [ "functor"; "("; a_uident as i; ":"; S as t; ")"; "->"; S as mt %{`Functor(_loc,i,t,mt)}]
      mtyp: 20 
      [ S as mt; "with"; constr as wc %{`With(_loc,mt,wc)}]
      mtyp: 30 
      [ S as mt1; S as mt2 %{
        match (mt1, mt2) with
        | ((#ident as i1), (#ident as i2)) -> apply i1 i2 
        | _ -> raise Streamf.NotConsumed }] (* FIXME *)
      mtyp: 40 
       [ S as mt1; "."; S as mt2 %{
          let acc0 mt1 mt2 =
            match (mt1, mt2) with
            | ((#ident as i1), (#ident as i2)) ->
              dot i1 i2 
            | _ -> raise Streamf.NotConsumed  in
          acc0 mt1 mt2 }] (*FIXME*)
      mtyp: 50
        [ "sig"; ? sigis as sg; "end" %{
          match sg with | Some sg ->  `Sig(_loc,sg) | None -> `SigEnd _loc}
        ]
      mtyp: 60 
      [ module_longident_with_app as i %{(i:ident:>mtyp)}
      | "("; S as mt; ")" %{mt}
      | "module"; "type"; "of"; mexp as me %{ `ModuleTypeOf(_loc,me)}] 
  
      module_declaration: (* syntax sugar *)
      [ ":"; mtyp as mt %{ mt}
      | "("; a_uident as i; ":"; mtyp as t; ")"; S as mt %{`Functor(_loc,i,t,mt)}]
      mtyp_quot:
      [ mtyp as x %{ x}  ]  };

  %extend{
  stru_sigi@Inline :
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
    [ "include"; mtyp as mt %{ `Include(_loc,mt)}
    | "module"; a_uident as i; module_declaration as mt %{ `Module(_loc,i,mt)}
    | "module"; "rec"; module_rec_declaration as mb %{  `RecModule (_loc, mb)}
    | "module"; "type"; a_uident as i  %{`ModuleTypeEnd(_loc,i)}
    | @stru_sigi
    | "val"; a_lident as i; ":"; ctyp as t %{ `Val(_loc,i,t)}
    | "class"; class_description as cd  %{ `Class(_loc,cd)}
    ]
    (* mli entrance *)    
    interf:
    [ sigi as si; ?";;";  S as rest %{
      let (sil,stopped) = rest in (si :: sil, stopped)}
    |  %{ ([], None)} ]  };

    %extend{
      exp_quot:
      [ exp as e1; ","; comma_exp as e2 %{ `Com(_loc,e1,e2)}
      | exp as e1; ";"; sem_exp as e2 %{ `Sem(_loc,e1,e2)}
      | exp as e  %{e}]

       (* {:stru|
       let f (type t) () =
          let module M = struct exception E of t ; end in
          ((fun x -> M.E x), (function [M.E x -> Some x | _ -> None]))}
       %stru{ let f : ! 'a . 'a -> 'a = fun x -> x } *)
      cvalue_bind:
      [ "="; exp as e %{ e}
      | ":"; "type"; unquoted_typevars as t1; "." ; ctyp as t2 ; "="; exp as e %{
          let u = %ctyp{ ! $t1 . $t2 } in  %exp{ ($e : $u) }}
      | ":"; ctyp as t; "="; exp as e %exp{ ($e : $t) }
      | ":"; ctyp as t; ":>"; ctyp as t2; "="; exp as e %{
        match t with
        | %ctyp{ ! $_ . $_ } ->
            raise (Streamf.Error "unexpected polytype here")
        | _ -> %exp{ ($e : $t :> $t2) } }
      | ":>"; ctyp as t; "="; exp as e %{ `Subtype(_loc,e,t)} ]
      fun_bind: RA
      [ "("; "type"; a_lident as i; ")"; S as e %{
        `LocalTypeFun(_loc,i,e)}
      | ipat as p; S as e  %{ `Fun(_loc,`Case(_loc,p,e))}
      | cvalue_bind as bi %{ bi}  ]
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
       name_space@Local:
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
            match Ast_quotation.resolve_name {domain = `Sub []; name = x}
            with 
            |None ->
                Locf.failf _loc "DDSL `%s' can not be resolved" x
            | Some  x -> x)} ]  
       fun_def_pat@Local:
       ["(";"type";a_lident as i;")" %{fun e ->  `LocalTypeFun (_loc, i, e)}
       | ipat as p %{ fun e -> `Fun(_loc,`Case(_loc,p,e))}(* %{ fun $p -> $e } *)
       | ipat as p; "when"; exp as w %{fun e -> `Fun(_loc,`CaseWhen(_loc,p,w,e))} ]
       fun_def: RA
       [ fun_def_pat as f; "->"; exp as e %{  f e}
       | fun_def_pat as f; S as e  %{f e}] 
           
       
       (************************)
       (*  How to handle S     *)    
       (************************)               
       let_stru_exp@Inline:
       [ "let"; opt_rec as r; bind as bi; "in"; exp as x %{`LetIn(_loc,r,bi,x)}
       | "let"; "module"; a_uident as m; mbind0 as mb; "in"; exp as e
           %{ `LetModule (_loc, m, mb, e)}
       | "let"; "open"; ? "!" as bang ; module_longident as i; "in"; exp as e %{
            `LetOpen (_loc,
                      match bang with | Some _   -> `Positive _loc | None -> `Negative _loc,
                        (i:vid :> ident), e)}
       | "let"; "try"; opt_rec as r; bind as bi; "in"; exp as x; "with"; case as a %{
              `LetTryInWith(_loc,r,bi,x,a)}]
       exp : 10  RA
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
       exp : 70 RA
        [ S as e1; ("::"@xloc as op); S as e2  %{
          let op = %exp@xloc{$uid:op} in %exp{$op $e1 $e2}}
        ]  
       exp : 110  RA
        [("fun"|"function"); "|";  L1 case0 SEP "|" as a  %{
           let cases = bar_of_list a in `Fun (_loc,cases)}
        | ("fun"|"function"); fun_def as e %{ e}

        | "object"; "(";pat as p; ")"; ? class_structure as cst;"end" %{
         match cst with |Some cst -> `ObjPat(_loc,p,cst) | None -> `ObjPatEnd (_loc,p)}
        | "object"; "(";pat as p;":";ctyp as t;")"; ? class_structure as cst;"end" %{
            match cst with
            | Some cst ->  `ObjPat(_loc,`Constraint(_loc,p,t),cst)
            | None -> `ObjPatEnd(_loc,`Constraint(_loc,p,t)) }
        | "object"; ? class_structure as cst;"end" %{
           match cst with
           | Some cst -> `Obj(_loc,cst)
           | None -> `ObjEnd _loc}
        ]
       exp : 120  
        [ ("-"|"-." as x); S as e %{ Fan_ops.mkumin _loc x e} (* Delayed into Dump *)
        ]
       exp : 130  
        [ S as e1; S as e2 %{ `App(_loc,e1,e2)}
        | "assert"; S as e %{ `Assert(_loc,e)}
        | "new"; class_longident as i %{ `New (_loc,i)} 
        | "lazy"; S as e %{ `Lazy(_loc,e)} ]
       exp : 140  
        [ "~"; a_lident as i; ":"; S as e %{ `Label (_loc, i, e)}
        | "~"; a_lident as i %{ `LabelS(_loc,i)}
        (* Here it's LABEL and not tilde_label since ~a:b is different than ~a : b *)
        | Label i; S as e %exp{ ~ $lid:i : $e }
        (* Same remark for ?a:b *)
        | Optlabel i; S as e %{  `OptLabl(_loc,`Lid(_loc,i),e)}
        | "?"; a_lident as i; ":"; S as e %{ `OptLabl(_loc,i,e)}
        | "?"; a_lident as i %{ `OptLablS(_loc,i)} ] 
       exp : 150  
        [ S as e1; "."; "("; S as e2; ")" %{ `ArrayDot (_loc, e1, e2)}
        | S as e1; "."; "["; S as e2; "]" %{ `StringDot (_loc, e1, e2)}
        | S as e1; "."; "{"; comma_exp as e2; "}" %{ Fan_ops.bigarray_get _loc e1 e2}
        | S as e1; "."; label_longident as e2 %{ `Field(_loc,e1,e2)}
        | S as e; "#"; a_lident as lab %{ `Send (_loc, e, lab)} ]
       exp : 160  
        [ ("!"@xloc as x); S as e %{`App(_loc,`Lid(xloc,x),e )}
        | Pre@xloc x; S as e %{`App(_loc,`Lid(xloc,x),e )}]
       exp : 170 
        [  @primitve 
        | TRY module_longident_dot_lparen as i;S as e; ")" %{
            `LetOpen (_loc,`Negative _loc, i, e)}
        | vid as i %{(i :vid :>exp) }
        | "`"; luident as s %{ `Vrn(_loc,s)}
        | "["; "]"  %exp{ [] }
        | "["; sem_exp_for_list as s; "]" %{ s }
        | "[|"; "|]" %{ `ArrayEmpty(_loc)}
        | "[|"; sem_exp as el; "|]" %{ `Array (_loc, el)}
        | "{"; Lid@xloc x ; "with"; label_exp_list as el; "}" %{
          `RecordWith (_loc, el, `Lid (xloc, x))}

        | "{"; label_exp_list as el; "}" %{ `Record (_loc, el)}
        | "{"; "("; S as e; ")"; "with"; label_exp_list as el; "}" %{
            `RecordWith (_loc, el, e)}
        | "{<"; ">}" %{ `OvrInstEmpty(_loc)}
        | "{<"; field_exp_list as fel; ">}" %{ `OvrInst(_loc,fel) }
        | "("; ")"  %{`Unit _loc }
        | "("; S as e; ":"; ctyp as t; ")" %{ `Constraint (_loc, e, t)}
        | "("; S as e; ","; comma_exp as el; ")" %{`Par (_loc, `Com (_loc, e, el))}
        | "("; S as e; ";"; sequence as seq; ")"  %{`Seq(_loc,`Sem(_loc,e,seq))}
        | "("; S as e; ";"; ")" %{ `Seq(_loc,e)} (* FIXME Seq or not?*)
        | "("; S as e; ":"; ctyp as t; ":>"; ctyp as t2; ")" %{`Coercion (_loc, e, t, t2)}
        | "("; S as e; ":>"; ctyp as t; ")" %{ `Subtype(_loc,e,t)}
        | "("; S as e; ")"  %{ e}
        | "begin"; ? sequence as seq; "end" %{
          match seq with
          | Some seq -> `Seq(_loc,seq)
          | None -> `Unit _loc }
        | "("; "module"; mexp as me; ")" %{`Package_exp (_loc, me)}
        | "("; "module"; mexp as me; ":"; mtyp as pt; ")" %{
            `Package_exp (_loc, `Constraint (_loc, me, pt))}  ] 

       sem_exp_for_list: (* duplicated with [sem_exp] and [make_semi]*)
       [ exp as e; ";"; S as el %exp{ $e :: $el }
       | exp as e; ?";" %exp{ [$e]}
       ]

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
      };
  %extend{ with_exp_lang: [ lang as old; ":"; exp as x %{ (Ast_quotation.default := old; x)}] } ;
  %extend{ with_stru_lang: [lang as old;":"; stru as x %{ (Ast_quotation.default:=old;x)}]  };
  %extend{
  bind_quot:
    [ bind as x  %{x}  ]
  bind:
    [ Ant ("bind"|"",s)  %{mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.bind) s}
    | Ant ("" ,s); "="; exp as e
        %bind{ ${mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.pat) s} = $e }
    | S as b1; "and"; S as b2 %{ `And (_loc, b1, b2)}
    | pat as p; fun_bind as e %{ `Bind (_loc, p, e)}
]};


  with rec_exp
      %extend{
        rec_exp_quot:
        [ label_exp_list as x %{x}  ]
        
        field_exp :
        [ Ant (""|"bi",s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s}
        | a_lident as l; "=";  exp  as e %{`RecBind (_loc, (l:>vid), e)}
            (* %{ $lid:l = $e } *) ]};

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
      ident_quot: 10 
       [ S as i; "."; S as j %{ %{ $i.$j  }} ]
      ident_quot: 20 
        [ Ant (""|"id" |"uid"|"lid" ,s)
            %{mk_ant
                ~c:(Dyn_tag.to_string Dyn_tag.ident)  s}
        | Ant (""|"id"|"uid",s); "."; S as i %{
            `Dot (_loc, mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.ident) s, i)}
        | Lid i %{ %{ $lid:i }}
        | Uid i %{ %{ $uid:i }}
        | Uid s ; "." ; S as j %{ %{$uid:s.$j}}
        | "("; S as i;S as j; ")" %{ `Apply (_loc, i, j)}  ] 

      (* parse [a] [b], [a.b] [A.b]*)
      ident:
      [ Ant (""|"id"|"uid"|"lid" ,s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.ident) s}
      | Ant (""|"id"|"uid" ,s); "."; S as i %{
           `Dot (_loc, mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s, i)}
      | Lid i %{ `Lid(_loc,i)}
      | Uid i %{ `Uid(_loc,i)}
      | Uid s ; "." ; S as j %{  `Dot (_loc, `Uid (_loc, s), j)}]
      
      vid: (* duplicate ident  FIXME *)
      [ Ant (""|"id" |"uid" |"lid",s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid)  s}
      | Ant (""|"id"|"uid",s); "."; S as i %{
           `Dot (_loc, mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.vid) s, i)}
      | Lid i %{ `Lid(_loc,i)}
      | Uid i %{ `Uid(_loc,i)}
      | Uid s ; "." ; S as j %{  `Dot (_loc, `Uid (_loc, s), j)}]
      
      uident:
      [Uid s %{ `Uid(_loc,s)}
      | Ant(""|"id"|"uid",s) %{mk_ant   ~c:(Dyn_tag.to_string Dyn_tag.uident)  s}
      |Uid s; "."; S as l %{ dot (`Uid (_loc,s)) l}
      |Ant(""|"id"|"uid",s) ;"." ; S as i %{
          dot (mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.uident)  s) i}]

      (* parse [a.b.c] no antiquot *)
      dot_lstrings:
      [ Lid i %{ {domain = `Sub[]; name = i}}
      | Uid i ; "." ; S  as x %{
        match x with
        |{domain = `Sub xs; _ } -> {x  with domain = `Sub (i::xs)}
        | _ -> raise (Streamf.Error "impossible dot_lstrings")}

      | "."; Uid i; "."; S as x %{
          match x with
          |{domain = `Sub xs; _} -> {x with domain = `Absolute (i::xs)}
          | _ -> raise (Streamf.Error "impossible dot_lstrings")} ]

      (* parse [A.B.(] *)
      module_longident_dot_lparen:
      [ Ant (""|"id"|"uid" ,s); "."; "(" %{ mk_ant   ~c:(Dyn_tag.to_string Dyn_tag.ident)  s }

      | Uid i; "."; S as l %{ %{$uid:i.$l}}
      | Uid i; "."; "(" %{ %{$uid:i}}
      | Ant ("uid"|"" ,s); "."; S as l %{ %{${mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.ident) s}.$l} }]
      (* parse [A.B] *)
      module_longident:
      [ Ant (""|"id"|"uid",s) %{mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid)  s }
      | Uid i; "."; S as l %{  `Dot (_loc, `Uid (_loc, i), l)}
      | Uid i %{ `Uid(_loc,i)}
      | Ant(""|"uid", s); "."; S as l
          %{`Dot (_loc, mk_ant
                    ~c:(Dyn_tag.to_string Dyn_tag.vid) s, l)}]

      module_longident_with_app: 10 
      
        [ S as i; S as j %{ `Apply(_loc,i,j)} ]
      module_longident_with_app: 20 
        [ S as i; "."; S as j %{ %{ $i.$j }} ]
      module_longident_with_app: 30
        [ Ant (""|"id"|"uid",s) %{mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident)  s}
        | Uid i %{ `Uid(_loc,i)}
        | "("; S as i; ")" %{ i} ] 

      (* parse [(A B).c ]*)
      type_longident : 10 
      [ S as i; S as j %{ `Apply(_loc,i,j)} ]
      type_longident : 20 
      [ S as i; "."; S as j %{ %{ $i.$j }} ]
      type_longident : 30 
      [ Ant (""|"id"|"uid"|"lid",s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident)  s}
      | Lid i %{ %{$lid:i}}
      | Uid i %{ %{$uid:i}}
      | "("; S as i; ")" %{ i} ] 

      label_longident:
      [ Ant (""|"id"|"lid",s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s}
      | Lid i %{ `Lid(_loc,i)}
      | Uid@iloc i; "."; S as l %{ `Dot(_loc,`Uid(iloc,i),l)}
      | Ant(""|"uid",s); "."; S as l %{
        `Dot (_loc, mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s, l)} ]
      
      cltyp_longident: [ type_longident as x  %{x} ]
      val_longident:[ ident as x %{ x} ]
      class_longident: [ label_longident as x %{(x : vid :>ident)} ]
      
       
      opt_override:
      [ ? "!" as bang %{
        match bang with
        | Some _ ->  `Positive _loc | None -> `Negative _loc}
      | Ant ("!"|"override" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s}]
      
      
      flag:
      [ "to" %{  `Positive _loc}
      | "downto" %{ `Negative _loc }
      | Ant ("to"|"" ,s) %{mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.flag) s}]

      opt_private: (* Should be inlined directly in the future *)
      [ "private" %{ `Positive _loc}
      | Ant ("private" ,s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.flag) s}
      | %{`Negative _loc}   ] 
      opt_mutable:
      [ "mutable" %{  `Positive _loc}
      | Ant ("mutable" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag)  s}
      |  %{`Negative _loc}  ] 
      opt_virtual:
      [ "virtual" %{ `Positive _loc }
      | Ant ("virtual" ,s) %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.flag)  s}
      | %{`Negative _loc}   ] 
      opt_dot_dot:
      [ ".." %{ `Positive _loc}
      | Ant (".." ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag)  s}
      |  %{`Negative _loc}   ]

      (*opt_rec@inline *)
      opt_rec:
      [ "rec" %{ `Positive _loc}
      | Ant ("rec" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag)  s}
      |  %{`Negative _loc}]
      a_lident:
      [ Ant(""|"lid" ,s) %{ mk_ant s}
      | Lid s  %{ `Lid (_loc, s)} ]
      a_uident:
      [ Ant(""|"uid" ,s) %{ mk_ant    s}
      | Uid s  %{ `Uid (_loc, s)} ]
      string_list:
      [ Ant("",s) %{ mk_ant  s}
      | Ant("",s) ; S as xs %{ `App(_loc,mk_ant  s, xs)}
      | Str  x %{ `Str(_loc,x)}
      | Str x; S as xs %{ `App(_loc,`Str(_loc,x),xs)}]
      rec_flag_quot:  [ opt_rec as x %{x} ]
      direction_flag_quot:  [ flag as x %{x} ] 
      mutable_flag_quot: [  opt_mutable as x %{x} ] 
      private_flag_quot: [  opt_private as x %{x} ]
      virtual_flag_quot: [  opt_virtual as x %{x} ] 
      row_var_flag_quot: [  opt_dot_dot as x %{x} ] 
      override_flag_quot:[  opt_override as x %{x} ] 
};
  with stru
    %extend{
    (** ml file  entrance *)
      quots@Local:
      [ DirQuotation x ; ";;" %{Ast_quotation.handle_quot x}
      | S ; S %{()}]           
      implem:
      [ ? quots; ?strus as x   %{x}]
      (* | stru as si; ";;"; S as rest %{ *)
      (*  (\* si::rest *\) *)
      (*  let (sil, stopped) = rest in (si :: sil, stopped)} *)
      (* | stru as si;  S as rest *)
      (*    %{let (sil, stopped) = rest in (si :: sil, stopped)} *)
      (*    (\* FIXME merge with the above in the future*\)             *)
      (* |  %{ ([], None)} ] *)
      (** entrance for toplevel *)
      top_phrase:
      [ "#"; a_lident as n; exp as dp; ";;" %{ `Directive(_loc,n,dp)}
      | "#"; a_lident as n; ";;" %{ (* Some *) `DirectiveSimple(_loc,n)}
      | stru as st; ";;" %{ st} ]
      (* used by [struct .... end]
         constains at least one element *)
      strus: (* FIXME dump seems to be incorrect *)
      [ Ant (""|"stri" ,s) ;?";;"
          %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s
           }
      | Ant (""|"stri" ,s); ?";;"; S as st
          %{ `Sem (_loc, mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.stru) s, st)}
      | stru as st; ? ";;" %{ st}
      | stru as st; ? ";;"; S as xs %{ `Sem(_loc,st,xs)}
      ]


      stru_quot:
      [ "#"; a_lident as n; exp as dp %{ `Directive(_loc,n,dp)}
      | "#"; a_lident as n %{ `DirectiveSimple(_loc,n)}
      | strus as x  %{x}]

      stru:
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
      | exp as e %{ `StExp(_loc,e)}
          (* this entry makes %{ let $rec:r $bi in $x } parsable *)
     ]   };


    %extend{
      

      class_signature:
      [ Ant (""|"csg" ,s); ? ";" %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s}
      | Ant (""|"csg" ,s); ? ";"; S as csg %{
          (`Sem (_loc, mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s, csg) : Astf.clsigi )}
      | clsigi as csg; ? ";" %{ csg}
      | clsigi as csg; ? ";"; S as xs %{ `Sem(_loc,csg,xs)}
      ]
      
      clsigi:
      [ "inherit"; cltyp as cs %{ `SigInherit(_loc,cs)}
      | "val"; opt_mutable as mf; opt_virtual as mv;a_lident as l; ":"; ctyp as t %{
        (`CgVal (_loc, l, mf, mv, t) : Astf.clsigi )}
      | "method"; "virtual"; opt_private as pf; a_lident as l; ":";ctyp as t 
          %{(`VirMeth (_loc, l, pf, t) : Astf.clsigi )}
      | "method"; opt_private as pf; a_lident as l; ":";ctyp as t %{(`Method (_loc, l, pf, t) : Astf.clsigi )}
      | "constraint"; ctyp as t1; "="; ctyp as t2 %{ (`Eq (_loc, t1, t2) : Astf.clsigi )} ] };  

    %extend{
      class_structure:
       [ Ant (""|"cst" ,s); ? ";" %{ mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.clfield)  s}
       | Ant (""|"cst" ,s); ? ";"; S as st %{
         `Sem(_loc, mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield)  s,st)}
       | clfield as st; ?";" %{ st}
       | clfield as st; ?";";S as xs %{ `Sem(_loc,st,xs)}
       ]

      value_val_opt_override:
       [ "val"; ? "!" as bang %{
         match bang with
         | Some _ -> `Positive _loc | None -> `Negative _loc}
       | "val"; Ant (""|"override"|"!" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s}]
       method_opt_override:
       [ "method"; ? "!" as bang  %{
        match bang with
        | Some _ ->  `Positive _loc
        | None -> `Negative _loc }
       | "method"; Ant (""|"override" ,s) %{mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s}]
      clfield:
        ["inherit"; opt_override as o; clexp as ce %{`Inherit(_loc,o,ce)}
        | "inherit"; opt_override as o; clexp as ce; "as"; a_lident as i %{`InheritAs(_loc,o,ce,i)}
        | value_val_opt_override as o; opt_mutable as mf; a_lident as lab; cvalue_bind as e %{
          (`CrVal (_loc, lab, o, mf, e) : Astf.clfield )}

        | "val"; "virtual"; opt_mutable as mf; a_lident as l; ":"; ctyp as t %{
          `VirVal (_loc, l, mf, t)}                    
        | "method"; "virtual"; opt_private as pf; a_lident as l; ":"; ctyp as t %{
          `VirMeth (_loc, l, pf, t)}  
        | method_opt_override as o; opt_private as pf; a_lident as l; ":"; ctyp as t ;
          fun_bind as e %{
          `CrMth(_loc,l,o,pf,e,t)}
        | method_opt_override as o; opt_private as pf;a_lident as l; fun_bind as e %{
          `CrMthS(_loc,l,o,pf,e)}
        | "constraint"; ctyp as t1; "="; ctyp as t2 %{ `Eq(_loc,t1,t2)}
        | "initializer"; exp as se %{ `Initializer(_loc,se)} ]};
    
    %extend{
      clexp_quot:
      [ clexp as x %{ x}]
      class_declaration:
      [ S as c1; "and"; S as c2 %{ `And(_loc,c1,c2)}
      | Ant (""|"cdcl" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cldecl)  s}
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
      clexp: 10
      [ ("fun"|"function"); ipat as p; class_fun_def as ce %{  `CeFun (_loc, p, ce)}
      | "let"; opt_rec as rf; bind as bi; "in"; S as ce %{ `LetIn(_loc,rf,bi,ce)}]
      clexp: 20 
      [ S as ce; exp Level 140 as e %{ `CeApp (_loc, ce, e)}]
      clexp: 30  (* "simple" *)
      [ vid as ci; "["; comma_ctyp as t; "]" %{ `ClApply(_loc,ci,t)}
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
      | "("; S as ce; ")" %{ce} ] } ;
  with cltyp
    %extend{
      class_description:
      [ S as cd1; "and"; S as cd2 %{ `And(_loc,cd1,cd2)}
      | Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl)  s}
      | opt_virtual as mv;  a_lident as i;"[";
          comma_type_parameter as x; "]"; ":"; cltyp_plus as ct %{
            `CtDecl(_loc,mv,(i:>ident),x,ct)}
      | opt_virtual as mv; a_lident as i ; ":"; cltyp_plus as ct %{
          `CtDeclS(_loc,mv,(i:>ident),ct)}]
      cltyp_declaration:
      [ S as cd1; "and"; S as cd2 %{ `And(_loc,cd1,cd2)}
      | Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl)  s}
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
      [ vid as i; "["; comma_ctyp as t; "]" %{ `ClApply(_loc,i,t)}
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
  | Ant (""|"typ" ,s) %{  mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp)  s}

  | a_lident as i %{ (i:>ctyp)} ]
  type_parameter:
  [ Ant (""|"typ" ,s) %{ mk_ant s}
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
  | Ant ("" ,s) %{mk_ant s ~c:(Dyn_tag.to_string Dyn_tag.ctyp)}]
  type_parameters:
  [ type_parameter as t1; S as t2 %{ fun acc -> t2 (`App(_loc,acc, (t1:>ctyp)))}
  | type_parameter as t %{ fun acc -> `App(_loc,acc, (t:>ctyp))}
  |  %{fun t -> t}  ]
  meth_list:
  [ meth_decl as m; ";"; S as rest   %{ let (ml, v) = rest in (`Sem(_loc,m,ml), v)}
  | meth_decl as m; ?";"; opt_dot_dot as v %{ (m, v)}]
  meth_decl:
  [ a_lident as lab; ":"; ctyp as t  %{`TyCol(_loc,lab,t)}]
  opt_meth_list:
  [ meth_list as rest  %{let (ml, v) = rest in `TyObj (_loc, ml, v)}
  | opt_dot_dot as v     %{ `TyObjEnd(_loc,v)} ]
  row_field:
  [ Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.row_field)  s}
  | Ant("vrn" , s) %{ `TyVrn(_loc,mk_ant ~c:(Dyn_tag.to_string Dyn_tag.astring)  s)} (* FIXME*)
  | Ant("vrn" , s) ; "of"; ctyp as t %{
    `TyVrnOf(_loc,mk_ant
             ~c:(Dyn_tag.to_string Dyn_tag.astring)  s,t)}
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
  [ Ant (""|"typ" ,s) %{  mk_ant ~c:(Dyn_tag.to_string Dyn_tag.tag_names)  s}
| S as t1; S as t2 %{ `App (_loc, t1, t2)}
| "`"; astr as i %{ `TyVrn (_loc, i)}  ]
  type_declaration:
  [ Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.typedecl)  s}
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
  | Ant (""|"typ" ,s) %{  mk_ant  ~c:(Dyn_tag.to_string Dyn_tag.ctyp)  s}
  | "'"; a_lident as i  %{ `Quote (_loc, `Normal _loc, i)}]
  ctyp: 10 
  [ S as t1; "as"; "'"; a_lident as i %{`Alias(_loc,t1,i)}]
  ctyp: 20  
  [ "!"; typevars as t1; "."; ctyp as t2 %{ `TyPol (_loc, t1, t2)} ]
  ctyp: 30 RA
  [ S as t1; "->"; S as t2 %{  `Arrow(_loc,t1,t2)} ]
  ctyp: 40  
  [ "~"; a_lident as i; ":"; S as t %{ `Label (_loc, i, t)}
      | Label s ; ":"; S as t %{ `Label (_loc, (`Lid (_loc, s)), t)} (* FIXME *)
      | Optlabel s ; S as t %{ `OptLabl(_loc,`Lid(_loc,s),t)}
      | "?"; a_lident as i; ":"; S as t %{ `OptLabl(_loc,i,t)}]
      ctyp: 50  
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
      ctyp: 60 
      [ "'"; a_lident as i %{  `Quote (_loc, `Normal _loc,  i)}
      | "_" %{ `Any _loc}
      | Ant (""|"typ"|"par"|"id" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp)  s}
      | Ant ("id" ,s); "."; S as t %{
        let try id = ident_of_ctyp t  in
        (`Dot(_loc,mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident)  s,id) :ctyp)
        with Invalid_argument s -> raise (Streamf.Error s)}
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
          
      | "[<"; row_field as rfl; ? [ ">"; name_tags as ntl ] ; "]" %{
        match ntl with
        | None -> `PolyInf(_loc,rfl)
        | Some ntl -> `PolyInfSup(_loc,rfl,ntl)}

      | "#"; class_longident as i %{  `ClassPath (_loc, i)}
      | "<"; opt_meth_list as t; ">" %{ t}
      | "("; "module"; mtyp as p; ")" %{ `Package(_loc,p)}
      ] 
      comma_ctyp: (* DUPLICATED removed later *)
      [ S as t1; ","; S as t2 %{ `Com (_loc, t1, t2) }
      | Ant ( "" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.type_parameters)  s}
      | ctyp as t %{ `Ctyp(_loc,t)}
      ]
      com_ctyp:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp)  s}
      | S as t1; ","; S as t2 %{ `Com(_loc,t1,t2)}
      | ctyp as t %{ t}
      ]
      star_ctyp:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp)  s}
      | S as t1; "*"; S as t2 %{ `Sta(_loc,t1,t2)}
      | ctyp as t %{ t}
      ]
      constructor_declarations:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.or_ctyp)  s}
      | S as t1; "|"; S as t2 %{    `Bar(_loc,t1,t2)}
      | a_uident as s; "of"; constructor_arg_list as t %{ `Of(_loc,s,t)}
      | a_uident as s; ":"; ctyp as t %{ (* GADT  *) `TyCol(_loc,s,t)}
      | a_uident as s %{ (s :> or_ctyp)}
      ]
      constructor_declaration:
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.of_ctyp)  s}
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
      [ Ant (""|"typ" ,s) %{ mk_ant ~c:(Dyn_tag.to_string Dyn_tag.name_ctyp)  s}
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
    ("fan", fill_parsers);;




(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_fan.cmo" *)
(* end: *)
