open FAst
let antiquot_exp = Gramlib.eoi_entry Syntaxf.exp
let antiquot_pat = Gramlib.eoi_entry Syntaxf.pat
let ep = Gramf.mk "ep"
let _ =
  Gramf.extend_single (ep : 'ep Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                 Tokenf.pattern )];
            annot = "(`Lid (_loc, x) : FAst.ep )\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    ((`Lid (_loc, x) : FAst.ep ) : 'ep ) : Tokenf.txt ->
                                                             Locf.t -> 'ep ))
          }]
     } : Gramf.olevel )
let antiquot_ident = Gramlib.eoi_entry Syntaxf.ident
let antiquot_ep = Gramlib.eoi_entry ep
let exp loc str = Gramlib.parse_string antiquot_exp ~loc str
let pat loc str = Gramlib.parse_string antiquot_pat ~loc str
let ep loc str = (Gramlib.parse_string antiquot_ep ~loc str : ep )
let ident loc str = Gramlib.parse_string antiquot_ident ~loc str
let anti_filter = Ant.antiquot_expander ~parse_exp:exp ~parse_pat:pat
let exp_filter (x : ep) = anti_filter#exp (x :>exp)
let pat_filter (x : ep) = anti_filter#pat (x :>pat)
let anti_filter_n = AntN.antiquot_expander ~parse_exp:exp ~parse_pat:pat
let exp_filter_n (x : ep) = anti_filter_n#exp (x :>exp)
let pat_filter_n (x : ep) = anti_filter_n#pat (x :>pat)
let expand_exp (x : Tokenf.quot) =
  if x.name = Tokenf.empty_name
  then let expander loc _ s = exp loc s in Tokenf.quot_expand expander x
  else Ast_quotation.expand x Dyn_tag.exp
