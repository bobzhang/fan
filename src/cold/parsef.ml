open Astf
let ep = Gramf.mk "ep"
let _ =
  Gramf.extend_single
    ({
       entry = (ep : 'ep Gramf.t);
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern)];
                 annot = "(`Lid (_loc, x) :> Astf.ep)\n";
                 fn =
                   (Gramf.mk_action
                      (function
                       | (__fan_0 : Tokenf.txt) ->
                           (function
                            | (_loc : Locf.t) ->
                                let x = __fan_0.txt in
                                ((`Lid (_loc, x) :> Astf.ep) : 'ep)) : 
                      Tokenf.txt -> Locf.t -> 'ep))
               }]
          } : Gramf.olevel)
     } : _ Gramf.single_extend_statement)
let exp =
  function
  | loc -> (function | str -> Gramlib.parse_string_eoi Syntaxf.exp ~loc str)
let pat =
  function
  | loc -> (function | str -> Gramlib.parse_string_eoi Syntaxf.pat ~loc str)
let stru =
  function
  | loc -> (function | str -> Gramlib.parse_string_eoi Syntaxf.stru ~loc str)
let ep =
  function
  | loc -> (function | str -> (Gramlib.parse_string_eoi ep ~loc str : ep))
let ident =
  function
  | loc ->
      (function | str -> Gramlib.parse_string_eoi Syntaxf.ident ~loc str)
let expand_exp =
  function
  | (x : Tokenf.quot) ->
      if x.name = Tokenf.empty_name
      then
        let expander =
          function | loc -> (function | _ -> (function | s -> exp loc s)) in
        Tokenf.quot_expand expander x
      else Ast_quotation.expand x Dyn_tag.exp
let expand_stru =
  function
  | (x : Tokenf.quot) ->
      if x.name = Tokenf.empty_name
      then
        let expander =
          function | loc -> (function | _ -> (function | s -> stru loc s)) in
        Tokenf.quot_expand expander x
      else Ast_quotation.expand x Dyn_tag.stru
