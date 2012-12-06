open Fan.Syntax
module Ast = Camlp4Ast
open FanUtil
let _ =
  let open FanParsers in
    pa_r (module Fan);
    pa_rp (module Fan);
    pa_q (module Fan);
    pa_g (module Fan);
    pa_l (module Fan);
    pa_m (module Fan)
let _ = Fan.iter_and_take_callbacks (fun (_,f)  -> f ())
let t e s = Gram.parse_string e FanLoc.string_loc s
let a = Gram.mk "a"
let b = Gram.mk "b"
let a_eoi = Gram.mk "a_eoi"
let _ =
  Gram.extend (a : 'a Gram.t )
    (None,
      [(None, None,
         [([`Stry
              (`Snterm
                 (Gram.obj
                    (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                     Gram.t )))],
            (Gram.mk_action
               (fun (s : 'module_longident_dot_lparen)  (_loc : FanLoc.t)  ->
                  (s : 'a ))));
         ([`Snterm (Gram.obj (b : 'b Gram.t ))],
           (Gram.mk_action (fun (s : 'b)  (_loc : FanLoc.t)  -> (s : 'a ))))])]);
  Gram.extend (b : 'b Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.IdLid (_loc, i) : 'b ))));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.token])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID i -> (Ast.IdUid (_loc, i) : 'b )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"));
          `Skeyword ".";
          `Sself],
           (Gram.mk_action
              (fun (j : 'b)  _  (__fan_0 : [> FanToken.token]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID i ->
                     (Ast.IdAcc (_loc, (Ast.IdUid (_loc, i)), j) : 'b )
                 | _ -> assert false)))])]);
  Gram.extend (a_eoi : 'a_eoi Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (a : 'a Gram.t ));
           `Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
            (Gram.mk_action
               (fun (__fan_1 : [> FanToken.token])  (i : 'a) 
                  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `EOI -> (i : 'a_eoi )
                  | _ -> assert false)))])])
let _ = t a_eoi "A.C.U.b"