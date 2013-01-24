open PreCast.Syntax
open Lib
open GramLib
let comprehension_or_sem_expr_for_list =
  Gram.mk "comprehension_or_sem_expr_for_list"
let apply () =
  Gram.delete_rule expr
    [`Skeyword "[";
    `Snterm (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
    `Skeyword "]"];
  (let grammar_entry_create = Gram.mk in
   let item: 'item Gram.t = grammar_entry_create "item" in
   Gram.extend_single (expr : 'expr Gram.t )
     ((Some (`Level "simple")),
       (None, None,
         [([`Skeyword "[";
           `Snterm
             (Gram.obj
                (comprehension_or_sem_expr_for_list : 'comprehension_or_sem_expr_for_list
                                                        Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (e : 'comprehension_or_sem_expr_for_list)  _ 
                  (_loc : FanLoc.t)  -> (e : 'expr ))))]));
   Gram.extend_single
     (comprehension_or_sem_expr_for_list : 'comprehension_or_sem_expr_for_list
                                             Gram.t )
     (None,
       (None, None,
         [([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
           `Skeyword ";";
           `Snterm
             (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ))],
            (Gram.mk_action
               (fun (mk : 'sem_expr_for_list)  _  (e : 'expr) 
                  (_loc : FanLoc.t)  ->
                  (`App
                     (_loc,
                       (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e)),
                       (mk (`Id (_loc, (`Uid (_loc, "[]")))))) : 'comprehension_or_sem_expr_for_list ))));
         ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
          `Skeyword ";"],
           (Gram.mk_action
              (fun _  (e : 'expr)  (_loc : FanLoc.t)  ->
                 (`App
                    (_loc,
                      (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e)),
                      (`Id (_loc, (`Uid (_loc, "[]"))))) : 'comprehension_or_sem_expr_for_list ))));
         ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
          `Skeyword "|";
          `Slist1sep
            ((`Snterm (Gram.obj (item : 'item Gram.t ))), (`Skeyword ";"))],
           (Gram.mk_action
              (fun (l : 'item list)  _  (e : 'expr)  (_loc : FanLoc.t)  ->
                 (Expr.compr _loc e l : 'comprehension_or_sem_expr_for_list ))));
         ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
           (Gram.mk_action
              (fun (e : 'expr)  (_loc : FanLoc.t)  ->
                 (`App
                    (_loc,
                      (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e)),
                      (`Id (_loc, (`Uid (_loc, "[]"))))) : 'comprehension_or_sem_expr_for_list ))))]));
   Gram.extend_single (item : 'item Gram.t )
     (None,
       (None, None,
         [([`Stry
              (Gram.srules
                 [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
                   `Skeyword "<-"],
                    (Gram.mk_action
                       (fun _  (p : 'patt)  (_loc : FanLoc.t)  ->
                          (p : 'e__1 ))))]);
           `Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
            (Gram.mk_action
               (fun (e : 'expr)  (p : 'e__1)  (_loc : FanLoc.t)  ->
                  (`gen (p, e) : 'item ))));
         ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
           (Gram.mk_action
              (fun (e : 'expr)  (_loc : FanLoc.t)  -> (`cond e : 'item ))))])));
  if is_revised ~expr ~sem_expr_for_list
  then
    Gram.extend_single
      (comprehension_or_sem_expr_for_list : 'comprehension_or_sem_expr_for_list
                                              Gram.t )
      (None,
        (None, None,
          [([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
            `Skeyword ";";
            `Snterm
              (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
            `Skeyword "::";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (last : 'expr)  _  (mk : 'sem_expr_for_list)  _ 
                   (e : 'expr)  (_loc : FanLoc.t)  ->
                   (`App
                      (_loc,
                        (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e)),
                        (mk last)) : 'comprehension_or_sem_expr_for_list ))));
          ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
           `Skeyword "::";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (last : 'expr)  _  (e : 'expr)  (_loc : FanLoc.t)  ->
                  (`App
                     (_loc,
                       (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e)),
                       last) : 'comprehension_or_sem_expr_for_list ))))]))
  else ()
let _ = AstParsers.register_parser ("ListComprehension", apply)