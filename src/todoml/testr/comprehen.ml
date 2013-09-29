
include PreCast.Make (struct end);
open Syntax;
open Lib;
open Ast;
let comprehension_or_sem_expr_for_list = Gram.mk "comprehension_or_sem_expr_for_list";
let item: Gram.t [= `cond of expr | `gen of (patt*expr) ]  = Gram.mk "item";
EXTEND Gram (* LOCAL: item; *)
    comprehension_or_sem_expr_for_list:
      [ "u"
       [  expr Level "top"{e}; ";"; sem_expr_for_list{mk} ->
            <:expr< [ $e :: $(mk <:expr< [] >>) ] >>
        | expr Level "top"{e}; ";" -> <:expr< [$e] >>
        | expr Level "top"{e}; "|"; LIST1 item SEP ";"{l} -> Expr.compr _loc e l
        | expr Level "top"{e}; "|"; LIST1 item SEP ";"{l}; "!" -> Expr.compr _loc e l
        | expr Level "top"{e} -> <:expr< [$e] >> ] ]  
    item:
      (* NP: These rules rely on being on this particular order. Which should
             be improved. *)
      [ [  TRY [ patt{p}; "<-" -> p]{p} ;  expr Level "top"{e} -> `gen (p, e)
        | expr Level "top"{e} -> `cond e ] ] 
  END;
  
EXTEND Gram (* LOCAL: item; *)
    comprehension_or_sem_expr_for_list:
      ["u"
       [ expr Level "top"{e}; "|"; LIST1 item SEP ";"{l} ; "!"-> Expr.compr _loc e l ] ]
  END;
  
Gram.dump Format.std_formatter comprehension_or_sem_expr_for_list;
