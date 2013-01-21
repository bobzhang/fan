

open PreCast.Syntax;
open Lib;
open GramLib;  
{:lang.create|Gram comprehension_or_sem_expr_for_list |}  ;
  
let apply () = begin 
  {:lang.delete|Gram expr: [ "["; sem_expr_for_list; "]"] |};
  {:lang.extend| Gram local: item;
     expr: Level "simple"
     [ "["; comprehension_or_sem_expr_for_list{e}; "]" -> e ]
   
     comprehension_or_sem_expr_for_list:
     [  expr Level "top"{e}; ";"; sem_expr_for_list{mk} ->
       {:expr| [ $e :: $(mk {:expr| [] |}) ] |}
     | expr Level "top"{e}; ";" -> {:expr| [$e] |}
     | expr Level "top"{e}; "|"; L1 item SEP ";"{l} -> Expr.compr _loc e l
     | expr Level "top"{e} -> {:expr| [$e] |} ]   
     item:
     (* NP: These rules rely on being on this particular order. Which should
        be improved. *)(* FIXME LL *)
     [  TRY [ patt{p}; "<-" -> p]{p} ;  expr Level "top"{e} -> `gen (p, e)
     | expr Level "top"{e} -> `cond e ] |};
  if is_revised ~expr ~sem_expr_for_list then
    {:lang.extend|Gram
      comprehension_or_sem_expr_for_list:
      [  expr Level "top"{e}; ";"; sem_expr_for_list{mk}; "::"; expr{last} ->
        {:expr| [ $e :: $(mk last) ] |}
      | expr Level "top"{e}; "::"; expr{last} ->
          {:expr| [ $e :: $last ] |} ] |}
  else ()
end;


AstParsers.register_parser ("ListComprehension", apply);

















