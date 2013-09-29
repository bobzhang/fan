
















t str_item {:str| external $i : $t = "gho" $x $y "gho" |} ;
- : Ast.str_item =
`External
  (, "\\$:i", `Ant (, "\\$ctyp:t"), `LCons ("gho", `Ant (, "\\$str_list:x")))


 t str_item {:str| external $i : $t = "gho" $x $y "gho" |} |> ME.meta_str_item _loc ;
- : FanAst.expr =
`ExApp
  (,
   `ExApp
     (,
      `ExApp
        (, `ExApp (, `ExVrn (, "External"), `Id (, `Lid (, "_loc"))),
         `Str (, "\\$:i")),
      `Ant (, "\\$ctyp:t")),
   `ExApp
     (, `ExApp (, `ExVrn (, "LCons"), `Str (, "gho")),
      `Ant (, "\\$str_list:x")))

t str_item {:str| external $i : $t = "gho" $x $y "gho" |} |> ME.meta_str_item _loc |> expr_filter;
- : Ast.expr =
`ExApp
  (,
   `ExApp
     (,
      `ExApp
        (, `ExApp (, `ExVrn (, "External"), `Id (, `Lid (, "_loc"))),
         `Id (, `Lid (, "i"))),
      `Id (, `Lid (, "t"))),
   `ExApp
     (, `ExApp (, `ExVrn (, "LCons"), `Str (, "gho")), `Id (, `Lid (, "x"))))  








(* A file demonstrate how meta works*)

let u = Ast.ExInt _loc "\\$int:\"32\""; (* came from $(int:"32") *)

(* directly dump will cause an error here *)

Meta.ME.meta_expr _loc u;

ExApp (,
 ExApp (, ExId (, IdAcc (, IdUid (, "Ast"), IdUid (, "ExInt"))),
  ExId (, IdLid (, "_loc"))),
 ExStr (, "\\$int:\"32\""))


(Expr.antiquot_expander
   ~parse_patt:Syntax.AntiquotSyntax.parse_patt
   ~parse_expr:Syntax.AntiquotSyntax.parse_expr)#expr (Meta.ME.meta_expr _loc u);


ExApp (,
 ExApp (, ExId (, IdAcc (, IdUid (, "Ast"), IdUid (, "ExInt"))),
  ExId (, IdLid (, "_loc"))),
 ExStr (, "32"))


(Expr.antiquot_expander ~parse_patt:Syntax.AntiquotSyntax.parse_patt ~parse_expr:Syntax.AntiquotSyntax.parse_expr)#expr (Ast.ExStr _loc "\\$int:\"32\"");
- : Lib.Expr.Ast.expr = ExStr (, "32")

Syntax.AntiquotSyntax.parse_expr FanLoc.string_loc "\"32\"";
- : Ast.expr = ExStr (, "32")


let u = Ast.ExInt _loc "\\$int:x";
Meta.ME.meta_expr _loc u;
ExApp (,
 ExApp (, ExId (, IdAcc (, IdUid (, "Ast"), IdUid (, "ExInt"))),
  ExId (, IdLid (, "_loc"))),
 ExStr (, "\\$int:x"))
(Expr.antiquot_expander
   ~parse_patt:Syntax.AntiquotSyntax.parse_patt
   ~parse_expr:Syntax.AntiquotSyntax.parse_expr)#expr (Meta.ME.meta_expr _loc u);
- : Lib.Expr.Ast.expr =
ExApp (,
 ExApp (, ExId (, IdAcc (, IdUid (, "Ast"), IdUid (, "ExInt"))),
  ExId (, IdLid (, "_loc"))),
 ExId (, IdLid (, "x")))

    sequence:
      [ "let"; opt_rec{rf}; binding{bi}; "in"; expr{e}; sequence'{k} ->
            k {:expr| let $rec:rf $bi in $e |}
      | "let"; opt_rec{rf}; binding{bi}; ";"; SELF{el} ->
          {:expr| let $rec:rf $bi in $(Expr.mksequence _loc el) |}
      | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; "in"; expr{e}; sequence'{k} ->
          k {:expr| let module $m = $mb in $e |}
      | "let"; "module"; a_UIDENT{m}; module_binding0{mb}; ";"; SELF{el} ->
          {:expr| let module $m = $mb in $(Expr.mksequence _loc el) |}
      | "let"; "open"; module_longident{i}; "in"; SELF{e} ->
          {:expr| let open $id:i in $e |}
      | `ANTIQUOT (("list" as n),s) -> {:expr| $(anti:mk_anti ~c:"expr;" n s) |}
      | expr{e}; sequence'{k} -> k e ]

{:expr| begin $list:x end|}    

let u = Ast.ExAnt _loc (mk_anti ~c:"expr;" "list" "fuck") ;    
val u : Ast.expr = ExAnt (, "\\$listexpr;:fuck")

Meta.ME.meta_expr _loc u;
- : Lib.Meta.Ast.Ast.expr = ExAnt (, "\\$listexpr;:fuck")
(Expr.antiquot_expander
   ~parse_patt:Syntax.AntiquotSyntax.parse_patt
   ~parse_expr:Syntax.AntiquotSyntax.parse_expr)#expr (Meta.ME.meta_expr _loc u);
ExApp (, ExId (, IdAcc (, IdUid (, "Ast"), IdLid (, "exSem_of_list"))),
 ExId (, IdLid (, "fuck")))
    
    

let u =Ast.ExAnt _loc (mk_anti ~c:"anti" "list" "fuck") ;
val u : Ast.expr = ExAnt (, "\\$listanti:fuck")
Meta.ME.meta_expr _loc u;
- : Lib.Meta.Ast.Ast.expr = ExAnt (, "\\$listanti:fuck")
(Expr.antiquot_expander
   ~parse_patt:Syntax.AntiquotSyntax.parse_patt
   ~parse_expr:Syntax.AntiquotSyntax.parse_expr)#expr (Meta.ME.meta_expr _loc u);
- : Lib.Expr.Ast.expr = ExId (, IdLid (, "fuck"))
    
