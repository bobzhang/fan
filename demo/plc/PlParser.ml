open PlAst ;
module Ast = Camlp4Ast;
{:extend.create|Gram prog rule goal term clauses args|};

{:extend|Gram
 prog:
  [ L0 rule{r} ->
    let res = List.fold_left
     (fun m ((p,ts,_),goals,_loc) ->
       let l =
	 try PredMap.find p m
	 with Not_found -> []
       in PredMap.add p [(ts,goals,_loc)::l] m
     ) PredMap.empty r in
    (* let prog = (List.concat [PlTranslate.prog_atoms _loc res; PlTranslate.prog_rules _loc res]) in *)
    (* {:str_item| $list:prog |} *)
    {:str_item| $(list:PlTranslate.prog_atoms _loc res); $(list:PlTranslate.prog_rules _loc res) |}
  ]
  rule:
  [goal{g}; OPT clauses{c};"." ->
    let c =
      match c with
      [Some c -> c | None -> []] in
    (g,c,_loc)
       ]
  clauses:
  [":"; "-";  L1 goal SEP "," {r} -> r ]
  goal:
  [`LID x; OPT args {t} ->
    let terms =
      match t with
      [Some t -> t | None -> []] in
    ((x, List.length terms), terms, _loc) ]
  args:
  ["("; L1 term SEP ","{r}; ")" -> r ]
  term:
  [ `LID x -> if x = "_" then Anon _loc else (Atom (x,_loc))
  | `UID x -> Var(x,_loc) ]
|};

Fan.P.Syntax.Quotation.add_quotation_of_str_item ~name:"plc" ~entry:prog; 

