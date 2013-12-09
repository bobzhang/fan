

let (u,v) = Lexgen.make_single_dfa
    {shortest=false;
     clauses =
     [
      (%re{"a"},"action0");
      (%re{"b"},"action1");
      (%re{"c"},"action2");
      (%re{"d"},"action3");

     ]};;

(* let s = match v with [| Lexgen.Shift (No_remember, s); b|] -> s;; *)


%lex{
| "a" %{"action0"}
| "b" %{"action1"}
| "c" %{"action2"}
| "d" %{"action3"}
}


%lex{
| "e"("a" as s) "b" as v %{s}
| "b" %{"action1"}
| "c" %{"action2"}
| "d" %{"action3"}
}
  
(* val u : string Lexgen.automata_entry = *)
(*   {Lexgen.auto_mem_size = 0; auto_initial_state = (0, []); *)
(*    auto_actions = *)
(*     [(0, [], "action0"); (1, [], "action1"); (2, [], "action2"); *)
(*      (3, [], "action3")]} *)
