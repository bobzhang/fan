


    
let setup_op_parser entry p =
  Gramf.setup_parser entry
    (%parser{| (`Key x | `Sym x) when p x.txt  ->
      let _loc = x.loc in %exp{ $(lid:x.txt) }})


let symbolchars =
  ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
   '@'; '^'; '|'; '~'; '\\']
    
let symbolchar s i =
  let len = String.length s in
  try
    (for j = i to len - 1 do
      if not (List.mem s.[j] symbolchars) then
        raise Not_found
    done; true)
  with  Not_found -> false


(* let mk_lang_meta fan_quots fan_quot fan_stru fan_exp fan_clfield fan_ctyp =  *)
(*   EXTEND Gramf GLOBAL: fan_quots fan_quot  fan_stru fan_exp  fan_clfield fan_ctyp  ; *)
(*   fan_quots: *)
(*     ["top" *)
(*        [  L0 [ fan_quot{x}; ";" -> x ]{strs} -> %exp{ begin $list:strs end } ] ] *)
(*   fan_quot: *)
(*     ["top" *)
(*        [ "lang"; STRING{quot}-> begin      Ast_quotation.default:= quot;  unit_literal _loc end  *)
(*        | "lang_at"; STRING{tag}; STRING{quot}->begin *)
(*            eprintf "Set: quotation expander %s at position %s@." quot tag ; *)
(*            Ast_quotation.default_at_pos tag quot; *)
(*            unit_literal _loc *)
(*              end *)
(*        | "lang_clear" -> begin *)
(*            Ast_quotation.default := ""; *)
(*            Hashtbl.clear Ast_quotation.default_tbl; *)
(*            unit_literal _loc *)
(*        end ] ] END; *)
  (* fan_stru:[[STRING{file}; STRING{c} -> stru_of_file (file,c) ] ] *)
  (* fan_exp: [[ STRING{file}; STRING{c}-> exp_of_file (file,c) ]] *)
  (* fan_clfield: [[ STRING{file}; STRING{c}->  clfield_of_file (file,c) ]] *)
  (* fan_ctyp: [[STRING{file};STRING{c} ->  ctyp_of_file (file,c) ]] *)


  (* let mk_semi_list nt nts = *)
  (*   with rec_exp *)
  (*   {:extend|Gramf *)
  (*     nts:[ nt{b1};";";S{b2} -> %{$b1;$b2} | nt{b1};";" -> b1 | nt{b1} -> b1 ] *)
  (*   |}; *)
    
  
(* test wheter revised or not hack*)  
(* let test_pat_lessminus = *)
(*     Gramf.of_parser "test_pat_lessminus" *)
(*       (fun strm -> *)
(*         let rec skip_pat n = *)
(*           match Streamf.peek_nth n strm with *)
(*           [ Some (`Key "<-",_) -> n *)
(*           | Some (`Key ("[" | "[<"),_) -> *)
(*               skip_pat (ignore_upto "]" (n + 1) + 1) *)
(*           | Some (`Key "(",_) -> *)
(*               skip_pat (ignore_upto ")" (n + 1) + 1) *)
(*           | Some (`Key "{",_) -> *)
(*               skip_pat (ignore_upto "}" (n + 1) + 1) *)
(*           | Some (`Key ("as" | "::" | "," | "_"),_) *)
(*           | Some (`Lid _ | `Uid _, _) -> skip_pat (n + 1) *)
(*           | Some _ | None -> raise Streamf.NotConsumed ] *)
(*         and ignore_upto end_kwd n = *)
(*           match Streamf.peek_nth n strm with *)
(*           [ Some (`Key prm,_) when prm = end_kwd -> n *)
(*           | Some (`Key ("[" | "[<"),_) -> *)
(*               ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1) *)
(*           | Some (`Key "(",_) -> *)
(*               ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1) *)
(*           | Some (`Key "{",_) -> *)
(*               ignore_upto end_kwd (ignore_upto "}" (n + 1) + 1) *)
(*           | Some _ -> ignore_upto end_kwd (n + 1) *)
(*           | None -> raise Streamf.NotConsumed ] *)
(*         in *)
(*         skip_pat 1); *)


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gramlib.cmo" *)
(* end: *)
