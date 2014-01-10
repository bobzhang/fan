
let lexer = Lex_fan.from_stream ;; 
%create{hash_p};;
begin
  %extend{
  hash_p:
  [L1 Str SEP "|" as xs %{
   let p =
     Ast_gen.bar_of_list
       (List.map
       (fun (x:Tokenf.txt) ->
         let v = x.txt in
         let i = Hashtbl.hash v  in
         %case{$int':i -> s = $str:v}) xs) in
   %exp{fun (s:string) -> (function | $p | _ -> false )} }]};
  %register{
  position:exp;
  name:hash_cmp;
  entry:hash_p;
  lexer:lexer
  }
end;;





(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_hash_cmp.cmo" *)
(* end: *)
