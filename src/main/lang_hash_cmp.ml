

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
  Ast_quotation.of_exp ~name:{domain = Ns.lang; name = "hash_cmp"} ~entry:hash_p ();
end;;  
