
%import{
Location_util:
  from_lexbuf as (!!)
   ;
};;    



let  rec token  = %lex_fan{
  | @whitespace %{token lexbuf}
  | @ocaml_lid("_"|"is")
  | @ocaml_uid
  | @ocaml_int
  | @kwd_symbol("."|":"|"-"|","|"("|")"|"="|
    "\\="|"=:="|"=\\="|"<"|
    "=<"|">"|">="|"+"| "!"|
    "["|"]"|"|"|"%:"|"?"|":-") 
  | @ocaml_comment %{token lexbuf}
  | @ocaml_eof
  | @default}

    


let from_stream = Lexing_util.adapt_to_stream token 
