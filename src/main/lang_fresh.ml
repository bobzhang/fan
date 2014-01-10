(** A cool example to show how to make use of the *lazy* expansion
    of such context sensitive keyworkds [__MODULE__] and [__BIND__]
 *)

%create{fresh};;
%extend{
fresh:
  [ Lid x %exp{Gensym.fresh ~prefix:$str:x ()}
  | Ant ("", x)
      %exp{
    Gensym.fresh ~prefix:${Tokenf.ant_expand Parsef.exp x} ()
    }
  | %exp{Gensym.fresh ()}]        
};;
let lexer = Lex_fan.from_stream in 
%register{
name:fresh;
entry:fresh;
position:exp;
lexer:lexer
};;


(* local variables: *)
(* compile-command: "cd .. && pmake  main_annot/lang_fresh.cmo" *)
(* end: *)
