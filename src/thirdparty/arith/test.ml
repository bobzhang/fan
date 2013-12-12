type arith =  (* Abstract syntax for the arith DDSL *)
   [ `Add    of (arith * arith)
   | `Minus  of (arith * arith)
   | `Times  of (arith * arith)
   | `Divide of (arith * arith)
   | `Power  of (arith * arith)
   | `Let    of (string * arith * arith)
   | `Int    of int
   | `Var    of string]

let rec token = %lex_fan{
 | @whitespace %{token lexbuf}
 | @ocaml_num_literal
 | @ocaml_lid("let"|"be"|"in")
 | @ocaml_ant
 | @kwd_symbol("+"|"-"|"*"|"/"|"**"|"("|")")
 | @ocaml_eof
 | @default};;

let lexer = Lexing_util.adapt_to_stream token;;

%create{ arith };;

let add_binop left nt action prec op = %extend{
    nt: $prec $bool:left
      [ nt; $key:op; nt   ${action}]}

let mk_ant nt prec = %extend{
    nt: $prec
      [ Ant("",x)   %{Tokenf.ant_expand Parsef.ep x}]}

let mk_action tag e2 _ e1 _loc =
  %ep{($vrn:tag ($e1,$e2) : arith)} 

let tbl = [("+",  ("Add",   10, true));
           ("-",  ("Minus", 10, true));
           ("*",  ("Times", 20, true));
           ("/",  ("Divide",20, true));
           ("**", ("Power", 30, false))] ;;

List.iter
 (fun (op,(tag,prec,left))
   -> add_binop left arith (mk_action tag) prec op) tbl ;;

%extend{
  arith: 40 true
    [ "("; arith as e; ")" %{e}
    | Lid s                %ep{`Var $str:s}
    | Int i                %ep{`Int $int:i} ]} ;;

mk_ant arith 40 ;;

%extend{
  arith: 0 false
    [ "let"; Lid x; "be"; arith as e1; "in"; arith as e2
        %ep{`Let($str:x,$e1,$e2)} ]} ;;

%register{
position: ep;
name:arith;
entry:arith;
lexer:lexer
}



