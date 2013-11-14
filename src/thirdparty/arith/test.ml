



let rec (kwds,lexer) = %lex_fan{
 | @whitespace %{lexer lexbuf}
 | @ocaml_num_literal
 | @ocaml_lid
 | @ocaml_float_literal
 | @kwd_symbol("+"|"-"|"*"|"/"|"**"|"("|")")
 | @ocaml_eof
 | @default};;

type arith =
   [`Add of (arith * arith)
   |`Minus of (arith * arith)
   |`Times of (arith * arith)
   |`Divide of (arith * arith)
   |`Power of (arith * arith)
   | `Flo of float
   | `Int of int
   | `Var of string]

let tbl = [("+",("Add",10,true));
           ("-",("Minus",10,true));
           ("*",("Times",20,true));
           ("/",("Divide",20,true));
           ("**",("Power",30,false))] ;;

%create{(lexer : Gramf.t)
          arith };;
    
let add_prod ?(left=true)  nt action i op =
  %extend{
  nt: $i $bool:left
    [ nt ; $key:op; nt ${action}]} in

let mk_action tag e2 (op:Tokenf.txt) e1 _loc =
  %exp{($vrn:tag ($e1,$e2) : arith)} in
begin
  List.iter
    (fun (op,(tag,i,left))
      -> add_prod ~left arith (mk_action tag) i op) tbl;

  %extend{
    arith:40
    ["("; arith as e ;")" %{e}
    |Flo f %exp{`Flo $flo:f}
    |Lid i %exp{`Var $lid:i}
    |Int i %exp{`Int $int:i} ]};
  
  Ast_quotation.of_exp ~lexer:from_stream
    ~name:(Ns.lang,"arith") ~entry:arith ()
end
    
(* better error message *)    
(* Gramlib.parse_string e_o "3 + 2 + 3 ";; *)
(* Exception: *)
(* Locf.Exc_located *)
(*  ({Locf.loc_start = [1,0+0]; loc_end = [1,0+1]; loc_ghost = false}, *)
(*  Streamf.Error "illegal begin of e_eoi"). *)
(* print eoi -- first *)
(* let from_lexbuf lb = *)

(* let from_stream (loc:Locf.t) strm = *)
(*   let lb = Lexing.from_function (Lexing_util.lexing_store strm) in begin *)
(*     lb.lex_abs_pos <- loc.loc_start.pos_cnum; *)
(*     lb.lex_curr_p <- loc.loc_start; *)
(*      Streamf.from (fun _ -> Some (token lb)); *)
(*   end;; *)
 (* | @symbol *)
(* %create_lexer{ *)
(*   name:"lexer" *)
(* };; *)
(* let g = *)
(*   Gramf.create_lexer ~annot:"arith" *)
(*     ~keywords:["+";"-";"*";"/";"**";"(";")"] ();; *)
