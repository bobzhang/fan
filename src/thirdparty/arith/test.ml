



let rec token = %lex_fan{
 | @whitespace %{token lexbuf}
 | @ocaml_num_literal
 | @ocaml_float_literal
 | @ocaml_lid("let"|"be"|"in")
 | @ocaml_ant
 | @kwd_symbol("+"|"-"|"*"|"/"|"**"|"("|")")
 | @ocaml_eof
 | @default};;
let lexer = Lexing_util.adapt_to_stream token 
type arith =
   [ `Add of (arith * arith)
   | `Minus of (arith * arith)
   | `Times of (arith * arith)
   | `Divide of (arith * arith)
   | `Power of (arith * arith)
   | `Bind of (string * arith * arith)
   | `Flo of float
   | `Int of int
   | `Var of string]

let tbl = [("+",("Add",10,true));
           ("-",("Minus",10,true));
           ("*",("Times",20,true));
           ("/",("Divide",20,true));
           ("**",("Power",30,false))] ;;

%create{arith };; 
    
let add_prod left  nt action i op =
  %extend{
  nt: $i $bool:left
    [ nt ; $key:op; nt ${action}]}
    
let mk_ant nt i =
  %extend{
  nt: $i
    [Ant("",x) %{Tokenf.ant_expand Parsef.ep x}]}

let mk_action tag e2 _ e1 _loc =
  %ep{($vrn:tag ($e1,$e2) : arith)} in
begin
  List.iter
    (fun (op,(tag,i,left))
      -> add_prod left arith (mk_action tag) i op) tbl;

  %extend{
    arith:40
    ["("; arith as e ;")" %{e}
    |Flo f %ep{`Flo $flo:f}
    |Lid i %ep{`Var $str:i}
    |Int i %ep{`Int $int:i} ]};
  mk_ant arith 40;
  %extend{
    arith: 0 RA
    ["let"; Lid x ; "be"; arith as e1; "in"; arith as e2 %ep{`Bind($str:x,$e1,$e2)}]
  };
  Ast_quotation.of_ep ~lexer
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
