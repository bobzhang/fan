
open Camlp4.PreCast;
open Mlast;
module MGram = MakeGram(Lexer);
value mk = MGram.Entry.mk ;
value clear = MGram.Entry.clear;  
value ml_exp  =  mk "ml_exp";
value ml_type = mk "ml_type"; 

begin 
  clear ml_exp;  clear ml_type; 
  EXTEND MGram GLOBAL:ml_exp  ml_type  ;
  ml_type:
    [ "arrow" RIGHTA
	[t1=SELF; "->"; t2 = SELF -> Arrow_type (t1,t2)]
    | "simple"
	[ "'"; i=LIDENT -> Var_type ("'" ^ i)
	| "("; t=SELF; ")" -> t
	| "("; l=SELF; "*"; r=SELF; ")" -> Pair_type (l,r)
	| "bool" -> Bool_type
	| "int" -> Int_type
	| `ANTIQUOT("",s) -> Type_Ant(_loc,s) ]] ;
  ml_exp:
    [ "top"
      [ "let"; r = opt_rec ; bi=LIDENT; "=" ; e = SELF ; "in"; x = SELF ->
         if r then Ml_letrec (Ml_pat_id bi,e,x)
	 else Ml_let (Ml_pat_id bi,e,x)
      | "let"; r = opt_rec; bi=LIDENT;
	ids = LIST0 [x=LIDENT->x]; "="; e = SELF; "in"; x=SELF ->
        let m_fun = List.fold_right (fun id x ->
	  Ml_fun(Ml_pat_id id,x)) ids e in 
        if r then Ml_letrec (Ml_pat_id bi,m_fun,x)
	else  Ml_let(Ml_pat_id bi,m_fun,x)
      | "let"; r = opt_rec; `ANTIQUOT(("patt" as n),s); "="; e = SELF;
	"in"; x=SELF ->	if r then
	  Ml_letrec (Ml_patAnt(_loc,n^":"^s),e,x)
	else
	  Ml_let (Ml_patAnt(_loc,n^":"^s),e,x)
      | "if"; b=SELF; "then"; l=SELF; "else"; r=SELF ->  Ml_if (b,l,r)
      | "fun"; l=LIDENT; "->"; e=SELF ->  Ml_fun (Ml_pat_id l,e)
      | "fun"; `ANTIQUOT(("patt" as n),s) ; "->"; e = SELF ->
          Ml_fun(Ml_patAnt(_loc, n^":"^s),e) (** needs a new antiquot *)
      ]
    | "<" LEFTA
      [ el = SELF; "<"; er=SELF -> Ml_binop(Ml_less,el,er)
      | el = SELF; ">"; er=SELF -> Ml_binop(Ml_gt,el,er)
      | el = SELF; "="; er=SELF -> Ml_binop(Ml_eq,el,er) ]
    | "+" LEFTA
      [ el =SELF ;"+"; er=SELF -> Ml_binop (Ml_add,el,er)
      | el =SELF; "-"; er=SELF -> Ml_binop (Ml_sub,el,er)]
    | "*"
      [ el=SELF; "*"; er=SELF -> Ml_binop(Ml_mult,el,er) ]	
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> Ml_app (e1, e2)
      | "fst"; e = SELF -> Ml_unop(Ml_fst,e)
      | "snd"; e = SELF -> Ml_unop(Ml_snd,e)]
    | "simple"
      [ "("; e= SELF;")" -> e
      | "("; el=SELF; ","; er=SELF; ")"-> Ml_pair (el,er)
      | a = INT -> Ml_int_const (int_of_string a)
      | a = "True" -> Ml_bool_const True
      | a = "False" -> Ml_bool_const False
      | a = LIDENT -> Ml_var a
      | `ANTIQUOT( ((""|"int"|"bool"|"lid") as n) ,s) -> begin
	(* prerr_endline (sprintf "antiquot string %s" s); *)
	Ml_Ant(_loc,(n^":"^s))
      end ] ] ;
  opt_rec:
    [["rec" -> True |  -> False ]]
  ;
  END ;
end ;

module M = Fan_basic.Make(MGram); 
let open M in do{
add_quotation  "ml"
  ~entry:ml_exp
  ~mexpr:MetaExpr.meta_ml_exp
  ~mpatt:MetaPatt.meta_ml_exp;
};
