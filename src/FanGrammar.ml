type loc = FanLoc.t;

(* every entry has a name *)  
type name  = { expr : Ast.expr; tvar : string; loc : loc };

(* we need to define a new ADT only because
   we did not find a way to express STself and STtok yet  *)
type styp =
 [ STlid of loc and string
 | STapp of loc and styp and styp
 | STquo of loc and string
 | STself of loc and string
 | STtok of loc
 | STtyp of Ast.ctyp ] ;



(* Normal, Antiquot, etc. translated to
   `Normal `Antiquot
 *)
type attr = string;

type entry   = {
  name : name ;
  pos : option Ast.expr;
  levels : list level;
}
and level  ={
  label : option string;
  assoc : option Ast.expr;
  rules : list rule
}
and rule ={
  prod : list symbol;
  action : option Ast.expr
}
and symbol ={
  text : text;
  styp : styp;
  pattern : option Ast.patt
}
and text =
 [ TXmeta of loc and list string and list text and Ast.expr and styp
 | TXlist of loc and bool and symbol and option symbol
 | TXnterm of loc and name  and option string
 | TXopt of loc and text 
 | TXtry of loc and text 
 | TXrules of loc and list (list text * Ast.expr)
 | TXself of loc
 | TXnext of loc       
 | TXkwd of loc and string
 | TXtok of loc and Ast.expr and attr and string];
(** The first is the match function expr,
    the second is the string description.
    The description string will be used for
    grammar insertion and left factoring.
    Keep this string normalized and well comparable. *) 
  
type used = [ Unused | UsedScanned | UsedNotScanned ];

