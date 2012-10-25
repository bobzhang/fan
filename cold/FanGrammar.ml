type loc =   FanLoc.t  
type name =  { expr: Ast.expr ; tvar: string ; loc: loc } 
type styp = 
  STlid of  loc * string 
| STapp of  loc * styp * styp 
| STquo of  loc * string 
| STself of  loc * string 
| STtok of  loc 
| STtyp of  Ast.ctyp  
type attr =   string  
type entry = 
{ name: name ; pos: Ast.expr  option ; levels: level  list } and level = 
{ label: string  option ; assoc: Ast.expr  option ; rules: rule  list } and
rule =  { prod: symbol  list ; action: Ast.expr  option } and symbol = 
{ used: string  list ; text: text ; styp: styp ; pattern: Ast.patt  option } and
text = 
  TXmeta of  loc * string * text  list * Ast.expr * styp 
| TXlist of  loc * bool * symbol * symbol  option 
| TXnext of  loc 
| TXnterm of  loc * name * string  option 
| TXopt of  loc * text 
| TXtry of  loc * text 
| TXrules of  loc *( text  list * Ast.expr ) list 
| TXself of  loc 
| TXkwd of  loc * string 
| TXtok of  loc * Ast.expr * attr * string  
type used =  Unused | UsedScanned | UsedNotScanned 