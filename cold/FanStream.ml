type 'a item_or_def = 
  SdStr of 'a 
| SdDef of  string *( string  list * Ast.expr ) option 
| SdUnd of  string 
| SdITE of  bool *'a  item_or_def  list *'a  item_or_def  list 
| SdLazy of 'a  Lazy.t  