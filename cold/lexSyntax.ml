type regular_expression =  
  | Epsilon
  | Characters of Cset.t
  | Eof
  | Sequence of regular_expression* regular_expression
  | Alternative of regular_expression* regular_expression
  | Repetition of regular_expression
  | Bind of regular_expression* Ast.lident 

type entry =  {
  shortest: bool;
  clauses: (regular_expression * Ast.exp) list} 