type regular_expression =  
  | Epsilon
  | Characters of Fcset.t
  | Eof
  | Sequence of regular_expression* regular_expression
  | Alternative of regular_expression* regular_expression
  | Repetition of regular_expression
  | Bind of regular_expression* FAst.lident 
type entry =  {
  shortest: bool;
  clauses: (regular_expression* FAst.exp) list} 