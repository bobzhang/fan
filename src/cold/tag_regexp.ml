type tag_info =  {
  id: string;
  start: bool;
  action: int} 
type regexp =  
  | Chars of int* bool
  | Action of int
  | Tag of tag_info
  | Empty
  | Seq of regexp* regexp
  | Alt of regexp* regexp
  | Star of regexp 
