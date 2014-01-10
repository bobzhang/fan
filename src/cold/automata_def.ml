type tag_base =  
  | Start
  | End
  | Mem of int 
type tag_addr = (tag_base* int) 
type ident_info =  
  | Ident_string of bool* (tag_base* int)* (tag_base* int)
  | Ident_char of bool* (tag_base* int) 
type t_env = ((Locf.t* string)* ident_info) list 
