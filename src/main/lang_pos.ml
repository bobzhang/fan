


(*
  %pos{filename[1]}

  %pos{filename[1,3+4]}
 *)  

%create{Gramf
(pos : Locf.position Gentry.t)
};;

%extend{
pos:
  [Str s ; "["; Int i; "]" %{
   {
    pos_fname = s ;
    pos_lnum = -1 ;
    pos_bol = 0;
    pos_cnum = int_of_string i} }
  |Str s ; "["; Int i; "-"; Int j ; "+"; Int k ; "]" %{
   let pos_lnum = int_of_string i in
   let pos_bol = int_of_string j in
   let pos_cnum = pos_bol + int_of_string k in   
   {
    pos_fname = s;
    pos_lnum ;
    pos_bol ;
    pos_cnum 
  }
  }
  ]
};;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_pos.cmo" *)
(* end: *)
