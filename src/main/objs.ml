open StdFan
open Astf
(* let strip_ant ant = ant ;; *)
%fans{
keep off;
derive((* Map2 Fold2 OIter   OEq *)
  Print OPrint Map Fold   MapWrapper PrintWrapper);
};;


%ocaml{%include{ "../common/astf.ml"};;  };;

(* change all the [loc] to [ghost] *)    
class reloc _loc = object
  inherit map 
  method! loc _ = _loc
end

(*
  {[]}
 *)  
let wildcarder = object (self)
  inherit map as super
  method! pat = function
    | %pat{ $lid:_ } -> %pat{ _ }
    | %pat{ ($p as $_) } -> self#pat p
    | p -> super#pat p 
end

(* let print = new print  *)

let () =     begin
  Ast2pt.dump_ident := dump_ident;
  Ast2pt.dump_ident := dump_ident ;           
  Ast2pt.dump_row_field := dump_row_field ;       
  Ast2pt.dump_name_ctyp := dump_name_ctyp ;       
  Ast2pt.dump_constr := dump_constr ;          
  Ast2pt.dump_mtyp := dump_mtyp ;            
  Ast2pt.dump_ctyp := dump_ctyp ;            
  Ast2pt.dump_or_ctyp := dump_or_ctyp ;         
  Ast2pt.dump_pat := dump_pat ;             
  Ast2pt.dump_type_parameters := dump_type_parameters ; 
  Ast2pt.dump_exp := dump_exp ;             
  Ast2pt.dump_case := dump_case ;            
  Ast2pt.dump_rec_exp := dump_rec_exp ;         
  Ast2pt.dump_type_constr := dump_type_constr ;     
  Ast2pt.dump_typedecl := dump_typedecl ;        
  Ast2pt.dump_sigi := dump_sigi ;            
  Ast2pt.dump_mbind := dump_mbind ;           
  Ast2pt.dump_mexp := dump_mexp ;            
  Ast2pt.dump_stru := dump_stru ;            
  Ast2pt.dump_cltyp := dump_cltyp ;           
  Ast2pt.dump_cldecl := dump_cldecl ;          
  Ast2pt.dump_cltdecl := dump_cltdecl ;         
  Ast2pt.dump_clsigi := dump_clsigi ;          
  Ast2pt.dump_clexp := dump_clexp ;           
  Ast2pt.dump_clfield := dump_clfield 
end    

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/objs.cmo" *)
(* end: *)
