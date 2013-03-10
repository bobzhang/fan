

(** This file is related to BOOTSTRAPPING
    take care when you want to remove some functions or
    rename the file itself
 *)  
open FanOps;
open  Ast;
module type META_LOC = sig
    (*   (\** The first location is where to put the returned pattern. *)
    (*       Generally it's _loc to match with {:patt| ... |} quotations. *)
    (*       The second location is the one to treat. *\) *)
    (* val meta_loc_patt : FanLoc.t -> FanLoc.t -> (\* patt *\)ep; *)
    (*   (\** The first location is where to put the returned expression. *)
    (*       Generally it's _loc to match with {:expr| ... |} quotations. *)
    (*       The second location is the one to treat. *\) *)
    (* val meta_loc_expr : FanLoc.t -> FanLoc.t -> (\* expr *\)ep; *)
  val meta_loc: loc -> loc -> ep;
end;



  

{:fans|keep off; derive ( MetaExpr (* GenLoc *)); |};
  
{:ocaml|INCLUDE "src/Ast.ml"; |};



module Make(MetaLoc:META_LOC) = struct
  include MetaLoc;
  __MetaExpr__;
end;
    


include AstLoc;
  
let match_pre = object (self)
  inherit Objs.map; (* as super; *)
  method! match_case = with match_case fun
   [ {| $pat:p -> $e |} -> {| $pat:p -> fun () -> $e |}
   | {| $pat:p when $e -> $e1 |} -> {| $pat:p when $e -> fun () -> $e1 |}
   | {| $a1 | $a2 |} -> {| $(self#match_case a1) | $(self#match_case a2) |}
   (* | {| |} -> {| |} *)
   | `Ant(_loc,x) -> `Ant(_loc, FanUtil.add_context x "lettry")];
end;
