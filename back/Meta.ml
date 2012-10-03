
module Ast = Camlp4Ast; (* FIXME: rename it into Camlp4Ast later *)

(* never used before *)    
module MetaLocVar : Ast.META_LOC= struct
  let meta_loc_patt _loc _ = <:patt< $(lid:!FanLoc.name) >>;
  let meta_loc_expr _loc _ = <:expr< $(lid:!FanLoc.name) >>;
end;
    
(* module MetaLoc *)

(* module MetaGhostLoc *)

module MetaLoc : Ast.META_LOC= struct
  let meta_loc_patt _loc location =
    let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple location in
    <:patt< FanLoc.of_tuple
      ($`str:a, $`int:b, $`int:c, $`int:d,
       $`int:e, $`int:f, $`int:g,
       $(if h then <:patt< True >> else <:patt< False >> )) >>;
  let meta_loc_expr _loc location =
    let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple location in
    <:expr< FanLoc.of_tuple
      ($`str:a, $`int:b, $`int:c, $`int:d,
       $`int:e, $`int:f, $`int:g,
       $(if h then <:expr< True >> else <:expr< False >> )) >>;
end;
  
module MetaGhostLoc : Ast.META_LOC= struct (* MetaAction *)
  let meta_loc_patt _loc _ = <:patt< FanLoc.ghost >>; (* FIXME *)
  let meta_loc_expr _loc _ = <:expr< FanLoc.ghost >>;
end;


module MetaLocQuotation = struct
  let loc_name = ref None;
  let meta_loc_expr _loc loc =
    match !loc_name with
    [ None -> <:expr< $(lid:!FanLoc.name) >>
    | Some "here" -> MetaLoc.meta_loc_expr _loc loc
    | Some x -> <:expr< $lid:x >> ];
   (* FIXME track the location of the quotation
      read the list for the detailed usage
    *)   
  let meta_loc_patt _loc _ = <:patt< _ >>;
end;


