value find_type_decls = object
  inherit Ast.fold as super;
  val accu = SMap.empty;
  (* method get = accu; *)
  (* method ctyp = *)
  (*   fun *)
  (*   [ Ast.TyDcl _ name _ _ _ as t -> {< accu = SMap.add name t accu >} *)
  (*   | t -> super#ctyp t ]; *)
end;


