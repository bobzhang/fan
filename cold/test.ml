let find_type_decls =
 object inherit Ast.fold as super val accu = SMap.empty end
