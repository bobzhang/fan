open LibUtil
class ['accu] c_fold_pattern_vars f init =
  object 
    inherit  FanAst.fold as super
    val acc = init
    method acc : 'accu= acc
    method! patt =
      function
      | `PaId (_loc,`IdLid (_,s))|`PaLab (_loc,s,`PaNil _)|`PaOlb
                                                             (_loc,s,
                                                              `PaNil _)
          -> {<acc = f s acc>}
      | p -> super#patt p
  end
let fold_pattern_vars f p init =
  (((new c_fold_pattern_vars) f init)#patt p)#acc
let rec fold_binding_vars f bi acc =
  match bi with
  | `BiAnd (_loc,bi1,bi2) ->
      fold_binding_vars f bi1 (fold_binding_vars f bi2 acc)
  | `BiEq (_loc,p,_) -> fold_pattern_vars f p acc
  | `BiNil _loc -> acc
  | `Ant (_loc,_) -> assert false
class ['accu] fold_free_vars (f : string -> 'accu -> 'accu) ?(env_init=
  SSet.empty) free_init =
  object (o)
    inherit  FanAst.fold as super
    val free = (free_init : 'accu )
    val env = (env_init : SSet.t )
    method free = free
    method set_env env = {<env = env>}
    method add_atom s = {<env = SSet.add s env>}
    method add_patt p = {<env = fold_pattern_vars SSet.add p env>}
    method add_binding bi = {<env = fold_binding_vars SSet.add bi env>}
    method! expr =
      function
      | `ExId (_loc,`IdLid (_,s))|`ExLab (_loc,s,`ExNil _)|`ExOlb
                                                             (_loc,s,
                                                              `ExNil _)
          -> if SSet.mem s env then o else {<free = f s free>}
      | `ExLet (_loc,`ReNil,bi,e) ->
          (((o#add_binding bi)#expr e)#set_env env)#binding bi
      | `ExLet (_loc,`ReRecursive,bi,e) ->
          (((o#add_binding bi)#expr e)#binding bi)#set_env env
      | `ExFor (_loc,s,e1,e2,_,e3) ->
          ((((o#expr e1)#expr e2)#add_atom s)#expr e3)#set_env env
      | `ExId (_loc,_)|`ExNew (_loc,_) -> o
      | `ExObj (_loc,p,cst) ->
          ((o#add_patt p)#class_str_item cst)#set_env env
      | e -> super#expr e
    method! match_case =
      function
      | `McArr (_loc,p,e1,e2) ->
          (((o#add_patt p)#expr e1)#expr e2)#set_env env
      | m -> super#match_case m
    method! str_item =
      function
      | `StExt (_loc,s,t,_) -> (o#ctyp t)#add_atom s
      | `StVal (_loc,`ReNil,bi) -> (o#binding bi)#add_binding bi
      | `StVal (_loc,`ReRecursive,bi) -> (o#add_binding bi)#binding bi
      | st -> super#str_item st
    method! class_expr =
      function
      | `CeFun (_loc,p,ce) -> ((o#add_patt p)#class_expr ce)#set_env env
      | `CeLet (_loc,`ReNil,bi,ce) ->
          (((o#binding bi)#add_binding bi)#class_expr ce)#set_env env
      | `CeLet (_loc,`ReRecursive,bi,ce) ->
          (((o#add_binding bi)#binding bi)#class_expr ce)#set_env env
      | `CeStr (_loc,p,cst) ->
          ((o#add_patt p)#class_str_item cst)#set_env env
      | ce -> super#class_expr ce
    method! class_str_item =
      function
      | `CrInh (_loc,_,_,"") as cst -> super#class_str_item cst
      | `CrInh (_loc,_,ce,s) -> (o#class_expr ce)#add_atom s
      | `CrVal (_loc,s,_,_,e) -> (o#expr e)#add_atom s
      | `CrVvr (_loc,s,_,t) -> (o#ctyp t)#add_atom s
      | cst -> super#class_str_item cst
    method! module_expr =
      function
      | `MeStr (_loc,st) -> (o#str_item st)#set_env env
      | me -> super#module_expr me
  end
let free_vars env_init e =
  let fold = (new fold_free_vars) SSet.add ~env_init SSet.empty in
  (fold#expr e)#free
