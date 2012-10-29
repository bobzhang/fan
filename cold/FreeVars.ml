open FanUtil
class ['accu] c_fold_pattern_vars f init
  = object 
      inherit  Camlp4Ast.fold as super
      val acc = init
      method acc : 'accu= acc
      method patt =
        function
        | Ast.PaId (_,Ast.IdLid (_,s))|Ast.PaLab (_,s,Ast.PaNil _)|Ast.PaOlb
            (_,s,Ast.PaNil _) -> {<acc = f s acc>}
        | p -> (super#patt) p
    end
let fold_pattern_vars f p init =
  ((((new c_fold_pattern_vars) f init)#patt) p)#acc
let rec fold_binding_vars f bi acc =
  match bi with
  | Ast.BiAnd (_,bi1,bi2) ->
      fold_binding_vars f bi1 (fold_binding_vars f bi2 acc)
  | Ast.BiEq (_,p,_) -> fold_pattern_vars f p acc
  | Ast.BiNil _ -> acc
  | Ast.BiAnt (_,_) -> assert false
class ['accu] fold_free_vars (f : string  -> 'accu -> 'accu)
  ?(env_init=SSet.empty) free_init
  = object (o)
      inherit  Camlp4Ast.fold as super
      val free = (free_init :'accu )
      val env = (env_init :SSet.t  )
      method free = free
      method set_env env = {<env = env>}
      method add_atom s = {<env = SSet.add s env>}
      method add_patt p = {<env = fold_pattern_vars SSet.add p env>}
      method add_binding bi = {<env = fold_binding_vars SSet.add bi env>}
      method expr =
        function
        | Ast.ExId (_,Ast.IdLid (_,s))|Ast.ExLab (_,s,Ast.ExNil _)|Ast.ExOlb
            (_,s,Ast.ExNil _) ->
            if SSet.mem s env then o else {<free = f s free>}
        | Ast.ExLet (_,Ast.ReNil ,bi,e) ->
            (((((((o#add_binding) bi)#expr) e)#set_env) env)#binding) bi
        | Ast.ExLet (_,Ast.ReRecursive ,bi,e) ->
            (((((((o#add_binding) bi)#expr) e)#binding) bi)#set_env) env
        | Ast.ExFor (_,s,e1,e2,_,e3) ->
            (((((((((o#expr) e1)#expr) e2)#add_atom) s)#expr) e3)#set_env)
              env
        | Ast.ExId (_,_)|Ast.ExNew (_,_) -> o
        | Ast.ExObj (_,p,cst) ->
            (((((o#add_patt) p)#class_str_item) cst)#set_env) env
        | e -> (super#expr) e
      method match_case =
        function
        | Ast.McArr (_,p,e1,e2) ->
            (((((((o#add_patt) p)#expr) e1)#expr) e2)#set_env) env
        | m -> (super#match_case) m
      method str_item =
        function
        | Ast.StExt (_,s,t,_) -> (((o#ctyp) t)#add_atom) s
        | Ast.StVal (_,Ast.ReNil ,bi) -> (((o#binding) bi)#add_binding) bi
        | Ast.StVal (_,Ast.ReRecursive ,bi) ->
            (((o#add_binding) bi)#binding) bi
        | st -> (super#str_item) st
      method class_expr =
        function
        | Ast.CeFun (_,p,ce) ->
            (((((o#add_patt) p)#class_expr) ce)#set_env) env
        | Ast.CeLet (_,Ast.ReNil ,bi,ce) ->
            (((((((o#binding) bi)#add_binding) bi)#class_expr) ce)#set_env)
              env
        | Ast.CeLet (_,Ast.ReRecursive ,bi,ce) ->
            (((((((o#add_binding) bi)#binding) bi)#class_expr) ce)#set_env)
              env
        | Ast.CeStr (_,p,cst) ->
            (((((o#add_patt) p)#class_str_item) cst)#set_env) env
        | ce -> (super#class_expr) ce
      method class_str_item =
        function
        | Ast.CrInh (_,_,_,"") as cst -> (super#class_str_item) cst
        | Ast.CrInh (_,_,ce,s) -> (((o#class_expr) ce)#add_atom) s
        | Ast.CrVal (_,s,_,_,e) -> (((o#expr) e)#add_atom) s
        | Ast.CrVvr (_,s,_,t) -> (((o#ctyp) t)#add_atom) s
        | cst -> (super#class_str_item) cst
      method module_expr =
        function
        | Ast.MeStr (_,st) -> (((o#str_item) st)#set_env) env
        | me -> (super#module_expr) me
    end
let free_vars env_init e =
  let fold = (new fold_free_vars) SSet.add ~env_init SSet.empty in
  ((fold#expr) e)#free