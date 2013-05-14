open Ast

open LibUtil

class ['accu] c_fold_pattern_vars (f : string -> 'accu -> 'accu) init =
  object 
    inherit  Objs.fold as super
    val acc = init
    method acc : 'accu= acc
    method! pat =
      function
      | (`Lid (_loc,s) : Ast.pat)|(`LabelS (_loc,`Lid (_,s)) : Ast.pat)
        |(`OptLablS (_loc,`Lid (_,s)) : Ast.pat) -> {<acc = f s acc>}
      | p -> super#pat p
  end

let fold_pattern_vars f p init =
  (((new c_fold_pattern_vars) f init)#pat p)#acc

let rec fold_bind_vars f bi acc =
  match bi with
  | (`And (_loc,bi1,bi2) : Ast.bind) ->
      fold_bind_vars f bi1 (fold_bind_vars f bi2 acc)
  | (`Bind (_loc,p,_) : Ast.bind) -> fold_pattern_vars f p acc
  | `Ant _ -> assert false

class ['accu] fold_free_vars (f : string -> 'accu -> 'accu) ?(env_init=
  SSet.empty) free_init =
  object (o)
    inherit  Objs.fold as super
    val free = (free_init : 'accu )
    val env = (env_init : SSet.t )
    method free = free
    method set_env env = {<env = env>}
    method add_atom s = {<env = SSet.add s env>}
    method add_pat p = {<env = fold_pattern_vars SSet.add p env>}
    method add_bind bi = {<env = fold_bind_vars SSet.add bi env>}
    method! exp =
      function
      | (`Lid (_loc,s) : Ast.exp)|(`LabelS (_loc,`Lid (_,s)) : Ast.exp)
        |(`OptLablS (_loc,`Lid (_,s)) : Ast.exp) ->
          if SSet.mem s env then o else {<free = f s free>}
      | (`LetIn (_loc,`Negative _,bi,e) : Ast.exp) ->
          (((o#add_bind bi)#exp e)#set_env env)#bind bi
      | (`LetIn (_loc,`Positive _,bi,e) : Ast.exp) ->
          (((o#add_bind bi)#exp e)#bind bi)#set_env env
      | (`For (_loc,`Lid (_,s),e1,e2,_,e3) : Ast.exp) ->
          ((((o#exp e1)#exp e2)#add_atom s)#exp e3)#set_env env
      | #vid'|(`New (_,_) : Ast.exp) -> o
      | (`ObjPat (_loc,p,cst) : Ast.exp) ->
          ((o#add_pat p)#clfield cst)#set_env env
      | e -> super#exp e
    method! case =
      function
      | (`CaseWhen (_loc,p,e1,e2) : Ast.case) ->
          (((o#add_pat p)#exp e1)#exp e2)#set_env env
      | m -> super#case m
    method! stru =
      function
      | (`External (_loc,`Lid (_,s),t,_) : Ast.stru) -> (o#ctyp t)#add_atom s
      | (`Value (_loc,`Negative _,bi) : Ast.stru) -> (o#bind bi)#add_bind bi
      | (`Value (_loc,`Positive _,bi) : Ast.stru) -> (o#add_bind bi)#bind bi
      | st -> super#stru st
    method! clexp =
      function
      | (`CeFun (_loc,p,ce) : Ast.clexp) ->
          ((o#add_pat p)#clexp ce)#set_env env
      | (`LetIn (_loc,`Negative _,bi,ce) : Ast.clexp) ->
          (((o#bind bi)#add_bind bi)#clexp ce)#set_env env
      | (`LetIn (_loc,`Positive _,bi,ce) : Ast.clexp) ->
          (((o#add_bind bi)#bind bi)#clexp ce)#set_env env
      | (`ObjPat (_loc,p,cst) : Ast.clexp) ->
          ((o#add_pat p)#clfield cst)#set_env env
      | ce -> super#clexp ce
    method! clfield =
      function
      | (`Inherit (_loc,_,_) : Ast.clfield) as cst -> super#clfield cst
      | (`InheritAs (_loc,_,ce,`Lid (_,s)) : Ast.clfield) ->
          (o#clexp ce)#add_atom s
      | (`CrVal (_loc,`Lid (_,s),_,_,e) : Ast.clfield) ->
          (o#exp e)#add_atom s
      | (`VirVal (_loc,`Lid (_,s),_,t) : Ast.clfield) ->
          (o#ctyp t)#add_atom s
      | cst -> super#clfield cst
    method! mexp =
      function
      | (`Struct (_loc,st) : Ast.mexp) -> (o#stru st)#set_env env
      | me -> super#mexp me
  end

let free_vars env_init e =
  let fold = (new fold_free_vars) SSet.add ~env_init SSet.empty in
  (fold#exp e)#free