open Astf
class ['accu] c_fold_pattern_vars (f : string -> 'accu -> 'accu) init =
  object (_this__003_ : 'this_type__004_)
    inherit  Astf_fold.fold as super
    val acc = init
    method acc : 'accu= acc
    method! pat =
      function
      | (`Lid (_loc,s) : Astf.pat)|(`LabelS (_loc,`Lid (_,s)) : Astf.pat)
        |(`OptLablS (_loc,`Lid (_,s)) : Astf.pat) -> {<acc = f s acc>}
      | p -> super#pat p
  end
let fold_pattern_vars f p init =
  (((new c_fold_pattern_vars) f init)#pat p)#acc
let rec fold_bind_vars f bi acc =
  match bi with
  | (`And (_loc,bi1,bi2) : Astf.bind) ->
      fold_bind_vars f bi1 (fold_bind_vars f bi2 acc)
  | (`Bind (_loc,p,_) : Astf.bind) -> fold_pattern_vars f p acc
  | `Ant _ -> assert false
class ['accu] fold_free_vars (f : string -> 'accu -> 'accu) ?(env_init=
  Setf.String.empty) free_init =
  object (o : 'this_type__002_)
    inherit  Astf_fold.fold as super
    val free = (free_init : 'accu )
    val env = (env_init : Setf.String.t )
    method free = free
    method set_env env = {<env = env>}
    method add_atom s = {<env = Setf.String.add s env>}
    method add_pat p = {<env = fold_pattern_vars Setf.String.add p env>}
    method add_bind bi = {<env = fold_bind_vars Setf.String.add bi env>}
    method! exp =
      function
      | (`Lid (_loc,s) : Astf.exp)|(`LabelS (_loc,`Lid (_,s)) : Astf.exp)
        |(`OptLablS (_loc,`Lid (_,s)) : Astf.exp) ->
          if Setf.String.mem s env then o else {<free = f s free>}
      | (`LetIn (_loc,`Negative _,bi,e) : Astf.exp) ->
          (((o#add_bind bi)#exp e)#set_env env)#bind bi
      | (`LetIn (_loc,`Positive _,bi,e) : Astf.exp) ->
          (((o#add_bind bi)#exp e)#bind bi)#set_env env
      | (`For (_loc,`Lid (_,s),e1,e2,_,e3) : Astf.exp) ->
          ((((o#exp e1)#exp e2)#add_atom s)#exp e3)#set_env env
      | #vid'|(`New (_,_) : Astf.exp) -> o
      | (`ObjPat (_loc,p,cst) : Astf.exp) ->
          ((o#add_pat p)#clfield cst)#set_env env
      | e -> super#exp e
    method! case =
      function
      | (`CaseWhen (_loc,p,e1,e2) : Astf.case) ->
          (((o#add_pat p)#exp e1)#exp e2)#set_env env
      | m -> super#case m
    method! stru =
      function
      | (`External (_loc,`Lid (_,s),t,_) : Astf.stru) ->
          (o#ctyp t)#add_atom s
      | (`Value (_loc,`Negative _,bi) : Astf.stru) -> (o#bind bi)#add_bind bi
      | (`Value (_loc,`Positive _,bi) : Astf.stru) -> (o#add_bind bi)#bind bi
      | st -> super#stru st
    method! clexp =
      function
      | (`CeFun (_loc,p,ce) : Astf.clexp) ->
          ((o#add_pat p)#clexp ce)#set_env env
      | (`LetIn (_loc,`Negative _,bi,ce) : Astf.clexp) ->
          (((o#bind bi)#add_bind bi)#clexp ce)#set_env env
      | (`LetIn (_loc,`Positive _,bi,ce) : Astf.clexp) ->
          (((o#add_bind bi)#bind bi)#clexp ce)#set_env env
      | (`ObjPat (_loc,p,cst) : Astf.clexp) ->
          ((o#add_pat p)#clfield cst)#set_env env
      | ce -> super#clexp ce
    method! clfield =
      function
      | (`Inherit (_loc,_,_) : Astf.clfield) as cst -> super#clfield cst
      | (`InheritAs (_loc,_,ce,`Lid (_,s)) : Astf.clfield) ->
          (o#clexp ce)#add_atom s
      | (`CrVal (_loc,`Lid (_,s),_,_,e) : Astf.clfield) ->
          (o#exp e)#add_atom s
      | (`VirVal (_loc,`Lid (_,s),_,t) : Astf.clfield) ->
          (o#ctyp t)#add_atom s
      | cst -> super#clfield cst
    method! mexp =
      function
      | (`Struct (_loc,st) : Astf.mexp) -> (o#stru st)#set_env env
      | me -> super#mexp me
  end
let free_vars env_init e =
  let fold = (new fold_free_vars) Setf.String.add ~env_init Setf.String.empty in
  (fold#exp e)#free
