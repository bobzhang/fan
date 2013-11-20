


open Astf


(* syntax error class declaration TODO FIXME *)
class c_fold_pattern_vars ['accu] (f:string -> 'accu->'accu) init =  object
  inherit Objs.fold as super
  val acc = init
  method acc : 'accu = acc
  method! pat = function
    | %pat{$lid:s} | %pat{~$lid:s} | %pat{?$lid:s}
      -> {< acc = f s acc >}
    | p -> super#pat p
end

let fold_pattern_vars f p init = ((new c_fold_pattern_vars f init)#pat p)#acc

let rec fold_bind_vars f bi acc =
  match bi with
  | %bind{ $bi1 and $bi2 } ->
    fold_bind_vars f bi1 (fold_bind_vars f bi2 acc)
  | %bind{ $p = $_ } -> fold_pattern_vars f p acc
  | `Ant _ -> assert false 


class fold_free_vars ['accu] (f : string -> 'accu -> 'accu) ?(env_init = Setf.String.empty) free_init =  object (o)
  inherit Objs.fold as super
  val free : 'accu = free_init
  val env : Setf.String.t = env_init
      
  method free = free
  method set_env env = {< env = env >}
  method add_atom s = {< env = Setf.String.add s env >}
  method add_pat p = {< env = fold_pattern_vars Setf.String.add p env >}
  method add_bind bi = {< env = fold_bind_vars Setf.String.add bi env >}

  method! exp = function
    | %exp{ $lid:s } | %exp{ ~ $lid:s } | %exp{ ? $lid:s } ->
        if Setf.String.mem s env then o else {< free = f s free >}
          
    | %exp{ let $bi in $e } ->
        (((o#add_bind bi)#exp e)#set_env env)#bind bi
          
    | %exp{ let rec $bi in $e } ->
        (((o#add_bind bi)#exp e)#bind bi)#set_env env
          
    | %exp{ for $lid:s = $e1 $to:_ $e2 do  $e3 done } ->
        ((((o#exp e1)#exp e2)#add_atom s)#exp e3)#set_env env
          
    | #vid' | %exp@_{ new $_ } -> o (* %exp{ $id:_} deprecated possible a bug*)
          
    | %exp{ object ($p) $cst end } ->
        ((o#add_pat p)#clfield cst)#set_env env
          
    | e -> super#exp e 

  method! case = function
    | %case{ $pat:p when $e1 -> $e2 } ->
        (((o#add_pat p)#exp e1)#exp e2)#set_env env
    | m -> super#case m 

  method! stru = function
    | %stru{ external $lid:s : $t = $_ } ->
        (o#ctyp t)#add_atom s
    | %stru{ let $bi } ->
        (o#bind bi)#add_bind bi
    | %stru{ let rec $bi } ->
        (o#add_bind bi)#bind bi
    | st -> super#stru st 

  method! clexp = function
    | %clexp{ fun $p -> $ce } ->
        ((o#add_pat p)#clexp ce)#set_env env
    | %clexp{ let $bi in $ce } ->
        (((o#bind bi)#add_bind bi)#clexp ce)#set_env env
    | %clexp{ let rec $bi in $ce } ->
        (((o#add_bind bi)#bind bi)#clexp ce)#set_env env
    | %clexp{ object ($p) $cst end } ->
        ((o#add_pat p)#clfield cst)#set_env env
    | ce -> super#clexp ce 

  method! clfield = function
    | %clfield{ inherit $override:_ $_ } as cst -> super#clfield cst
    | %clfield{ inherit $override:_ $ce as $lid:s } ->
        (o#clexp ce)#add_atom s
    | %clfield{ val $override:_ $mutable:_ $lid:s = $e } ->
        (o#exp e)#add_atom s
    | %clfield{ val virtual $mutable:_ $lid:s : $t } ->
        (o#ctyp t)#add_atom s
    | cst -> super#clfield cst 

  method! mexp = function
    | %mexp{ struct $st end } ->
        (o#stru st)#set_env env
    | me -> super#mexp me 
end

let free_vars env_init e =
  let fold = new fold_free_vars Setf.String.add ~env_init Setf.String.empty in (fold#exp e)#free


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/freeVars.cmo" *)
(* end: *)
