
open PlAst
open FAst
open Ast_gen

let format_loc _loc =
  Locf.to_string _loc

let warning _loc msg =
  Printf.eprintf "%s:\nWarning: %s\n" (format_loc _loc) msg

let concat_expr ?sep _loc strs : exp option =
  List.fold_left (fun acc s ->
    match acc with
    | Some e ->
	begin
	  match sep with
	  | Some sep -> Some %exp{$e ^ $str:sep ^ $s}
	  | None -> Some %exp{$e ^ $s}
	end
    | None -> Some s)
    None strs

let fun_args _loc args body =
  if args = [] then
    %exp{fun () -> $body}
  else List.fold_right (fun arg body ->
    %exp{fun $arg -> $body}) args body

let fun_apply _loc e args =
  if args = [] then
    %exp{$e ()}
  else List.fold_left (fun e arg ->
    %exp{$e $arg}) e args

let sequence _loc = fun
  | [e] -> e
  | es -> %exp{do ${sem_of_list es} done}

let test_expr _loc l =
  let l = List.map (fun (a,b) -> (lid _loc a, lid _loc b)) l in
  let rec aux e = fun
    | [] -> e
    | (a,b)::l -> aux %exp{$e && $a = $b} l
  in match l with
    | [] -> None
    | (a,b)::l -> Some (aux %exp{$a = $b} l)

let if_expr _loc t body nbody : exp =
  match t with
  | None -> body
  | Some t -> %exp{if $t then $body else $nbody}

let is_always_patt = fun
 | %pat{$lid:_ } -> true
 | _ -> false

let remove_skip l skip =
  List.rev (List.fold_left2 (fun l e s -> if s then l else (e::l)) [] l skip)

let filter_matches es cases =
  let skip = List.fold_left (fun skip _ -> (true::skip)) [] es in
  let skip = List.fold_left (fun skip (ps,_,_) ->
    List.rev (List.fold_left2 (fun skip p s ->
      (s && %p{ %pat{_} } p)::skip) [] ps skip)) skip cases in
  (remove_skip es skip,
   List.map (fun (ps,t,body) -> (remove_skip ps skip,t,body)) cases)

let match_expr _loc es cases nbody : exp =
  let (es, cases) = filter_matches es cases in
  match (es, cases) with
  | (es, [ (ps, t, body) ]) ->
    (match es with
    | [] -> if_expr _loc t body nbody
    | _ ->

	(match t with
	| None ->
	  if List.for_all is_always_patt ps
	  then %exp{let ${tuple_com_unit _loc ps} = ${tuple_com_unit _loc es} in $body}
	  else %exp{match ${tuple_com_unit _loc es} with | $pat{tuple_com_unit _loc ps} -> $body | _ -> $nbody}
	| Some t -> %exp{match ${tuple_com_unit _loc es} with | $pat{tuple_com_unit _loc ps} when $t -> $body | _ -> $nbody}))
  | _ ->
      let cases = List.map (fun (ps,t,body) -> match t with
	| None -> %case{$pat{tuple_com_unit _loc ps} -> $body}
        | Some t ->
            %case{$pat{tuple_com_unit _loc ps} when $t -> $body}) cases in
              let case = bar_of_list cases in
                %exp{match ${tuple_com_unit _loc es} with
                  | $case
                  | _ -> $nbody}

let single_match_expr _loc es ps t body nbody =
  match_expr _loc es [(ps,t,body)] nbody

let apply_test _loc ep t abort pbody nbody =
  if abort then nbody
  else
    let (es,ps) = List.split ep in
    single_match_expr _loc es ps t pbody nbody

let wrap _loc call ins outs es ps t body =
  let body = single_match_expr _loc es ps t body (unit _loc) in
  let body = fun_args _loc ins body in
  fun_apply _loc call (body::outs)

let excep_decl _loc n =
  %stru{exception $uid:n}

let safe_catch _loc e1 e2 exc =
  %exp{try begin $e1; fun () -> $e2 end with $uid:exc -> fun () -> () () }


(** {6 Auxiliary functions} *)

let rec fold_range f accu l u =
  if l < u then let u = u - 1 in fold_range f (f accu u) l u else accu

(** The following functions are Ast or Names specific. *)

let rec bound env = fun
 | Integer (_,_) -> true
 | Var (v,_loc) -> PlEnv.bound env v
 | Comp (_,ts,_) -> List.for_all (bound env) ts
 | Anon _loc -> false

let int_expr _loc i =
  %exp{$uid{PlNames.int_cons} $int':i}

let int_patt _loc i =
  %pat{$uid{PlNames.int_cons} $int':i}

(** {6 Matching versions against argument masks} *)

let arg_decl_allows a o =
  match a with
  | ArgOpen _ -> o
  | ArgClosed _ -> not o
  | ArgAny _ -> true

(** [version_matches_mask v m] is true if version [v] is allowed by mask [m]. *)
let version_matches_mask v ((m,_):  'a mask) =
  let (m,s) = PlVersion.fold
      (fun (m,s) o ->
        match m with
        | (a::m) -> (m, s && arg_decl_allows a o)
        | [] -> assert false) (m,true) v in
  (assert (m = []); s)

(** {6 Statics translation} *)

let comps_contains comps n i =
  try StringMap.find n comps = i
  with Not_found -> false

let or_of_list xs =
  let orf (a:'a) (b:'a) = let _loc =  a <+> b in `Bar(_loc,a,b)
  in
  	Ast_basic.of_listr orf xs

(** Generate "plval" type declaration with Prolog compounds (and integers). *)
let value_type _loc (comps : int StringMap.t) : FAst.stru =
  let ctyp_of_cons _loc n cs : or_ctyp =
    match cs with
    | [] -> uid _loc n
    | _  -> `Of(_loc,uid _loc n, sta_of_list cs )
  in
    let ts = StringMap.fold
      (fun comp n l ->
        let args = fold_range (fun acc _ -> (%ctyp{$lid{PlNames.val_type}}::acc)) [] 1 (n+1)
        in
	      ((ctyp_of_cons _loc (PlNames.comp comp) args)::l)
	  )
      comps
      [ %or_ctyp{$uid{PlNames.int_cons} of int} ]
    in
      let t_repr : type_repr = `Sum _loc (or_of_list ts) in
        let t_info : type_info = `TyRepr _loc (`Negative _loc) t_repr
        and l_id : alident = `Lid _loc PlNames.val_type in
          let t_decl : typedecl = `TyDcl _loc l_id (`None _loc) t_info (`None _loc) in
            `Type _loc t_decl

(** Generate "list_of_plval" function declaration. *)
let list_repr _loc comps =
  let cases = [ %case{ _ -> raise $uid{PlNames.notalist_exc}} ] in
    let cases = if not (comps_contains comps PlNames.nil 0) then
      cases
    else
      %case{$uid{PlNames.comp PlNames.nil} -> []} :: cases
    in
      let cases = if not (comps_contains comps PlNames.cons 2) then
        cases
      else
        %case{$uid{PlNames.comp PlNames.cons} (a, b) ->
          a :: $lid{PlNames.list_of} b } :: cases
      in
        let cases = bar_of_list cases in
          %stru{let rec $lid{PlNames.list_of} = fun | $cases}

(** Generate "int_of_plval" function declaration. *)
let int_repr _loc comps =
  let cases = [ %case{ _ -> raise $uid{PlNames.notanint_exc}} ] in
    let cases =
      if not (comps_contains comps PlNames.neg 1) then
        cases
      else
        %case{$uid{PlNames.comp PlNames.neg} x -> -($lid{PlNames.int_of} x)} :: cases
      in
        let cases = if not (comps_contains comps PlNames.sub 2) then
          cases
        else
          %case{$uid{PlNames.comp PlNames.sub} (x, y) ->
           ($lid{PlNames.int_of} x) - ($lid{PlNames.int_of} y)} :: cases
       in
         let cases = if not (comps_contains comps PlNames.add 2) then
           cases
         else
           %case{$uid{PlNames.comp PlNames.add} (x, y) ->
             ($lid{PlNames.int_of} x) + ($lid{PlNames.int_of} y)} :: cases
         in
           let cases = %case{$uid{PlNames.int_cons} i -> i} :: cases in
             let cases = bar_of_list cases in
               %stru{let rec $lid{PlNames.int_of} = fun | $cases }

(** Generate "string_of_plval" function declaration. *)
let value_repr _loc comps =
  let cases = StringMap.fold (fun comp n l ->
    let args = fold_range (fun acc i -> (PlNames.pred_var i)::acc) [] 1 (n+1) in
      let pat = ep_of_cons _loc (PlNames.comp comp) (List.map (lid _loc) args)
      and expr = match concat_expr ~sep:"," _loc
        (List.map (fun a -> %exp{$lid{PlNames.string_of} $lid:a}) args)
        with
        | None -> %exp{$str:comp}
        | Some e -> %exp{$str{comp ^ "("} ^ $e ^ ")"}
      in
        %case{$pat:pat -> $expr} ::l)
    comps
    [ %case{$uid{PlNames.int_cons} i -> string_of_int i} ]
  in
    %stru{let rec $lid{PlNames.string_of} v =
      try
        "[" ^ String.concat "," (List.map $lid{PlNames.string_of} ($lid{PlNames.list_of} v)) ^ "]"
      with | $uid{PlNames.notalist_exc} -> match v with | ${bar_of_list cases} }


(** Generate "Cut" exception. *)
let cut_excep_decl _loc =
	%stru{exception $uid{PlNames.cut_exc} of int}

(** Generate "_cutid" variable. *)
let cut_id_decl _loc =
	%stru{let $lid{PlNames.cut_id} = ref 0}

(** Wrap body with cut bookkeeping. *)
let cut_rule_body _loc body =
  %exp{
   let $lid{PlNames.my_cut_id} =
     begin incr $lid{PlNames.cut_id}; ! $lid{PlNames.cut_id} end in
   try begin $body; decr $lid{PlNames.cut_id} end
   with
   | $uid{PlNames.cut_exc} id when id = $lid{PlNames.my_cut_id} -> decr $lid{PlNames.cut_id}
   | e -> begin decr $lid{PlNames.cut_id}; raise e end
   }

(** {6 Rule grouping} **)

(** Check whether two Prolog terms are equivalent. *)
let rec equiv_t = fun
  | (Integer (i,_), Integer (i',_)) -> i = i'
  | (Comp (cn,ts,_), Comp (cn',ts',_)) ->
      cn = cn' && List.length ts = List.length ts' && equiv_ts ts ts'
  | (Var (v,_), Var (v',_)) -> v = v'
  | (Anon _, Anon _) -> true
  | (_, _) -> false
and equiv_ts ts ts' = List.for_all equiv_t (List.combine ts ts')

(** Check whether two Prolog terms are exclusive. *)
let rec exclu_t = fun
  | (Integer (i,_), Integer (i',_)) -> i <> i'
  | (Comp (cn,ts,_), Comp (cn',ts',_)) ->
      cn <> cn' || List.length ts <> List.length ts' || exclu_ts ts ts'
  | (Comp (_,_,_), Integer (_,_)) | (Integer (_,_), Comp (_,_,_)) -> true
  | (_, _) -> false
and exclu_ts ts ts' = List.exists exclu_t (List.combine ts ts')

let extend_exclus xs hd bodies =
  match xs with
  | None -> ([(hd,bodies)], [])
  | Some (xs,r) ->
      if List.for_all (fun (hd',_) -> exclu_ts hd hd') xs
      then ((hd,bodies)::xs, r)
      else ([(hd,bodies)], (List.rev xs)::r)

let extend_equivs qs hd body =
  match qs with
  | None -> (hd, [body], None)
  | Some (hd',bodies',xs) ->
      if equiv_ts hd hd' then
        (hd', body::bodies', xs)
      else
        (hd, [body], Some (extend_exclus xs hd' (List.rev bodies')))

let finish_equivs qs =
  match qs with
  | None -> []
  | Some (hd,bodies,xs) ->
      let (xs, r) = extend_exclus xs hd (List.rev bodies) in
      List.rev ((List.rev xs)::r)

let group_rs v (rs: ('a rule) list) =
  let qs = List.fold_left
      (fun qs (ts,goals,_loc) ->
        let (t1,t2) = PlVersion.partition v ts in
	Some (extend_equivs qs t1 (t2,goals,_loc))
      ) None rs in finish_equivs qs

let nogroup_rs v (rs: ('a rule) list) =
  List.map (fun (ts,goals,_loc) ->
    let (t1,t2) = PlVersion.partition v ts in
      [(t1, [(t2, goals, _loc)])]
    )
  rs

(** Generate [env] identifiers for the closed arguments in version [v]. *)
let arg_ids env v =
  let (env, ids) = PlVersion.fold (fun (env,ids) o ->
    if o then (env, ids)
    else
      let (env, id) =
        PlEnv.fresh_id env in
      (env, id::ids)) (env,[]) v in
  (env, List.rev ids)

exception UnboundVar of string * Locf.t

(** Prepare exprs from open terms **)
let rec goal_out_comp env c ts _loc =
  ep_of_cons _loc (PlNames.comp c) (goal_outs env ts)
and goal_out env = fun
  | Integer (i, _loc) -> int_expr _loc i
  | Comp (c, ts, _loc) -> goal_out_comp env c ts _loc
  | Var (v, _loc) ->
      (try lid _loc (PlEnv.lookup env v)
      with Not_found -> raise (UnboundVar (v,_loc)))
  | Anon _loc -> raise (UnboundVar ("_",_loc))
and goal_outs env t2 = List.map (goal_out env) t2

(** Create patterns from a list of terms, meanwhile update env,c,t **)
let rec patt_of_comp env t cn ts _loc =
  let (env,t,in_p) = patts_of_ts env t ts in
  (env, t, (ep_of_cons _loc (PlNames.comp cn) in_p))
and patt_of_t env t = fun
  | Integer (i,_loc) -> (env, t, int_patt _loc i)
  | Comp (cn,ts,_loc) -> patt_of_comp env t cn ts _loc
  | Var (v,_loc) ->
      let (env, t, id) = PlEnv.gen_bind_or_test env t v in
      (env, t, (lid _loc id))
  | Anon _loc -> (env, t, %pat{ _ })
and patts_of_ts env t ts =
  let (env,t,p) = List.fold_left (fun (env,t,p) term ->
    let (env,t,patt) = patt_of_t env t term in
    (env,t,patt::p)) (env,t,[]) ts in
  (env, t, List.rev p)

exception OpenUnify of Locf.t

let (* rec *) unify _loc (env,tep,tst,a) = fun
  | (Integer (i,_), Integer (i',_)) ->
    if i = i' then (env, tep, tst, a)
    else (env, tep, tst, true)
  | (Comp (cn,ts,_), Comp (cn',ts',_)) ->
      if cn = cn' && List.length ts = List.length ts' then
	(* List.fold_left (unify _loc) (env,tep,tst,a) (List.combine ts ts')
	   Unfortunately not valid, since unifications have to be parallel *)
	(if ts = [] then (env, tep, tst, a) else
	Locf.raise _loc (Failure "Deep compound-to-compound unification unsupported"))
      else (env, tep, tst, true)
  | (Integer (_,_), Comp (_,_,_)) | (Comp (_,_,_), Integer (_,_))
    -> (env, tep, tst, true)
  | (Var (v,_), Var (v',_)) ->
      (try let (env, tst) = PlEnv.unify env tst v v' in (env, tep, tst, a)
      with Not_found -> raise (OpenUnify _loc))
  | (Anon _, _) | (_, Anon _) -> (env, tep, tst, a)
  | (Var (v,_locv), Integer (i,_loci)) | (Integer (i,_loci), Var (v,_locv)) ->
      PlEnv.dispatch env v (fun id -> (* existing id *)
	(env, (lid _locv id, int_patt _loci i)::tep, tst, a))
        (fun env id -> (* fresh id *)
	  (env, (int_expr _loc i, lid _locv id)::tep, tst, a))
  | (Var (v,_locv), Comp (cn,ts,_locc)) | (Comp (cn,ts,_locc), Var (v,_locv)) ->
      PlEnv.dispatch env v (fun id -> (* existing id *)
	let (env,tst,tp) = patt_of_comp env tst cn ts _locc in
	(env, (lid _locv id,tp)::tep, tst, a))
        (fun env id -> (* fresh id *)
	  try (env, (goal_out_comp env cn ts _locc, lid _locv id)::tep, tst, a)
	  with UnboundVar _ -> raise (OpenUnify _loc))

(** {6 Main rules translation} *)

(**
	Rules translation:
	pred -> pred_version -> rule (-> terms) -> goal -> terms
**)

let terms _loc env t1 =
  let (env, ps, t) =
    List.fold_left (fun (env,ps,t) -> fun
  | (Integer (i, _loc), _ )->
      (env, (int_patt _loc i)::ps, t)
  | (Comp (cn, ts, _loc), _) ->
      let (env,t,p) = patt_of_comp env t cn ts _loc in
      (env, p::ps, t)
  | (Var (v, _loc), id) ->
      let (env,t) = PlEnv.bind_or_test env t v id in
      (env, %pat{ _ }::ps, t)
  | (Anon _loc, _) ->
      (env, %pat{ _ }::ps, t)) (env,[],[]) t1 in
  (env, List.rev ps, test_expr _loc t)

(** Visit goals of a rule **)
let pred_goal _loc (env,f) n ts =
  let v = PlVersion.reconstruct (List.rev_map (fun t -> not (bound env t)) ts) in
  let call = lid _loc (PlNames.pred n v) and nv = PlVersion.neg v in
  let (env,ids) = arg_ids env nv and (t1,t2) = PlVersion.partition nv ts in
  let ins = List.map (lid _loc) ids and outs =
    try goal_outs env t2
	(* we select a version with only bound vars *)
    with UnboundVar _ -> assert false  in
  let es = List.map (lid _loc) ids in
  let (env, ps, t) = terms _loc env (List.combine t1 ids) in
  begin
    if t <> None || List.exists (fun | %pat{ _ } -> false | _ -> true) ps then
      warning _loc "will unify after satisfying goal, might not match Prolog semantics" else ();
    (env, (fun body -> f (wrap _loc call ins outs es ps t body)))
  end

let same_goal _loc (env,f) pos t t' =
  let (env', tep, tst, a) = unify _loc (env,[],[],false) (t,t') in
  let tst = test_expr _loc tst and nbody = unit _loc in
  if pos then (env', (fun body -> f (apply_test _loc tep tst a body nbody)))
  else (env, (fun body -> f (apply_test _loc tep tst a nbody body)))

let rec int_expr_of_term env = fun
  | Integer (i,_loc) -> %exp{$int':i}
  | Comp (n,[e1;e2],_loc) when n = PlNames.add ->
      let e1 = int_expr_of_term env e1 and e2 = int_expr_of_term env e2 in
      %exp{$e1 + $e2}
  | Comp (n,[e1;e2],_loc) when n = PlNames.sub ->
      let e1 = int_expr_of_term env e1
      and e2 = int_expr_of_term env e2 in
      %exp{$e1 - $e2}
  | Comp (n,[e],_loc) when n = PlNames.neg ->
      let e = int_expr_of_term env e in %exp{ - $e}
  | Comp (n,_,_loc) -> Locf.raise _loc (Failure ("Function " ^ n ^ " not supported"))
  | Anon _loc -> raise (UnboundVar ("_",_loc))
  | Var (v,_loc) ->
      try
	let id = PlEnv.lookup env v in
	%exp{$lid{PlNames.int_of} ${lid _loc id}}
      with Not_found -> raise (UnboundVar (v,_loc))

let relation_goal _loc (env,f) r t1 t2 =
  let e1 = int_expr_of_term env t1 and e2 = int_expr_of_term env t2 in
  let tst = %exp{$lid{r} $e1 $e2} in
  (env, (fun body -> f %exp{if $tst then $body else ()}))

let is_goal _loc (env,f) t t' =
  let e = %exp{$uid{PlNames.int_cons} ${int_expr_of_term env t'}} in
  let (env, tst, p) = patt_of_t env [] t in
  let tst = test_expr _loc tst and nbody = unit _loc in
  (env, (fun body -> f (single_match_expr _loc [e] [p] tst body nbody)))

let cut_goal _loc (env,f) =
  (env, (fun body -> f %exp{
  begin
    $body;
    raise ($uid{PlNames.cut_exc} $lid{PlNames.my_cut_id})
  end
  }))

let true_fail_goal _loc (env,f) pos =
  (env, if pos then f else
  let body = f (unit _loc) in (fun _ -> body))

let repeat_goal _loc (env,f) =
  (env, (fun body -> f %exp{while true do   $body done}))

let write_goal _loc (env,f) t =
  (env, let e = goal_out env t in
  let e = %exp{print_string ($lid{PlNames.string_of} $e)} in
  (fun body -> f %exp{do $e; $body done}))

let nl_goal _loc (env,f) =
  (env, (fun body -> f %exp{do  print_newline (); $body done}))

let rec goal acc = fun
  | Integer (_,_loc) -> Locf.raise _loc (Failure ("Integer not callable"))
  | Var (_,_loc) | Anon _loc ->
      Locf.raise _loc (Failure ("Meta-call not supported"))
  | Comp ("=",[t;t'],_loc)  -> same_goal _loc acc true t t'
  | Comp (n,[t;t'],_loc) when n = PlNames.diff -> same_goal _loc acc false t t'
  | Comp (n,[t;t'],_loc) when n = PlNames.is -> is_goal _loc acc t t'
  | Comp (n,[t;t'],_loc) when n = PlNames.eq -> relation_goal _loc acc "=" t t'
  | Comp (n,[t;t'],_loc) when n = PlNames.ne -> relation_goal _loc acc "<>" t t'
  | Comp (n,[t;t'],_loc) when n = PlNames.lt -> relation_goal _loc acc "<" t t'
  | Comp (n,[t;t'],_loc) when n = PlNames.lte -> relation_goal _loc acc "<=" t t'
  | Comp (n,[t;t'],_loc) when n = PlNames.gt -> relation_goal _loc acc ">" t t'
  | Comp (n,[t;t'],_loc) when n = PlNames.gte -> relation_goal _loc acc ">=" t t'
  | Comp (n,[t],_loc) when n = PlNames.notp -> not_goal _loc acc t
  | Comp (n,[],_loc) when n = PlNames.cut -> cut_goal _loc acc
  | Comp (n,[],_loc) when n = PlNames.truep -> true_fail_goal _loc acc true
  | Comp (n,[],_loc) when n = PlNames.fail -> true_fail_goal _loc acc false
  | Comp (n,[],_loc) when n = PlNames.repeat -> repeat_goal _loc acc
  | Comp (n,[t],_loc) when n = PlNames.write -> write_goal _loc acc t
  | Comp (n,[],_loc) when n = PlNames.nl -> nl_goal _loc acc
  | Comp (n,ts,_loc) -> pred_goal _loc acc n ts
and not_goal _loc (env,f) g =
  let (_,notf) = goal (env,(fun body -> body)) g in
  let nbody = notf %exp{raise $uid{PlNames.found_exc}} in
  (env, (fun body -> f (safe_catch _loc nbody body PlNames.found_exc)))

let rule_has_cut (_,goals,_) =
  List.exists (fun
  | Comp (n,[],_) when n = PlNames.cut -> true
  | _ -> false) goals

let rule_body env (t2,goals,_loc) =
  let (env,f) = List.fold_left goal (env,(fun body -> body)) goals in
  let outs = goal_outs env t2 in
  let body = fun_apply _loc (lid _loc PlNames.f) outs in
  f body

let rule_equiv _loc env ids (t1,bodies) =
  let (env,ps,t) = terms _loc env (List.combine t1 ids) in
  (ps, t, sequence _loc (List.map (rule_body env) bodies))

let rule_excl _loc ids env exclus =
  let exprs = List.map (lid _loc) ids
  and cases = List.map (rule_equiv _loc env ids) exclus in
  match_expr _loc exprs cases (unit _loc)

(** Visit a predicate to produce a certain version **)
let pred_version _loc group_rs name rs v =
  let has_cut = List.exists rule_has_cut rs in
  let rs = group_rs v rs in
  let (env,ids) = arg_ids (PlEnv.empty PlNames.pred_var) v in
  let args_patt = List.map (lid _loc) ids in
  let body = sequence _loc (List.map (rule_excl _loc ids env) rs) in
  let body = if not has_cut then body else cut_rule_body _loc body in
  let f = fun_args _loc ((lid _loc PlNames.f)::args_patt) body in
  %bind{${lid _loc name} = $f}

let string_of_pred (name,n) = name ^ "/" ^ (string_of_int n)

(** Visit a predicate to produce all possible versions **)
let pred _loc group_rs ((name,n) as p) rs ms =
  if rs = [] then
    Locf.raise (let (_,_loc) = List.hd ms in _loc)
      (Failure
	 ("Mask without definition for predicate " ^ (string_of_pred p)))
  else (if List.mem p PlNames.builtin_preds then
    Locf.raise (let (_,_,_loc) = List.hd rs in _loc)
      (Failure
	 ("Can't redefine builtin predicate " ^ (string_of_pred p)))
  else PlVersion.make (fun a v ->
    if ms = [] || List.exists (version_matches_mask v) ms then
      let fname = PlNames.pred name v in
      try (pred_version _loc group_rs fname rs v)::a with
      | UnboundVar(var,_loc) -> begin
	  warning _loc (Printf.sprintf "skipping %s, unbound %s" fname var);
	  a end
      | OpenUnify(_loc) -> begin
	  warning _loc (Printf.sprintf "skipping %s, open unification" fname);
	  a end
    else a) [] n)

(** {6 Main entry points} **)

(** Generate the static declarations. *)
let prog_statics _loc (prog :  'a prog) =
	let statics : int StringMap.t = statics prog in
	[value_type _loc statics;
	excep_decl _loc PlNames.notalist_exc;
	list_repr _loc statics;
	value_repr _loc statics;
	excep_decl _loc PlNames.notanint_exc;
	int_repr _loc statics;
	excep_decl _loc PlNames.found_exc;
	cut_excep_decl _loc;
	cut_id_decl _loc]


(** Generate the functions that encode the Prolog rules. *)
let prog_rules _loc
	(group_rs :
		((int * int) -> 'a PlAst.rule list ->
		('a PlAst.term list *
			('a PlAst.term list * 'a PlAst.term list * 'a) list)
		list list
		)
	)
(prog :  'a prog) =
  let defs = PredMap.fold (fun p (rs,ms) acc ->
    List.append (pred _loc group_rs p (List.rev rs) (List.rev ms)) acc) prog [] in
    let defs = and_of_list defs in
      [ %stru{let rec $defs} ]


