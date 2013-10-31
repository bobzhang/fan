module StringMap = Map.Make(String)

type t = ((int -> string) *  string StringMap.t * int )

let empty f = (f, StringMap.empty, 0)

let lookup (_,map,_) v =StringMap.find v map

let bind (f,map,c) v id =
  (f, StringMap.add v id map, c)

let bound (_,map,_) v =
  StringMap.mem v map

let fresh_id (f,map,c) =
  let id = f c and c = c + 1 in
  ((f, map, c), id)

let dispatch env v f1 f2 =
  (try
    let id = lookup env v in
    fun () -> f1 id
  with Not_found ->
    let (env, id) = fresh_id env in
    let env = bind env v id in
    fun () -> f2 env id) ()

let bind_or_test env tsts v id =
  try
    let id' = lookup env v in
    (env, (id,id')::tsts)
  with Not_found -> (bind env v id, tsts)

let gen_bind_or_test env tsts v =
  let (env, id) = fresh_id env in
  let (env, tsts) =	bind_or_test env tsts v id in
  (env, tsts, id)

let unify env tsts v1 v2 =
  try
    let id1 = lookup env v1 in
    bind_or_test env tsts v2 id1
  with Not_found ->
    let id2 = lookup env v2 in
    bind_or_test env tsts v1 id2
