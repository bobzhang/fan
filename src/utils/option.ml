

let may f = function
  | None -> ()
  | Some v -> f v
        (*$T may
          let x = ref 3 in may incr (Some x); !x = 4 *)


let map f = function
  | None -> None
  | Some v -> Some (f v)
        (*$T map
          map succ None = None
          map succ (Some 3) = (Some 4)
         *)


let bind  x f =
  match x with 
  | None -> None
  | Some v -> f v
        (*$T bind
          bind (fun s -> Some s) None = None
          bind (fun s -> Some s) (Some ()) = Some ()
         *)


let apply = function
  | None -> (fun x -> x)
  | Some f -> f
        (*$T apply
          apply None 3 = 3
          apply (Some succ) 3 = 4
         *)


let filter f = function
  | Some x when f x -> Some x
  | _ -> None
        (*$T filter
          filter (fun _ -> true) None = None
          filter (fun _ -> true) (Some 3) = Some 3
          filter (fun _ -> false) (Some 3) = None
         *)


let default v = function
  |None -> v
  | Some v -> v
        (*$T default
          default 3 None = 3
          default 3 (Some 4) = 4
         *)

let is_some = function
  |None -> false
  | _ -> true
        (*$T is_some
          not (is_some None)
          is_some (Some ())
         *)

let is_none = function
  | None -> true
  | _ -> false
        (*$T is_none
          is_none None
          not (is_none (Some ()))
         *)

let get_exn s e =
  match s with
  | None   -> raise e
  | Some v -> v
        (*$T get_exn
          try get_exn None Exit with Exit -> true
          try get_exn (Some true) Exit with Exit -> false
         *)

let get s = get_exn s (Invalid_argument "Option.get")
    (*$T get
      try get None with Invalid_argument _ -> true
      try get (Some true) with Invalid_argument _ -> false
     *)

let map_default f v = function
  | None -> v
  | Some v2 -> f v2
        (*$T map_default
          map_default succ 2 None = 2
          map_default succ 2 (Some 3) = 4
         *)

let compare ?(cmp=Pervasives.compare) a b =
  match a with
  |None ->
      (match b with
      |None -> 0
      | Some _ -> -1)
  | Some x ->
      (match b with
      |None -> 1
      | Some y -> cmp x y)
        (*$T compare
          compare (Some 0) (Some 1) < 0
          compare (Some 0) (Some 0) = 0
          compare (Some 0) (Some (-1)) > 0
          compare None (Some ()) < 0
          compare None None = 0
          compare (Some ()) None > 0
          compare ~cmp:(fun _ _ -> 0) (Some (fun x -> x)) (Some (fun y -> y)) = 0
         *)


let eq ?(eq=(=)) x y =
  match (x,y) with
  | (None, None) -> true
  | (Some a, Some b) -> eq a b
  | _ -> false



(* local variables: *)
(* compile-command: "pmake option.cmo" *)
(* end: *)
