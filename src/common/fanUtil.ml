

(** About Fan's antiquot context management *)  

type anti_cxt = Tokenf.ant =  {
    loc          : Locf.t;
    cxt          : string option;
    kind : string;
    txt          : string;
    shift        : int;
    retract      : int;
  }
  
let pp_print_anti_cxt fmt  (x:anti_cxt) =
  Format.fprintf fmt "cxt:%S;content:%S"
  (match x.cxt with None ->""|Some s -> s)  x.txt 



let mk_anti ?c  (x:Tokenf.ant)  =
  match c with
  | None -> `Ant(x.loc,x)
  | Some _  -> `Ant(x.loc, {x with cxt = c })


let expand p (x:anti_cxt) =
  let content =
    String.sub x.txt x.shift (String.length x.txt - x.retract - x.shift ) in
  let loc =
    Location_util.join
      { x.loc with
        loc_start =
        { x.loc.loc_start with
          pos_cnum = x.loc.loc_start.pos_cnum + x.shift}} in
  p loc content

let add_context s c =
  {s with kind  = s.kind ^ c}

(**
   {[
   destruct_poly "`a";
   Some "a"
   ]}
 *)
let destruct_poly s =
  let n = String.length s in
  if n = 0 then
    invalid_arg "destruct_poly length=0"
  else
    if s.[0] = '`' then
      Some (String.sub s 1 (n-1))
    else None

(* local variables: *)
(* compile-command: "cd .. && pmake common/fanUtil.cmo" *)
(* end: *)
