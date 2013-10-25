

(** About Fan's antiquot context management *)  

type anti_cxt = {
    cxt:string;

    mutable kind :   string; (* keep it simple first*)
    txt : string;
  }
(* TODO with ("Print") get rid of dependency on a module as much as possible  *)
  
let pp_print_anti_cxt fmt  {cxt;txt ;_} =
  let open Format in
  fprintf fmt "cxt:%S;content:%S"
    cxt
    txt 

let dummy = {cxt="";kind = "";txt =""}

let mk_anti ?(c="") (x:Tokenf.ant) (* loc n s *) =
  let c =
    {
     cxt = c;
     kind =  x.kind;
     txt  = x.txt } in `Ant(x.loc,c)
    
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
