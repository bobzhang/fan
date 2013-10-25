%import{
StdFan:
  pp_print_string;
Objs:
  pp_print_vid'
  pp_print_vid
  pp_print_alident
  pp_print_ant;
Format:
  fprintf
  ;
}
open FAst
let pp_print_loc _f _loc  = ()
class mapbase = object
  method loc (x:loc) =  x
  method string (x:string) = x
  method ant (x:ant) = x
end


type lident =
  [ `Lid of (loc * string) ]  
and t =
  [
   `Vrn of (loc * string)
  |`App of (loc * t * t )
  |`Lid of (loc * string)
  | ant 
  |`Com of (loc * t * t)
  |`Alias of (loc * t * lident)
  |`Str of (loc * string)
  |`Any of loc] with ("Print" "Map")

let wildcarder = object (self)
  inherit map as super
  method! t = function
    | `Lid (_loc,_) -> `Any _loc
    | %pat'{ ($p as $_) } -> self#t p
    | p  -> super#t p 
end;;



let p = fprintf

let rec unparse  f (x:t)=
  match x with
  | `Vrn (_,s) -> p f "`%s" s
  | `App _ ->
      let l = Ast_basic.list_of_app x [] in
      begin match l with
      | [ (`Vrn _ as x) ]  -> unparse  f x
      | [ (`Vrn _ as  x) ; v ] ->
          p f "%a %a" unparse x unparse v
      | (`Vrn _ as x) :: rest ->
          p f "%a (%a)"
            unparse x (Formatf.pp_list unparse ~sep:",") rest 
      | _ ->
          (p Format.err_formatter  "impossible pattern %a@." pp_print_t x ;
          invalid_arg "unparse")
      end
  | `Com(_,a,b) -> p f "%a, %a" unparse a unparse b
  | `Alias (_,p,_) -> unparse f  p
  | `Str(_,s) -> p f "%S" s
  | `Any _ -> p f "_"
  | `Lid (_,s) -> p f "%s" s 
  | `Ant (_, x) -> p f "$%s" x.txt 

let to_string = Formatf.to_string unparse

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gram_pat.cmo" *)
(* end: *)
