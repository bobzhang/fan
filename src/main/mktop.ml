(** Until this module, Dynamic linking is not required,

    mktop + fanTop.ml -> fanTop.cma
    
    mktop + mkFan + fan.ml -> fan
    mktop + dynloader + mkFan  + fanX.ml -> fane 
 *)

%import{
Ast_quotation:
  of_exp
  of_stru_with_filter
  of_stru
  of_exp_with_filter
  of_clfield_with_filter
  add
  ;
Ast_gen:
   loc_of
   ;

};;

open! Syntaxf
include Prelude




let domain = Ns.lang

let _ = begin (* FIXME make the printer more restict later *)
  of_stru_with_filter ~name:{domain; name = "ocaml"} ~entry:strus
    ~filter:(fun s  ->
    let _loc = loc_of s in
    let v = %mexp{ struct $s end } in
    let mexp = (Typehook.traversal ())#mexp v in
    let code =
      match mexp with
      | %mexp{ struct $s end } -> s
      | _ -> failwith "can not find items back " in
    (if !Typehook.show_code then
      (try Ast2pt.print_stru Format.std_formatter code
      with
      | _ ->
          Util.prerr_endlinef 
          "There is a printer bug\
           Our code generator may still work when Printer is broken\
           Plz send bug report to %s" Configf.bug_main_address)
  ; code))  () ;

  
end;;
    



(** Small languages for convenience *)

%create{ p};;

%extend{
  p:
  [pat as p;"when"; exp as e %{ %exp{ function | $pat:p when $e -> true |_ -> false } }
  |pat as p %{ %exp'{ function | $pat:p -> true | _ -> false } } ] };;

let ()  =
  of_exp ~name:{domain; name = "p"} ~entry:p () ;;



%create{import} ;;

%extend{
  a@Local:
  [Uid m ; ":"; L1 n as ns ; ";" %{
    Ast_gen.sem_of_list (* add antiquotation automatically ?? *)
      (List.map
         (fun ((l:Tokenf.txt),r)  ->
           let xloc = l.loc in
           let pr = %pat'@xloc{$lid{l.txt}} in
           let pl =
             match r with
             | None -> pr
             | Some (y:Tokenf.txt) ->
                 let yloc = y.loc in
                 %exp'@yloc{$lid{y.txt}} in 
           %stru{ let $pl = $uid:m.$pr } ) ns) }]
  n@Local:
  [ Lid as x  %{ (x,None) }
  | Lid as x ; "as"; Lid as y %{(x, Some y)} ]      
  import:
  [ L1 a  as xs  %{ Ast_gen.sem_of_list xs} ]  
  };;
let domain = Ns.lang
let () = begin 
    of_exp ~name:{domain; name =  "with_exp"} ~entry:with_exp_lang ();
  of_stru ~name:{domain; name =  "with_stru"} ~entry:with_stru_lang ();
  add {domain; name =  "str"} Dyn_tag.exp
    (fun _loc  _loc_option  s  -> `Str (_loc, s));
  add {domain; name =  "str"} Dyn_tag.stru
    (fun _loc  _loc_option  s  -> `StExp (_loc, (`Str (_loc, s))))
end

(**
   improved
   --- alias
   --- nested modules
   --- operators 
*)

(* such simple macro would be replaced by cmacros later *)

let () =
  of_stru ~name:{domain; name = "import"} ~entry:import ();;

(***********************************)
(*   simple error qq               *)
(***********************************)





(*** poor man's here expansion available for expr and stru*)
let () =
  let f  = fun (loc:Locf.t) _meta _content ->
    let s = Locf.to_string loc in
    %exp@loc{$str:s} in
  let f2 = fun (loc:Locf.t) _meta _content ->
    let s = Locf.to_string loc in
    %stru@loc{$str:s} in
  begin 
    Ast_quotation.add {domain; name = "here"} Dyn_tag.exp f;
    Ast_quotation.add {domain; name = "here"} Dyn_tag.stru f2
  end
    
let () =
  Printexc.register_printer @@ function
  | Out_of_memory ->  Some "Out of memory"
  | Assert_failure ((file, line, char)) ->
      Some (Format.sprintf "Assertion failed, file %S, line %d, char %d" file line
              char)
  | Match_failure ((file, line, char)) ->
      Some (Format.sprintf "Pattern matching failed, file %S, line %d, char %d" file
              line char)
  | Failure str -> Some (Format.sprintf "Failure: %S" str)
  | Invalid_argument str -> Some (Format.sprintf "Invalid argument: %S" str)
  | Sys_error str -> Some (Format.sprintf "I/O error: %S" str)
  | Streamf.NotConsumed -> Some (Format.sprintf "Parse failure(NotConsumed)")
  | Streamf.Error str -> Some (Format.sprintf  "Streamf.Error %s" str)
  | _ -> None;;


    

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/mktop.cmo" *)
(* end: *)
