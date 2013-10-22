open Gram_def
open FAst


val print_warning : Locf.t -> string -> unit

val prefix : string

val module_name : vid ref
val gm : unit -> vid

val mk_entry :
    local:bool->
      name:name ->
        pos:exp option -> levels:levels -> entry
            
val mk_level :
  label:string option ->
  assoc:exp option -> rules:rule list -> level
      
val mk_rule :
  prod:symbol list -> action:exp option -> rule
      
val mk_symbol :
  ?pattern:pat option ->
  text:text -> styp:styp -> symbol

val mk_slist :
    loc -> bool -> symbol option -> symbol -> text
val gensym : unit -> int ref
val gen_lid : unit -> string
    

(** translate [styp] into [ctyp], given the assumption that the entry output [tvar] type *)
val make_ctyp : styp -> string -> ctyp



(** generate action, collecting patterns into action
   [rtvar] stands for the type of the return value
   [tvar] refers to the current entry's type
   It is in charge of generating code like this 
   {[
   (Fgram.mk_action
               (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t) 
                  -> (`Try (_loc, e, a) : 'exp )))
   ]} *)

(* val text_of_action : *)
(*   loc -> rule -> string -> exp *)

(** transform [text] to [exp] which represents [symbol]
   compute the [lhs]
   it generates code which has type [Fgram.symbol]
   tvar provides type informatoin
   {[
   `Skeyword "let"

   `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ))

   `Smeta
      (["FOLD1"; "SEP"],
          [Fgram.srules declare_regexp
                [([`Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                          (`Normal, "`Lid _"));
                     `Skeyword ":";
                     `Snterm (Fgram.obj (regexp : 'regexp Fgram.t ))],
                      (Fgram.mk_action
                         (fun (r : 'regexp)  _  (__fan_0 : [> Ftoken.t]) 
                            (_loc : Locf.t)  ->
                            match __fan_0 with
                            | `Lid x -> ((x, r) : 'e__2 )
                            | _ -> assert false)))];
                `Skeyword ";"],
               (Fgram.Action.mk
                   (Fgram.sfold1sep
                      (fun (x,r)  ()  ->
                         if Hashtbl.mem FanLexTools.named_regexps x
                         then
                           Printf.eprintf
                             "pa_ulex (warning): multiple definition of named regexp '%s'\n"
                             x
                         else ();
                         Hashtbl.add FanLexTools.named_regexps x r) () : 
                   (_,'e__2,'e__3) Fgram.foldsep )))
   `Slist0
     (Fgram.srules sigis
                 [([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t ));
                   `Snterm (Fgram.obj (semi : 'semi Fgram.t ))],
                    (Fgram.mk_action
                       (fun _  (sg : 'sigi)  (_loc : Locf.t)  ->
                          (sg : 'e__1 ))))])

   `Slist0sep
       ((`Snterm (Fgram.obj (case0 : 'case0 Fgram.t ))),
        (`Skeyword "|"))


   `Slist1sep
        ((Fgram.srules pos_exps
          [([`Stoken
                        (((function | `Lid _ -> true | _ -> false)),
                          (`Normal, "`Lid _"));
                     `Skeyword ":";
                     `Snterm
                       (Fgram.obj (dot_lstrings : 'dot_lstrings Fgram.t ))],
                      (Fgram.mk_action
                         (fun (y : 'dot_lstrings)  _ 
                            (__fan_0 : [> Ftoken.t])  (_loc : Locf.t)  ->
                            match __fan_0 with
                            | `Lid x ->
                                (((x : string ), (Ftoken.resolve_name y)) : 
                                'e__2 )
                            | _ -> assert false)));
                   ([`Stoken
                       (((function | `Lid _ -> true | _ -> false)),
                         (`Normal, "`Lid _"))],
                     (Fgram.mk_action
                        (fun (__fan_0 : [> Ftoken.t])  (_loc : Locf.t) 
                           ->
                           match __fan_0 with
                           | `Lid x ->
                               (((x : string ),
                                  (Ftoken.resolve_name ((`Sub []), x))) : 
                               'e__2 )
                           | _ -> assert false)))]), (`Skeyword ";"))
   `Snext
   `Sself
   `Snterml ((Fgram.obj (exp : 'exp Fgram.t )), "top")
   `Stoken
     (((function | `Ant ((""|"mexp"|"anti"|"list"),_) -> true
        | _ -> false)),
       (`Normal, "`Ant ((\"\"|\"mexp\"|\"anti\"|\"list\"),_)"))
   ]} *)          


(** the [rhs] was computed, compute the [lhs]
   the generated expession has type [production] *)        
val make_exp_rules :
    loc -> (text list * exp * exp option) list -> string -> exp










(** the [locals] is local entry name list,
   [el] is entry list
   [gram] is the grammar
   [gmod] is the [Fgram] module true
   generate the extend, the main entrance
   the [entrance] point for generating code

   It call [text_of_entry]
 *)
val make : loc -> entries -> exp 


val filter_pat_with_captured_variables : pat -> pat * (exp * exp) list
