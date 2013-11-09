open Gram_def
open FAst


val print_warning : Locf.t -> string -> unit

val prefix : string

val module_name : vid ref

val gm : unit -> vid

      
val mk_prule :
    prod:osymbol list decorate list -> action:exp option -> rule    

val gen_lid : unit -> string
    

(** translate [styp] into [ctyp], given the assumption that the entry output [tvar] type *)
(* val make_ctyp : styp -> string -> ctyp *)



(** generate action, collecting patterns into action
   [rtvar] stands for the type of the return value
   [tvar] refers to the current entry's type
   It is in charge of generating code like this 
   {[
   (Gramf.mk_action
               (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t) 
                  -> (Try (_loc, e, a) : 'exp )))
   ]} *)

(* val text_of_action : *)
(*   loc -> rule -> string -> exp *)

(** transform [text] to [exp] which represents [symbol]
   compute the [lhs]
   it generates code which has type [Gramf.symbol]
   tvar provides type informatoin
   {[
   Keyword "let"

   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))

   `Smeta
      (["FOLD1"; "SEP"],
          [Gramf.srules declare_regexp
                [([Token
                    (((function | `Lid _ -> true | _ -> false)),
                          (`Normal, "`Lid _"));
                     Keyword ":";
                     Nterm (Gramf.obj (regexp : 'regexp Gramf.t ))],
                      (Gramf.mk_action
                         (fun (r : 'regexp)  _  (__fan_0 : [> Tokenf.t]) 
                            (_loc : Locf.t)  ->
                            match __fan_0 with
                            | `Lid x -> ((x, r) : 'e__2 )
                            | _ -> assert false)))];
                Keyword ";"],
               (Gramf.Action.mk
                   (Gramf.sfold1sep
                      (fun (x,r)  ()  ->
                         if Hashtbl.mem FanLexTools.named_regexps x
                         then
                           Printf.eprintf
                             "pa_ulex (warning): multiple definition of named regexp '%s'\n"
                             x
                         else ();
                         Hashtbl.add FanLexTools.named_regexps x r) () : 
                   (_,'e__2,'e__3) Gramf.foldsep )))
   List0
     (Gramf.srules sigis
                 [([Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
                   Nterm (Gramf.obj (semi : 'semi Gramf.t ))],
                    (Gramf.mk_action
                       (fun _  (sg : 'sigi)  (_loc : Locf.t)  ->
                          (sg : 'e__1 ))))])

   List0sep
       ((Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
        (Keyword "|"))


   List1sep
        ((Gramf.srules pos_exps
          [([Token
                        (((function | `Lid _ -> true | _ -> false)),
                          (`Normal, "`Lid _"));
                     Keyword ":";
                     Nterm
                       (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))],
                      (Gramf.mk_action
                         (fun (y : 'dot_lstrings)  _ 
                            (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                            match __fan_0 with
                            | `Lid x ->
                                (((x : string ), (Tokenf.resolve_name y)) : 
                                'e__2 )
                            | _ -> assert false)));
                   ([Token
                       (((function | `Lid _ -> true | _ -> false)),
                         (`Normal, "`Lid _"))],
                     (Gramf.mk_action
                        (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t) 
                           ->
                           match __fan_0 with
                           | `Lid x ->
                               (((x : string ),
                                  (Tokenf.resolve_name ((`Sub []), x))) : 
                               'e__2 )
                           | _ -> assert false)))]), (Keyword ";"))
   `Snext
   Self
   Snterml ((Gramf.obj (exp : 'exp Gramf.t )), "top")
   Token
     (((function | `Ant ((""|"mexp"|"anti"|"list"),_) -> true
        | _ -> false)),
       (`Normal, "`Ant ((\"\"|\"mexp\"|\"anti\"|\"list\"),_)"))
   ]} *)          


(** the [rhs] was computed, compute the [lhs]
   the generated expession has type [production] *)        
(* val make_exp_rules : *)
(*     loc -> (text list * exp * exp option) list -> string -> exp *)










(** the [locals] is local entry name list,
   [el] is entry list
   [gram] is the grammar
   [gmod] is the [Gramf] module true
   generate the extend, the main entrance
   the [entrance] point for generating code

   It call [text_of_entry]
 *)
val make : loc -> entries -> exp 


val filter_pat_with_captured_variables : pat -> pat * (exp * exp) list
