open FGramDef
open FAst
val print_warning : FLoc.t -> string -> unit

val prefix : string

val grammar_module_name : vid ref
val gm : unit -> vid
val mk_entry :
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
val string_of_pat : pat -> string

(** FIXME why deprecate such syntax
    It makes [OPT STRING] invalid
    You shoud write [OPT [x=STRING -> x] ] *)      
val check_not_tok : symbol -> unit
    
val new_type_var: unit -> string

val gensym: unit -> int ref
val gen_lid: unit -> string
    
val retype_rule_list_without_patterns:  loc -> rule list -> rule list

(** translate [styp] into [ctyp], given the assumption that the entry output [tvar] type *)
val make_ctyp : styp -> string -> ctyp



(** generate action, collecting patterns into action
   [rtvar] stands for the type of the return value
   [tvar] refers to the current entry's type
   It is in charge of generating code like this 
   {[
   (Fgram.mk_action
               (fun (a : 'case)  _  (e : 'exp)  _  (_loc : FLoc.t) 
                  -> (`Try (_loc, e, a) : 'exp )))
   ]} *)

val text_of_action :
  loc ->
  symbol list -> ?action:exp -> string   -> string -> exp
val mk_srules :
  loc ->
  string ->
  rule list -> string -> (text list * exp) list

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
                         (fun (r : 'regexp)  _  (__fan_0 : [> FToken.t]) 
                            (_loc : FLoc.t)  ->
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
                       (fun _  (sg : 'sigi)  (_loc : FLoc.t)  ->
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
                            (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Lid x ->
                                (((x : string ), (FToken.resolve_name y)) : 
                                'e__2 )
                            | _ -> assert false)));
                   ([`Stoken
                       (((function | `Lid _ -> true | _ -> false)),
                         (`Normal, "`Lid _"))],
                     (Fgram.mk_action
                        (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t) 
                           ->
                           match __fan_0 with
                           | `Lid x ->
                               (((x : string ),
                                  (FToken.resolve_name ((`Sub []), x))) : 
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
val make_exp : string -> text -> exp

(** the [rhs] was computed, compute the [lhs]
   the generated expession has type [production] *)        
val make_exp_rules :
    loc -> (text list * exp) list -> string -> exp

(* val exp_of_delete_rule : *)
val exp_delete_rule:
  loc -> name -> symbol list list  -> exp
      
val mk_name : loc -> (* ident *)vid -> name
val mk_slist :
  loc ->
  bool -> symbol option -> symbol -> text

(**
  return [(ent,pos,txt)] the [txt] has type [olevel],
  [ent] is something like
  {[
  (module_exp : 'mexp Fgram.t )
  ]}
  [pos] is something like
  {[(Some `LA)]} it has type [position option] *)        
val text_of_entry :   entry -> exp

(** [gl] is the name  list option

   {[
   loc -> ident option ->exp name list option ->
   (exp, 'a) entry list -> exp -> exp
   ]}

   This function generate some local entries
 *)   
val let_in_of_extend :  loc ->vid option -> name list option -> exp -> exp

(** the [locals] is local entry name list,
   [el] is entry list
   [gram] is the grammar
   [gmod] is the [Fgram] module true
   generate the extend, the main entrance
   the [entrance] point for generating code

   It call [text_of_entry]
 *)
val text_of_functorial_extend :  loc ->vid option ->  name list option -> entry list -> exp

(** generate Stok *)  
val mk_tok :  loc ->  ?restrict:exp ->  pattern:pat -> styp -> symbol

