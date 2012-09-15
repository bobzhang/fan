module type Type = sig type t
 end

module type Id =
                                     sig
                                      val name : string

                                      val version : string

                                     end

module Warning =
                                           functor (Loc : Type) ->
                                            struct
                                             module type S =
                                              sig
                                               type warning =
                                                (Loc.t -> (string -> unit))

                                               val default_warning : warning

                                               val current_warning :
                                                warning ref

                                               val print_warning : warning

                                              end

                                            end

module type Ast =
                                                  sig
                                                   type loc

                                                   type meta_bool

                                                   type 'a meta_option

                                                   type 'a meta_list

                                                   type ctyp

                                                   type patt

                                                   type expr

                                                   type module_type

                                                   type sig_item

                                                   type with_constr

                                                   type module_expr

                                                   type str_item

                                                   type class_type

                                                   type class_sig_item

                                                   type class_expr

                                                   type class_str_item

                                                   type match_case

                                                   type ident

                                                   type binding

                                                   type rec_binding

                                                   type module_binding

                                                   type rec_flag

                                                   type direction_flag

                                                   type mutable_flag

                                                   type private_flag

                                                   type virtual_flag

                                                   type row_var_flag

                                                   type override_flag

                                                   val loc_of_ctyp :
                                                    (ctyp -> loc)

                                                   val loc_of_patt :
                                                    (patt -> loc)

                                                   val loc_of_expr :
                                                    (expr -> loc)

                                                   val loc_of_module_type :
                                                    (module_type -> loc)

                                                   val loc_of_module_expr :
                                                    (module_expr -> loc)

                                                   val loc_of_sig_item :
                                                    (sig_item -> loc)

                                                   val loc_of_str_item :
                                                    (str_item -> loc)

                                                   val loc_of_class_type :
                                                    (class_type -> loc)

                                                   val loc_of_class_sig_item :
                                                    (class_sig_item -> loc)

                                                   val loc_of_class_expr :
                                                    (class_expr -> loc)

                                                   val loc_of_class_str_item :
                                                    (class_str_item -> loc)

                                                   val loc_of_with_constr :
                                                    (with_constr -> loc)

                                                   val loc_of_binding :
                                                    (binding -> loc)

                                                   val loc_of_rec_binding :
                                                    (rec_binding -> loc)

                                                   val loc_of_module_binding :
                                                    (module_binding -> loc)

                                                   val loc_of_match_case :
                                                    (match_case -> loc)

                                                   val loc_of_ident :
                                                    (ident -> loc)

                                                   class map :
                                                    object ('self_type)
                                                     method string :
                                                      (string -> string)
                                                     method list :
                                                      'a 'b .
                                                       (('self_type ->
                                                         ('a -> 'b)) ->
                                                        ('a list -> 'b list))
                                                     method meta_bool :
                                                      (meta_bool ->
                                                       meta_bool)
                                                     method meta_option :
                                                      'a 'b .
                                                       (('self_type ->
                                                         ('a -> 'b)) ->
                                                        ('a meta_option ->
                                                         'b meta_option))
                                                     method meta_list :
                                                      'a 'b .
                                                       (('self_type ->
                                                         ('a -> 'b)) ->
                                                        ('a meta_list ->
                                                         'b meta_list))
                                                     method loc :
                                                      (loc -> loc)
                                                     method expr :
                                                      (expr -> expr)
                                                     method patt :
                                                      (patt -> patt)
                                                     method ctyp :
                                                      (ctyp -> ctyp)
                                                     method str_item :
                                                      (str_item -> str_item)
                                                     method sig_item :
                                                      (sig_item -> sig_item)
                                                     method module_expr :
                                                      (module_expr ->
                                                       module_expr)
                                                     method module_type :
                                                      (module_type ->
                                                       module_type)
                                                     method class_expr :
                                                      (class_expr ->
                                                       class_expr)
                                                     method class_type :
                                                      (class_type ->
                                                       class_type)
                                                     method class_sig_item :
                                                      (class_sig_item ->
                                                       class_sig_item)
                                                     method class_str_item :
                                                      (class_str_item ->
                                                       class_str_item)
                                                     method with_constr :
                                                      (with_constr ->
                                                       with_constr)
                                                     method binding :
                                                      (binding -> binding)
                                                     method rec_binding :
                                                      (rec_binding ->
                                                       rec_binding)
                                                     method module_binding :
                                                      (module_binding ->
                                                       module_binding)
                                                     method match_case :
                                                      (match_case ->
                                                       match_case)
                                                     method ident :
                                                      (ident -> ident)
                                                     method override_flag :
                                                      (override_flag ->
                                                       override_flag)
                                                     method mutable_flag :
                                                      (mutable_flag ->
                                                       mutable_flag)
                                                     method private_flag :
                                                      (private_flag ->
                                                       private_flag)
                                                     method virtual_flag :
                                                      (virtual_flag ->
                                                       virtual_flag)
                                                     method direction_flag :
                                                      (direction_flag ->
                                                       direction_flag)
                                                     method rec_flag :
                                                      (rec_flag -> rec_flag)
                                                     method row_var_flag :
                                                      (row_var_flag ->
                                                       row_var_flag)
                                                     method unknown :
                                                      'a . ('a -> 'a)
                                                    end

                                                    class fold :
                                                     object ('self_type)
                                                      method string :
                                                       (string -> 'self_type)
                                                      method list :
                                                       'a .
                                                        (('self_type ->
                                                          ('a -> 'self_type))
                                                         ->
                                                         ('a list ->
                                                          'self_type))
                                                      method meta_bool :
                                                       (meta_bool ->
                                                        'self_type)
                                                      method meta_option :
                                                       'a .
                                                        (('self_type ->
                                                          ('a -> 'self_type))
                                                         ->
                                                         ('a meta_option ->
                                                          'self_type))
                                                      method meta_list :
                                                       'a .
                                                        (('self_type ->
                                                          ('a -> 'self_type))
                                                         ->
                                                         ('a meta_list ->
                                                          'self_type))
                                                      method loc :
                                                       (loc -> 'self_type)
                                                      method expr :
                                                       (expr -> 'self_type)
                                                      method patt :
                                                       (patt -> 'self_type)
                                                      method ctyp :
                                                       (ctyp -> 'self_type)
                                                      method str_item :
                                                       (str_item ->
                                                        'self_type)
                                                      method sig_item :
                                                       (sig_item ->
                                                        'self_type)
                                                      method module_expr :
                                                       (module_expr ->
                                                        'self_type)
                                                      method module_type :
                                                       (module_type ->
                                                        'self_type)
                                                      method class_expr :
                                                       (class_expr ->
                                                        'self_type)
                                                      method class_type :
                                                       (class_type ->
                                                        'self_type)
                                                      method class_sig_item :
                                                       (class_sig_item ->
                                                        'self_type)
                                                      method class_str_item :
                                                       (class_str_item ->
                                                        'self_type)
                                                      method with_constr :
                                                       (with_constr ->
                                                        'self_type)
                                                      method binding :
                                                       (binding ->
                                                        'self_type)
                                                      method rec_binding :
                                                       (rec_binding ->
                                                        'self_type)
                                                      method module_binding :
                                                       (module_binding ->
                                                        'self_type)
                                                      method match_case :
                                                       (match_case ->
                                                        'self_type)
                                                      method ident :
                                                       (ident -> 'self_type)
                                                      method rec_flag :
                                                       (rec_flag ->
                                                        'self_type)
                                                      method direction_flag :
                                                       (direction_flag ->
                                                        'self_type)
                                                      method mutable_flag :
                                                       (mutable_flag ->
                                                        'self_type)
                                                      method private_flag :
                                                       (private_flag ->
                                                        'self_type)
                                                      method virtual_flag :
                                                       (virtual_flag ->
                                                        'self_type)
                                                      method row_var_flag :
                                                       (row_var_flag ->
                                                        'self_type)
                                                      method override_flag :
                                                       (override_flag ->
                                                        'self_type)
                                                      method unknown :
                                                       'a .
                                                        ('a -> 'self_type)
                                                     end

                                                    end

module type Camlp4Ast =
                                                          sig
                                                           module Loc :
                                                            FanSig.Loc

                                                           type loc = Loc.t
                                                           and meta_bool =
                                                               BTrue
                                                             | BFalse
                                                             | BAnt of string
                                                           and rec_flag =
                                                               ReRecursive
                                                             | ReNil
                                                             | ReAnt of
                                                                string
                                                           and direction_flag =
                                                               DiTo
                                                             | DiDownto
                                                             | DiAnt of
                                                                string
                                                           and mutable_flag =
                                                               MuMutable
                                                             | MuNil
                                                             | MuAnt of
                                                                string
                                                           and private_flag =
                                                               PrPrivate
                                                             | PrNil
                                                             | PrAnt of
                                                                string
                                                           and virtual_flag =
                                                               ViVirtual
                                                             | ViNil
                                                             | ViAnt of
                                                                string
                                                           and override_flag =
                                                               OvOverride
                                                             | OvNil
                                                             | OvAnt of
                                                                string
                                                           and row_var_flag =
                                                               RvRowVar
                                                             | RvNil
                                                             | RvAnt of
                                                                string
                                                           and 'a meta_option =
                                                               ONone
                                                             | OSome of 'a
                                                             | OAnt of string
                                                           and 'a meta_list =
                                                               LNil
                                                             | LCons of 'a *
                                                                'a meta_list
                                                             | LAnt of string
                                                           and ident =
                                                               IdAcc of 
                                                                loc * 
                                                                ident * 
                                                                ident
                                                             | IdApp of 
                                                                loc * 
                                                                ident * 
                                                                ident
                                                             | IdLid of 
                                                                loc * 
                                                                string
                                                             | IdUid of 
                                                                loc * 
                                                                string
                                                             | IdAnt of 
                                                                loc * 
                                                                string
                                                           and ctyp =
                                                               TyNil of loc
                                                             | TyAli of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyAny of loc
                                                             | TyApp of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyArr of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyCls of 
                                                                loc * 
                                                                ident
                                                             | TyLab of 
                                                                loc *
                                                                string * 
                                                                ctyp
                                                             | TyId of 
                                                                loc * 
                                                                ident
                                                             | TyMan of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyDcl of 
                                                                loc *
                                                                string *
                                                                ctyp list *
                                                                ctyp *
                                                                (ctyp * ctyp) list
                                                             | TyObj of 
                                                                loc * 
                                                                ctyp *
                                                                row_var_flag
                                                             | TyOlb of 
                                                                loc *
                                                                string * 
                                                                ctyp
                                                             | TyPol of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyTypePol of
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyQuo of 
                                                                loc * 
                                                                string
                                                             | TyQuP of 
                                                                loc * 
                                                                string
                                                             | TyQuM of 
                                                                loc * 
                                                                string
                                                             | TyAnP of loc
                                                             | TyAnM of loc
                                                             | TyVrn of 
                                                                loc * 
                                                                string
                                                             | TyRec of 
                                                                loc * 
                                                                ctyp
                                                             | TyCol of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TySem of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyCom of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TySum of 
                                                                loc * 
                                                                ctyp
                                                             | TyOf of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyAnd of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyOr of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyPrv of 
                                                                loc * 
                                                                ctyp
                                                             | TyMut of 
                                                                loc * 
                                                                ctyp
                                                             | TyTup of 
                                                                loc * 
                                                                ctyp
                                                             | TySta of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyVrnEq of
                                                                loc * 
                                                                ctyp
                                                             | TyVrnSup of
                                                                loc * 
                                                                ctyp
                                                             | TyVrnInf of
                                                                loc * 
                                                                ctyp
                                                             | TyVrnInfSup of
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyAmp of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyOfAmp of
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | TyPkg of 
                                                                loc *
                                                                module_type
                                                             | TyAnt of 
                                                                loc * 
                                                                string
                                                           and patt =
                                                               PaNil of loc
                                                             | PaId of 
                                                                loc * 
                                                                ident
                                                             | PaAli of 
                                                                loc * 
                                                                patt * 
                                                                patt
                                                             | PaAnt of 
                                                                loc * 
                                                                string
                                                             | PaAny of loc
                                                             | PaApp of 
                                                                loc * 
                                                                patt * 
                                                                patt
                                                             | PaArr of 
                                                                loc * 
                                                                patt
                                                             | PaCom of 
                                                                loc * 
                                                                patt * 
                                                                patt
                                                             | PaSem of 
                                                                loc * 
                                                                patt * 
                                                                patt
                                                             | PaChr of 
                                                                loc * 
                                                                string
                                                             | PaInt of 
                                                                loc * 
                                                                string
                                                             | PaInt32 of
                                                                loc * 
                                                                string
                                                             | PaInt64 of
                                                                loc * 
                                                                string
                                                             | PaNativeInt of
                                                                loc * 
                                                                string
                                                             | PaFlo of 
                                                                loc * 
                                                                string
                                                             | PaLab of 
                                                                loc *
                                                                string * 
                                                                patt
                                                             | PaOlb of 
                                                                loc *
                                                                string * 
                                                                patt
                                                             | PaOlbi of
                                                                loc *
                                                                string *
                                                                patt * 
                                                                expr
                                                             | PaOrp of 
                                                                loc * 
                                                                patt * 
                                                                patt
                                                             | PaRng of 
                                                                loc * 
                                                                patt * 
                                                                patt
                                                             | PaRec of 
                                                                loc * 
                                                                patt
                                                             | PaEq of 
                                                                loc * 
                                                                ident * 
                                                                patt
                                                             | PaStr of 
                                                                loc * 
                                                                string
                                                             | PaTup of 
                                                                loc * 
                                                                patt
                                                             | PaTyc of 
                                                                loc * 
                                                                patt * 
                                                                ctyp
                                                             | PaTyp of 
                                                                loc * 
                                                                ident
                                                             | PaVrn of 
                                                                loc * 
                                                                string
                                                             | PaLaz of 
                                                                loc * 
                                                                patt
                                                             | PaMod of 
                                                                loc * 
                                                                string
                                                           and expr =
                                                               ExNil of loc
                                                             | ExId of 
                                                                loc * 
                                                                ident
                                                             | ExAcc of 
                                                                loc * 
                                                                expr * 
                                                                expr
                                                             | ExAnt of 
                                                                loc * 
                                                                string
                                                             | ExApp of 
                                                                loc * 
                                                                expr * 
                                                                expr
                                                             | ExAre of 
                                                                loc * 
                                                                expr * 
                                                                expr
                                                             | ExArr of 
                                                                loc * 
                                                                expr
                                                             | ExSem of 
                                                                loc * 
                                                                expr * 
                                                                expr
                                                             | ExAsf of loc
                                                             | ExAsr of 
                                                                loc * 
                                                                expr
                                                             | ExAss of 
                                                                loc * 
                                                                expr * 
                                                                expr
                                                             | ExChr of 
                                                                loc * 
                                                                string
                                                             | ExCoe of 
                                                                loc * 
                                                                expr * 
                                                                ctyp * 
                                                                ctyp
                                                             | ExFlo of 
                                                                loc * 
                                                                string
                                                             | ExFor of 
                                                                loc *
                                                                string *
                                                                expr * 
                                                                expr *
                                                                direction_flag *
                                                                expr
                                                             | ExFun of 
                                                                loc *
                                                                match_case
                                                             | ExIfe of 
                                                                loc * 
                                                                expr * 
                                                                expr * 
                                                                expr
                                                             | ExInt of 
                                                                loc * 
                                                                string
                                                             | ExInt32 of
                                                                loc * 
                                                                string
                                                             | ExInt64 of
                                                                loc * 
                                                                string
                                                             | ExNativeInt of
                                                                loc * 
                                                                string
                                                             | ExLab of 
                                                                loc *
                                                                string * 
                                                                expr
                                                             | ExLaz of 
                                                                loc * 
                                                                expr
                                                             | ExLet of 
                                                                loc *
                                                                rec_flag *
                                                                binding *
                                                                expr
                                                             | ExLmd of 
                                                                loc *
                                                                string *
                                                                module_expr *
                                                                expr
                                                             | ExMat of 
                                                                loc * 
                                                                expr *
                                                                match_case
                                                             | ExNew of 
                                                                loc * 
                                                                ident
                                                             | ExObj of 
                                                                loc * 
                                                                patt *
                                                                class_str_item
                                                             | ExOlb of 
                                                                loc *
                                                                string * 
                                                                expr
                                                             | ExOvr of 
                                                                loc *
                                                                rec_binding
                                                             | ExRec of 
                                                                loc *
                                                                rec_binding *
                                                                expr
                                                             | ExSeq of 
                                                                loc * 
                                                                expr
                                                             | ExSnd of 
                                                                loc * 
                                                                expr * 
                                                                string
                                                             | ExSte of 
                                                                loc * 
                                                                expr * 
                                                                expr
                                                             | ExStr of 
                                                                loc * 
                                                                string
                                                             | ExTry of 
                                                                loc * 
                                                                expr *
                                                                match_case
                                                             | ExTup of 
                                                                loc * 
                                                                expr
                                                             | ExCom of 
                                                                loc * 
                                                                expr * 
                                                                expr
                                                             | ExTyc of 
                                                                loc * 
                                                                expr * 
                                                                ctyp
                                                             | ExVrn of 
                                                                loc * 
                                                                string
                                                             | ExWhi of 
                                                                loc * 
                                                                expr * 
                                                                expr
                                                             | ExOpI of 
                                                                loc * 
                                                                ident * 
                                                                expr
                                                             | ExFUN of 
                                                                loc *
                                                                string * 
                                                                expr
                                                             | ExPkg of 
                                                                loc *
                                                                module_expr
                                                           and module_type =
                                                               MtNil of loc
                                                             | MtId of 
                                                                loc * 
                                                                ident
                                                             | MtFun of 
                                                                loc *
                                                                string *
                                                                module_type *
                                                                module_type
                                                             | MtQuo of 
                                                                loc * 
                                                                string
                                                             | MtSig of 
                                                                loc *
                                                                sig_item
                                                             | MtWit of 
                                                                loc *
                                                                module_type *
                                                                with_constr
                                                             | MtOf of 
                                                                loc *
                                                                module_expr
                                                             | MtAnt of 
                                                                loc * 
                                                                string
                                                           and sig_item =
                                                               SgNil of loc
                                                             | SgCls of 
                                                                loc *
                                                                class_type
                                                             | SgClt of 
                                                                loc *
                                                                class_type
                                                             | SgSem of 
                                                                loc *
                                                                sig_item *
                                                                sig_item
                                                             | SgDir of 
                                                                loc *
                                                                string * 
                                                                expr
                                                             | SgExc of 
                                                                loc * 
                                                                ctyp
                                                             | SgExt of 
                                                                loc *
                                                                string *
                                                                ctyp *
                                                                string meta_list
                                                             | SgInc of 
                                                                loc *
                                                                module_type
                                                             | SgMod of 
                                                                loc *
                                                                string *
                                                                module_type
                                                             | SgRecMod of
                                                                loc *
                                                                module_binding
                                                             | SgMty of 
                                                                loc *
                                                                string *
                                                                module_type
                                                             | SgOpn of 
                                                                loc * 
                                                                ident
                                                             | SgTyp of 
                                                                loc * 
                                                                ctyp
                                                             | SgVal of 
                                                                loc *
                                                                string * 
                                                                ctyp
                                                             | SgAnt of 
                                                                loc * 
                                                                string
                                                           and with_constr =
                                                               WcNil of loc
                                                             | WcTyp of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | WcMod of 
                                                                loc * 
                                                                ident * 
                                                                ident
                                                             | WcTyS of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | WcMoS of 
                                                                loc * 
                                                                ident * 
                                                                ident
                                                             | WcAnd of 
                                                                loc *
                                                                with_constr *
                                                                with_constr
                                                             | WcAnt of 
                                                                loc * 
                                                                string
                                                           and binding =
                                                               BiNil of loc
                                                             | BiAnd of 
                                                                loc *
                                                                binding *
                                                                binding
                                                             | BiEq of 
                                                                loc * 
                                                                patt * 
                                                                expr
                                                             | BiAnt of 
                                                                loc * 
                                                                string
                                                           and rec_binding =
                                                               RbNil of loc
                                                             | RbSem of 
                                                                loc *
                                                                rec_binding *
                                                                rec_binding
                                                             | RbEq of 
                                                                loc * 
                                                                ident * 
                                                                expr
                                                             | RbAnt of 
                                                                loc * 
                                                                string
                                                           and module_binding =
                                                               MbNil of loc
                                                             | MbAnd of 
                                                                loc *
                                                                module_binding *
                                                                module_binding
                                                             | MbColEq of
                                                                loc *
                                                                string *
                                                                module_type *
                                                                module_expr
                                                             | MbCol of 
                                                                loc *
                                                                string *
                                                                module_type
                                                             | MbAnt of 
                                                                loc * 
                                                                string
                                                           and match_case =
                                                               McNil of loc
                                                             | McOr of 
                                                                loc *
                                                                match_case *
                                                                match_case
                                                             | McArr of 
                                                                loc * 
                                                                patt * 
                                                                expr * 
                                                                expr
                                                             | McAnt of 
                                                                loc * 
                                                                string
                                                           and module_expr =
                                                               MeNil of loc
                                                             | MeId of 
                                                                loc * 
                                                                ident
                                                             | MeApp of 
                                                                loc *
                                                                module_expr *
                                                                module_expr
                                                             | MeFun of 
                                                                loc *
                                                                string *
                                                                module_type *
                                                                module_expr
                                                             | MeStr of 
                                                                loc *
                                                                str_item
                                                             | MeTyc of 
                                                                loc *
                                                                module_expr *
                                                                module_type
                                                             | MePkg of 
                                                                loc * 
                                                                expr
                                                             | MeAnt of 
                                                                loc * 
                                                                string
                                                           and str_item =
                                                               StNil of loc
                                                             | StCls of 
                                                                loc *
                                                                class_expr
                                                             | StClt of 
                                                                loc *
                                                                class_type
                                                             | StSem of 
                                                                loc *
                                                                str_item *
                                                                str_item
                                                             | StDir of 
                                                                loc *
                                                                string * 
                                                                expr
                                                             | StExc of 
                                                                loc * 
                                                                ctyp *
                                                                ident meta_option
                                                             | StExp of 
                                                                loc * 
                                                                expr
                                                             | StExt of 
                                                                loc *
                                                                string *
                                                                ctyp *
                                                                string meta_list
                                                             | StInc of 
                                                                loc *
                                                                module_expr
                                                             | StMod of 
                                                                loc *
                                                                string *
                                                                module_expr
                                                             | StRecMod of
                                                                loc *
                                                                module_binding
                                                             | StMty of 
                                                                loc *
                                                                string *
                                                                module_type
                                                             | StOpn of 
                                                                loc * 
                                                                ident
                                                             | StTyp of 
                                                                loc * 
                                                                ctyp
                                                             | StVal of 
                                                                loc *
                                                                rec_flag *
                                                                binding
                                                             | StAnt of 
                                                                loc * 
                                                                string
                                                           and class_type =
                                                               CtNil of loc
                                                             | CtCon of 
                                                                loc *
                                                                virtual_flag *
                                                                ident * 
                                                                ctyp
                                                             | CtFun of 
                                                                loc * 
                                                                ctyp *
                                                                class_type
                                                             | CtSig of 
                                                                loc * 
                                                                ctyp *
                                                                class_sig_item
                                                             | CtAnd of 
                                                                loc *
                                                                class_type *
                                                                class_type
                                                             | CtCol of 
                                                                loc *
                                                                class_type *
                                                                class_type
                                                             | CtEq of 
                                                                loc *
                                                                class_type *
                                                                class_type
                                                             | CtAnt of 
                                                                loc * 
                                                                string
                                                           and class_sig_item =
                                                               CgNil of loc
                                                             | CgCtr of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | CgSem of 
                                                                loc *
                                                                class_sig_item *
                                                                class_sig_item
                                                             | CgInh of 
                                                                loc *
                                                                class_type
                                                             | CgMth of 
                                                                loc *
                                                                string *
                                                                private_flag *
                                                                ctyp
                                                             | CgVal of 
                                                                loc *
                                                                string *
                                                                mutable_flag *
                                                                virtual_flag *
                                                                ctyp
                                                             | CgVir of 
                                                                loc *
                                                                string *
                                                                private_flag *
                                                                ctyp
                                                             | CgAnt of 
                                                                loc * 
                                                                string
                                                           and class_expr =
                                                               CeNil of loc
                                                             | CeApp of 
                                                                loc *
                                                                class_expr *
                                                                expr
                                                             | CeCon of 
                                                                loc *
                                                                virtual_flag *
                                                                ident * 
                                                                ctyp
                                                             | CeFun of 
                                                                loc * 
                                                                patt *
                                                                class_expr
                                                             | CeLet of 
                                                                loc *
                                                                rec_flag *
                                                                binding *
                                                                class_expr
                                                             | CeStr of 
                                                                loc * 
                                                                patt *
                                                                class_str_item
                                                             | CeTyc of 
                                                                loc *
                                                                class_expr *
                                                                class_type
                                                             | CeAnd of 
                                                                loc *
                                                                class_expr *
                                                                class_expr
                                                             | CeEq of 
                                                                loc *
                                                                class_expr *
                                                                class_expr
                                                             | CeAnt of 
                                                                loc * 
                                                                string
                                                           and class_str_item =
                                                               CrNil of loc
                                                             | CrSem of 
                                                                loc *
                                                                class_str_item *
                                                                class_str_item
                                                             | CrCtr of 
                                                                loc * 
                                                                ctyp * 
                                                                ctyp
                                                             | CrInh of 
                                                                loc *
                                                                override_flag *
                                                                class_expr *
                                                                string
                                                             | CrIni of 
                                                                loc * 
                                                                expr
                                                             | CrMth of 
                                                                loc *
                                                                string *
                                                                override_flag *
                                                                private_flag *
                                                                expr * 
                                                                ctyp
                                                             | CrVal of 
                                                                loc *
                                                                string *
                                                                override_flag *
                                                                mutable_flag *
                                                                expr
                                                             | CrVir of 
                                                                loc *
                                                                string *
                                                                private_flag *
                                                                ctyp
                                                             | CrVvr of 
                                                                loc *
                                                                string *
                                                                mutable_flag *
                                                                ctyp
                                                             | CrAnt of 
                                                                loc * 
                                                                string

                                                           val loc_of_ctyp :
                                                            (ctyp -> loc)

                                                           val loc_of_patt :
                                                            (patt -> loc)

                                                           val loc_of_expr :
                                                            (expr -> loc)

                                                           val loc_of_module_type :
                                                            (module_type ->
                                                             loc)

                                                           val loc_of_module_expr :
                                                            (module_expr ->
                                                             loc)

                                                           val loc_of_sig_item :
                                                            (sig_item -> loc)

                                                           val loc_of_str_item :
                                                            (str_item -> loc)

                                                           val loc_of_class_type :
                                                            (class_type ->
                                                             loc)

                                                           val loc_of_class_sig_item :
                                                            (class_sig_item
                                                             -> loc)

                                                           val loc_of_class_expr :
                                                            (class_expr ->
                                                             loc)

                                                           val loc_of_class_str_item :
                                                            (class_str_item
                                                             -> loc)

                                                           val loc_of_with_constr :
                                                            (with_constr ->
                                                             loc)

                                                           val loc_of_binding :
                                                            (binding -> loc)

                                                           val loc_of_rec_binding :
                                                            (rec_binding ->
                                                             loc)

                                                           val loc_of_module_binding :
                                                            (module_binding
                                                             -> loc)

                                                           val loc_of_match_case :
                                                            (match_case ->
                                                             loc)

                                                           val loc_of_ident :
                                                            (ident -> loc)

                                                           module Meta :
                                                            sig
                                                             module type META_LOC
                                                               = sig
                                                                  val meta_loc_patt :
                                                                   (loc ->
                                                                    (loc ->
                                                                    patt))

                                                                  val meta_loc_expr :
                                                                   (loc ->
                                                                    (loc ->
                                                                    expr))

                                                                 end

                                                             module MetaLoc :
                                                              sig
                                                               val meta_loc_patt :
                                                                (loc ->
                                                                 (loc ->
                                                                  patt))

                                                               val meta_loc_expr :
                                                                (loc ->
                                                                 (loc ->
                                                                  expr))

                                                              end

                                                             module
                                                              MetaGhostLoc :
                                                              sig
                                                               val meta_loc_patt :
                                                                (loc ->
                                                                 ('a -> patt))

                                                               val meta_loc_expr :
                                                                (loc ->
                                                                 ('a -> expr))

                                                              end

                                                             module
                                                              MetaLocVar :
                                                              sig
                                                               val meta_loc_patt :
                                                                (loc ->
                                                                 ('a -> patt))

                                                               val meta_loc_expr :
                                                                (loc ->
                                                                 ('a -> expr))

                                                              end

                                                             module Make :
                                                              functor
                                                               (MetaLoc : META_LOC) ->
                                                               sig
                                                                module Expr :
                                                                 sig
                                                                  val meta_string :
                                                                   (loc ->
                                                                    (string
                                                                    -> 
                                                                    expr))

                                                                  val meta_int :
                                                                   (loc ->
                                                                    (string
                                                                    -> 
                                                                    expr))

                                                                  val meta_float :
                                                                   (loc ->
                                                                    (string
                                                                    -> 
                                                                    expr))

                                                                  val meta_char :
                                                                   (loc ->
                                                                    (string
                                                                    -> 
                                                                    expr))

                                                                  val meta_bool :
                                                                   (loc ->
                                                                    (bool ->
                                                                    expr))

                                                                  val meta_list :
                                                                   ((loc ->
                                                                    ('a ->
                                                                    expr)) ->
                                                                    (loc ->
                                                                    ('a list
                                                                    -> 
                                                                    expr)))

                                                                  val meta_binding :
                                                                   (loc ->
                                                                    (binding
                                                                    -> 
                                                                    expr))

                                                                  val meta_rec_binding :
                                                                   (loc ->
                                                                    (rec_binding
                                                                    -> 
                                                                    expr))

                                                                  val meta_class_expr :
                                                                   (loc ->
                                                                    (class_expr
                                                                    -> 
                                                                    expr))

                                                                  val meta_class_sig_item :
                                                                   (loc ->
                                                                    (class_sig_item
                                                                    -> 
                                                                    expr))

                                                                  val meta_class_str_item :
                                                                   (loc ->
                                                                    (class_str_item
                                                                    -> 
                                                                    expr))

                                                                  val meta_class_type :
                                                                   (loc ->
                                                                    (class_type
                                                                    -> 
                                                                    expr))

                                                                  val meta_ctyp :
                                                                   (loc ->
                                                                    (ctyp ->
                                                                    expr))

                                                                  val meta_expr :
                                                                   (loc ->
                                                                    (expr ->
                                                                    expr))

                                                                  val meta_ident :
                                                                   (loc ->
                                                                    (ident ->
                                                                    expr))

                                                                  val meta_match_case :
                                                                   (loc ->
                                                                    (match_case
                                                                    -> 
                                                                    expr))

                                                                  val meta_module_binding :
                                                                   (loc ->
                                                                    (module_binding
                                                                    -> 
                                                                    expr))

                                                                  val meta_module_expr :
                                                                   (loc ->
                                                                    (module_expr
                                                                    -> 
                                                                    expr))

                                                                  val meta_module_type :
                                                                   (loc ->
                                                                    (module_type
                                                                    -> 
                                                                    expr))

                                                                  val meta_patt :
                                                                   (loc ->
                                                                    (patt ->
                                                                    expr))

                                                                  val meta_sig_item :
                                                                   (loc ->
                                                                    (sig_item
                                                                    -> 
                                                                    expr))

                                                                  val meta_str_item :
                                                                   (loc ->
                                                                    (str_item
                                                                    -> 
                                                                    expr))

                                                                  val meta_with_constr :
                                                                   (loc ->
                                                                    (with_constr
                                                                    -> 
                                                                    expr))

                                                                  val meta_rec_flag :
                                                                   (loc ->
                                                                    (rec_flag
                                                                    -> 
                                                                    expr))

                                                                  val meta_mutable_flag :
                                                                   (loc ->
                                                                    (mutable_flag
                                                                    -> 
                                                                    expr))

                                                                  val meta_virtual_flag :
                                                                   (loc ->
                                                                    (virtual_flag
                                                                    -> 
                                                                    expr))

                                                                  val meta_private_flag :
                                                                   (loc ->
                                                                    (private_flag
                                                                    -> 
                                                                    expr))

                                                                  val meta_row_var_flag :
                                                                   (loc ->
                                                                    (row_var_flag
                                                                    -> 
                                                                    expr))

                                                                  val meta_override_flag :
                                                                   (loc ->
                                                                    (override_flag
                                                                    -> 
                                                                    expr))

                                                                  val meta_direction_flag :
                                                                   (loc ->
                                                                    (direction_flag
                                                                    -> 
                                                                    expr))

                                                                 end

                                                                module Patt :
                                                                 sig
                                                                  val meta_string :
                                                                   (loc ->
                                                                    (string
                                                                    -> 
                                                                    patt))

                                                                  val meta_int :
                                                                   (loc ->
                                                                    (string
                                                                    -> 
                                                                    patt))

                                                                  val meta_float :
                                                                   (loc ->
                                                                    (string
                                                                    -> 
                                                                    patt))

                                                                  val meta_char :
                                                                   (loc ->
                                                                    (string
                                                                    -> 
                                                                    patt))

                                                                  val meta_bool :
                                                                   (loc ->
                                                                    (bool ->
                                                                    patt))

                                                                  val meta_list :
                                                                   ((loc ->
                                                                    ('a ->
                                                                    patt)) ->
                                                                    (loc ->
                                                                    ('a list
                                                                    -> 
                                                                    patt)))

                                                                  val meta_binding :
                                                                   (loc ->
                                                                    (binding
                                                                    -> 
                                                                    patt))

                                                                  val meta_rec_binding :
                                                                   (loc ->
                                                                    (rec_binding
                                                                    -> 
                                                                    patt))

                                                                  val meta_class_expr :
                                                                   (loc ->
                                                                    (class_expr
                                                                    -> 
                                                                    patt))

                                                                  val meta_class_sig_item :
                                                                   (loc ->
                                                                    (class_sig_item
                                                                    -> 
                                                                    patt))

                                                                  val meta_class_str_item :
                                                                   (loc ->
                                                                    (class_str_item
                                                                    -> 
                                                                    patt))

                                                                  val meta_class_type :
                                                                   (loc ->
                                                                    (class_type
                                                                    -> 
                                                                    patt))

                                                                  val meta_ctyp :
                                                                   (loc ->
                                                                    (ctyp ->
                                                                    patt))

                                                                  val meta_expr :
                                                                   (loc ->
                                                                    (expr ->
                                                                    patt))

                                                                  val meta_ident :
                                                                   (loc ->
                                                                    (ident ->
                                                                    patt))

                                                                  val meta_match_case :
                                                                   (loc ->
                                                                    (match_case
                                                                    -> 
                                                                    patt))

                                                                  val meta_module_binding :
                                                                   (loc ->
                                                                    (module_binding
                                                                    -> 
                                                                    patt))

                                                                  val meta_module_expr :
                                                                   (loc ->
                                                                    (module_expr
                                                                    -> 
                                                                    patt))

                                                                  val meta_module_type :
                                                                   (loc ->
                                                                    (module_type
                                                                    -> 
                                                                    patt))

                                                                  val meta_patt :
                                                                   (loc ->
                                                                    (patt ->
                                                                    patt))

                                                                  val meta_sig_item :
                                                                   (loc ->
                                                                    (sig_item
                                                                    -> 
                                                                    patt))

                                                                  val meta_str_item :
                                                                   (loc ->
                                                                    (str_item
                                                                    -> 
                                                                    patt))

                                                                  val meta_with_constr :
                                                                   (loc ->
                                                                    (with_constr
                                                                    -> 
                                                                    patt))

                                                                  val meta_rec_flag :
                                                                   (loc ->
                                                                    (rec_flag
                                                                    -> 
                                                                    patt))

                                                                  val meta_mutable_flag :
                                                                   (loc ->
                                                                    (mutable_flag
                                                                    -> 
                                                                    patt))

                                                                  val meta_virtual_flag :
                                                                   (loc ->
                                                                    (virtual_flag
                                                                    -> 
                                                                    patt))

                                                                  val meta_private_flag :
                                                                   (loc ->
                                                                    (private_flag
                                                                    -> 
                                                                    patt))

                                                                  val meta_row_var_flag :
                                                                   (loc ->
                                                                    (row_var_flag
                                                                    -> 
                                                                    patt))

                                                                  val meta_override_flag :
                                                                   (loc ->
                                                                    (override_flag
                                                                    -> 
                                                                    patt))

                                                                  val meta_direction_flag :
                                                                   (loc ->
                                                                    (direction_flag
                                                                    -> 
                                                                    patt))

                                                                 end

                                                               end

                                                            end

                                                           class map :
                                                            object
                                                             ('self_type)
                                                             method string :
                                                              (string ->
                                                               string)
                                                             method list :
                                                              'a 'b .
                                                               (('self_type
                                                                 ->
                                                                 ('a -> 'b))
                                                                ->
                                                                ('a list ->
                                                                 'b list))
                                                             method meta_bool :
                                                              (meta_bool ->
                                                               meta_bool)
                                                             method meta_option :
                                                              'a 'b .
                                                               (('self_type
                                                                 ->
                                                                 ('a -> 'b))
                                                                ->
                                                                ('a meta_option
                                                                 ->
                                                                 'b meta_option))
                                                             method meta_list :
                                                              'a 'b .
                                                               (('self_type
                                                                 ->
                                                                 ('a -> 'b))
                                                                ->
                                                                ('a meta_list
                                                                 ->
                                                                 'b meta_list))
                                                             method loc :
                                                              (loc -> loc)
                                                             method expr :
                                                              (expr -> expr)
                                                             method patt :
                                                              (patt -> patt)
                                                             method ctyp :
                                                              (ctyp -> ctyp)
                                                             method str_item :
                                                              (str_item ->
                                                               str_item)
                                                             method sig_item :
                                                              (sig_item ->
                                                               sig_item)
                                                             method module_expr :
                                                              (module_expr ->
                                                               module_expr)
                                                             method module_type :
                                                              (module_type ->
                                                               module_type)
                                                             method class_expr :
                                                              (class_expr ->
                                                               class_expr)
                                                             method class_type :
                                                              (class_type ->
                                                               class_type)
                                                             method class_sig_item :
                                                              (class_sig_item
                                                               ->
                                                               class_sig_item)
                                                             method class_str_item :
                                                              (class_str_item
                                                               ->
                                                               class_str_item)
                                                             method with_constr :
                                                              (with_constr ->
                                                               with_constr)
                                                             method binding :
                                                              (binding ->
                                                               binding)
                                                             method rec_binding :
                                                              (rec_binding ->
                                                               rec_binding)
                                                             method module_binding :
                                                              (module_binding
                                                               ->
                                                               module_binding)
                                                             method match_case :
                                                              (match_case ->
                                                               match_case)
                                                             method ident :
                                                              (ident ->
                                                               ident)
                                                             method mutable_flag :
                                                              (mutable_flag
                                                               ->
                                                               mutable_flag)
                                                             method private_flag :
                                                              (private_flag
                                                               ->
                                                               private_flag)
                                                             method virtual_flag :
                                                              (virtual_flag
                                                               ->
                                                               virtual_flag)
                                                             method direction_flag :
                                                              (direction_flag
                                                               ->
                                                               direction_flag)
                                                             method rec_flag :
                                                              (rec_flag ->
                                                               rec_flag)
                                                             method row_var_flag :
                                                              (row_var_flag
                                                               ->
                                                               row_var_flag)
                                                             method override_flag :
                                                              (override_flag
                                                               ->
                                                               override_flag)
                                                             method unknown :
                                                              'a . ('a -> 'a)
                                                            end

                                                            class fold :
                                                             object
                                                              ('self_type)
                                                              method string :
                                                               (string ->
                                                                'self_type)
                                                              method list :
                                                               'a .
                                                                (('self_type
                                                                  ->
                                                                  ('a ->
                                                                   'self_type))
                                                                 ->
                                                                 ('a list ->
                                                                  'self_type))
                                                              method meta_bool :
                                                               (meta_bool ->
                                                                'self_type)
                                                              method meta_option :
                                                               'a .
                                                                (('self_type
                                                                  ->
                                                                  ('a ->
                                                                   'self_type))
                                                                 ->
                                                                 ('a meta_option
                                                                  ->
                                                                  'self_type))
                                                              method meta_list :
                                                               'a .
                                                                (('self_type
                                                                  ->
                                                                  ('a ->
                                                                   'self_type))
                                                                 ->
                                                                 ('a meta_list
                                                                  ->
                                                                  'self_type))
                                                              method loc :
                                                               (loc ->
                                                                'self_type)
                                                              method expr :
                                                               (expr ->
                                                                'self_type)
                                                              method patt :
                                                               (patt ->
                                                                'self_type)
                                                              method ctyp :
                                                               (ctyp ->
                                                                'self_type)
                                                              method str_item :
                                                               (str_item ->
                                                                'self_type)
                                                              method sig_item :
                                                               (sig_item ->
                                                                'self_type)
                                                              method module_expr :
                                                               (module_expr
                                                                ->
                                                                'self_type)
                                                              method module_type :
                                                               (module_type
                                                                ->
                                                                'self_type)
                                                              method class_expr :
                                                               (class_expr ->
                                                                'self_type)
                                                              method class_type :
                                                               (class_type ->
                                                                'self_type)
                                                              method class_sig_item :
                                                               (class_sig_item
                                                                ->
                                                                'self_type)
                                                              method class_str_item :
                                                               (class_str_item
                                                                ->
                                                                'self_type)
                                                              method with_constr :
                                                               (with_constr
                                                                ->
                                                                'self_type)
                                                              method binding :
                                                               (binding ->
                                                                'self_type)
                                                              method rec_binding :
                                                               (rec_binding
                                                                ->
                                                                'self_type)
                                                              method module_binding :
                                                               (module_binding
                                                                ->
                                                                'self_type)
                                                              method match_case :
                                                               (match_case ->
                                                                'self_type)
                                                              method ident :
                                                               (ident ->
                                                                'self_type)
                                                              method rec_flag :
                                                               (rec_flag ->
                                                                'self_type)
                                                              method direction_flag :
                                                               (direction_flag
                                                                ->
                                                                'self_type)
                                                              method mutable_flag :
                                                               (mutable_flag
                                                                ->
                                                                'self_type)
                                                              method private_flag :
                                                               (private_flag
                                                                ->
                                                                'self_type)
                                                              method virtual_flag :
                                                               (virtual_flag
                                                                ->
                                                                'self_type)
                                                              method row_var_flag :
                                                               (row_var_flag
                                                                ->
                                                                'self_type)
                                                              method override_flag :
                                                               (override_flag
                                                                ->
                                                                'self_type)
                                                              method unknown :
                                                               'a .
                                                                ('a ->
                                                                 'self_type)
                                                             end

                                                             val map_expr :
                                                              ((expr -> expr)
                                                               -> map)

                                                             val map_patt :
                                                              ((patt -> patt)
                                                               -> map)

                                                             val map_ctyp :
                                                              ((ctyp -> ctyp)
                                                               -> map)

                                                             val map_str_item :
                                                              ((str_item ->
                                                                str_item) ->
                                                               map)

                                                             val map_sig_item :
                                                              ((sig_item ->
                                                                sig_item) ->
                                                               map)

                                                             val map_loc :
                                                              ((loc -> loc)
                                                               -> map)

                                                             val ident_of_expr :
                                                              (expr -> ident)

                                                             val ident_of_patt :
                                                              (patt -> ident)

                                                             val ident_of_ctyp :
                                                              (ctyp -> ident)

                                                             val biAnd_of_list :
                                                              (binding list
                                                               -> binding)

                                                             val rbSem_of_list :
                                                              (rec_binding list
                                                               ->
                                                               rec_binding)

                                                             val paSem_of_list :
                                                              (patt list ->
                                                               patt)

                                                             val paCom_of_list :
                                                              (patt list ->
                                                               patt)

                                                             val tyOr_of_list :
                                                              (ctyp list ->
                                                               ctyp)

                                                             val tyAnd_of_list :
                                                              (ctyp list ->
                                                               ctyp)

                                                             val tyAmp_of_list :
                                                              (ctyp list ->
                                                               ctyp)

                                                             val tySem_of_list :
                                                              (ctyp list ->
                                                               ctyp)

                                                             val tyCom_of_list :
                                                              (ctyp list ->
                                                               ctyp)

                                                             val tySta_of_list :
                                                              (ctyp list ->
                                                               ctyp)

                                                             val stSem_of_list :
                                                              (str_item list
                                                               -> str_item)

                                                             val sgSem_of_list :
                                                              (sig_item list
                                                               -> sig_item)

                                                             val crSem_of_list :
                                                              (class_str_item list
                                                               ->
                                                               class_str_item)

                                                             val cgSem_of_list :
                                                              (class_sig_item list
                                                               ->
                                                               class_sig_item)

                                                             val ctAnd_of_list :
                                                              (class_type list
                                                               -> class_type)

                                                             val ceAnd_of_list :
                                                              (class_expr list
                                                               -> class_expr)

                                                             val wcAnd_of_list :
                                                              (with_constr list
                                                               ->
                                                               with_constr)

                                                             val meApp_of_list :
                                                              (module_expr list
                                                               ->
                                                               module_expr)

                                                             val mbAnd_of_list :
                                                              (module_binding list
                                                               ->
                                                               module_binding)

                                                             val mcOr_of_list :
                                                              (match_case list
                                                               -> match_case)

                                                             val idAcc_of_list :
                                                              (ident list ->
                                                               ident)

                                                             val idApp_of_list :
                                                              (ident list ->
                                                               ident)

                                                             val exSem_of_list :
                                                              (expr list ->
                                                               expr)

                                                             val exCom_of_list :
                                                              (expr list ->
                                                               expr)

                                                             val list_of_ctyp :
                                                              (ctyp ->
                                                               (ctyp list ->
                                                                ctyp list))

                                                             val list_of_binding :
                                                              (binding ->
                                                               (binding list
                                                                ->
                                                                binding list))

                                                             val list_of_rec_binding :
                                                              (rec_binding ->
                                                               (rec_binding list
                                                                ->
                                                                rec_binding list))

                                                             val list_of_with_constr :
                                                              (with_constr ->
                                                               (with_constr list
                                                                ->
                                                                with_constr list))

                                                             val list_of_patt :
                                                              (patt ->
                                                               (patt list ->
                                                                patt list))

                                                             val list_of_expr :
                                                              (expr ->
                                                               (expr list ->
                                                                expr list))

                                                             val list_of_str_item :
                                                              (str_item ->
                                                               (str_item list
                                                                ->
                                                                str_item list))

                                                             val list_of_sig_item :
                                                              (sig_item ->
                                                               (sig_item list
                                                                ->
                                                                sig_item list))

                                                             val list_of_class_sig_item :
                                                              (class_sig_item
                                                               ->
                                                               (class_sig_item list
                                                                ->
                                                                class_sig_item list))

                                                             val list_of_class_str_item :
                                                              (class_str_item
                                                               ->
                                                               (class_str_item list
                                                                ->
                                                                class_str_item list))

                                                             val list_of_class_type :
                                                              (class_type ->
                                                               (class_type list
                                                                ->
                                                                class_type list))

                                                             val list_of_class_expr :
                                                              (class_expr ->
                                                               (class_expr list
                                                                ->
                                                                class_expr list))

                                                             val list_of_module_expr :
                                                              (module_expr ->
                                                               (module_expr list
                                                                ->
                                                                module_expr list))

                                                             val list_of_module_binding :
                                                              (module_binding
                                                               ->
                                                               (module_binding list
                                                                ->
                                                                module_binding list))

                                                             val list_of_match_case :
                                                              (match_case ->
                                                               (match_case list
                                                                ->
                                                                match_case list))

                                                             val list_of_ident :
                                                              (ident ->
                                                               (ident list ->
                                                                ident list))

                                                             val safe_string_escaped :
                                                              (string ->
                                                               string)

                                                             val is_irrefut_patt :
                                                              (patt -> bool)

                                                             val is_constructor :
                                                              (ident -> bool)

                                                             val is_patt_constructor :
                                                              (patt -> bool)

                                                             val is_expr_constructor :
                                                              (expr -> bool)

                                                             val ty_of_stl :
                                                              ((Loc.t *
                                                                string *
                                                                ctyp list) ->
                                                               ctyp)

                                                             val ty_of_sbt :
                                                              ((Loc.t *
                                                                string *
                                                                bool * 
                                                                ctyp) ->
                                                               ctyp)

                                                             val bi_of_pe :
                                                              ((patt * expr)
                                                               -> binding)

                                                             val pel_of_binding :
                                                              (binding ->
                                                               (patt * expr) list)

                                                             val binding_of_pel :
                                                              ((patt * expr) list
                                                               -> binding)

                                                             val sum_type_of_list :
                                                              ((Loc.t *
                                                                string *
                                                                ctyp list) list
                                                               -> ctyp)

                                                             val record_type_of_list :
                                                              ((Loc.t *
                                                                string *
                                                                bool * 
                                                                ctyp) list ->
                                                               ctyp)

                                                            end

module Camlp4AstToAst =
                                                                  functor (M : Camlp4Ast) ->
                                                                   (M :
                                                                    (Ast with
                                                                    type
                                                                     loc =
                                                                    M.loc
                                                                    and type
                                                                     loc =
                                                                    M.loc
                                                                    and type
                                                                     meta_bool =
                                                                    M.meta_bool
                                                                    and type
                                                                     meta_bool =
                                                                    M.meta_bool
                                                                    and type
                                                                    'a meta_option =
                                                                    'a M.meta_option
                                                                    and type
                                                                    'a meta_option =
                                                                    'a M.meta_option
                                                                    and type
                                                                    'a meta_list =
                                                                    'a M.meta_list
                                                                    and type
                                                                    'a meta_list =
                                                                    'a M.meta_list
                                                                    and type
                                                                     ctyp =
                                                                    M.ctyp
                                                                    and type
                                                                     ctyp =
                                                                    M.ctyp
                                                                    and type
                                                                     patt =
                                                                    M.patt
                                                                    and type
                                                                     patt =
                                                                    M.patt
                                                                    and type
                                                                     expr =
                                                                    M.expr
                                                                    and type
                                                                     expr =
                                                                    M.expr
                                                                    and type
                                                                     module_type =
                                                                    M.module_type
                                                                    and type
                                                                     module_type =
                                                                    M.module_type
                                                                    and type
                                                                     sig_item =
                                                                    M.sig_item
                                                                    and type
                                                                     sig_item =
                                                                    M.sig_item
                                                                    and type
                                                                     with_constr =
                                                                    M.with_constr
                                                                    and type
                                                                     with_constr =
                                                                    M.with_constr
                                                                    and type
                                                                     module_expr =
                                                                    M.module_expr
                                                                    and type
                                                                     module_expr =
                                                                    M.module_expr
                                                                    and type
                                                                     str_item =
                                                                    M.str_item
                                                                    and type
                                                                     str_item =
                                                                    M.str_item
                                                                    and type
                                                                     class_type =
                                                                    M.class_type
                                                                    and type
                                                                     class_type =
                                                                    M.class_type
                                                                    and type
                                                                     class_sig_item =
                                                                    M.class_sig_item
                                                                    and type
                                                                     class_sig_item =
                                                                    M.class_sig_item
                                                                    and type
                                                                     class_expr =
                                                                    M.class_expr
                                                                    and type
                                                                     class_expr =
                                                                    M.class_expr
                                                                    and type
                                                                     class_str_item =
                                                                    M.class_str_item
                                                                    and type
                                                                     class_str_item =
                                                                    M.class_str_item
                                                                    and type
                                                                     binding =
                                                                    M.binding
                                                                    and type
                                                                     binding =
                                                                    M.binding
                                                                    and type
                                                                     rec_binding =
                                                                    M.rec_binding
                                                                    and type
                                                                     rec_binding =
                                                                    M.rec_binding
                                                                    and type
                                                                     module_binding =
                                                                    M.module_binding
                                                                    and type
                                                                     module_binding =
                                                                    M.module_binding
                                                                    and type
                                                                     match_case =
                                                                    M.match_case
                                                                    and type
                                                                     match_case =
                                                                    M.match_case
                                                                    and type
                                                                     ident =
                                                                    M.ident
                                                                    and type
                                                                     ident =
                                                                    M.ident
                                                                    and type
                                                                     rec_flag =
                                                                    M.rec_flag
                                                                    and type
                                                                     rec_flag =
                                                                    M.rec_flag
                                                                    and type
                                                                     direction_flag =
                                                                    M.direction_flag
                                                                    and type
                                                                     direction_flag =
                                                                    M.direction_flag
                                                                    and type
                                                                     mutable_flag =
                                                                    M.mutable_flag
                                                                    and type
                                                                     mutable_flag =
                                                                    M.mutable_flag
                                                                    and type
                                                                     private_flag =
                                                                    M.private_flag
                                                                    and type
                                                                     private_flag =
                                                                    M.private_flag
                                                                    and type
                                                                     virtual_flag =
                                                                    M.virtual_flag
                                                                    and type
                                                                     virtual_flag =
                                                                    M.virtual_flag
                                                                    and type
                                                                     row_var_flag =
                                                                    M.row_var_flag
                                                                    and type
                                                                     row_var_flag =
                                                                    M.row_var_flag
                                                                    and type
                                                                     override_flag =
                                                                    M.override_flag))


                                                          module MakeCamlp4Ast =
                                                           functor (Loc : Type) ->
                                                            struct
                                                             type loc = Loc.t
                                                             and meta_bool =
                                                                 BTrue
                                                               | BFalse
                                                               | BAnt of
                                                                  string
                                                             and rec_flag =
                                                                 ReRecursive
                                                               | ReNil
                                                               | ReAnt of
                                                                  string
                                                             and direction_flag =
                                                                 DiTo
                                                               | DiDownto
                                                               | DiAnt of
                                                                  string
                                                             and mutable_flag =
                                                                 MuMutable
                                                               | MuNil
                                                               | MuAnt of
                                                                  string
                                                             and private_flag =
                                                                 PrPrivate
                                                               | PrNil
                                                               | PrAnt of
                                                                  string
                                                             and virtual_flag =
                                                                 ViVirtual
                                                               | ViNil
                                                               | ViAnt of
                                                                  string
                                                             and override_flag =
                                                                 OvOverride
                                                               | OvNil
                                                               | OvAnt of
                                                                  string
                                                             and row_var_flag =
                                                                 RvRowVar
                                                               | RvNil
                                                               | RvAnt of
                                                                  string
                                                             and 'a meta_option =
                                                                 ONone
                                                               | OSome of 'a
                                                               | OAnt of
                                                                  string
                                                             and 'a meta_list =
                                                                 LNil
                                                               | LCons of
                                                                  'a *
                                                                  'a meta_list
                                                               | LAnt of
                                                                  string
                                                             and ident =
                                                                 IdAcc of
                                                                  loc *
                                                                  ident *
                                                                  ident
                                                               | IdApp of
                                                                  loc *
                                                                  ident *
                                                                  ident
                                                               | IdLid of
                                                                  loc *
                                                                  string
                                                               | IdUid of
                                                                  loc *
                                                                  string
                                                               | IdAnt of
                                                                  loc *
                                                                  string
                                                             and ctyp =
                                                                 TyNil of loc
                                                               | TyAli of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyAny of loc
                                                               | TyApp of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyArr of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyCls of
                                                                  loc * 
                                                                  ident
                                                               | TyLab of
                                                                  loc *
                                                                  string *
                                                                  ctyp
                                                               | TyId of
                                                                  loc * 
                                                                  ident
                                                               | TyMan of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyDcl of
                                                                  loc *
                                                                  string *
                                                                  ctyp list *
                                                                  ctyp *
                                                                  (ctyp *
                                                                   ctyp) list
                                                               | TyObj of
                                                                  loc *
                                                                  ctyp *
                                                                  row_var_flag
                                                               | TyOlb of
                                                                  loc *
                                                                  string *
                                                                  ctyp
                                                               | TyPol of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyTypePol of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyQuo of
                                                                  loc *
                                                                  string
                                                               | TyQuP of
                                                                  loc *
                                                                  string
                                                               | TyQuM of
                                                                  loc *
                                                                  string
                                                               | TyAnP of loc
                                                               | TyAnM of loc
                                                               | TyVrn of
                                                                  loc *
                                                                  string
                                                               | TyRec of
                                                                  loc * 
                                                                  ctyp
                                                               | TyCol of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TySem of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyCom of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TySum of
                                                                  loc * 
                                                                  ctyp
                                                               | TyOf of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyAnd of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyOr of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyPrv of
                                                                  loc * 
                                                                  ctyp
                                                               | TyMut of
                                                                  loc * 
                                                                  ctyp
                                                               | TyTup of
                                                                  loc * 
                                                                  ctyp
                                                               | TySta of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyVrnEq of
                                                                  loc * 
                                                                  ctyp
                                                               | TyVrnSup of
                                                                  loc * 
                                                                  ctyp
                                                               | TyVrnInf of
                                                                  loc * 
                                                                  ctyp
                                                               | TyVrnInfSup
                                                                  of 
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyAmp of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyOfAmp of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | TyPkg of
                                                                  loc *
                                                                  module_type
                                                               | TyAnt of
                                                                  loc *
                                                                  string
                                                             and patt =
                                                                 PaNil of loc
                                                               | PaId of
                                                                  loc * 
                                                                  ident
                                                               | PaAli of
                                                                  loc *
                                                                  patt * 
                                                                  patt
                                                               | PaAnt of
                                                                  loc *
                                                                  string
                                                               | PaAny of loc
                                                               | PaApp of
                                                                  loc *
                                                                  patt * 
                                                                  patt
                                                               | PaArr of
                                                                  loc * 
                                                                  patt
                                                               | PaCom of
                                                                  loc *
                                                                  patt * 
                                                                  patt
                                                               | PaSem of
                                                                  loc *
                                                                  patt * 
                                                                  patt
                                                               | PaChr of
                                                                  loc *
                                                                  string
                                                               | PaInt of
                                                                  loc *
                                                                  string
                                                               | PaInt32 of
                                                                  loc *
                                                                  string
                                                               | PaInt64 of
                                                                  loc *
                                                                  string
                                                               | PaNativeInt
                                                                  of 
                                                                  loc *
                                                                  string
                                                               | PaFlo of
                                                                  loc *
                                                                  string
                                                               | PaLab of
                                                                  loc *
                                                                  string *
                                                                  patt
                                                               | PaOlb of
                                                                  loc *
                                                                  string *
                                                                  patt
                                                               | PaOlbi of
                                                                  loc *
                                                                  string *
                                                                  patt * 
                                                                  expr
                                                               | PaOrp of
                                                                  loc *
                                                                  patt * 
                                                                  patt
                                                               | PaRng of
                                                                  loc *
                                                                  patt * 
                                                                  patt
                                                               | PaRec of
                                                                  loc * 
                                                                  patt
                                                               | PaEq of
                                                                  loc *
                                                                  ident *
                                                                  patt
                                                               | PaStr of
                                                                  loc *
                                                                  string
                                                               | PaTup of
                                                                  loc * 
                                                                  patt
                                                               | PaTyc of
                                                                  loc *
                                                                  patt * 
                                                                  ctyp
                                                               | PaTyp of
                                                                  loc * 
                                                                  ident
                                                               | PaVrn of
                                                                  loc *
                                                                  string
                                                               | PaLaz of
                                                                  loc * 
                                                                  patt
                                                               | PaMod of
                                                                  loc *
                                                                  string
                                                             and expr =
                                                                 ExNil of loc
                                                               | ExId of
                                                                  loc * 
                                                                  ident
                                                               | ExAcc of
                                                                  loc *
                                                                  expr * 
                                                                  expr
                                                               | ExAnt of
                                                                  loc *
                                                                  string
                                                               | ExApp of
                                                                  loc *
                                                                  expr * 
                                                                  expr
                                                               | ExAre of
                                                                  loc *
                                                                  expr * 
                                                                  expr
                                                               | ExArr of
                                                                  loc * 
                                                                  expr
                                                               | ExSem of
                                                                  loc *
                                                                  expr * 
                                                                  expr
                                                               | ExAsf of loc
                                                               | ExAsr of
                                                                  loc * 
                                                                  expr
                                                               | ExAss of
                                                                  loc *
                                                                  expr * 
                                                                  expr
                                                               | ExChr of
                                                                  loc *
                                                                  string
                                                               | ExCoe of
                                                                  loc *
                                                                  expr *
                                                                  ctyp * 
                                                                  ctyp
                                                               | ExFlo of
                                                                  loc *
                                                                  string
                                                               | ExFor of
                                                                  loc *
                                                                  string *
                                                                  expr *
                                                                  expr *
                                                                  direction_flag *
                                                                  expr
                                                               | ExFun of
                                                                  loc *
                                                                  match_case
                                                               | ExIfe of
                                                                  loc *
                                                                  expr *
                                                                  expr * 
                                                                  expr
                                                               | ExInt of
                                                                  loc *
                                                                  string
                                                               | ExInt32 of
                                                                  loc *
                                                                  string
                                                               | ExInt64 of
                                                                  loc *
                                                                  string
                                                               | ExNativeInt
                                                                  of 
                                                                  loc *
                                                                  string
                                                               | ExLab of
                                                                  loc *
                                                                  string *
                                                                  expr
                                                               | ExLaz of
                                                                  loc * 
                                                                  expr
                                                               | ExLet of
                                                                  loc *
                                                                  rec_flag *
                                                                  binding *
                                                                  expr
                                                               | ExLmd of
                                                                  loc *
                                                                  string *
                                                                  module_expr *
                                                                  expr
                                                               | ExMat of
                                                                  loc *
                                                                  expr *
                                                                  match_case
                                                               | ExNew of
                                                                  loc * 
                                                                  ident
                                                               | ExObj of
                                                                  loc *
                                                                  patt *
                                                                  class_str_item
                                                               | ExOlb of
                                                                  loc *
                                                                  string *
                                                                  expr
                                                               | ExOvr of
                                                                  loc *
                                                                  rec_binding
                                                               | ExRec of
                                                                  loc *
                                                                  rec_binding *
                                                                  expr
                                                               | ExSeq of
                                                                  loc * 
                                                                  expr
                                                               | ExSnd of
                                                                  loc *
                                                                  expr *
                                                                  string
                                                               | ExSte of
                                                                  loc *
                                                                  expr * 
                                                                  expr
                                                               | ExStr of
                                                                  loc *
                                                                  string
                                                               | ExTry of
                                                                  loc *
                                                                  expr *
                                                                  match_case
                                                               | ExTup of
                                                                  loc * 
                                                                  expr
                                                               | ExCom of
                                                                  loc *
                                                                  expr * 
                                                                  expr
                                                               | ExTyc of
                                                                  loc *
                                                                  expr * 
                                                                  ctyp
                                                               | ExVrn of
                                                                  loc *
                                                                  string
                                                               | ExWhi of
                                                                  loc *
                                                                  expr * 
                                                                  expr
                                                               | ExOpI of
                                                                  loc *
                                                                  ident *
                                                                  expr
                                                               | ExFUN of
                                                                  loc *
                                                                  string *
                                                                  expr
                                                               | ExPkg of
                                                                  loc *
                                                                  module_expr
                                                             and module_type =
                                                                 MtNil of loc
                                                               | MtId of
                                                                  loc * 
                                                                  ident
                                                               | MtFun of
                                                                  loc *
                                                                  string *
                                                                  module_type *
                                                                  module_type
                                                               | MtQuo of
                                                                  loc *
                                                                  string
                                                               | MtSig of
                                                                  loc *
                                                                  sig_item
                                                               | MtWit of
                                                                  loc *
                                                                  module_type *
                                                                  with_constr
                                                               | MtOf of
                                                                  loc *
                                                                  module_expr
                                                               | MtAnt of
                                                                  loc *
                                                                  string
                                                             and sig_item =
                                                                 SgNil of loc
                                                               | SgCls of
                                                                  loc *
                                                                  class_type
                                                               | SgClt of
                                                                  loc *
                                                                  class_type
                                                               | SgSem of
                                                                  loc *
                                                                  sig_item *
                                                                  sig_item
                                                               | SgDir of
                                                                  loc *
                                                                  string *
                                                                  expr
                                                               | SgExc of
                                                                  loc * 
                                                                  ctyp
                                                               | SgExt of
                                                                  loc *
                                                                  string *
                                                                  ctyp *
                                                                  string meta_list
                                                               | SgInc of
                                                                  loc *
                                                                  module_type
                                                               | SgMod of
                                                                  loc *
                                                                  string *
                                                                  module_type
                                                               | SgRecMod of
                                                                  loc *
                                                                  module_binding
                                                               | SgMty of
                                                                  loc *
                                                                  string *
                                                                  module_type
                                                               | SgOpn of
                                                                  loc * 
                                                                  ident
                                                               | SgTyp of
                                                                  loc * 
                                                                  ctyp
                                                               | SgVal of
                                                                  loc *
                                                                  string *
                                                                  ctyp
                                                               | SgAnt of
                                                                  loc *
                                                                  string
                                                             and with_constr =
                                                                 WcNil of loc
                                                               | WcTyp of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | WcMod of
                                                                  loc *
                                                                  ident *
                                                                  ident
                                                               | WcTyS of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | WcMoS of
                                                                  loc *
                                                                  ident *
                                                                  ident
                                                               | WcAnd of
                                                                  loc *
                                                                  with_constr *
                                                                  with_constr
                                                               | WcAnt of
                                                                  loc *
                                                                  string
                                                             and binding =
                                                                 BiNil of loc
                                                               | BiAnd of
                                                                  loc *
                                                                  binding *
                                                                  binding
                                                               | BiEq of
                                                                  loc *
                                                                  patt * 
                                                                  expr
                                                               | BiAnt of
                                                                  loc *
                                                                  string
                                                             and rec_binding =
                                                                 RbNil of loc
                                                               | RbSem of
                                                                  loc *
                                                                  rec_binding *
                                                                  rec_binding
                                                               | RbEq of
                                                                  loc *
                                                                  ident *
                                                                  expr
                                                               | RbAnt of
                                                                  loc *
                                                                  string
                                                             and module_binding =
                                                                 MbNil of loc
                                                               | MbAnd of
                                                                  loc *
                                                                  module_binding *
                                                                  module_binding
                                                               | MbColEq of
                                                                  loc *
                                                                  string *
                                                                  module_type *
                                                                  module_expr
                                                               | MbCol of
                                                                  loc *
                                                                  string *
                                                                  module_type
                                                               | MbAnt of
                                                                  loc *
                                                                  string
                                                             and match_case =
                                                                 McNil of loc
                                                               | McOr of
                                                                  loc *
                                                                  match_case *
                                                                  match_case
                                                               | McArr of
                                                                  loc *
                                                                  patt *
                                                                  expr * 
                                                                  expr
                                                               | McAnt of
                                                                  loc *
                                                                  string
                                                             and module_expr =
                                                                 MeNil of loc
                                                               | MeId of
                                                                  loc * 
                                                                  ident
                                                               | MeApp of
                                                                  loc *
                                                                  module_expr *
                                                                  module_expr
                                                               | MeFun of
                                                                  loc *
                                                                  string *
                                                                  module_type *
                                                                  module_expr
                                                               | MeStr of
                                                                  loc *
                                                                  str_item
                                                               | MeTyc of
                                                                  loc *
                                                                  module_expr *
                                                                  module_type
                                                               | MePkg of
                                                                  loc * 
                                                                  expr
                                                               | MeAnt of
                                                                  loc *
                                                                  string
                                                             and str_item =
                                                                 StNil of loc
                                                               | StCls of
                                                                  loc *
                                                                  class_expr
                                                               | StClt of
                                                                  loc *
                                                                  class_type
                                                               | StSem of
                                                                  loc *
                                                                  str_item *
                                                                  str_item
                                                               | StDir of
                                                                  loc *
                                                                  string *
                                                                  expr
                                                               | StExc of
                                                                  loc *
                                                                  ctyp *
                                                                  ident meta_option
                                                               | StExp of
                                                                  loc * 
                                                                  expr
                                                               | StExt of
                                                                  loc *
                                                                  string *
                                                                  ctyp *
                                                                  string meta_list
                                                               | StInc of
                                                                  loc *
                                                                  module_expr
                                                               | StMod of
                                                                  loc *
                                                                  string *
                                                                  module_expr
                                                               | StRecMod of
                                                                  loc *
                                                                  module_binding
                                                               | StMty of
                                                                  loc *
                                                                  string *
                                                                  module_type
                                                               | StOpn of
                                                                  loc * 
                                                                  ident
                                                               | StTyp of
                                                                  loc * 
                                                                  ctyp
                                                               | StVal of
                                                                  loc *
                                                                  rec_flag *
                                                                  binding
                                                               | StAnt of
                                                                  loc *
                                                                  string
                                                             and class_type =
                                                                 CtNil of loc
                                                               | CtCon of
                                                                  loc *
                                                                  virtual_flag *
                                                                  ident *
                                                                  ctyp
                                                               | CtFun of
                                                                  loc *
                                                                  ctyp *
                                                                  class_type
                                                               | CtSig of
                                                                  loc *
                                                                  ctyp *
                                                                  class_sig_item
                                                               | CtAnd of
                                                                  loc *
                                                                  class_type *
                                                                  class_type
                                                               | CtCol of
                                                                  loc *
                                                                  class_type *
                                                                  class_type
                                                               | CtEq of
                                                                  loc *
                                                                  class_type *
                                                                  class_type
                                                               | CtAnt of
                                                                  loc *
                                                                  string
                                                             and class_sig_item =
                                                                 CgNil of loc
                                                               | CgCtr of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | CgSem of
                                                                  loc *
                                                                  class_sig_item *
                                                                  class_sig_item
                                                               | CgInh of
                                                                  loc *
                                                                  class_type
                                                               | CgMth of
                                                                  loc *
                                                                  string *
                                                                  private_flag *
                                                                  ctyp
                                                               | CgVal of
                                                                  loc *
                                                                  string *
                                                                  mutable_flag *
                                                                  virtual_flag *
                                                                  ctyp
                                                               | CgVir of
                                                                  loc *
                                                                  string *
                                                                  private_flag *
                                                                  ctyp
                                                               | CgAnt of
                                                                  loc *
                                                                  string
                                                             and class_expr =
                                                                 CeNil of loc
                                                               | CeApp of
                                                                  loc *
                                                                  class_expr *
                                                                  expr
                                                               | CeCon of
                                                                  loc *
                                                                  virtual_flag *
                                                                  ident *
                                                                  ctyp
                                                               | CeFun of
                                                                  loc *
                                                                  patt *
                                                                  class_expr
                                                               | CeLet of
                                                                  loc *
                                                                  rec_flag *
                                                                  binding *
                                                                  class_expr
                                                               | CeStr of
                                                                  loc *
                                                                  patt *
                                                                  class_str_item
                                                               | CeTyc of
                                                                  loc *
                                                                  class_expr *
                                                                  class_type
                                                               | CeAnd of
                                                                  loc *
                                                                  class_expr *
                                                                  class_expr
                                                               | CeEq of
                                                                  loc *
                                                                  class_expr *
                                                                  class_expr
                                                               | CeAnt of
                                                                  loc *
                                                                  string
                                                             and class_str_item =
                                                                 CrNil of loc
                                                               | CrSem of
                                                                  loc *
                                                                  class_str_item *
                                                                  class_str_item
                                                               | CrCtr of
                                                                  loc *
                                                                  ctyp * 
                                                                  ctyp
                                                               | CrInh of
                                                                  loc *
                                                                  override_flag *
                                                                  class_expr *
                                                                  string
                                                               | CrIni of
                                                                  loc * 
                                                                  expr
                                                               | CrMth of
                                                                  loc *
                                                                  string *
                                                                  override_flag *
                                                                  private_flag *
                                                                  expr * 
                                                                  ctyp
                                                               | CrVal of
                                                                  loc *
                                                                  string *
                                                                  override_flag *
                                                                  mutable_flag *
                                                                  expr
                                                               | CrVir of
                                                                  loc *
                                                                  string *
                                                                  private_flag *
                                                                  ctyp
                                                               | CrVvr of
                                                                  loc *
                                                                  string *
                                                                  mutable_flag *
                                                                  ctyp
                                                               | CrAnt of
                                                                  loc *
                                                                  string

                                                            end

type 
                                                                 ('a, 'loc) stream_filter =
                                                                  (('a *
                                                                    'loc) Stream.t
                                                                   ->
                                                                   ('a *
                                                                    'loc) Stream.t)


                                                          module type AstFilters =
                                                           sig
                                                            module Ast :
                                                             Camlp4Ast

                                                            type 'a filter =
                                                             ('a -> 'a)

                                                            val register_sig_item_filter :
                                                             (Ast.sig_item filter
                                                              -> unit)

                                                            val register_str_item_filter :
                                                             (Ast.str_item filter
                                                              -> unit)

                                                            val register_topphrase_filter :
                                                             (Ast.str_item filter
                                                              -> unit)

                                                            val fold_interf_filters :
                                                             (('a ->
                                                               (Ast.sig_item filter
                                                                -> 'a)) ->
                                                              ('a -> 'a))

                                                            val fold_implem_filters :
                                                             (('a ->
                                                               (Ast.str_item filter
                                                                -> 'a)) ->
                                                              ('a -> 'a))

                                                            val fold_topphrase_filters :
                                                             (('a ->
                                                               (Ast.str_item filter
                                                                -> 'a)) ->
                                                              ('a -> 'a))

                                                           end

module type DynAst =
                                                                 sig
                                                                  module
                                                                   Ast : Ast

                                                                  type 'a tag

                                                                  val ctyp_tag :
                                                                   Ast.ctyp tag

                                                                  val patt_tag :
                                                                   Ast.patt tag

                                                                  val expr_tag :
                                                                   Ast.expr tag

                                                                  val module_type_tag :
                                                                   Ast.module_type tag

                                                                  val sig_item_tag :
                                                                   Ast.sig_item tag

                                                                  val with_constr_tag :
                                                                   Ast.with_constr tag

                                                                  val module_expr_tag :
                                                                   Ast.module_expr tag

                                                                  val str_item_tag :
                                                                   Ast.str_item tag

                                                                  val class_type_tag :
                                                                   Ast.class_type tag

                                                                  val class_sig_item_tag :
                                                                   Ast.class_sig_item tag

                                                                  val class_expr_tag :
                                                                   Ast.class_expr tag

                                                                  val class_str_item_tag :
                                                                   Ast.class_str_item tag

                                                                  val match_case_tag :
                                                                   Ast.match_case tag

                                                                  val ident_tag :
                                                                   Ast.ident tag

                                                                  val binding_tag :
                                                                   Ast.binding tag

                                                                  val rec_binding_tag :
                                                                   Ast.rec_binding tag

                                                                  val module_binding_tag :
                                                                   Ast.module_binding tag

                                                                  val string_of_tag :
                                                                   ('a tag ->
                                                                    string)

                                                                  module
                                                                   Pack :
                                                                   functor
                                                                    (X : 
                                                                    sig
                                                                    type 'a t

                                                                    end) ->
                                                                    sig
                                                                    type pack

                                                                    val pack :
                                                                    ('a tag
                                                                    ->
                                                                    ('a X.t
                                                                    -> 
                                                                    pack))

                                                                    val unpack :
                                                                    ('a tag
                                                                    ->
                                                                    (pack ->
                                                                    'a X.t))

                                                                    val print_tag :
                                                                    (Format.formatter
                                                                    ->
                                                                    (pack ->
                                                                    unit))

                                                                    end

                                                                 end


                                                          module type Quotation =
                                                           sig
                                                            module Ast : Ast

                                                            module DynAst :
                                                             (DynAst with
                                                              module Ast =
                                                              Ast)

                                                            open Ast

                                                            type 'a expand_fun =
                                                             (loc ->
                                                              (string option
                                                               ->
                                                               (string -> 'a)))

                                                            val add :
                                                             (string ->
                                                              ('a DynAst.tag
                                                               ->
                                                               ('a expand_fun
                                                                -> unit)))

                                                            val find :
                                                             (string ->
                                                              ('a DynAst.tag
                                                               ->
                                                               'a expand_fun))

                                                            val default :
                                                             string ref

                                                            val default_tbl :
                                                             (string,
                                                              string) Hashtbl.t

                                                            val default_at_pos :
                                                             (string ->
                                                              (string ->
                                                               unit))

                                                            val parse_quotation_result :
                                                             ((loc ->
                                                               (string -> 'a))
                                                              ->
                                                              (loc ->
                                                               (FanSig.quotation
                                                                ->
                                                                (string ->
                                                                 (string ->
                                                                  'a)))))

                                                            val translate :
                                                             (string ->
                                                              string) ref

                                                            val expand :
                                                             (loc ->
                                                              (FanSig.quotation
                                                               ->
                                                               ('a DynAst.tag
                                                                -> 'a)))

                                                            val dump_file :
                                                             string option ref

                                                            module Error :
                                                             FanSig.Error

                                                           end

module type DynLoader =
                                                                 sig
                                                                  type t

                                                                  exception Error
                                                                   of
                                                                   string *
                                                                   string

                                                                  val mk :
                                                                   (?ocaml_stdlib :
                                                                    bool ->
                                                                    (?camlp4_stdlib :
                                                                    bool ->
                                                                    (unit ->
                                                                    t)))

                                                                  val fold_load_path :
                                                                   (t ->
                                                                    ((string
                                                                    ->
                                                                    ('a ->
                                                                    'a)) ->
                                                                    ('a ->
                                                                    'a)))

                                                                  val load :
                                                                   (t ->
                                                                    (string
                                                                    -> 
                                                                    unit))

                                                                  val include_dir :
                                                                   (t ->
                                                                    (string
                                                                    -> 
                                                                    unit))

                                                                  val find_in_path :
                                                                   (t ->
                                                                    (string
                                                                    ->
                                                                    string))

                                                                  val is_native :
                                                                   bool

                                                                 end


                                                          module Grammar =
                                                           struct
                                                            module type Action =
                                                             sig
                                                              type t

                                                              val mk :
                                                               ('a -> t)

                                                              val get :
                                                               (t -> 'a)

                                                              val getf :
                                                               (t ->
                                                                ('a -> 'b))

                                                              val getf2 :
                                                               (t ->
                                                                ('a ->
                                                                 ('b -> 'c)))

                                                             end

                                                            type assoc =
                                                               NonA
                                                             | RightA
                                                             | LeftA

                                                            type position =
                                                               First
                                                             | Last
                                                             | Before of
                                                                string
                                                             | After of
                                                                string
                                                             | Level of
                                                                string

                                                            module type Structure =
                                                             sig
                                                              module Loc :
                                                               FanSig.Loc

                                                              module Action :
                                                               Action

                                                              module Token :
                                                               (FanSig.Token
                                                                with
                                                                module Loc =
                                                                Loc)

                                                              type gram = 
                                                              {
                                                                gfilter:
                                                                 Token.Filter.t;
                                                                gkeywords:
                                                                 (string,
                                                                  int ref) Hashtbl.t;
                                                                glexer:
                                                                 (Loc.t ->
                                                                  (char Stream.t
                                                                   ->
                                                                   (Token.t *
                                                                    Loc.t) Stream.t));
                                                                warning_verbose:
                                                                 bool ref;
                                                                error_verbose:
                                                                 bool ref}

                                                              type internal_entry
                                                              
 type tree

                                                              type token_pattern =
                                                               ((Token.t ->
                                                                 bool) *
                                                                string)

                                                              type token_info

                                                              type token_stream =
                                                               (Token.t *
                                                                token_info) Stream.t

                                                              val token_location :
                                                               (token_info ->
                                                                Loc.t)

                                                              type symbol =
                                                                 Smeta of
                                                                  string *
                                                                  symbol list *
                                                                  Action.t
                                                               | Snterm of
                                                                  internal_entry
                                                               | Snterml of
                                                                  internal_entry *
                                                                  string
                                                               | Slist0 of
                                                                  symbol
                                                               | Slist0sep of
                                                                  symbol *
                                                                  symbol
                                                               | Slist1 of
                                                                  symbol
                                                               | Slist1sep of
                                                                  symbol *
                                                                  symbol
                                                               | Sopt of
                                                                  symbol
                                                               | Stry of
                                                                  symbol
                                                               | Sself
                                                               | Snext
                                                               | Stoken of
                                                                  token_pattern
                                                               | Skeyword of
                                                                  string
                                                               | Stree of
                                                                  tree

                                                              type production_rule =
                                                               (symbol list *
                                                                Action.t)

                                                              type single_extend_statment =
                                                               (string option *
                                                                assoc option *
                                                                production_rule list)

                                                              type extend_statment =
                                                               (position option *
                                                                single_extend_statment list)

                                                              type delete_statment =
                                                               symbol list

                                                              type ('a, 'b,
                                                                    'c) fold =
                                                               (internal_entry
                                                                ->
                                                                (symbol list
                                                                 ->
                                                                 (('a Stream.t
                                                                   -> 'b) ->
                                                                  ('a Stream.t
                                                                   -> 'c))))

                                                              type ('a, 'b,
                                                                    'c) foldsep =
                                                               (internal_entry
                                                                ->
                                                                (symbol list
                                                                 ->
                                                                 (('a Stream.t
                                                                   -> 'b) ->
                                                                  (('a Stream.t
                                                                    -> 
                                                                    unit) ->
                                                                   ('a Stream.t
                                                                    -> 'c)))))

                                                             end

                                                            module type Dynamic =
                                                             sig
                                                              include
                                                               Structure

                                                              val mk :
                                                               (unit -> gram)

                                                              module Entry :
                                                               sig
                                                                type 'a t

                                                                val mk :
                                                                 (gram ->
                                                                  (string ->
                                                                   'a t))

                                                                val of_parser :
                                                                 (gram ->
                                                                  (string ->
                                                                   ((token_stream
                                                                    -> 'a) ->
                                                                    'a t)))

                                                                val setup_parser :
                                                                 ('a t ->
                                                                  ((token_stream
                                                                    -> 'a) ->
                                                                   unit))

                                                                val name :
                                                                 ('a t ->
                                                                  string)

                                                                val print :
                                                                 (Format.formatter
                                                                  ->
                                                                  ('a t ->
                                                                   unit))

                                                                val dump :
                                                                 (Format.formatter
                                                                  ->
                                                                  ('a t ->
                                                                   unit))

                                                                val obj :
                                                                 ('a t ->
                                                                  internal_entry)

                                                                val clear :
                                                                 ('a t ->
                                                                  unit)

                                                               end

                                                              val get_filter :
                                                               (gram ->
                                                                Token.Filter.t)

                                                              type 'a not_filtered
                                                              

                                                              val extend :
                                                               ('a Entry.t ->
                                                                (extend_statment
                                                                 -> unit))

                                                              val delete_rule :
                                                               ('a Entry.t ->
                                                                (delete_statment
                                                                 -> unit))

                                                              val srules :
                                                               ('a Entry.t ->
                                                                ((symbol list *
                                                                  Action.t) list
                                                                 -> symbol))

                                                              val sfold0 :
                                                               (('a ->
                                                                 ('b -> 'b))
                                                                ->
                                                                ('b ->
                                                                 (_, 'a,
                                                                  'b) fold))

                                                              val sfold1 :
                                                               (('a ->
                                                                 ('b -> 'b))
                                                                ->
                                                                ('b ->
                                                                 (_, 'a,
                                                                  'b) fold))

                                                              val sfold0sep :
                                                               (('a ->
                                                                 ('b -> 'b))
                                                                ->
                                                                ('b ->
                                                                 (_, 'a,
                                                                  'b) foldsep))

                                                              val lex :
                                                               (gram ->
                                                                (Loc.t ->
                                                                 (char Stream.t
                                                                  ->
                                                                  (Token.t *
                                                                   Loc.t) Stream.t not_filtered)))

                                                              val lex_string :
                                                               (gram ->
                                                                (Loc.t ->
                                                                 (string ->
                                                                  (Token.t *
                                                                   Loc.t) Stream.t not_filtered)))

                                                              val filter :
                                                               (gram ->
                                                                ((Token.t *
                                                                  Loc.t) Stream.t not_filtered
                                                                 ->
                                                                 token_stream))

                                                              val parse :
                                                               ('a Entry.t ->
                                                                (Loc.t ->
                                                                 (char Stream.t
                                                                  -> 'a)))

                                                              val parse_string :
                                                               ('a Entry.t ->
                                                                (Loc.t ->
                                                                 (string ->
                                                                  'a)))

                                                              val parse_tokens_before_filter :
                                                               ('a Entry.t ->
                                                                ((Token.t *
                                                                  Loc.t) Stream.t not_filtered
                                                                 -> 'a))

                                                              val parse_tokens_after_filter :
                                                               ('a Entry.t ->
                                                                (token_stream
                                                                 -> 'a))

                                                             end

                                                            module type Static =
                                                             sig
                                                              include
                                                               Structure

                                                              val trace_parser :
                                                               bool ref

                                                              val gram : gram

                                                              module Entry :
                                                               sig
                                                                type 'a t

                                                                val mk :
                                                                 (string ->
                                                                  'a t)

                                                                val of_parser :
                                                                 (string ->
                                                                  ((token_stream
                                                                    -> 'a) ->
                                                                   'a t))

                                                                val setup_parser :
                                                                 ('a t ->
                                                                  ((token_stream
                                                                    -> 'a) ->
                                                                   unit))

                                                                val name :
                                                                 ('a t ->
                                                                  string)

                                                                val print :
                                                                 (Format.formatter
                                                                  ->
                                                                  ('a t ->
                                                                   unit))

                                                                val dump :
                                                                 (Format.formatter
                                                                  ->
                                                                  ('a t ->
                                                                   unit))

                                                                val obj :
                                                                 ('a t ->
                                                                  internal_entry)

                                                                val clear :
                                                                 ('a t ->
                                                                  unit)

                                                               end

                                                              val get_filter :
                                                               (unit ->
                                                                Token.Filter.t)

                                                              type 'a not_filtered
                                                              

                                                              val extend :
                                                               ('a Entry.t ->
                                                                (extend_statment
                                                                 -> unit))

                                                              val delete_rule :
                                                               ('a Entry.t ->
                                                                (delete_statment
                                                                 -> unit))

                                                              val srules :
                                                               ('a Entry.t ->
                                                                ((symbol list *
                                                                  Action.t) list
                                                                 -> symbol))

                                                              val sfold0 :
                                                               (('a ->
                                                                 ('b -> 'b))
                                                                ->
                                                                ('b ->
                                                                 (_, 'a,
                                                                  'b) fold))

                                                              val sfold1 :
                                                               (('a ->
                                                                 ('b -> 'b))
                                                                ->
                                                                ('b ->
                                                                 (_, 'a,
                                                                  'b) fold))

                                                              val sfold0sep :
                                                               (('a ->
                                                                 ('b -> 'b))
                                                                ->
                                                                ('b ->
                                                                 (_, 'a,
                                                                  'b) foldsep))

                                                              val lex :
                                                               (Loc.t ->
                                                                (char Stream.t
                                                                 ->
                                                                 (Token.t *
                                                                  Loc.t) Stream.t not_filtered))

                                                              val lex_string :
                                                               (Loc.t ->
                                                                (string ->
                                                                 (Token.t *
                                                                  Loc.t) Stream.t not_filtered))

                                                              val filter :
                                                               ((Token.t *
                                                                 Loc.t) Stream.t not_filtered
                                                                ->
                                                                token_stream)

                                                              val parse :
                                                               ('a Entry.t ->
                                                                (Loc.t ->
                                                                 (char Stream.t
                                                                  -> 'a)))

                                                              val parse_string :
                                                               ('a Entry.t ->
                                                                (Loc.t ->
                                                                 (string ->
                                                                  'a)))

                                                              val parse_tokens_before_filter :
                                                               ('a Entry.t ->
                                                                ((Token.t *
                                                                  Loc.t) Stream.t not_filtered
                                                                 -> 'a))

                                                              val parse_tokens_after_filter :
                                                               ('a Entry.t ->
                                                                (token_stream
                                                                 -> 'a))

                                                             end

                                                           end

module type Lexer =
                                                                 sig
                                                                  module
                                                                   Loc :
                                                                   FanSig.Loc

                                                                  module
                                                                   Token :
                                                                   (FanSig.Token
                                                                    with
                                                                    module Loc =
                                                                    Loc)

                                                                  module
                                                                   Error :
                                                                   FanSig.Error

                                                                  val mk :
                                                                   (unit ->
                                                                    (Loc.t ->
                                                                    (char Stream.t
                                                                    ->
                                                                    (Token.t *
                                                                    Loc.t) Stream.t)))

                                                                 end


                                                          module Parser =
                                                           functor (Ast : Ast) ->
                                                            struct
                                                             module type SIMPLE =
                                                              sig
                                                               val parse_expr :
                                                                (Ast.loc ->
                                                                 (string ->
                                                                  Ast.expr))

                                                               val parse_patt :
                                                                (Ast.loc ->
                                                                 (string ->
                                                                  Ast.patt))

                                                              end

                                                             module type S =
                                                              sig
                                                               val parse_implem :
                                                                (?directive_handler :
                                                                 (Ast.str_item
                                                                  ->
                                                                  Ast.str_item option)
                                                                 ->
                                                                 (Ast.loc ->
                                                                  (char Stream.t
                                                                   ->
                                                                   Ast.str_item)))

                                                               val parse_interf :
                                                                (?directive_handler :
                                                                 (Ast.sig_item
                                                                  ->
                                                                  Ast.sig_item option)
                                                                 ->
                                                                 (Ast.loc ->
                                                                  (char Stream.t
                                                                   ->
                                                                   Ast.sig_item)))

                                                              end

                                                            end

module Printer =
                                                                  functor (Ast : Ast) ->
                                                                   struct
                                                                    module type S =
                                                                    sig
                                                                    val print_interf :
                                                                    (?input_file :
                                                                    string ->
                                                                    (?output_file :
                                                                    string ->
                                                                    (Ast.sig_item
                                                                    -> 
                                                                    unit)))

                                                                    val print_implem :
                                                                    (?input_file :
                                                                    string ->
                                                                    (?output_file :
                                                                    string ->
                                                                    (Ast.str_item
                                                                    -> 
                                                                    unit)))

                                                                    end

                                                                   end


                                                          module type Syntax =
                                                           sig
                                                            module Loc :
                                                             FanSig.Loc

                                                            module Ast :
                                                             (Ast with type
                                                               loc = 
                                                              Loc.t)

                                                            module Token :
                                                             (FanSig.Token
                                                              with
                                                              module Loc =
                                                              Loc)

                                                            module Gram :
                                                             (Grammar.Static
                                                              with
                                                              module Loc =
                                                              Loc
                                                              and module Loc =
                                                              Loc
                                                              and module Token =
                                                              Token)

                                                            module
                                                             Quotation :
                                                             (Quotation with
                                                              module Ast =
                                                              Ast)

                                                            module
                                                             AntiquotSyntax :
                                                             Parser(Ast).SIMPLE

                                                            include
                                                             Warning(Loc).S

                                                            include
                                                             Parser(Ast).S

                                                            include
                                                             Printer(Ast).S

                                                           end

module type Camlp4Syntax =
                                                                 sig
                                                                  module
                                                                   Loc :
                                                                   FanSig.Loc

                                                                  module
                                                                   Ast :
                                                                   (Camlp4Ast
                                                                    with
                                                                    module Loc =
                                                                    Loc)

                                                                  module
                                                                   Token :
                                                                   (FanSig.Camlp4Token
                                                                    with
                                                                    module Loc =
                                                                    Loc)

                                                                  module
                                                                   Gram :
                                                                   (Grammar.Static
                                                                    with
                                                                    module Loc =
                                                                    Loc
                                                                    and module Loc =
                                                                    Loc
                                                                    and module Token =
                                                                    Token)

                                                                  module
                                                                   Quotation :
                                                                   (Quotation
                                                                    with
                                                                    module Ast =
                                                                    Camlp4AstToAst(Ast))

                                                                  module
                                                                   AntiquotSyntax :
                                                                   Parser(Ast).SIMPLE

                                                                  include
                                                                   Warning(Loc).S

                                                                  include
                                                                   Parser(Ast).S

                                                                  include
                                                                   Printer(Ast).S

                                                                  val interf :
                                                                   (Ast.sig_item list *
                                                                    Loc.t option) Gram.Entry.t

                                                                  val implem :
                                                                   (Ast.str_item list *
                                                                    Loc.t option) Gram.Entry.t

                                                                  val top_phrase :
                                                                   Ast.str_item option Gram.Entry.t

                                                                  val use_file :
                                                                   (Ast.str_item list *
                                                                    Loc.t option) Gram.Entry.t

                                                                  val a_CHAR :
                                                                   string Gram.Entry.t

                                                                  val a_FLOAT :
                                                                   string Gram.Entry.t

                                                                  val a_INT :
                                                                   string Gram.Entry.t

                                                                  val a_INT32 :
                                                                   string Gram.Entry.t

                                                                  val a_INT64 :
                                                                   string Gram.Entry.t

                                                                  val a_LABEL :
                                                                   string Gram.Entry.t

                                                                  val a_LIDENT :
                                                                   string Gram.Entry.t

                                                                  val a_NATIVEINT :
                                                                   string Gram.Entry.t

                                                                  val a_OPTLABEL :
                                                                   string Gram.Entry.t

                                                                  val a_STRING :
                                                                   string Gram.Entry.t

                                                                  val a_UIDENT :
                                                                   string Gram.Entry.t

                                                                  val a_ident :
                                                                   string Gram.Entry.t

                                                                  val amp_ctyp :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val and_ctyp :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val match_case :
                                                                   Ast.match_case Gram.Entry.t

                                                                  val match_case0 :
                                                                   Ast.match_case Gram.Entry.t

                                                                  val match_case_quot :
                                                                   Ast.match_case Gram.Entry.t

                                                                  val binding :
                                                                   Ast.binding Gram.Entry.t

                                                                  val binding_quot :
                                                                   Ast.binding Gram.Entry.t

                                                                  val rec_binding_quot :
                                                                   Ast.rec_binding Gram.Entry.t

                                                                  val class_declaration :
                                                                   Ast.class_expr Gram.Entry.t

                                                                  val class_description :
                                                                   Ast.class_type Gram.Entry.t

                                                                  val class_expr :
                                                                   Ast.class_expr Gram.Entry.t

                                                                  val class_expr_quot :
                                                                   Ast.class_expr Gram.Entry.t

                                                                  val class_fun_binding :
                                                                   Ast.class_expr Gram.Entry.t

                                                                  val class_fun_def :
                                                                   Ast.class_expr Gram.Entry.t

                                                                  val class_info_for_class_expr :
                                                                   Ast.class_expr Gram.Entry.t

                                                                  val class_info_for_class_type :
                                                                   Ast.class_type Gram.Entry.t

                                                                  val class_longident :
                                                                   Ast.ident Gram.Entry.t

                                                                  val class_longident_and_param :
                                                                   Ast.class_expr Gram.Entry.t

                                                                  val class_name_and_param :
                                                                   (string *
                                                                    Ast.ctyp) Gram.Entry.t

                                                                  val class_sig_item :
                                                                   Ast.class_sig_item Gram.Entry.t

                                                                  val class_sig_item_quot :
                                                                   Ast.class_sig_item Gram.Entry.t

                                                                  val class_signature :
                                                                   Ast.class_sig_item Gram.Entry.t

                                                                  val class_str_item :
                                                                   Ast.class_str_item Gram.Entry.t

                                                                  val class_str_item_quot :
                                                                   Ast.class_str_item Gram.Entry.t

                                                                  val class_structure :
                                                                   Ast.class_str_item Gram.Entry.t

                                                                  val class_type :
                                                                   Ast.class_type Gram.Entry.t

                                                                  val class_type_declaration :
                                                                   Ast.class_type Gram.Entry.t

                                                                  val class_type_longident :
                                                                   Ast.ident Gram.Entry.t

                                                                  val class_type_longident_and_param :
                                                                   Ast.class_type Gram.Entry.t

                                                                  val class_type_plus :
                                                                   Ast.class_type Gram.Entry.t

                                                                  val class_type_quot :
                                                                   Ast.class_type Gram.Entry.t

                                                                  val comma_ctyp :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val comma_expr :
                                                                   Ast.expr Gram.Entry.t

                                                                  val comma_ipatt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val comma_patt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val comma_type_parameter :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val constrain :
                                                                   (Ast.ctyp *
                                                                    Ast.ctyp) Gram.Entry.t

                                                                  val constructor_arg_list :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val constructor_declaration :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val constructor_declarations :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val ctyp :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val ctyp_quot :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val cvalue_binding :
                                                                   Ast.expr Gram.Entry.t

                                                                  val direction_flag :
                                                                   Ast.direction_flag Gram.Entry.t

                                                                  val direction_flag_quot :
                                                                   Ast.direction_flag Gram.Entry.t

                                                                  val dummy :
                                                                   unit Gram.Entry.t

                                                                  val eq_expr :
                                                                   (string ->
                                                                    (Ast.patt
                                                                    ->
                                                                    Ast.patt)) Gram.Entry.t

                                                                  val expr :
                                                                   Ast.expr Gram.Entry.t

                                                                  val expr_eoi :
                                                                   Ast.expr Gram.Entry.t

                                                                  val expr_quot :
                                                                   Ast.expr Gram.Entry.t

                                                                  val field_expr :
                                                                   Ast.rec_binding Gram.Entry.t

                                                                  val field_expr_list :
                                                                   Ast.rec_binding Gram.Entry.t

                                                                  val fun_binding :
                                                                   Ast.expr Gram.Entry.t

                                                                  val fun_def :
                                                                   Ast.expr Gram.Entry.t

                                                                  val ident :
                                                                   Ast.ident Gram.Entry.t

                                                                  val ident_quot :
                                                                   Ast.ident Gram.Entry.t

                                                                  val ipatt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val ipatt_tcon :
                                                                   Ast.patt Gram.Entry.t

                                                                  val label :
                                                                   string Gram.Entry.t

                                                                  val label_declaration :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val label_declaration_list :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val label_expr :
                                                                   Ast.rec_binding Gram.Entry.t

                                                                  val label_expr_list :
                                                                   Ast.rec_binding Gram.Entry.t

                                                                  val label_ipatt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val label_ipatt_list :
                                                                   Ast.patt Gram.Entry.t

                                                                  val label_longident :
                                                                   Ast.ident Gram.Entry.t

                                                                  val label_patt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val label_patt_list :
                                                                   Ast.patt Gram.Entry.t

                                                                  val labeled_ipatt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val let_binding :
                                                                   Ast.binding Gram.Entry.t

                                                                  val meth_list :
                                                                   (Ast.ctyp *
                                                                    Ast.row_var_flag) Gram.Entry.t

                                                                  val meth_decl :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val module_binding :
                                                                   Ast.module_binding Gram.Entry.t

                                                                  val module_binding0 :
                                                                   Ast.module_expr Gram.Entry.t

                                                                  val module_binding_quot :
                                                                   Ast.module_binding Gram.Entry.t

                                                                  val module_declaration :
                                                                   Ast.module_type Gram.Entry.t

                                                                  val module_expr :
                                                                   Ast.module_expr Gram.Entry.t

                                                                  val module_expr_quot :
                                                                   Ast.module_expr Gram.Entry.t

                                                                  val module_longident :
                                                                   Ast.ident Gram.Entry.t

                                                                  val module_longident_with_app :
                                                                   Ast.ident Gram.Entry.t

                                                                  val module_rec_declaration :
                                                                   Ast.module_binding Gram.Entry.t

                                                                  val module_type :
                                                                   Ast.module_type Gram.Entry.t

                                                                  val package_type :
                                                                   Ast.module_type Gram.Entry.t

                                                                  val module_type_quot :
                                                                   Ast.module_type Gram.Entry.t

                                                                  val more_ctyp :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val name_tags :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val opt_as_lident :
                                                                   string Gram.Entry.t

                                                                  val opt_class_self_patt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val opt_class_self_type :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val opt_comma_ctyp :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val opt_dot_dot :
                                                                   Ast.row_var_flag Gram.Entry.t

                                                                  val row_var_flag_quot :
                                                                   Ast.row_var_flag Gram.Entry.t

                                                                  val opt_eq_ctyp :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val opt_expr :
                                                                   Ast.expr Gram.Entry.t

                                                                  val opt_meth_list :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val opt_mutable :
                                                                   Ast.mutable_flag Gram.Entry.t

                                                                  val mutable_flag_quot :
                                                                   Ast.mutable_flag Gram.Entry.t

                                                                  val opt_override :
                                                                   Ast.override_flag Gram.Entry.t

                                                                  val override_flag_quot :
                                                                   Ast.override_flag Gram.Entry.t

                                                                  val opt_polyt :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val opt_private :
                                                                   Ast.private_flag Gram.Entry.t

                                                                  val private_flag_quot :
                                                                   Ast.private_flag Gram.Entry.t

                                                                  val opt_rec :
                                                                   Ast.rec_flag Gram.Entry.t

                                                                  val rec_flag_quot :
                                                                   Ast.rec_flag Gram.Entry.t

                                                                  val opt_virtual :
                                                                   Ast.virtual_flag Gram.Entry.t

                                                                  val virtual_flag_quot :
                                                                   Ast.virtual_flag Gram.Entry.t

                                                                  val opt_when_expr :
                                                                   Ast.expr Gram.Entry.t

                                                                  val patt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val patt_as_patt_opt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val patt_eoi :
                                                                   Ast.patt Gram.Entry.t

                                                                  val patt_quot :
                                                                   Ast.patt Gram.Entry.t

                                                                  val patt_tcon :
                                                                   Ast.patt Gram.Entry.t

                                                                  val phrase :
                                                                   Ast.str_item Gram.Entry.t

                                                                  val poly_type :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val row_field :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val sem_expr :
                                                                   Ast.expr Gram.Entry.t

                                                                  val sem_expr_for_list :
                                                                   (Ast.expr
                                                                    ->
                                                                    Ast.expr) Gram.Entry.t

                                                                  val sem_patt :
                                                                   Ast.patt Gram.Entry.t

                                                                  val sem_patt_for_list :
                                                                   (Ast.patt
                                                                    ->
                                                                    Ast.patt) Gram.Entry.t

                                                                  val semi :
                                                                   unit Gram.Entry.t

                                                                  val sequence :
                                                                   Ast.expr Gram.Entry.t

                                                                  val do_sequence :
                                                                   Ast.expr Gram.Entry.t

                                                                  val sig_item :
                                                                   Ast.sig_item Gram.Entry.t

                                                                  val sig_item_quot :
                                                                   Ast.sig_item Gram.Entry.t

                                                                  val sig_items :
                                                                   Ast.sig_item Gram.Entry.t

                                                                  val star_ctyp :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val str_item :
                                                                   Ast.str_item Gram.Entry.t

                                                                  val str_item_quot :
                                                                   Ast.str_item Gram.Entry.t

                                                                  val str_items :
                                                                   Ast.str_item Gram.Entry.t

                                                                  val type_constraint :
                                                                   unit Gram.Entry.t

                                                                  val type_declaration :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val type_ident_and_parameters :
                                                                   (string *
                                                                    Ast.ctyp list) Gram.Entry.t

                                                                  val type_kind :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val type_longident :
                                                                   Ast.ident Gram.Entry.t

                                                                  val type_longident_and_parameters :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val type_parameter :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val type_parameters :
                                                                   (Ast.ctyp
                                                                    ->
                                                                    Ast.ctyp) Gram.Entry.t

                                                                  val typevars :
                                                                   Ast.ctyp Gram.Entry.t

                                                                  val val_longident :
                                                                   Ast.ident Gram.Entry.t

                                                                  val value_let :
                                                                   unit Gram.Entry.t

                                                                  val value_val :
                                                                   unit Gram.Entry.t

                                                                  val with_constr :
                                                                   Ast.with_constr Gram.Entry.t

                                                                  val with_constr_quot :
                                                                   Ast.with_constr Gram.Entry.t

                                                                  val prefixop :
                                                                   Ast.expr Gram.Entry.t

                                                                  val infixop0 :
                                                                   Ast.expr Gram.Entry.t

                                                                  val infixop1 :
                                                                   Ast.expr Gram.Entry.t

                                                                  val infixop2 :
                                                                   Ast.expr Gram.Entry.t

                                                                  val infixop3 :
                                                                   Ast.expr Gram.Entry.t

                                                                  val infixop4 :
                                                                   Ast.expr Gram.Entry.t

                                                                 end


                                                          module type SyntaxExtension =
                                                           functor
                                                            (Syn : Syntax) ->
                                                            (Syntax with
                                                             module Loc =
                                                             Syn.Loc
                                                             and module Loc =
                                                             Syn.Loc
                                                             and module Ast =
                                                             Syn.Ast
                                                             and module Ast =
                                                             Syn.Ast
                                                             and module Token =
                                                             Syn.Token
                                                             and module Token =
                                                             Syn.Token
                                                             and module Gram =
                                                             Syn.Gram
                                                             and module Gram =
                                                             Syn.Gram
                                                             and module Quotation =
                                                             Syn.Quotation)


                                                          module type PLUGIN =
                                                           functor
                                                            (Unit : sig end) ->
                                                            sig end


                                                          module type OCAML_SYNTAX_EXTENSION =
                                                           functor
                                                            (Syn : Camlp4Syntax) ->
                                                            Camlp4Syntax


                                                          module type SYNTAX_PLUGIN =
                                                           functor
                                                            (Syn : Syntax) ->
                                                            sig end


                                                          module type PRINTER_PLUGIN =
                                                           functor
                                                            (Syn : Syntax) ->
                                                            Printer(Syn.Ast).S


                                                          module type OCAML_PRINTER_PLUGIN =
                                                           functor
                                                            (Syn : Camlp4Syntax) ->
                                                            Printer(Syn.Ast).S


                                                          module type PARSER =
                                                           functor
                                                            (Ast : Camlp4Ast) ->
                                                            Parser(Ast).S


                                                          module type OCAML_PARSER =
                                                           functor
                                                            (Ast : Camlp4Ast) ->
                                                            Parser(Ast).S


                                                          module type ASTFILTER_PLUGIN =
                                                           functor
                                                            (F : AstFilters) ->
                                                            sig end


                                                          module type LEXER =
                                                           functor
                                                            (Token : FanSig.Camlp4Token) ->
                                                            (Lexer with
                                                             module Loc =
                                                             Token.Loc
                                                             and module Loc =
                                                             Token.Loc
                                                             and module Token =
                                                             Token)


                                                          module type PRECAST =
                                                           sig
                                                            type token =
                                                             FanSig.camlp4_token

                                                            module Loc :
                                                             FanSig.Loc

                                                            module Ast :
                                                             (Camlp4Ast with
                                                              module Loc =
                                                              Loc)

                                                            module Token :
                                                             (FanSig.Token
                                                              with
                                                              module Loc =
                                                              Loc
                                                              and module Loc =
                                                              Loc and type
                                                               t =
                                                              FanSig.camlp4_token)

                                                            module Lexer :
                                                             (Lexer with
                                                              module Loc =
                                                              Loc
                                                              and module Loc =
                                                              Loc
                                                              and module Token =
                                                              Token)

                                                            module Gram :
                                                             (Grammar.Static
                                                              with
                                                              module Loc =
                                                              Loc
                                                              and module Loc =
                                                              Loc
                                                              and module Token =
                                                              Token)

                                                            module
                                                             Quotation :
                                                             (Quotation with
                                                              module Ast =
                                                              Camlp4AstToAst(Ast))

                                                            module
                                                             DynLoader :
                                                             DynLoader

                                                            module
                                                             AstFilters :
                                                             (AstFilters with
                                                              module Ast =
                                                              Ast)

                                                            module Syntax :
                                                             (Camlp4Syntax
                                                              with
                                                              module Loc =
                                                              Loc
                                                              and module Loc =
                                                              Loc
                                                              and module Token =
                                                              Token
                                                              and module Token =
                                                              Token
                                                              and module Ast =
                                                              Ast
                                                              and module Ast =
                                                              Ast
                                                              and module Gram =
                                                              Gram
                                                              and module Gram =
                                                              Gram
                                                              and module Quotation =
                                                              Quotation)

                                                            module Printers :
                                                             sig
                                                              module OCaml :
                                                               Printer(Ast).S

                                                              module
                                                               DumpOCamlAst :
                                                               Printer(Ast).S

                                                              module
                                                               DumpCamlp4Ast :
                                                               Printer(Ast).S

                                                              module Null :
                                                               Printer(Ast).S

                                                             end

                                                            module MakeGram :
                                                             functor
                                                              (Lexer : 
                                                              (Lexer with
                                                               module Loc =
                                                               Loc)) ->
                                                              (Grammar.Static
                                                               with
                                                               module Loc =
                                                               Loc
                                                               and module Loc =
                                                               Loc
                                                               and module Token =
                                                               Lexer.Token)

                                                            module
                                                             MakeSyntax :
                                                             functor
                                                              (U : sig end) ->
                                                              Syntax

                                                            type 'a parser_fun =
                                                             (?directive_handler :
                                                              ('a ->
                                                               'a option) ->
                                                              (Loc.t ->
                                                               (char Stream.t
                                                                -> 'a)))

                                                            type 'a printer_fun =
                                                             (?input_file :
                                                              string ->
                                                              (?output_file :
                                                               string ->
                                                               ('a -> unit)))

                                                            val loaded_modules :
                                                             string list ref

                                                            val iter_and_take_callbacks :
                                                             (((string *
                                                                (unit ->
                                                                 unit)) ->
                                                               unit) -> 
                                                              unit)

                                                            val register_str_item_parser :
                                                             (Ast.str_item parser_fun
                                                              -> unit)

                                                            val register_sig_item_parser :
                                                             (Ast.sig_item parser_fun
                                                              -> unit)

                                                            val register_parser :
                                                             (Ast.str_item parser_fun
                                                              ->
                                                              (Ast.sig_item parser_fun
                                                               -> unit))

                                                            val current_parser :
                                                             (unit ->
                                                              (Ast.str_item parser_fun *
                                                               Ast.sig_item parser_fun))

                                                            val plugin :
                                                             ((module Id 
                                                              ) ->
                                                              ((module PLUGIN
                                                               ) -> unit))

                                                            val syntax_plugin :
                                                             ((module Id 
                                                              ) ->
                                                              ((module SYNTAX_PLUGIN
                                                               ) -> unit))

                                                            val syntax_extension :
                                                             ((module Id 
                                                              ) ->
                                                              ((module SyntaxExtension
                                                               ) -> unit))

                                                            val ocaml_syntax_extension :
                                                             ((module Id 
                                                              ) ->
                                                              ((module OCAML_SYNTAX_EXTENSION
                                                               ) -> unit))

                                                            val parser_plugin :
                                                             ((module Id 
                                                              ) ->
                                                              ((module PARSER
                                                               ) -> unit))

                                                            val ocaml_parser_plugin :
                                                             ((module Id 
                                                              ) ->
                                                              ((module OCAML_PARSER
                                                               ) -> unit))

                                                            val ocaml_precast_parser_plugin :
                                                             ((module Id 
                                                              ) ->
                                                              ((module Parser(Syntax.Ast).S
                                                               ) -> unit))

                                                            val register_str_item_printer :
                                                             (Ast.str_item printer_fun
                                                              -> unit)

                                                            val register_sig_item_printer :
                                                             (Ast.sig_item printer_fun
                                                              -> unit)

                                                            val register_printer :
                                                             (Ast.str_item printer_fun
                                                              ->
                                                              (Ast.sig_item printer_fun
                                                               -> unit))

                                                            val current_printer :
                                                             (unit ->
                                                              (Ast.str_item printer_fun *
                                                               Ast.sig_item printer_fun))

                                                            val printer :
                                                             ((module Id 
                                                              ) ->
                                                              ((module PRINTER_PLUGIN
                                                               ) -> unit))

                                                            val ocaml_printer :
                                                             ((module Id 
                                                              ) ->
                                                              ((module OCAML_PRINTER_PLUGIN
                                                               ) -> unit))

                                                            val ocaml_precast_printer :
                                                             ((module Id 
                                                              ) ->
                                                              ((module Printer(Syntax.Ast).S
                                                               ) -> unit))

                                                            val ast_filter :
                                                             ((module Id 
                                                              ) ->
                                                              ((module ASTFILTER_PLUGIN
                                                               ) -> unit))

                                                            val declare_dyn_module :
                                                             (string ->
                                                              ((unit -> unit)
                                                               -> unit))

                                                            module
                                                             CurrentParser :
                                                             Parser(Ast).S

                                                            module
                                                             CurrentPrinter :
                                                             Printer(Ast).S

                                                            val enable_ocaml_printer :
                                                             (unit -> unit)

                                                            val enable_null_printer :
                                                             (unit -> unit)

                                                            val enable_dump_ocaml_ast_printer :
                                                             (unit -> unit)

                                                            val enable_dump_camlp4_ast_printer :
                                                             (unit -> unit)

                                                            val enable_auto :
                                                             ((unit -> bool)
                                                              -> unit)

                                                           end

module type PRECAST_PLUGIN =
                                                                 sig
                                                                  val apply :
                                                                   ((module PRECAST
                                                                    ) ->
                                                                    unit)

                                                                 end
