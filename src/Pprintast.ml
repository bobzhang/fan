(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Original Code from Ber-metaocaml, modified fo 3.12.0 and fixed *)

(* Printing code expressions *)
(* Authors:  Ed Pizzi, Fabrice Le Fessant *)
(* Refactoring: Hongbo Zhang: University of Pennsylvania*)

open Asttypes
open Format
open Location
open Longident
open Parsetree


let indent    = 1 ;; (* standard indentation increment *)
let bar_on_first_case = true ;;

(* These sets of symbols are taken from the manual. However, it's
   unclear what the sets infix_symbols and prefix_symbols are for, as
   operator_chars, which contains their union seems to be the only set
   useful to determine whether an identifier is prefix or infix.
   The set postfix_chars I added, which is the set of characters allowed
   at the end of an identifier to allow for internal MetaOCaml variable
   renaming. *)

let prefix_symbols  = [ '!'; '?'; '~' ] ;;
let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-';
                       '*'; '/'; '$'; '%' ] 
let operator_chars = [ '!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/';
                       ':'; '<'; '='; '>'; '?'; '@'; '^'; '|'; '~' ] 
let numeric_chars  = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] 

type fixity =
  | Infix
  | Prefix 

let is_infix fx =
  match fx with
  | Infix  -> true
  | Prefix -> false 

let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!=" ] 

let is_in_list e l = List.exists (fun x -> (x = e)) l


(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let fixity_of_string s =
  if ((is_in_list s special_infix_strings)
      || (is_in_list (String.get s 0) infix_symbols)) then Infix else Prefix

let fixity_of_longident li =
  match li.txt with
  | Longident.Lident name ->
      fixity_of_string name
(* This is wrong (and breaks RTT):
  | Longident.Ldot (_, name)
    when is_in_list name special_infix_strings -> Infix
*)
  | _ -> Prefix ;;

let fixity_of_exp e =
  match e.pexp_desc with
  | Pexp_ident (li) ->
      (fixity_of_longident li)
(*
  | Pexp_cspval (_,li) ->
          if false (* default valu of !Clflags.prettycsp *)
          then (fixity_of_longident li)
          else Prefix
*)
      | _ -> Prefix ;;

(* It's not recommended to fill [sep_action] and [sep] simultaneously *)
let pp_print_list
    ?(indent=0) ?(breakfirst=false) ?(breaklast=false) ?(space=1)
    ?(sep_action=fun ppf _ -> fprintf ppf "@ ")
    ?sep
    ?(first="")
    ?(last="")
    f ppf xs =
  let rec loop bf ppf  = function
    | [] -> ()
    | [x] -> begin 
        if bf then pp_print_break ppf space indent ;
        fprintf ppf "%a" f x;
        if breaklast then pp_print_break ppf space indent
    end
    | x::xs -> begin
        if bf then pp_print_break ppf space indent ;
        fprintf ppf "%a" f x ;
        (match sep with
        | None -> sep_action ppf ();
        | Some sep -> begin
            sep_action ppf ();
            fprintf ppf "%s" sep;
            sep_action ppf ();
        end);
        loop false ppf xs 
    end in begin
      fprintf ppf "%s%a%s@ " first (loop breakfirst) xs last 
    end
    
let pp_print_option f ppf a = match a with
| None -> ()
| Some x -> fprintf ppf "%a" f x
      
class _indent= object(self:'self)
  method longident ppf (x:Longident.t) = match x with
    | Lident s -> fprintf ppf "%s" s
    | Ldot(y,s) -> (match s.[0] with
          | 'a'..'z' | 'A' .. 'Z' ->
              fprintf ppf "%a.%s" self#longident y s
          | _ ->
              fprintf ppf "%a.(@ %s@ )@ " self#longident y s)
    | Longident.Lapply (y,s)->
        fprintf ppf "%a(%a)" self#longident y self#longident s
  method longident_loc ppf ({txt;_}:Longident.t Location.loc) =
    fprintf ppf "%a" self#longident txt
  method constant ppf x = match x with
  | Const_int i -> fprintf ppf "%d" i

  | Const_char i -> fprintf ppf "%C"  i 
  | Const_string i -> fprintf ppf "%S" i
  | Const_float  i -> fprintf ppf "%s" i 
  | Const_int32 i -> fprintf ppf "%ldl" i
  | Const_int64 i -> fprintf ppf "%LdL" i
  | Const_nativeint i -> fprintf ppf "%ndn" i
  method mutable_flag ppf   = function
    | Immutable -> ()
    | Mutable -> fprintf ppf "mutable@ "
  method virtual_flag ppf  = function
    | Concrete -> ()
    | Virtual -> fprintf ppf "virtual@ "
  method rec_flag ppf = function
    | Nonrecursive -> ()
    | Recursive | Default -> fprintf ppf "rec@ "
  method direction_flag ppf = function
    | Upto -> fprintf ppf "to@ "
    | Downto -> fprintf ppf "downto@ "
  method private_flag ppf = function
    | Public -> ()
    | Private -> fprintf ppf "private@ "
end;;



let rec fmt_longident_aux f x =
  match x with
  | Longident.Lident s -> fprintf f "%s" s;
  | Longident.Ldot(y, s) when is_in_list s special_infix_strings ->
      fprintf f "%a.( %s )@ " fmt_longident_aux y s
(* This is wrong (and breaks RTT):
      fprintf f "@ %s@ " s
*)
  | Longident.Ldot (y, s) ->
      begin
            match s.[0] with
          'a'..'z' | 'A'..'Z' ->
            fprintf f "%a.%s" fmt_longident_aux y s
        | _ ->
            fprintf f "%a.( %s )@ " fmt_longident_aux y s

      end

  | Longident.Lapply (y, z) ->
      fprintf f "%a(%a)" fmt_longident_aux y fmt_longident_aux z;
;;

let fmt_longident ppf x = fprintf ppf "%a" fmt_longident_aux x.txt;;

let fmt_constant f x =
  match x with
  | Const_int (i) ->
      if (i < 0) then fprintf f "(%d)" i
      else fprintf f "%d" i;
  | Const_char (c) -> fprintf f "%C" (* fmt_char *) c ;
  | Const_string (s) ->
      fprintf f "%S" s;
  | Const_float (s) ->
      if ((String.get s 0) = '-') then fprintf f "(%s)" s
      else fprintf f "%s" s;
      (* maybe parenthesize all floats for consistency? *)
  | Const_int32 (i) ->
      if i < 0l then fprintf f "(%ldl)" i
      else fprintf f "%ldl" i;
  | Const_int64 (i) ->
      if i < 0L then fprintf f "(%LdL)" i
      else fprintf f "%LdL" i;
  | Const_nativeint (i) ->
      if i < 0n then
        fprintf f "(%ndn)" i
      else fprintf f "%ndn" i;
;;

let fmt_mutable_flag ppf x =
  match x with
  | Immutable -> ();
  | Mutable -> fprintf ppf "mutable ";
;;

let string ppf s =
  fprintf ppf "%s" s ;;

let text ppf s =
  fprintf ppf "%s" s.txt ;;

let constant_string ppf s =
  fprintf ppf "\"%s\"" (String.escaped s) ;;

let fmt_virtual_flag f x =
  match x with
  | Virtual -> fprintf f "virtual ";
  | Concrete -> ();
;;


let type_var_print ppf str =
  fprintf ppf "'%s" str.txt ;;

let type_var_option_print ppf str =
  match str with
      None -> () (* TODO check *)
    | Some str ->
      fprintf ppf "'%s" str.txt ;;

let fmt_class_params ppf l = match l with
| [] -> ()
| [x] -> fprintf ppf "%s@ " x
| _ ->  pp_print_list pp_print_string ppf l ~sep:"," ~first:"(" ~last:")"

let fmt_class_params_def ppf  l = match l with
| [] -> ()
| _ -> pp_print_list type_var_print ppf l ~sep:"," ~first:"[" ~last:"]"

let fmt_rec_flag f x =
  match x with
  | Nonrecursive -> ();
  | Recursive | Default -> fprintf f " rec";
    (* todo - what is "default" recursion??
        this seemed safe, as it's better to falsely make a non-recursive
        let recursive than the opposite. *)
;;

let fmt_direction_flag ppf x =
  match x with
  | Upto   -> fprintf ppf "to" ;
  | Downto -> fprintf ppf "downto" ;
;;

let fmt_private_flag f x =
  match x with
  | Public -> () 
  | Private -> fprintf f "private "



let option_quiet_p f ppf x = match x with
  | None -> ()
  | Some x -> begin 
      fprintf ppf "@ (" ;
      f ppf x;
      fprintf ppf ")";
  end
;;
let option_quiet f ppf x =
  match x with
  | None -> ();
  | Some x ->
      fprintf ppf "@ " ;
      f ppf x;
;;

let rec expression_is_terminal_list exp =
  match exp with
  | {pexp_desc = Pexp_construct ({ txt = Lident("[]");_}, None, _);_}
     -> true ;
  | {pexp_desc = Pexp_construct ({ txt = Lident("::");_},
                   Some({pexp_desc = Pexp_tuple([_exp1 ; exp2]);_}), _);_}
     -> (expression_is_terminal_list exp2)
  | {pexp_desc = _;_}
     -> false
;;

let rec core_type ppf x =
  match x.ptyp_desc with
  | Ptyp_any -> fprintf ppf "_";         (* done *)
  | Ptyp_var (s) -> fprintf ppf "'%s" s; (* done *)
  | Ptyp_arrow (l, ct1, ct2) ->          (* done *)
      pp_open_hovbox ppf indent ;
      fprintf ppf "(" ;
      (match l with
        | "" -> core_type ppf ct1;
        | s when (String.get s 0 = '?')  ->
            (match ct1.ptyp_desc with
              | Ptyp_constr
                  ({ txt = Longident.Ldot (Longident.Lident "*predef*", "option");_}, l) ->
                  fprintf ppf "%s :@ " s ;
                  type_constr_list ppf l ;
              | _ -> core_type ppf ct1; (* todo: what do we do here? *)
            );
        | s ->
            fprintf ppf "%s :@ " s ;
            core_type ppf ct1; (* todo: what do we do here? *)
      );
      fprintf ppf "@ ->@ " ;
      core_type ppf ct2 ;
      fprintf ppf ")" ;
      pp_close_box ppf () ;
  | Ptyp_tuple l ->                      (* done *)
      pp_open_hovbox ppf indent ;
      fprintf ppf "(" ;
      (* list2 core_type ppf l " *" ; *)
      pp_print_list core_type ppf l ~sep:"*";
      fprintf ppf ")" ;
      pp_close_box ppf () ;
  | Ptyp_constr (li, l) ->               (* done *)
      pp_open_hovbox ppf indent ;
      type_constr_list ppf ~space:true l ;
      fprintf ppf "%a" fmt_longident li ;
      pp_close_box ppf () ;
  | Ptyp_variant (l, closed, _low) ->
      pp_open_hovbox ppf indent ;
      (match closed with
        | true  -> fprintf ppf "[ " ;
        | false -> fprintf ppf "[> " ;
      );
      (* list2 type_variant_helper ppf l " |" ; *)
      pp_print_list type_variant_helper ppf l ~sep:"|";
      fprintf ppf " ]";
      pp_close_box ppf () ;
  | Ptyp_object (l) ->
      if ((List.length l) > 0) then begin
        pp_open_hovbox ppf indent ;
        fprintf ppf "< " ;
        (* list2 core_field_type ppf l " ;" ; *)
        pp_print_list core_field_type ppf l ~sep:";";
        fprintf ppf " >" ;
        pp_close_box ppf () ;
        end else fprintf ppf "< >" ;
  | Ptyp_class (li, l, low) ->           (* done... sort of *)
      pp_open_hovbox ppf indent ;
      (* list2 core_type ppf l ~breaklast:true "" ; *)
      pp_print_list core_type ppf l ~breaklast:true ;
      fprintf ppf "#%a" fmt_longident li;
      if ((List.length low) < 0) then begin (* done, untested *)
        fprintf ppf "@ [> " ;
        (* list2 class_var ppf low "" ; *)
        pp_print_list class_var ppf low;
        fprintf ppf " ]";
        end ;
      pp_close_box ppf ();
  | Ptyp_alias (ct, s) ->                (* done *)
      pp_open_hovbox ppf indent ;
      fprintf ppf "(" ;
      core_type ppf ct ;
      fprintf ppf "@ as@ '%s)" s;
      pp_close_box ppf () ;
  | Ptyp_poly (sl, ct) ->                (* done? *)
      pp_open_hovbox ppf indent ;
      if ((List.length sl) > 0) then begin
          (* list2 (fun ppf x -> fprintf ppf "'%s" x) ppf sl ~breaklast:true ""; *)
        pp_print_list (fun ppf x -> fprintf ppf "'%s" x) ppf sl ~breaklast:true;
        fprintf ppf ".@ " ;
        end ;
      core_type ppf ct ;
      pp_close_box ppf () ;
  | Ptyp_package (lid, cstrs) ->
      fprintf ppf "(module %a@ " fmt_longident lid;
      pp_open_hovbox ppf indent;
      begin match cstrs with
          [] -> ()
        | _ ->
            fprintf ppf "@ with@ ";
            string_x_core_type_ands ppf cstrs ;
      end;
      pp_close_box ppf ();
      fprintf ppf ")";

and class_var ppf s =
  fprintf ppf "`%s" s ;

and core_field_type ppf x =
  match x.pfield_desc with
  | Pfield (s, ct) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "%s :@ " s;
      core_type ppf ct;
      pp_close_box ppf () ;
  | Pfield_var ->
      fprintf ppf "..";

and type_constr_list ppf ?(space=false) l =
  match (List.length l) with
  | 0 -> ()
  | 1 ->
      (* list2 core_type ppf l "" ; *)
      pp_print_list core_type ppf l ;
      if space then fprintf ppf " " ;
  | _ -> fprintf ppf "(" ;
      (* list2 core_type ppf l "," ; *)
      pp_print_list core_type ppf l ~sep:",";
      fprintf ppf ")" ;
      if (space) then fprintf ppf " " ;

and pattern_with_label ppf x s =
  if (s = "") then simple_pattern ppf x
  else begin
      let s =
        if (String.get s 0 = '?') then begin
            fprintf ppf "?" ;
            String.sub s 1 ((String.length s) - 1)
          end else begin
            fprintf ppf "~" ;
            s
          end in
      fprintf ppf "%s" s ;
      match x.ppat_desc with
      | Ppat_var (s2) ->
          if (s <> s2.txt) then begin
              fprintf ppf ":" ;
              simple_pattern ppf x ;
            end
      | _ -> fprintf ppf ":" ;
          simple_pattern ppf x
    end ;

and pattern_with_when ppf whenclause x =
  match whenclause with
  | None -> pattern ppf x ;
  | Some (e) ->
      pp_open_hovbox ppf indent ;
      pattern ppf x ;
      fprintf ppf "@ when@ " ;
      expression ppf e ;
      pp_close_box ppf () ;

and pattern ppf x =
  match x.ppat_desc with
    | Ppat_construct (li, po, _b) ->
      pp_open_hovbox ppf indent ;
      (match li.txt,po with
        | Longident.Lident("::"),
          Some ({ppat_desc = Ppat_tuple([pat1; pat2]);_}) ->
            fprintf ppf "(" ;
            pattern ppf pat1 ;
            fprintf ppf "@ ::@ " ;
            pattern_list_helper ppf pat2 ;
            fprintf ppf ")";
        | _,_ ->
            fprintf ppf "%a" fmt_longident li;
            option_quiet pattern_in_parens ppf po;);
      pp_close_box ppf () ;
(* OXX what is this boolean ??
         bool i ppf b;               *)

  | _ ->
      simple_pattern ppf x

and simple_pattern ppf x =
  match x.ppat_desc with
  | Ppat_construct (li, None, _) ->
      fprintf ppf "%a@ " fmt_longident li
  | Ppat_any -> fprintf ppf "_";            (* OXX done *)
  | Ppat_var ({txt = txt;_}) ->
      if (is_infix (fixity_of_string txt)) || List.mem txt.[0] prefix_symbols then
        fprintf ppf "( %s )" txt                (* OXX done *) (* fix bug ( *** ) *)
      else
        fprintf ppf "%s" txt;
  | Ppat_alias (p, s) ->                    (* OXX done ... *)
      pp_open_hovbox ppf indent ;
      fprintf ppf "(" ;
      pattern ppf p ;
      fprintf ppf " as@ %s)" s.txt;
      pp_close_box ppf () ;
  | Ppat_constant (c) ->                    (* OXX done *)
      fprintf ppf "%a" fmt_constant c;
  | Ppat_tuple (l) ->                       (* OXX done *)
      fprintf ppf "@[<hov 1>(";
      pp_print_list pattern ppf l ~sep:",";
      (* list2 pattern ppf l ","; *)
      fprintf ppf "@])";
  | Ppat_variant (l, po) ->
      (match po with
        | None ->
            fprintf ppf "`%s" l;
        | Some (p) ->
            pp_open_hovbox ppf indent ;
            fprintf ppf "(`%s@ " l ;
            pattern ppf p ;
            fprintf ppf ")" ;
            pp_close_box ppf () ;
      );
  | Ppat_record (l, closed) ->                     (* OXX done *)
      fprintf ppf "{" ;
      (* list2 longident_x_pattern ppf l ";" ; *)
      pp_print_list longident_x_pattern ppf l ~sep:";";
      begin match closed with
          Open -> fprintf ppf "; _ "; (* bug fix *)
        | Closed -> ()
      end;
      fprintf ppf "}" ;
  | Ppat_array (l) ->                      (* OXX done *)
      pp_open_hovbox ppf 2 ;
      fprintf ppf "[|" ;
      (* list2 pattern ppf l ";" ; *)
      pp_print_list pattern ppf l ~sep:";";
      fprintf ppf "|]" ;
      pp_close_box ppf () ;
  | Ppat_or (p1, p2) ->                    (* OXX done *)
      pp_open_hovbox ppf indent ;
      fprintf ppf "(" ;
      pattern ppf p1 ;
      fprintf ppf "@ | " ;
      pattern ppf p2 ;
      fprintf ppf ")" ;
      pp_close_box ppf () ;
  | Ppat_constraint (p, ct) ->             (* OXX done, untested *)
      fprintf ppf "(" ;
      pattern ppf p ;
      fprintf ppf " :" ;
      pp_print_break ppf 1 indent ;
      core_type ppf ct ;
      fprintf ppf ")" ;
  | Ppat_type (li) ->                        (* OXX done *)
      fprintf ppf "#%a" fmt_longident li ;
  | Ppat_lazy p ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "(lazy @ ";
      pattern ppf p ;
      fprintf ppf ")" ;
      pp_close_box ppf ()
  | Ppat_unpack (s) ->
      fprintf ppf "(module@ %s)@ " s.txt
  | _ ->
      fprintf ppf "@[<hov 1>(";
      pattern ppf x;
      fprintf ppf "@])";

and simple_expr ppf x =
  match x.pexp_desc with
  | Pexp_construct (li, None, _) ->
      fprintf ppf "%a@ " fmt_longident li
  | Pexp_ident (li) -> (* was (li, b) *)
      if is_infix (fixity_of_longident li)
        || match li.txt with
          | Longident.Lident (li) -> List.mem li.[0] prefix_symbols
          | _ -> false
      then
        fprintf ppf "(%a)" fmt_longident li
      else
        fprintf ppf "%a" fmt_longident li ;
  | Pexp_constant (c) -> fprintf ppf "%a" fmt_constant c;
  | Pexp_pack (me) ->
      fprintf ppf "(module@ ";
      pp_open_hovbox ppf indent;
      module_expr ppf me;
      pp_close_box ppf ();
      fprintf ppf ")";
  | Pexp_newtype (lid, e) ->
      fprintf ppf "fun (type %s)@ ->" lid; (* bug fix *)
      expression ppf e
  | Pexp_tuple (l) ->
      fprintf ppf "@[<hov 1>(";
      (* list2 simple_expr ppf l ","; *)
      pp_print_list simple_expr ppf l ~sep:",";
      fprintf ppf ")@]";
  | Pexp_variant (l, eo) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "`%s" l ;
      option_quiet_p expression ppf eo ; (* bug fix*)
      pp_close_box ppf () ;
  | Pexp_record (l, eo) ->
      pp_open_hovbox ppf indent ; (* maybe just 1? *)
      fprintf ppf "{" ;
      begin
        match eo with
          None -> ()
        | Some e ->
            expression ppf e;
            fprintf ppf "@ with@ "
      end;
      (* list2 longident_x_expression ppf l ";" ; *)
      pp_print_list longident_x_expression ppf l ~sep:";";
      fprintf ppf "}" ;
      pp_close_box ppf () ;
  | Pexp_array (l) ->
      pp_open_hovbox ppf 2 ;
      fprintf ppf "[|" ;
      (* list2 simple_expr ppf l ";" ; *)
      pp_print_list simple_expr ppf l ~sep:";";
      fprintf ppf "|]" ;
      pp_close_box ppf () ;
  | Pexp_while (e1, e2) ->
      pp_open_hvbox  ppf 0 ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "while@ " ;
      expression ppf e1 ;
      fprintf ppf " do" ;
      pp_close_box ppf () ;
      pp_print_break ppf 1 indent ;
      expression_sequence ppf e2 ~first:false;
      pp_print_break ppf 1 0 ;
      fprintf ppf "done" ;
      pp_close_box ppf () ;
  | Pexp_for (s, e1, e2, df, e3) ->
      pp_open_hvbox  ppf 0 ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "for %s =@ " s.txt ;
      expression ppf e1 ;
      fprintf ppf "@ %a@ " fmt_direction_flag df ;
      expression ppf e2 ;
      fprintf ppf " do" ;
      pp_close_box ppf () ;

      pp_print_break ppf 1 indent ;
      expression_sequence ppf ~first:false e3 ;
      pp_print_break ppf 1 0 ;
      fprintf ppf "done" ;
      pp_close_box ppf () ;


  | _ ->
      fprintf ppf "(@ ";
      expression ppf x;
      fprintf ppf "@ )"

and expression ppf x =
  match x.pexp_desc with
  | Pexp_let (rf, l, e) ->
      let l1 = (List.hd l) in
      let l2 = (List.tl l) in
      pp_open_hvbox ppf 0 ;
      pp_open_hvbox ppf indent ;
      fprintf ppf "let%a " fmt_rec_flag rf;
      pattern_x_expression_def ppf l1;
      pattern_x_expression_def_list ppf l2;
      pp_close_box ppf () ;
      fprintf ppf " in" ;
      pp_print_space ppf () ;
      expression_sequence ppf ~first:false ~indent:0 e ;
      pp_close_box ppf () ;
  | Pexp_function (label, None, [
        { ppat_desc = Ppat_var { txt ="*opt*" ;_};_ },
        { pexp_desc = Pexp_let (_, [
              arg ,
              { pexp_desc = Pexp_match (_, [ _; _, eo ] ) ;_} ], e) ;_}
      ]
    ) ->
      expression ppf { x with pexp_desc = Pexp_function(label, Some eo,
          [arg, e]) }

  | Pexp_function (p, eo, l) ->
      if (List.length l = 1) then begin
          pp_open_hvbox ppf indent;
          fprintf ppf "fun " ;
          pattern_x_expression_case_single ppf (List.hd l) eo p
        end else begin
          pp_open_hvbox ppf 0;
          fprintf ppf "function" ;
          option_quiet expression_in_parens ppf eo ;
          pp_print_space ppf () ;
          pattern_x_expression_case_list ppf l ;
        end ;
      pp_close_box ppf ();
  | Pexp_apply (e, l) -> (* was (e, l, _) *)
      let fixity = (is_infix (fixity_of_exp e)) in
      let sd =
        (match e.pexp_desc with
          | Pexp_ident ({ txt = Longident.Ldot (Longident.Lident(modname), valname) ;_})
            -> (modname, valname)
          | Pexp_ident ({ txt = Longident.Lident(valname) ;_})
            -> ("",valname)
          | _ -> ("",""))
      in
      (match sd,l with
        | ("Array", "get"), [(_,exp1) ; (_,exp2)] ->
            pp_open_hovbox ppf indent;
            (match exp1.pexp_desc with
              | Pexp_ident (_) ->
                  expression ppf exp1 ;
              | _ ->
                  expression_in_parens ppf exp1 ;
            );
            fprintf ppf ".";
            expression_in_parens ppf exp2;
            pp_close_box ppf ();
        | ("Array", "set"), [(_,array) ; (_,index) ; (_, valu)] ->
            pp_open_hovbox ppf indent;
            (match array.pexp_desc with
              | Pexp_ident (_) ->
                  expression ppf array ;
              | _ ->
                  expression_in_parens ppf array ;
            );
            fprintf ppf ".";
            expression_in_parens ppf index;
            fprintf ppf "@ <-@ ";
            expression ppf valu;
            pp_close_box ppf ();
        | ("","!"),[(_,exp1)] ->
            fprintf ppf "!" ;
            simple_expr ppf exp1 ;
(* | ("","raise"),[(_,exp)] ->
               fprintf ppf "raising [" ;
               expression ppf exp;
               fprintf ppf "], says %s" st; *)
        | (_,_) ->
            pp_open_hovbox ppf (indent + 1) ;
            fprintf ppf "(" ;
            if (fixity = false) then
              begin
                (match e.pexp_desc with
                  | Pexp_ident(_) -> expression ppf e ;
                  | Pexp_send (_,_) -> expression ppf e ;
                  | _ -> pp_open_hovbox ppf indent;
                      expression_in_parens ppf e ;
                      pp_close_box ppf () );
                fprintf ppf "@ " ;
                (* list2 label_x_expression_param ppf l ""; *)
                pp_print_list label_x_expression_param ppf l ;
              end
            else begin
                match l with
                  [ arg1; arg2 ] ->
                    label_x_expression_param ppf arg1 ;
                    pp_print_space ppf () ;
                    (match e.pexp_desc with
                      | Pexp_ident(li) ->
(* override parenthesization of infix identifier *)
                          fprintf ppf "%a" fmt_longident li ;
                      | _ -> simple_expr ppf e) ;
                    pp_print_space ppf () ;
                    label_x_expression_param ppf arg2
                | _ ->
(* fprintf ppf "(" ; *)
                    simple_expr ppf e ;
(* fprintf ppf ")" ; *)
                    (* list2 label_x_expression_param ppf l ~breakfirst:true "" *)
                    pp_print_list label_x_expression_param ppf l ~breakfirst:true;
              end ;
            fprintf ppf ")" ;
            pp_close_box ppf () ;)
  | Pexp_match (e, l) ->
      fprintf ppf "(" ;
      pp_open_hvbox ppf 0;
      pp_open_hovbox ppf 2;
      fprintf ppf "match@ " ;
      expression ppf e ;
      fprintf ppf " with" ;
      pp_close_box ppf () ;
      pp_print_space ppf () ;
      pattern_x_expression_case_list ppf l ;
      pp_close_box ppf () ;
      fprintf ppf ")" ;
  | Pexp_try (e, l) ->
      fprintf ppf "(";
      pp_open_vbox ppf 0; (* <-- always break here, says style manual *)
      pp_open_hvbox ppf 0;
      fprintf ppf "try";
      pp_print_break ppf 1 indent ;
      expression_sequence ppf ~first:false e;
      pp_print_break ppf 1 0;
      fprintf ppf "with";
      pp_close_box ppf ();
      pp_print_cut ppf ();
      pattern_x_expression_case_list ppf l ;
      pp_close_box ppf ();
      fprintf ppf ")";
  | Pexp_construct (li, eo, _b) ->
      (match li.txt with
        | Longident.Lident ("::") ->
            (match eo with
                Some ({pexp_desc = Pexp_tuple ([exp1 ; exp2]);_}) ->
                  pp_open_hovbox ppf indent ;
                  if (expression_is_terminal_list exp2) then begin
                      fprintf ppf "[" ;
                      simple_expr ppf exp1 ;
                      expression_list_helper ppf exp2 ;
                      fprintf ppf "]" ;
                    end
                  else begin
                      pp_open_hovbox ppf indent ;
                      fprintf ppf "(@ ";
                      simple_expr ppf exp1 ;
                      fprintf ppf " ) ::@ " ;
                      expression_list_nonterminal ppf exp2 ;
                      fprintf ppf "@ " ;
                      pp_close_box ppf () ;
                    end ;
                  pp_close_box ppf () ;
              | _ -> assert false
            );
        | Longident.Lident ("()") -> fprintf ppf "()" ;
        | _ ->
            fprintf ppf "(";
            pp_open_hovbox ppf indent ;
            fmt_longident ppf li;
            option_quiet expression_in_parens ppf eo;
            pp_close_box ppf () ;
            fprintf ppf ")"
      );
  | Pexp_field (e, li) ->
      pp_open_hovbox ppf indent ;
      (match e.pexp_desc with
        | Pexp_ident (_) ->
            simple_expr ppf e ;
        | _ ->
            expression_in_parens ppf e ;
      );
      fprintf ppf ".%a" fmt_longident li ;
      pp_close_box ppf () ;
  | Pexp_setfield (e1, li, e2) ->
      pp_open_hovbox ppf indent ;
      (match e1.pexp_desc with
        | Pexp_ident (_) ->
            simple_expr ppf e1 ;
        | _ ->
            expression_in_parens ppf e1 ;
      );
      fprintf ppf ".%a" fmt_longident li;
      fprintf ppf "@ <-@ ";
      expression ppf e2;
      pp_close_box ppf () ;
  | Pexp_ifthenelse (e1, e2, eo) ->
      fprintf ppf "@[<hv 0>" ;
      expression_if_common ppf e1 e2 eo;
      fprintf ppf "@]";

  | Pexp_sequence (_e1,_e2) ->
      fprintf ppf "@[<hv 0>begin" ;
      pp_print_break ppf 1 indent ;
      expression_sequence ppf ~first:false x ;
      fprintf ppf "@;<1 0>end@]" ;
  | Pexp_constraint (e, cto1, cto2) ->
      (match (cto1, cto2) with
        | (None, None) -> expression ppf e ;
        | (Some (x1), Some (x2)) ->
            pp_open_hovbox ppf 2 ;
            fprintf ppf "(" ;
            expression ppf e ;
            fprintf ppf " :@ " ;
            core_type ppf x1 ;
            fprintf ppf " :>@ " ;
            core_type ppf x2 ;
            fprintf ppf ")" ;
            pp_close_box ppf () ;
        | (Some (x), None) ->
            pp_open_hovbox ppf 2 ;
            fprintf ppf "(" ;
            expression ppf e ;
            fprintf ppf " :@ " ;
            core_type ppf x ;
            fprintf ppf ")" ;
            pp_close_box ppf ()
        | (None, Some (x)) ->
            pp_open_hovbox ppf 2 ;
            fprintf ppf "(" ;
            expression ppf e ;
            fprintf ppf " :>@ " ;
            core_type ppf x ;
            fprintf ppf ")" ;
            pp_close_box ppf ()
      )
  | Pexp_when (_e1, _e2) ->
      assert false ;
(* This is a wierd setup. The ocaml phrase
          "pattern when condition -> expression"
          found in pattern matching contexts is encoded as:
          "pattern -> when condition expression"
         Thus, the when clause ("when condition"), which one might expect
          to be part of the pattern, is encoded as part of the expression
          following the pattern.
         A "when clause" should never exist in a vaccum. It should always
          occur in a pattern matching context and be printed as part of the
          pattern (in pattern_x_expression_case_list).
         Thus these Pexp_when expressions are printed elsewhere, and if
          this code is executed, an error has occurred. *)
  | Pexp_send (e, s) ->
      pp_open_hovbox ppf indent;
      (match e.pexp_desc with
        | Pexp_ident(_) ->
            expression ppf e;
            fprintf ppf "#%s" s;
        | _ ->
            fprintf ppf "(" ;
            expression_in_parens ppf e;
            fprintf ppf "@,#%s" s;
            fprintf ppf ")"
      );
      pp_close_box ppf (); (* bug fixed? *)
  | Pexp_new (li) ->
      pp_open_hovbox ppf indent;
      fprintf ppf "new@ %a" fmt_longident li;
      pp_close_box ppf ();
  | Pexp_setinstvar (s, e) ->
      pp_open_hovbox ppf indent;
      fprintf ppf "%s <-@ " s.txt;
      expression ppf e;
      pp_close_box ppf ();
  | Pexp_override (l) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "{< " ;
      if ((List.length l) > 0) then begin
          (* list2 string_x_expression ppf l ";"; *)
        pp_print_list string_x_expression ppf l ~sep:";";
        fprintf ppf " " ;
      end ;
      fprintf ppf ">}" ;
      pp_close_box ppf () ;
  | Pexp_letmodule (s, me, e) ->
      pp_open_hvbox ppf 0 ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "let module %s =@ " s.txt ;
      module_expr ppf me ;
      fprintf ppf " in" ;
      pp_close_box ppf () ;
      pp_print_space ppf () ;
      expression_sequence ppf ~first:false ~indent:0 e ;
      pp_close_box ppf () ;
  | Pexp_assert (e) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "assert@ " ;
      expression ppf e ;
      pp_close_box ppf () ;
  | Pexp_assertfalse ->
      fprintf ppf "assert false" ;
  | Pexp_lazy (e) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "lazy@ " ;
      simple_expr ppf e ;
      pp_close_box ppf () ;
  | Pexp_poly (e, cto) ->
(* should this even print by itself? *)
      (match cto with
        | None -> expression ppf e ;
        | Some (ct) ->
            pp_open_hovbox ppf indent ;
            expression ppf e ;
            fprintf ppf "@ (* poly:@ " ;
            core_type ppf ct ;
            fprintf ppf " *)" ;
            pp_close_box ppf () );
  | Pexp_object cs ->
      pp_open_hovbox ppf indent ;
      class_structure ppf cs ;
      pp_close_box ppf () ;
  | Pexp_open (lid, e) ->
      pp_open_hvbox ppf 0 ;
      fprintf ppf "let open@ %a in@ " fmt_longident lid;
      expression_sequence ppf ~first:false ~indent:0 e ;
      pp_close_box ppf () ;
  | _ -> simple_expr ppf x


and value_description ppf x =
  pp_open_hovbox ppf indent ;
  core_type ppf x.pval_type;
  if ((List.length x.pval_prim) > 0) then begin
    fprintf ppf " =@ " ;
    (* list2 constant_string ppf x.pval_prim ""; *)
    pp_print_list constant_string ppf x.pval_prim ;
  end ;
  pp_close_box ppf () ;

and type_declaration ppf x = begin
  pp_open_hovbox ppf indent ;
  (match (x.ptype_manifest,x.ptype_kind,x.ptype_private) with
     | (None,_,Asttypes.Public) -> ()
     | (None,_,Asttypes.Private) -> fprintf ppf "private@ "
     | (Some y, Ptype_abstract,Asttypes.Private) -> begin
         fprintf ppf "private@ ";
         core_type ppf y;
     end
     | (Some y, _, Asttypes.Private) -> begin
         core_type ppf y;
         fprintf ppf "@ =@ private@ "
     end
     | (Some y,Ptype_abstract, Asttypes.Public) -> begin
         core_type ppf y;
     end
     | (Some y, _,Asttypes.Public) -> begin
         core_type ppf y;
         fprintf ppf "@ =@ "
     end
  );
  (match x.ptype_kind with
    | Ptype_variant (first::rest) -> begin 
        pp_open_hovbox ppf indent ;
        pp_open_hvbox ppf 0 ;
        type_variant_leaf ppf first true ;
        type_variant_leaf_list ppf rest ;
        pp_close_box ppf () ;
        pp_close_box ppf ()
    end
    | Ptype_variant [] ->
        assert false ;
    | Ptype_abstract -> ()
    | Ptype_record l -> begin 
        pp_open_hovbox ppf indent ;
        fprintf ppf "{" ;
        pp_print_break ppf 0 indent ;
        pp_open_hvbox ppf 0;
        (* list2 type_record_field ppf l ";" ; *)
        pp_print_list type_record_field ppf l ~sep:";";
        pp_close_box ppf () ;
        fprintf ppf "@," ;
        pp_close_box ppf () ;
        fprintf ppf "}" ;
        pp_close_box ppf ()
    end );
  (* list2 typedef_constraint ppf x.ptype_cstrs ~breakfirst:true "" ; *)
  pp_print_list typedef_constraint ppf x.ptype_cstrs ~breakfirst:true ;
  pp_close_box ppf () 
end
and exception_declaration ppf x =
  match x with
  | [] -> ()
  | _first:: _rest ->
      fprintf ppf "@ of@ ";
      (* list2 core_type ppf x " *"; *)
      pp_print_list core_type ppf x ~sep:"*";

and class_type ppf x =
  match x.pcty_desc with
  | Pcty_signature (cs) ->
      class_signature ppf cs;
  | Pcty_constr (li, l) ->
      pp_open_hovbox ppf indent ;
      (match l with
        | [] -> ()
        | _  -> fprintf ppf "[" ;
            (* list2 core_type ppf l "," ; *)
            pp_print_list core_type ppf l ~sep:",";
            fprintf ppf "]@ " );
      fprintf ppf "%a" fmt_longident li ;
      pp_close_box ppf () ;
  | Pcty_fun (l, co, cl) ->
      pp_open_hovbox ppf indent ;

      (match l with
      | "" ->
          core_type ppf co
      | s when (String.get s 0 = '?') ->
          (match co.ptyp_desc with
          | Ptyp_constr
              ({txt=
                Longident.Ldot
                  (Longident.Lident "*predef*", "option");_},l)
            -> begin
              fprintf ppf "%s :@ " s ;
              type_constr_list ppf l
            end
          | _ -> core_type ppf co )
      | s -> begin
          fprintf ppf "%s :@ "s;
          core_type ppf co ;
      end );
      fprintf ppf " ->@ ";
      class_type ppf cl ;
      pp_close_box ppf () ;

and class_signature ppf { pcsig_self = ct; pcsig_fields = l ;_} =
  pp_open_hvbox ppf 0;
  pp_open_hovbox ppf indent ;
  fprintf ppf "object";
  (match ct.ptyp_desc with
    | Ptyp_any -> ()
    | _ -> fprintf ppf "@ (";
        core_type ppf ct;
        fprintf ppf ")" );
  pp_close_box ppf () ;
  (* list2 class_type_field ppf l ~indent:indent ~breakfirst:true ""; *)
  pp_print_list class_type_field ppf l ~indent ~breakfirst:true ;
  pp_print_break ppf 1 0;
  fprintf ppf "end";

and class_type_field ppf x =
  match x.pctf_desc with
  | Pctf_inher (ct) ->      (* todo: test this *)
      pp_open_hovbox ppf indent ;
      fprintf ppf "inherit@ " ;
      class_type ppf ct ;
      pp_close_box ppf () ;
  | Pctf_val (s, mf, vf, ct) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "val %s%s%s :@ "
        (match mf with
        | Mutable -> "mutable "
        | _       -> "")
      (match vf with
        | Virtual -> "virtual "
        | _       -> "")
      s;
      core_type ppf ct ;
      pp_close_box ppf () ;
  | Pctf_virt (s, pf, ct) ->    (* todo: test this *)
      pp_open_hovbox ppf indent ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "method@ %avirtual@ %s" fmt_private_flag pf s ;
      pp_close_box ppf () ;
      fprintf ppf " :@ " ;
      core_type ppf ct ;
      pp_close_box ppf () ;
  | Pctf_meth (s, pf, ct) ->
      pp_open_hovbox ppf indent ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "method %a%s" fmt_private_flag pf s;
      pp_close_box ppf () ;
      fprintf ppf " :@ " ;
      core_type ppf ct ;
      pp_close_box ppf () ;
  | Pctf_cstr (ct1, ct2) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "constraint@ " ;
      core_type ppf ct1;
      fprintf ppf " =@ " ;
      core_type ppf ct2;
      pp_close_box ppf () ;

and class_description ppf ({pci_params=(ls,_);pci_name={txt;_};_} as x) =
  pp_open_hvbox ppf 0 ;
  pp_open_hovbox ppf indent ;
  fprintf ppf "class %a%a%s :" fmt_virtual_flag x.pci_virt
    fmt_class_params_def ls txt ;
  pp_close_box ppf () ;
  pp_print_break ppf 1 indent ;
  class_type ppf x.pci_expr ;
  pp_close_box ppf () ;

and class_type_declaration ppf x =
  class_type_declaration_ext ppf true x ;

and class_type_declaration_ext ppf first ({pci_params=(ls,_);pci_name={txt;_};_} as x) =
  pp_open_hvbox ppf 0;
  pp_open_hovbox ppf indent ;
  fprintf ppf "%s@ %a%a%s =" (if (first) then "class type" else "and")
  fmt_virtual_flag x.pci_virt fmt_class_params_def ls
    txt ;
  pp_close_box ppf ();
  pp_print_break ppf 1 indent ;
  class_type ppf x.pci_expr;
  pp_close_box ppf ();

and class_type_declaration_list ppf ?(first=true) l =
  if (first) then pp_open_hvbox ppf 0 ;
  match l with
  | [] -> if (first) then pp_close_box ppf () ;
  | h :: [] ->
      class_type_declaration_ext ppf first h ;
      pp_close_box ppf () ;
  | h :: t ->
      class_type_declaration_ext ppf first h ;
      pp_print_space ppf () ;
      class_type_declaration_list ppf ~first:false t ;

and class_expr ppf x =
  match x.pcl_desc with
  | Pcl_structure (cs) ->
      class_structure ppf cs ;
  | Pcl_fun (l, eo, p, e) ->
      pp_open_hvbox ppf indent;
      pp_open_hovbox ppf indent;
      fprintf ppf "fun@ ";
      pattern ppf p;
      fprintf ppf " ->";
      pp_close_box ppf ();
      (match (eo, l) with
        | (None, "") -> () ;
        | (_,_) ->
            pp_open_hovbox ppf indent;
            (* fprintf ppf " (\* eo: "; *)
            (* option expression ppf eo; *)
            (* fprintf ppf "@ label: "; *)
            (* label 0 ppf l; *)
            (* fprintf ppf " *\)"; *)
            pp_close_box ppf ()
      );
      fprintf ppf "@ ";
      class_expr ppf e;
      pp_close_box ppf ();
  | Pcl_let (rf, l, ce) ->
      let l1 = (List.hd l) in
      let l2 = (List.tl l) in
      pp_open_hvbox ppf 0 ;
      pp_open_hvbox ppf indent ;
      fprintf ppf "let%a " fmt_rec_flag rf;
      pattern_x_expression_def ppf l1;
      pattern_x_expression_def_list ppf l2;
      pp_close_box ppf () ;
      pp_close_box ppf () ;
      fprintf ppf " in" ;
      pp_print_space ppf () ;
      class_expr ppf ce;
  | Pcl_apply (ce, l) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "(";
      class_expr ppf ce;
      (* list2 label_x_expression_param ppf l ~breakfirst:true ""; *)
      pp_print_list label_x_expression_param ppf l ~breakfirst:true ;
      fprintf ppf ")";
      pp_close_box ppf () ;
  | Pcl_constr (li, l) ->
      pp_open_hovbox ppf indent;
      if ((List.length l) != 0) then begin
        fprintf ppf "[" ;
          (* list2 core_type ppf l "," ; *)
        pp_print_list core_type ppf l ~sep:",";
        fprintf ppf "]@ " ;
        end ;
      fprintf ppf "%a" fmt_longident li;
      pp_close_box ppf ();
  | Pcl_constraint (ce, ct) ->
      pp_open_hovbox ppf indent;
      fprintf ppf "(";
      class_expr ppf ce;
      fprintf ppf "@ : ";
      class_type ppf ct;
      fprintf ppf ")";
      pp_close_box ppf ();

and class_structure ppf { pcstr_pat = p; pcstr_fields =  l } =
  pp_open_hvbox ppf 0 ;
  pp_open_hovbox ppf indent ;
  fprintf ppf "object" ;
  (match p.ppat_desc with
    | Ppat_any -> ();
    | _ -> fprintf ppf "@ " ;
        pattern_in_parens ppf p );
  pp_close_box ppf () ;
  (* list2 class_field ppf l ~indent:indent ~breakfirst:true ""; *)
  pp_print_list class_field ppf l ~indent ~breakfirst:true;
  fprintf ppf "@ end" ;
  pp_close_box ppf () ;

and override ovf = match ovf with
    Override -> "!"
  | Fresh -> ""

and class_field ppf x =
  match x.pcf_desc with
  | Pcf_inher (ovf, ce, so) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "inherit%s@ " (override ovf);
      class_expr ppf ce;
      (match so with
        | None -> ();
        | Some (s) -> fprintf ppf "@ as %s" s );
      pp_close_box ppf ();
  | Pcf_val (s, mf, ovf, e) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "val%s %a%s =@ " (override ovf) fmt_mutable_flag mf s.txt ;
      expression_sequence ppf ~indent:0 e ;
      pp_close_box ppf () ;
  | Pcf_virt (s, pf, ct) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "method virtual %a%s" fmt_private_flag pf s.txt ;
      fprintf ppf " :@ " ;
      core_type ppf ct;
      pp_close_box ppf () ;
  | Pcf_valvirt (s, mf, ct) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "val virtual %s%s"
        (match mf with
        | Mutable -> "mutable "
        | _       -> "")
      s.txt;
      fprintf ppf " :@ " ;
      core_type ppf ct;
      pp_close_box ppf () ;
  | Pcf_meth (s, pf, ovf, e) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "method%s %a%s" (override ovf) fmt_private_flag pf s.txt ;
      (match e.pexp_desc with
        | Pexp_poly (e, Some(ct)) ->
            fprintf ppf " :@ " ;
            core_type ppf ct ;
            fprintf ppf " =@ " ;
            expression ppf e ;
        | _ ->
            fprintf ppf " =@ " ;
            expression ppf e;
      ) ;
(* special Pexp_poly handling? *)
      pp_close_box ppf () ;
  | Pcf_constr (ct1, ct2) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "constraint@ ";
      core_type ppf ct1;
      fprintf ppf " =@ " ;
      core_type ppf ct2;
      pp_close_box ppf ();
(*  | Pcf_let (rf, l) ->
(* at the time that this was written, Pcf_let was commented out
         of the parser, rendering this untestable. In the interest of
         completeness, the following code is designed to print what
         the parser seems to expect *)
(* todo: test this, eventually *)
      let l1 = (List.hd l) in
      let l2 = (List.tl l) in
      pp_open_hvbox ppf indent ;
      fprintf ppf "let%a " fmt_rec_flag rf;
      pattern_x_expression_def ppf l1;
      pattern_x_expression_def_list ppf l2;
      fprintf ppf " in" ;
      pp_close_box ppf () ; *)
  | Pcf_init (e) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "initializer@ " ;
      expression_sequence ppf ~indent:0 e ;
      pp_close_box ppf () ;
(* only pretty print [class_fun] *)
and class_fun_helper ppf e =
  match e.pcl_desc with
  | Pcl_fun (l, eo, p, e) ->
      (* pattern ppf p; *)
      (* fprintf ppf "@ "; *)
      (match (eo, l) with
        | (None, "") -> fprintf ppf "%a@ " pattern p
        | (Some x,_) ->
            fprintf ppf "%s:(%a=%a)@ " l pattern p expression x
            (* let len =String.length l in *)
            (* if len = 0 then failwith "class_fun_helper" *)
            (* else  *)
            (*   let label = String.sub l 1 (len-1) in  *)
            (*   fprintf ppf "?(%s=%a)@ " label   expression x *)
            (* fprintf ppf "(\* "; *)
            (* option expression ppf eo; *)
            (* label 0 ppf l; *)
            (* fprintf ppf " *\)@ " *)
        | (None,_) ->
            fprintf ppf "%s:(%a)@ " l pattern p
            (* let len = String.length l in *)
            (* if len = 0 then failwith "class_fun_helper" *)
            (* else *)
            (*   let label = String.sub l 1 (len - 1) in  *)
            (*   fprintf ppf "?%s@ "  label   *)

      );
      class_fun_helper ppf e;
  | _ ->
      e;

and class_declaration_list ppf ?(first=true) l =
  match l with
  | [] ->
      if (first = false) then pp_close_box ppf ();
  | cd::l ->
      let s = (if first then begin pp_open_hvbox ppf 0 ; "class" end
          else begin pp_print_space ppf () ; "and" end) in
      class_declaration ppf ~str:s cd ;
      class_declaration_list ppf ~first:false l ;

and class_declaration ppf ?(str="class") ({pci_params=(ls,_);pci_name={txt;_};_} as x) =
  pp_open_hvbox ppf indent ;
  pp_open_hovbox ppf indent ;
  fprintf ppf "%s %a%a%s@ " str fmt_virtual_flag x.pci_virt
    fmt_class_params_def ls txt ;
  let ce =
    (match x.pci_expr.pcl_desc with
      | Pcl_fun (_l, _eo, _p, _e) ->
          class_fun_helper ppf x.pci_expr;
      | _ -> x.pci_expr) in
  let ce =
    (match ce.pcl_desc with
      | Pcl_constraint (ce, ct) ->
          fprintf ppf ":@ " ;
          class_type ppf ct ;
          fprintf ppf "@ " ;
          ce
      | _ -> ce ) in
  fprintf ppf "=" ;
  pp_close_box ppf () ;
  fprintf ppf "@ " ;
  class_expr ppf ce ;
  pp_close_box ppf () ;

and module_type ppf x =
  match x.pmty_desc with
  | Pmty_ident (li) ->
      fprintf ppf "%a" fmt_longident li;
  | Pmty_signature (s) ->
      pp_open_hvbox ppf 0;
      fprintf ppf "sig";
      (* list2 signature_item ppf s ~breakfirst:true ~indent:indent ""; *)
      pp_print_list signature_item ppf s ~breakfirst:true ~indent;
      pp_print_break ppf 1 0;
      fprintf ppf "end";
      pp_close_box ppf ();
  | Pmty_functor (s, mt1, mt2) ->
      pp_open_hvbox ppf indent;
      pp_open_hovbox ppf indent;
      fprintf ppf "functor@ (%s : " s.txt ;
      module_type ppf mt1;
      fprintf ppf ") ->";
      pp_close_box ppf ();
      pp_print_space ppf ();
      module_type ppf mt2;
      pp_close_box ppf ();
  | Pmty_with (mt, l) ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "(" ;
      module_type ppf mt ;
      fprintf ppf "@ with@ " ;
      longident_x_with_constraint_list ppf l ;
      fprintf ppf ")" ;
      pp_close_box ppf () ;
  | Pmty_typeof me ->
      pp_open_hovbox ppf indent ;
      fprintf ppf "module type of " ;
      module_expr ppf me ;
      pp_close_box ppf ()

and signature ppf x =  pp_print_list ~sep_action:pp_print_newline signature_item ppf x

and signature_item ppf x :unit=
  begin
    match x.psig_desc with
    | Psig_type (l) ->
        let first = (List.hd l) in
        let rest  = (List.tl l) in
        pp_open_hvbox ppf 0;
        pp_open_hvbox ppf 0;
        fprintf ppf "type " ;
        string_x_type_declaration ppf first;
        pp_close_box ppf ();
        type_def_list_helper ppf rest;
        pp_close_box ppf ();
    | Psig_value (s, vd) ->
      let intro = if vd.pval_prim = [] then "val" else "external" in
        pp_open_hovbox ppf indent ;
        if (is_infix (fixity_of_string s.txt))
          || List.mem s.txt.[0] prefix_symbols then
          fprintf ppf "%s ( %s ) :@ "
            intro s.txt                (* OXX done *)
        else
        fprintf ppf "%s %s :@ " intro s.txt;
        value_description ppf vd;
        pp_close_box ppf () ;
    | Psig_exception (s, ed) ->
        pp_open_hovbox ppf indent ;
        fprintf ppf "exception %s" s.txt;
        exception_declaration ppf ed;
        pp_close_box ppf ();
    | Psig_class (l) ->
        pp_open_hvbox ppf 0 ;
        (* list2 class_description ppf l ""; *)
        pp_print_list class_description ppf l ;
        pp_close_box ppf () ;
    | Psig_module (s, mt) ->  (* todo: check this *)
        pp_open_hovbox ppf indent ;
        pp_open_hovbox ppf indent ;
        fprintf ppf "module@ %s :" s.txt ;
        pp_close_box ppf () ;
        pp_print_space ppf () ;
        module_type ppf mt;
        pp_close_box ppf () ;
    | Psig_open (li) ->
        pp_open_hovbox ppf indent ;
        fprintf ppf "open@ %a" fmt_longident li ;
        pp_close_box ppf () ;
    | Psig_include (mt) ->  (* todo: check this *)
        pp_open_hovbox ppf indent ;
        fprintf ppf "include@ " ;
        module_type ppf mt;
        pp_close_box ppf () ;
    | Psig_modtype (s, md) -> (* todo: check this *)
        pp_open_hovbox ppf indent ;
        fprintf ppf "module type %s" s.txt ;
        (match md with
          | Pmodtype_abstract -> ()
          | Pmodtype_manifest (mt) ->
              pp_print_space ppf () ;
              fprintf ppf " = " ;
              module_type ppf mt;
        );
        pp_close_box ppf () ;
    | Psig_class_type (l) ->
        class_type_declaration_list ppf l ;
    | Psig_recmodule decls ->
        pp_open_hvbox ppf 0 ;
        pp_open_hovbox ppf indent ;
        fprintf ppf "module rec@ " ;
        string_x_module_type_list ppf decls ; (* closes hov box *)
        pp_close_box ppf () ;
  end;
  (* fprintf ppf "@\n" *)


and module_expr ppf x =
  match x.pmod_desc with
  | Pmod_structure (s) ->
      pp_open_hvbox ppf 0;
      fprintf ppf "struct";
      (* list2 structure_item ppf s ~breakfirst:true ~indent:indent ""; *)
      pp_print_list structure_item ppf s ~breakfirst:true ~indent;
      pp_print_break ppf 1 0;
      fprintf ppf "end";
      pp_close_box ppf (); (* bug fixed? *)
  | Pmod_constraint (me, mt) ->
      fprintf ppf "(";
      pp_open_hovbox ppf indent;
      module_expr ppf me;
      fprintf ppf " :@ ";  (* <-- incorrect indentation? *)
      module_type ppf mt;
      pp_close_box ppf ();
      fprintf ppf ")";
  | Pmod_ident (li) ->
      fprintf ppf "%a" fmt_longident li;
  | Pmod_functor (s, mt, me) ->
      pp_open_hvbox ppf indent ;
      fprintf ppf "functor (%s : " s.txt;
      module_type ppf mt;
      fprintf ppf ") ->@ ";
      module_expr ppf me;
      pp_close_box ppf () ;
  | Pmod_apply (me1, me2) ->
      pp_open_hovbox ppf indent;
      fprintf ppf "(" ;
      module_expr ppf me1;
      fprintf ppf ")" ;
      pp_print_cut ppf ();
      fprintf ppf "(" ;
      module_expr ppf me2;
      fprintf ppf ")" ;
      pp_close_box ppf ();
  | Pmod_unpack e ->
      fprintf ppf "(val@ ";
      pp_open_hovbox ppf indent;
      expression ppf e;
      pp_close_box ppf ();
      fprintf ppf ")";

and structure ppf x = pp_print_list ~sep_action:pp_print_newline structure_item ppf x ;

(* closes one box *)
and text_x_modtype_x_module ppf (s, mt, me) =
(*
  (match me.pmod_desc with
   | Pmod_constraint (me, ({pmty_desc=(Pmty_ident (_)
        | Pmty_signature (_))} as mt)) ->
       (* assert false ; *) (* 3.07 - should this ever happen here? *)
       fprintf ppf "%s :@ " s ;
       module_type ppf mt ;
       fprintf ppf " =" ;
       pp_close_box ppf () ;
       pp_print_space ppf () ;
       module_expr ppf me ;
   | _ ->
*)
  fprintf ppf "%s :@ " s.txt;
  module_type ppf mt ;
  fprintf ppf " =" ;
  pp_close_box ppf () ;
  pp_print_space ppf () ;
  module_expr ppf me ;
(*  ) ; *)

(*
(* net gain of one box (-1, +2) *)
and string_x_modtype_x_module_list ppf l =
  match l with
  | [] -> ()
  | hd :: tl ->
      pp_close_box ppf () ;
      pp_print_space ppf () ;
      pp_open_hvbox ppf indent ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "and " ;
      string_x_modtype_x_module ppf hd; (* closes a box *)
      string_x_modtype_x_module_list ppf tl ; (* net open of one box *)
*)

(* net gain of one box (-1, +2) *)
and text_x_modtype_x_module_list ppf l =
  match l with
  | [] -> ()
  | hd :: tl ->
      pp_close_box ppf () ;
      pp_print_space ppf () ;
      pp_open_hvbox ppf indent ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "and " ;
      text_x_modtype_x_module ppf hd; (* closes a box *)
      text_x_modtype_x_module_list ppf tl ; (* net open of one box *)

(* context: [hv [hov .]]  returns [hv .]
   closes inner hov box. *)
and string_x_module_type_list ppf ?(first=true) l =
  match l with
  | [] -> () ;
  | hd :: tl ->
      if (first=false) then begin
          pp_print_space ppf () ;
          pp_open_hovbox ppf indent ;
          fprintf ppf "and " ;
        end ;
      string_x_module_type ppf hd ;
      pp_close_box ppf () ;
      string_x_module_type_list ppf ~first:false tl ;

and string_x_module_type ppf (s, mty) =
  fprintf ppf "%s :@ " s.txt ;
  module_type ppf mty ;

and structure_item ppf x =
  begin
    match x.pstr_desc with
    | Pstr_eval (e) ->
        pp_open_hvbox ppf 0 ;
        fprintf ppf "let _ = " ;
        expression_sequence ppf ~first:false ~indent:0 e ;
        pp_close_box ppf () ;
    | Pstr_type [] -> assert false
    | Pstr_type (first :: rest) ->
        pp_open_vbox ppf 0;
        pp_open_hvbox ppf 0;
        fprintf ppf "type " ;
        string_x_type_declaration ppf first;
        pp_close_box ppf ();
        type_def_list_helper ppf rest;
        pp_close_box ppf ();
    | Pstr_value (rf, l) ->
        let l1 = (List.hd l) in
        let l2 = (List.tl l) in
        pp_open_hvbox ppf 0 ;
        pp_open_hvbox ppf indent ;
        fprintf ppf "let%a " fmt_rec_flag rf;
        pattern_x_expression_def ppf l1;
        pattern_x_expression_def_list ppf l2;
        pp_close_box ppf () ;
        pp_close_box ppf () ;
    | Pstr_exception (s, ed) ->
        pp_open_hovbox ppf indent ;
        fprintf ppf "exception@ %s" s.txt;
        exception_declaration ppf ed;
        pp_close_box ppf () ;
    | Pstr_module (s, me) ->
        pp_open_hvbox ppf indent;
        pp_open_hovbox ppf indent ;
        fprintf ppf "module %s" s.txt ;
        (match me.pmod_desc with
          | Pmod_constraint (me, ({pmty_desc=(Pmty_ident (_)
                  | Pmty_signature (_));_} as mt)) ->
              fprintf ppf " :@ " ;
              module_type ppf mt ;
              fprintf ppf " =" ;
              pp_close_box ppf () ;
              pp_print_space ppf () ;
              module_expr ppf me ;
          | _ ->
              fprintf ppf " =" ;
              pp_close_box ppf () ;
              pp_print_space ppf () ;
              module_expr ppf me ;
        ) ;
        pp_close_box ppf ();
    | Pstr_open (li) ->
        fprintf ppf "open %a" fmt_longident li;
    | Pstr_modtype (s, mt) ->
        pp_open_hovbox ppf indent;
        fprintf ppf "module type %s =@ " s.txt;
        module_type ppf mt;
        pp_close_box ppf () ; (* bug fixed? *)
    | Pstr_class (l) ->
        class_declaration_list ppf l;
    | Pstr_class_type (l) ->
        class_type_declaration_list ppf l ;
    | Pstr_primitive (s, vd) ->
        pp_open_hovbox ppf indent ;
        let need_parens =
          match s.txt with
          | "or"
          | "mod"
          | "land"
          | "lor"
          | "lxor"
          | "lsl"
          | "lsr"
          | "asr"
            -> true

          | _ ->
              match s.txt.[0] with
                'a'..'z' -> false
              | _ -> true
        in
        if need_parens then
          fprintf ppf "external@ ( %s ) :@ " s.txt
        else
          fprintf ppf "external@ %s :@ " s.txt;
        value_description ppf vd;
        pp_close_box ppf () ;
    | Pstr_include me ->
        pp_open_hovbox ppf indent ;
        fprintf ppf "include " ;
        module_expr ppf me ;
        pp_close_box ppf () ;
    | Pstr_exn_rebind (s, li) ->        (* todo: check this *)
        pp_open_hovbox ppf indent ;
        fprintf ppf "exception@ %s =@ %a" s.txt fmt_longident li ;
        pp_close_box ppf () ;
    | Pstr_recmodule decls -> (* 3.07 *)
        let l1 = (List.hd decls) in
        let l2 = (List.tl decls) in
        pp_open_hvbox ppf 0;        (* whole recmodule box *)
        pp_open_hvbox ppf indent ;  (* this definition box *)
        pp_open_hovbox ppf indent ; (* first line box *)
        fprintf ppf "module rec " ;
        text_x_modtype_x_module ppf l1; (* closes a box *)
        text_x_modtype_x_module_list ppf l2; (* net opens one box *)
        pp_close_box ppf () ;
        pp_close_box ppf () ;
        pp_close_box ppf () ;
  end;


and type_def_list_helper ppf l =
  match l with
  | [] -> ()
  | first :: rest ->
      pp_print_space ppf () ;
      pp_open_hovbox ppf indent ;
      fprintf ppf "and " ;
      string_x_type_declaration ppf first;
      pp_close_box ppf () ;
      type_def_list_helper ppf rest ;

and string_x_type_declaration ppf (s, td) =
  let l = td.ptype_params in
  (match (List.length l) with
    | 0 -> ()
    | 1 ->
        (* list2 type_var_option_print ppf l "" ; *)
        pp_print_list type_var_option_print ppf l ;
        fprintf ppf " " ;
    | _ -> pp_open_hovbox ppf indent ;
        fprintf ppf "(" ;
        (* list2 type_var_option_print ppf l "," ; *)
        pp_print_list type_var_option_print ppf l ~sep:",";
        fprintf ppf ")" ;
        pp_close_box ppf ();
        fprintf ppf " " ;
  );
  fprintf ppf "%s" s.txt ;
  (match (td.ptype_kind, td.ptype_manifest) with
    | Ptype_abstract, None -> ()
    | Ptype_record _, _ -> fprintf ppf " = " ;
    | _ , _ -> fprintf ppf " =" ;
        pp_print_break ppf 1 indent ;
  );
  type_declaration ppf td;

and longident_x_with_constraint_list ?(first=true) ppf l =
  match l with
  | [] -> () ;
  | h :: [] ->
      if (first = false) then fprintf ppf "@ and " ;
      longident_x_with_constraint ppf h ;
  | h :: t  ->
      if (first = false) then fprintf ppf "@ and " ;
      longident_x_with_constraint ppf h ;
      (* fprintf ppf "@ and " ; *) (* duplicated here *)
      (* longident_x_with_constraint ppf h ; *)
      longident_x_with_constraint_list ~first:false ppf t;

and string_x_core_type_ands ?(first=true) ppf l =
  match l with
  | [] -> () ;
  | h :: [] ->
      if (first = false) then fprintf ppf "@ and " ;
      string_x_core_type ppf h ;
  | h :: t  ->
      if (first = false) then fprintf ppf "@ and " ;
      string_x_core_type ppf h;
      string_x_core_type_ands ~first:false ppf t;

and string_x_core_type ppf (s, ct) =
  fprintf ppf "type %a@ =@ %a" fmt_longident s core_type ct (* bug fix *)

and longident_x_with_constraint ppf (li, wc) =
  match wc with
  | Pwith_type ({ptype_params= ls ;_} as td) ->
      fprintf ppf "type@ %a %a =@ "
        (fun ppf ls ->
          let len = List.length ls in
          if len >= 2 then begin
            fprintf ppf "(";
            (* list2 type_var_option_print ppf ls ","; *)
            pp_print_list type_var_option_print ppf ls ~sep:";";
            fprintf ppf ")";
           end
        else
            (* list2 type_var_option_print ppf ls ","; *)
            pp_print_list type_var_option_print ppf ls ~sep:",";
        ) ls
        fmt_longident li;
      type_declaration ppf td ;
  | Pwith_module (li2) ->
      fprintf ppf "module %a =@ %a" fmt_longident li fmt_longident li2;
  | Pwith_typesubst ({ptype_params=ls;_} as td) -> (* bug fix *)
      (* fprintf ppf "type@ %a :=@ " fmt_longident li; *)
      fprintf ppf "type@ %a %a :=@ "
        (fun ppf ls ->
          let len = List.length ls in
          if len >= 2 then begin
            fprintf ppf "(";
            (* list2 type_var_option_print ppf ls ","; *)
            pp_print_list type_var_option_print ppf ls ~sep:",";
            fprintf ppf ")";
           end
        else
            (* list2 type_var_option_print ppf ls ","; *)
            pp_print_list type_var_option_print ppf ls ~sep:",";
        ) ls fmt_longident li;
      type_declaration ppf td ;
  | Pwith_modsubst (li2) ->
      fprintf ppf "module %a :=@ %a" fmt_longident li fmt_longident li2;

and typedef_constraint ppf (ct1, ct2, _l) =
  pp_open_hovbox ppf indent ;
  fprintf ppf "constraint@ " ;
  core_type ppf ct1;
  fprintf ppf " =@ " ;
  core_type ppf ct2;
  pp_close_box ppf () ;

and type_variant_leaf ppf (s, l,_, _) first = (* TODO *)
  if (first) then begin
      pp_print_if_newline ppf ();
      pp_print_string ppf "  ";
    end else begin
      pp_print_space ppf ();
      fprintf ppf "| " ;
    end ;
  pp_open_hovbox ppf indent ;
  fprintf ppf "%s" s.txt ;
  if ((List.length l) > 0) then begin
    fprintf ppf "@ of@ " ;
      (* list2 core_type ppf l " *" *)
    pp_print_list core_type ppf l ~sep:"*";
    end ;
  pp_close_box ppf ();

and type_variant_leaf_list ppf list =
  match list with
  | [] -> ()
  | first :: rest ->
      type_variant_leaf ppf first false ;
      type_variant_leaf_list ppf rest ;

and type_record_field ppf (s, mf, ct,_) =
  pp_open_hovbox ppf indent ;
  fprintf ppf "%a%s:" fmt_mutable_flag mf s.txt ;
  core_type ppf ct ;
  pp_close_box ppf () ;

and longident_x_pattern ppf (li, p) =
  pp_open_hovbox ppf indent ;
  fprintf ppf "%a =@ " fmt_longident li;
  pattern ppf p;
  pp_close_box ppf () ;



and pattern_x_expression_case_list
    ppf ?(first:bool=true) ?(special_first_case=bar_on_first_case)
  (l:(pattern * expression) list) =
  match l with
  | []        -> ()
  | (p,e)::[] -> (* last time *)
      if (first=false) then
        fprintf ppf "| " ;
      pp_open_hvbox ppf indent ;
      let (e,w) =
        (match e with
          | {pexp_desc = Pexp_when (e1, e2);_} -> (e2, Some (e1))
          | _ -> (e, None)) in
      pattern_with_when ppf w p ;
      fprintf ppf " ->@ " ;
      pp_open_hvbox ppf 0 ;
      expression_sequence ppf ~indent:0 e ;
      pp_close_box ppf () ;
      pp_close_box ppf () ;
  | (p,e)::r  -> (* not last  *)
      pp_open_hvbox ppf (indent + 2) ;
      if ((first=true) && (special_first_case=false)) then begin (* fix the convention*)
          pp_print_if_newline ppf () ;
          pp_print_string ppf "  "
        end else
        fprintf ppf "| " ;
      let (e,w) =
        (match e with
          | {pexp_desc = Pexp_when (e1, e2);_} -> (e2, Some (e1))
          | _ -> (e, None)) in
      pattern_with_when ppf w p ;
      fprintf ppf " ->@ " ;
      pp_open_hvbox ppf 0 ;
      expression_sequence ppf ~indent:0 e ;
      pp_close_box ppf () ;
      pp_close_box ppf () ;
      pp_print_break ppf 1 0;
      (pattern_x_expression_case_list ppf ~first:false r);

and pattern_x_expression_def ppf (p, e) =
  pattern ppf p ;
  fprintf ppf " =@ " ;
  expression ppf e;

and pattern_list_helper ppf p =
  match p with
  | {ppat_desc = Ppat_construct ({ txt = Longident.Lident("::") ;_},
        Some ({ppat_desc = Ppat_tuple([pat1; pat2]);_}),
        _);_}
    -> pattern ppf pat1 ;
      fprintf ppf "@ ::@ " ;
      pattern_list_helper ppf pat2 ;
  | _ -> pattern ppf p ;

and string_x_expression ppf (s, e) =
  pp_open_hovbox ppf indent ;
  fprintf ppf "%s =@ " s.txt ;
  expression ppf e ;
  pp_close_box ppf () ;

and longident_x_expression ppf (li, e) =
  pp_open_hovbox ppf indent ;
  fprintf ppf "%a =@ " fmt_longident li;
  simple_expr ppf e;
  pp_close_box ppf () ;

and label_x_expression_param ppf (l,e) =
  match l with
  | ""  -> simple_expr ppf e ;
  | lbl ->
      if ((String.get lbl 0) = '?') then begin
          fprintf ppf "%s:" lbl ;
          simple_expr ppf e ;
        end else begin
          fprintf ppf "~%s:" lbl ;
          simple_expr ppf e ;
        end ;

and expression_in_parens ppf e =
  let already_has_parens =
    (match e.pexp_desc with
        Pexp_apply ({pexp_desc=Pexp_ident ({ txt = Longident.Ldot (
                Longident.Lident(modname), funname) ;_});_},_)
        -> (match modname,funname with
            | "Array","get" -> false;
            | "Array","set" -> false;
            | _,_ -> true) ;
      | Pexp_apply ({pexp_desc=Pexp_ident ({ txt = Longident.Lident(funname) ;_});_},_)
        -> (match funname with
            | "!" -> false;
            | _ -> true);
      | Pexp_apply (_,_) -> true;
      | Pexp_match (_,_) -> true;
      | Pexp_tuple (_) -> true ;
      | Pexp_constraint (_,_,_) -> true ;
      | _ -> false) in
  if (already_has_parens) then expression ppf e
  else begin
      fprintf ppf "(" ;
      expression ppf e ;
      fprintf ppf ")" ;
    end ;

and pattern_in_parens ppf p =
  let already_has_parens =
    match p.ppat_desc with
    | Ppat_alias (_,_) -> true
    | Ppat_tuple (_) -> true
    | Ppat_or (_,_) -> true
    | Ppat_constraint (_,_) -> true
    | _ -> false in
  if (already_has_parens) then pattern ppf p
  else begin
      fprintf ppf "(" ;
      pattern ppf p ;
      fprintf ppf ")" ;
    end;

and pattern_constr_params_option ppf po =
  match po with
  | None -> ();
  | Some pat ->
      pp_print_space ppf ();
      pattern_in_parens ppf pat;

and type_variant_helper ppf x =
  match x with
  | Rtag (l, _b, ctl) ->  (* is b important? *)
      pp_open_hovbox ppf indent ;
      fprintf ppf "`%s" l ;
      if ((List.length ctl) != 0) then begin
          fprintf ppf " of@ " ;
          (* list2 core_type ppf ctl " *" ; *)
        pp_print_list core_type ppf ctl ~sep:"*";
        end ;
      pp_close_box ppf () ;
  | Rinherit (ct) ->
      core_type ppf ct

(* prints a list of definitions as found in a let statement
   note! breaks "open and close boxes in same function" convention, however
         does always open and close the same number of boxes. (i.e. no "net
         gain or loss" of box depth.                                         *)
and pattern_x_expression_def_list ppf l =
  match l with
  | [] -> ()
  | hd :: tl ->
      pp_close_box ppf () ;
      pp_print_space ppf () ;
      pp_open_hvbox ppf indent ;
      fprintf ppf "and " ;
      pattern_x_expression_def ppf hd;
      pattern_x_expression_def_list ppf tl ;

(* end an if statement by printing an else phrase if there is an "else"
   statement in the ast. otherwise just close the box. *)
(* added: special case for "else if" case *)

and expression_eo ppf eo extra =
  match eo with
  | None   -> ();
  | Some x ->
      if extra then fprintf ppf " "
      else fprintf ppf "@ " ;
      match x.pexp_desc with
      | Pexp_ifthenelse (e1, e2, eo) ->   (* ... else if ...*)
          fprintf ppf "else" ;
          expression_elseif ppf (e1, e2, eo)
      | Pexp_sequence (_e1, _e2) ->
          fprintf ppf "else" ;
          expression_ifbegin ppf x;       (* ... else begin ... end*)
      | _ ->                              (* ... else ... *)
          pp_open_hvbox ppf indent ;
          fprintf ppf "else@ " ;
          expression ppf x ;
          pp_close_box ppf () ;

and expression_elseif ppf (e1,e2,eo) =
  fprintf ppf " " ;
  expression_if_common ppf e1 e2 eo ;

and expression_ifbegin ppf e =
  fprintf ppf " begin";
  pp_print_break ppf 1 indent ; (* "@;<1 2>"; *)
  expression_sequence ppf e;
  pp_print_break ppf 1 0 ; (* fprintf ppf "@;<1 0>" *)
  fprintf ppf "end";

and expression_if_common ppf e1 e2 eo =
  match eo, e2.pexp_desc with
  | None, Pexp_sequence (_, _) ->
      fprintf ppf "if@ " ;
      expression ppf e1;
      fprintf ppf "@ then@ " ;
      expression_ifbegin ppf e2
  | None, _ ->
      fprintf ppf "if@ " ;
      expression ppf e1;
      fprintf ppf "@ then@ " ;
      simple_expr ppf e2
  | Some _, Pexp_sequence _ ->
      fprintf ppf "if " ;
      expression ppf e1;
      fprintf ppf "@ then@ " ;
      expression_ifbegin ppf e2;
      expression_eo ppf eo true;   (* ... then begin ... end *)
  | Some _, _ ->
      pp_open_hvbox ppf indent ;
      fprintf ppf "if " ;
      expression ppf e1;
      fprintf ppf " then@ " ;
      simple_expr ppf e2;
      pp_close_box ppf () ;
      expression_eo ppf eo false;

and expression_sequence ppf ?(skip=1) ?(indent=indent) ?(first=true) expr =
  if (first = true) then begin
    pp_open_hvbox ppf 0 ;
    expression_sequence ppf ~skip:skip ~indent:0 ~first:false expr ;
    pp_close_box ppf () ;
  end else
    match expr.pexp_desc with
    | Pexp_sequence (e1, e2) ->
         simple_expr ppf e1 ;
         fprintf ppf ";" ;
         pp_print_break ppf skip indent ; (* "@;<1 2>" ; *)
         expression_sequence ppf ~skip:skip ~indent:indent ~first:false e2 ;
    | _ ->
         expression ppf expr ;

and expression_list_helper ppf exp =
  match exp with
  | {pexp_desc = Pexp_construct ({ txt = Longident.Lident("[]") ;_}, None, _);_}
     -> () ;
  | {pexp_desc = Pexp_construct ({ txt = Longident.Lident("::") ;_},
                   Some({pexp_desc = Pexp_tuple([exp1 ; exp2]);_}), _);_}
     -> fprintf ppf ";@ " ;
        simple_expr ppf exp1 ;
        expression_list_helper ppf exp2 ;
  | _ -> assert false;

and expression_list_nonterminal ppf exp =
  match exp with
  | {pexp_desc = Pexp_construct ({ txt = Longident.Lident("[]") ;_}, None, _);_}
     -> fprintf ppf "[]" ; (* assert false; *)
  | {pexp_desc = Pexp_construct ({ txt = Longident.Lident("::") ;_},
                   Some({pexp_desc = Pexp_tuple([exp1 ; exp2]);_}), _);_}
     -> simple_expr ppf exp1;
        fprintf ppf " ::@ ";
        expression_list_nonterminal ppf exp2;
  | _ -> expression ppf exp;
;

and directive_argument ppf x =
  match x with
  | Pdir_none -> ()
  | Pdir_string (s) -> fprintf ppf "@ \"%s\"" s;
  | Pdir_int (i) -> fprintf ppf "@ %d" i;
  | Pdir_ident (li) -> fprintf ppf "@ %a" fmt_longident_aux li;
  | Pdir_bool (b) -> fprintf ppf "@ %s" (string_of_bool b);



and pattern_x_expression_case_single ppf (p, e) eo lbl =
  (match eo with
     None ->   pattern_with_label ppf p lbl
    | Some x ->
        fprintf ppf "?" ;
        pp_open_hovbox ppf indent ;
        fprintf ppf "(" ;
        begin
          match p.ppat_desc with
            Ppat_constraint ({ ppat_desc = Ppat_var s ;_}, ct) ->
              fprintf ppf "%s@ :@ %a" s.txt core_type ct
          | Ppat_var s ->
              fprintf ppf "%s" s.txt
          | _ -> assert false
        end;
        fprintf ppf " =@ " ;
        expression ppf x ;
        fprintf ppf ")" ;
        pp_close_box ppf ()
  ) ;
  fprintf ppf " ->@ " ;
  expression_sequence ppf ~indent:0 e ;;

let toplevel_phrase ppf x =
  match x with
  | Ptop_def (s) ->
   pp_open_hvbox ppf 0;
      (* list2 structure_item ppf s ~breakfirst:false ~indent:0 ""; *)
   pp_print_list structure_item ppf s ~breakfirst:false ~indent:0 ;
   pp_close_box ppf ();
  | Ptop_dir (s, da) ->
      pp_open_hovbox ppf indent;
      fprintf ppf "#%s" s;
      directive_argument ppf da;
      pp_close_box ppf () ;;

let expression ppf x =
  fprintf ppf "@[";
  expression ppf x;
  fprintf ppf "@]";;

let string_of_expression x =
  ignore (flush_str_formatter ()) ;
  let ppf = str_formatter in
  expression ppf x ;
  flush_str_formatter () ;;

let top_phrase ppf x =
  pp_print_newline ppf () ;
  toplevel_phrase ppf x;
  fprintf ppf ";;" ;
  pp_print_newline ppf ();;
