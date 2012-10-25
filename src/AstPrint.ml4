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
(* Rewrite: Hongbo Zhang: University of Pennsylvania*)

(* FIXME what kind of error check should we do ?,
   Location for error message
 *)

(*
  label:
        | Ptyp_arrow of label * core_type * core_type
        | Pcty_fun of label * core_type * class_type
        | Pcl_fun of label * expression option * pattern * class_expr
        | Pexp_function of label * expression option * (pattern * expression) list
 *)
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
let infix_symbols = [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%' ] 
let operator_chars = [ '!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/';
                       ':'; '<'; '='; '>'; '?'; '@'; '^'; '|'; '~' ] 
let numeric_chars  = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] 

type fixity = Infix| Prefix 

let is_infix  = function 
  | Infix  -> true
  | Prefix -> false 

let special_infix_strings =
  ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!=" ] 

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let fixity_of_string s =
  if ((List.mem s special_infix_strings) || (List.mem  s.[0] infix_symbols)) then
    Infix
  else Prefix

let fixity_of_longident = function
  | {txt=Lident name;_} -> fixity_of_string name
  | _ -> Prefix 

let fixity_of_exp = function 
  | {pexp_desc = Pexp_ident li;_} -> fixity_of_longident li
  | _ -> Prefix ;;

(* 
   for empty list,Nothing was done
   for singleton list, simply print it 
   for list whose length is more than 1, we decorate first, last, sep 
    *)
let pp_print_list
    ?sep:(sep:('a,'b,'c)format option)
    ?first:(first:('a,'b,'c)format option)
    ?last:(last:('a,'b,'c)format option)
    f ppf xs =
  let first = match first with Some x -> x |None -> ""
  and last = match last with Some x -> x |None -> ""
  and sep = match sep with Some x -> x |None -> "@ " in
  let aux  ppf  = function
    | [] -> ()
    | [x] -> f ppf x
    | xs ->
        let rec loop  ppf = function
          | [x] -> f ppf x ;
          | x::xs -> begin
              fprintf ppf ("%a"^^sep) f x ;
              loop  ppf xs
          end
          | _ -> assert false in begin 
              fprintf ppf first;
              loop ppf xs;
              fprintf ppf last;
          end in
  aux ppf xs 

(*
  for none, it's a nil operation
 *)    
let pp_print_option
    ?first:(first:('a,'b,'c)format option)
    ?last:(last:('a,'b,'c)format option)
    f ppf a =
      let first = match first with Some x -> x | None -> ""
      and last = match last with Some x -> x | None -> "" in
      match a with
      | None -> ()
      | Some x -> begin
          fprintf ppf first;
          f ppf x ;
          fprintf ppf last;  (* fprintf ppf (first^^"%a"^^last) f x *)
      end
let paren b f ppf x =
  if b then fprintf ppf "(%a)" f  x
  else f ppf x

      
let rec longident ppf  =function
  | Lident s -> fprintf ppf "%s" s
  | Ldot(y,s) -> (match s.[0] with
    | 'a'..'z' | 'A' .. 'Z' ->
        fprintf ppf "%a.%s" longident y s
    | _ ->
        fprintf ppf "%a.(@ %s@ )@ " longident y s)
  | Lapply (y,s)->
        fprintf ppf "%a(%a)" longident y longident s

let longident_loc ppf x = fprintf ppf "%a" longident x.txt;;

(** no trailing space  *)
let constant ppf  = function
  | Const_int i -> fprintf ppf "%d" i
  | Const_char i -> fprintf ppf "%C"  i 
  | Const_string i -> fprintf ppf "%S" i
  | Const_float  i -> fprintf ppf "%s" i 
  | Const_int32 i -> fprintf ppf "%ldl" i
  | Const_int64 i -> fprintf ppf "%LdL" i
  | Const_nativeint i -> fprintf ppf "%ndn" i


let mutable_flag ppf   = function
  | Immutable -> ()
  | Mutable -> fprintf ppf "mutable@ "
let  virtual_flag ppf  = function
  | Concrete -> ()
  | Virtual -> fprintf ppf "virtual@ "
let rec_flag ppf = function
  | Nonrecursive -> ()
  | Recursive | Default -> fprintf ppf "rec@ "
let direction_flag ppf = function
  | Upto -> fprintf ppf "to@ "
  | Downto -> fprintf ppf "downto@ "
let private_flag ppf = function
  | Public -> ()
  | Private -> fprintf ppf "private@ "

let constant_string ppf s = fprintf ppf "%S" s 
let pp_print_tyvar ppf str = fprintf ppf "'%s@ " str ;;

let pp_print_type_var_option ppf str = 
  match str with
  |  None -> () 
  | Some {txt;_} -> pp_print_tyvar ppf txt ;;

let class_params_def ppf   = function
  | [] -> ()
  | l ->  
        fprintf ppf "[%a]"
        (pp_print_list (fun ppf {txt;_} -> pp_print_tyvar ppf txt) ~sep:",") l 

let is_predef_option = function
  | (Ldot (Lident "*predef*","option")) -> true
  | _ -> false
        
let pp_print_string_quot ppf x =
  fprintf ppf "`%s" x 
(*********************************************************************************)
(***************core_type*********************************************************)
let rec print_type_with_label ppf (label,({ptyp_desc;_}as c) ) =
  match label with
  | "" ->  core_type ppf c
  | s  ->
    if s.[0]='?' then 
      match ptyp_desc with
      | Ptyp_constr ({txt;_}, l) -> begin 
          assert (is_predef_option txt);
          fprintf ppf "%s:%a" s (pp_print_list core_type) l 
      end
      | _ -> failwith "invalid input in print_type_with_label"
    else
      fprintf ppf "%s:%a" s core_type c

and core_type ppf ({ptyp_desc;_}:Parsetree.core_type) =
  match ptyp_desc with
  | Ptyp_any -> fprintf ppf "_";       
  | Ptyp_var s -> pp_print_tyvar ppf  s; 
  | Ptyp_arrow (l, ct1, ct2) ->
      fprintf ppf "@[<hov2>(%a@ ->@ %a)@ @]" (* FIXME remove parens later *)
        print_type_with_label (l,ct1) core_type ct2
  | Ptyp_tuple l ->
      fprintf ppf "@[<hov2>(%a)@]" (pp_print_list core_type ~sep:"*") l ;
  | Ptyp_constr (li, l) ->
      fprintf ppf "%a@ %a@ " 
        (pp_print_list ~first:"(" ~last:")" core_type ~sep:",")  l longident_loc li
  | Ptyp_variant (l, closed, low) ->
     let type_variant_helper ppf x =
       match x with
       | Rtag (l, _, ctl) -> fprintf ppf "@[<hov2>%a%a@]"  pp_print_string_quot l
             (fun ppf l -> match l with
             |[] -> ()
             | _ -> fprintf ppf "@ of@ %a"
                   (pp_print_list core_type ~sep:"&")  ctl) ctl
       | Rinherit ct -> core_type ppf ct in 
     fprintf ppf "@[<hov2>[%a%a]@]"
       (fun ppf l -> match l with
       | [] -> ()
       | _ -> fprintf ppf "%s@ %a"
             (match (closed,low) with
             | (true,None) -> ""
             | (true,Some _) -> ""
             | (false,_) -> ">") 
             (pp_print_list type_variant_helper ~sep:"|") l) l 
       (fun ppf low -> match low with
        |Some [] |None -> ()  
        |Some xs ->
            fprintf ppf ">@ %a"
            (pp_print_list pp_print_string_quot) xs) low
  | Ptyp_object l ->
      let  core_field_type ppf {pfield_desc;_} =
        match pfield_desc with
        | Pfield (s, ct) ->
            fprintf ppf "@[<hov2>%s@ :%a@ @]" s core_type ct 
        | Pfield_var ->
            fprintf ppf ".." in
      fprintf ppf "@[<hov2><@ %a@ >@]" (pp_print_list core_field_type ~sep:";") l
  | Ptyp_class (li, l, low) ->   (*FIXME*)
      fprintf ppf "@[<hov2>%a#%a%a@]"
        (pp_print_list core_type ~sep:"," ~first:"(" ~last:")") l
        longident_loc li
        (fun ppf low -> match low with
        | [] -> ()
        | _ -> fprintf ppf "@ [>@ %a]" (pp_print_list pp_print_string_quot) low) low
  | Ptyp_alias (ct, s) ->               
      fprintf ppf "@[<hov2>(%a @ as@ '%s)@]" core_type ct s
  | Ptyp_poly (sl, ct) ->   
      fprintf ppf "@[<hov2>%a%a@]"
        (fun ppf l ->
          fprintf ppf "%a"
            (fun ppf l ->
              match l with
              | [] -> ()
              | _ ->
                  fprintf ppf "%a@ .@ "
                    (pp_print_list pp_print_tyvar ~sep:"")  l) (List.rev l)) sl  core_type ct 
  | Ptyp_package (lid, cstrs) ->
      let aux ppf (s, ct) =
        fprintf ppf "type %a@ =@ %a" longident_loc s core_type ct  in
      match cstrs with
      |[] -> fprintf ppf "@[<hov2>(module@ %a)@]" longident_loc lid
      |_ ->  
          fprintf ppf "@[<hov2>(module@ %a@ with@ %a)@]" longident_loc lid
            (pp_print_list aux  ~sep:"@ and@ ")  cstrs


(*********************************************************************************)
(****************************pattern**********************************************)        

and pattern ppf x =
  let rec pattern_list_helper ppf  = function
    | {ppat_desc =
       Ppat_construct
         ({ txt = Lident("::") ;_},
          Some ({ppat_desc = Ppat_tuple([pat1; pat2]);_}),
          _);_} ->
        fprintf ppf "%a::%a"
          pattern  pat1
          pattern_list_helper pat2
    | p -> pattern ppf p in
  let rec pattern_or_helper  cur = function
    |{ppat_desc = Ppat_constant (Const_char a);_}
        -> 
          if  Char.code a = Char.code cur + 1 then
            Some a
          else None
    |{ppat_desc =
      Ppat_or({ppat_desc=Ppat_constant (Const_char a);_}, p2);_} -> 
        if Char.code a = Char.code cur + 1 then
          pattern_or_helper a p2
        else None
    | _ -> None in 
  match x.ppat_desc with
    | Ppat_construct (({txt;_} as li), po, _) -> (* FIXME The third field always false *)
        if txt = Lident "::" then
          fprintf ppf "%a" pattern_list_helper x
        else
          (match po with
          |Some x ->
              fprintf ppf "%a%a"
                longident_loc li
                pattern_in_parens x
          | None -> fprintf ppf "%a@ "longident_loc li )
    | Ppat_any -> fprintf ppf "_";           
    | Ppat_var ({txt = txt;_}) ->
        if (is_infix (fixity_of_string txt)) || List.mem txt.[0] prefix_symbols then
          if txt.[0]='*' then
            fprintf ppf "(@ %s@ )@ " txt
          else
            fprintf ppf "(%s)" txt 
        else
          fprintf ppf "%s" txt;
    | Ppat_alias (p, s) -> fprintf ppf "@[<hov2>(%a@ as@ %s)@]"  pattern p  s.txt
    | Ppat_constant (c) -> fprintf ppf "%a" constant c;
    | Ppat_tuple (l) ->                      
        fprintf ppf "@[<hov1>(%a)@]"
          (pp_print_list  ~sep:"," pattern)  l;
    | Ppat_variant (l, po) ->
        (match po with
        | None ->
            fprintf ppf "`%s" l;
        | Some (p) ->
            fprintf ppf "@[<hov2>(`%s@ %a)@]" l pattern p)
    | Ppat_record (l, closed) ->
        let longident_x_pattern ppf (li, p) =
          fprintf ppf "@[<hov2>%a@ =@ %a@]" longident_loc li pattern p in
        (match closed with
        |Closed -> 
            fprintf ppf "@[<hov2>{%a}@]"
              (pp_print_list longident_x_pattern ~sep:";") l
        | _ -> 
            fprintf ppf "@[<hov2>{%a;_}@]"
              (pp_print_list longident_x_pattern ~sep:";") l)
    | Ppat_array l ->
        fprintf ppf "@[<hov2>[|%a|]@]"  (pp_print_list pattern ~sep:";") l 
    | Ppat_or (p1, p2) ->
        (match p1 with
        | {ppat_desc=Ppat_constant (Const_char a);_} -> begin 
            match pattern_or_helper a p2 with
            |Some b -> fprintf ppf "@[<hov2>(%C..%C)@]" a b 
            |None ->   fprintf ppf "@[<hov2>(%a@ |%a)@]"  pattern p1  pattern p2 end
        | _ -> fprintf ppf "@[<hov2>(%a@ |%a)@]"  pattern p1  pattern p2 )
    | Ppat_constraint (p, ct) ->
        fprintf ppf "@[<hov2>(%a@ :@ %a)@]" pattern p core_type ct 
    | Ppat_type li ->
        fprintf ppf "#%a" longident_loc li 
    | Ppat_lazy p ->
        fprintf ppf "@[<hov2>(lazy@ %a)@]" pattern p 
    | Ppat_unpack (s) ->
        fprintf ppf "(module@ %s)@ " s.txt
          
and simple_expr ppf x =
  match x.pexp_desc with
  | Pexp_construct (li, None, _) ->
      fprintf ppf "%a@ " longident_loc li
  | Pexp_ident (li) -> 
      let flag = is_infix (fixity_of_longident li)
        || match li.txt with
          | Lident (li) -> List.mem li.[0] prefix_symbols
          | _ -> false in 
        paren flag longident_loc ppf li 
  | Pexp_constant (c) -> fprintf ppf "%a" constant c;
  | Pexp_pack (me) ->
      fprintf ppf "(module@ %a)"  module_expr me
  | Pexp_newtype (lid, e) ->
      fprintf ppf "fun@ (type@ %s)@ ->@ %a"  lid  expression  e
  | Pexp_tuple (l) ->
      fprintf ppf "@[<hov 1>(%a)@]"  (pp_print_list simple_expr  ~sep:",")  l
  | Pexp_variant (l, eo) ->
      fprintf ppf "`%s%a" l (pp_print_option ~first:"@ (" ~last:")" expression) eo
  | Pexp_record (l, eo) ->
      let longident_x_expression ppf (li, e) =
        fprintf ppf "@[<hov2>%a@ =@ %a@]" longident_loc li simple_expr e in 
      fprintf ppf "@[<hov2>{%a%a}@]"
        (pp_print_option ~last:"@ with@ " expression) eo
        (pp_print_list longident_x_expression ~sep:";")  l
  | Pexp_array (l) ->
      fprintf ppf "@[<hov2>[|%a|]@]"
      (pp_print_list simple_expr ~sep:";") l
  | Pexp_while (e1, e2) ->
      fprintf ppf "@[<hov2>while@ %a@ do@ %a@ done@]"
        expression e1 expression e2 
  | Pexp_for (s, e1, e2, df, e3) ->
      fprintf ppf "@[<hov2>for@ %s@ =@ %a@ %a@ %a@ do@ %a@ done@]"
        s.txt  expression e1  direction_flag df expression e2 
        expression e3
  | _ -> (* complex case delegated to expression *)
      fprintf ppf "(@ %a@ )" expression x 
        
and pp_print_label_exp ppf (l,opt,p) =
  if l = "" then
    fprintf ppf "(%a)@ " pattern p (*single case pattern parens needed here *)
  else
    if l.[0] = '?' then 
      let len = String.length l - 1 in 
      let rest = String.sub l 1 len in begin
        match p.ppat_desc with
        | Ppat_var {txt;_} when txt = rest ->
              (match opt with
              |Some o -> fprintf ppf "?(%s=%a)@ " rest  expression o
              | None -> fprintf ppf "?%s@ " rest)
        | _ -> (match opt with
          | Some o -> fprintf ppf "%s:(%a=%a)@ " l pattern p expression o
          | None -> fprintf ppf "%s:(%a)@ " l pattern p  )
      end
    else
      (match p.ppat_desc with
      | Ppat_var {txt;_} when txt = l ->
          fprintf ppf "~%s@ " l 
      | _ ->  fprintf ppf "~%s:(%a)@ " l pattern p )
and sugar_expr ppf e =
  match e.pexp_desc with 
  | Pexp_apply
      ({pexp_desc=
        Pexp_ident
          {txt= Ldot (Lident (("Array"|"String") as s),"get");_};_},[(_,e1);(_,e2)]) -> begin
            let fmt:(_,_,_)format =
              if s= "Array" then "@[<hov>%a.(%a)@]" else "@[<hov>%a.[%a]@]" in
              fprintf ppf fmt   expression e1 expression e2;
            true
          end
  |Pexp_apply
      ({pexp_desc=
        Pexp_ident
          {txt= Ldot (Lident (("Array"|"String") as s),
                      "set");_};_},[(_,e1);(_,e2);(_,e3)])
    -> 
      let fmt :(_,_,_) format= if s= "Array" then
        "@[<hov>%a.(%a)<-%a@]"
      else "@[<hov>%a.[%a]<-%a@]" in  
        fprintf ppf fmt
           expression e1  expression e2  expression e3;
      true
  | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident "!";_};_}, [(_,e)]) -> begin
      fprintf ppf "@[<hov>(!(%a))@]" expression e;
      true
  end
  | _ -> false
and expression ppf x =
  match x.pexp_desc with
  | Pexp_let (rf, l, e) ->
      fprintf ppf "@\n@[<hov>let@ %a@ %a@ in@ %a@]" (*no identation here, a new line*)
        rec_flag rf
        pattern_x_expression_def_list l
        expression e 
  | Pexp_function (p, eo, l) ->
     ( match l with
      | [(p',e')] ->
          (match e'.pexp_desc with
          | Pexp_when(e1,e2) ->
              fprintf ppf "@[<hov2>(fun@ %a@ when@ %a@ ->@ %a)@]"
                pattern p' expression e1 expression e2 
          | _ -> 
              fprintf ppf "@[<hov2>(fun@ %a->@ %a)@]" (* FIXME IMPROVE later *)
                pp_print_label_exp (p,eo,p') expression e')
      | _ -> 
          fprintf ppf "@\n@[<hov>(function@\n%a)@]" (* a new line *)
          case_list  l ;)

  | Pexp_apply (e, l) ->
      if not (sugar_expr ppf x) then
        let fixity = (is_infix (fixity_of_exp e)) in
        if not fixity  then
          fprintf ppf "@[<hov2>%a@]" begin fun ppf (e,l) -> 
            fprintf ppf "(%a@ %a)" (fun ppf e ->
              (match e.pexp_desc with
              | Pexp_ident(_) -> expression ppf e ;
              | Pexp_send (_,_) -> expression ppf e ;
              | _ -> fprintf ppf "@[<hov2>%a@]"  expression_in_parens e )) e
              (pp_print_list label_x_expression_param)  l 
          end (e,l)
      else 
          (match l with
          | [ arg1; arg2 ] ->
              fprintf ppf "@[<hov2>(%a@ %a@ %a)@]"
                label_x_expression_param  arg1 
              (fun ppf e -> match e.pexp_desc with
              | Pexp_ident(li) ->
                  fprintf ppf "%a" longident_loc li ;
              | _ -> simple_expr ppf e) e 
              label_x_expression_param arg2
          | _ ->
              fprintf ppf "@[<hov2>(%a %a)@]" simple_expr e 
                (fun ppf l -> pp_print_list label_x_expression_param (* ~breakfirst:true *) ppf l)  l)

  | Pexp_match (e, l) ->
      fprintf ppf "@\n@[<hov>(match@ %a@ with@\n@[<hov>%a@])@]" (*a new line*)
        expression e
        case_list  l 
  | Pexp_try (e, l) ->
      fprintf ppf "@\n@[<hov>(try@ %a@ with@\n@[<hov>%a@])@]"
        expression e  case_list l 
  | Pexp_construct (li, eo, _)  ->
      (*
        either a::b::c::d
        or a::b::c::[]
       *)
      let view_expression_list (exp:Parsetree.expression) =
        let rec loop exp acc = match exp with
        |{pexp_desc=Pexp_construct ({txt=Lident "[]";_},_,_);_} -> (List.rev acc,true)
        |{pexp_desc=
          Pexp_construct ({txt=Lident "::";_},Some ({pexp_desc= Pexp_tuple([e1;e2]);_}),_);_} ->
            loop e2 (e1::acc)
        | e -> (List.rev (e::acc),false) in loop exp [] in
      (match li.txt with
        | Lident ("::") ->
            
            (match view_expression_list x with
            | ls,true ->
                fprintf ppf "[%a]" (pp_print_list expression ~sep:";") ls 

            | ls,false -> 
                pp_print_list expression ppf ls ~sep:"::")
        | Lident ("()") -> fprintf ppf "()" ;
        | _ ->
            fprintf ppf "@[<hov2>%a%a@]" longident_loc li
              (pp_print_option expression ~first:"@ (" ~last:")") eo)
  | Pexp_field (e, li) ->
      fprintf ppf "@[<hov2>%a.%a@]"
      (fun ppf e -> match e.pexp_desc with
        | Pexp_ident (_) ->
            simple_expr ppf e 
        | _ ->
            expression_in_parens ppf e ) e longident_loc li 
  | Pexp_setfield (e1, li, e2) ->
      fprintf ppf "@[<hov2>%a.%a@ <-@ %a@]"
      (fun ppf e1 -> match e1.pexp_desc with
        | Pexp_ident (_) ->
            simple_expr ppf e1 
        | _ ->
            expression_in_parens ppf e1) e1
        longident_loc li 
      expression e2;
  | Pexp_ifthenelse (e1, e2, eo) ->
      fprintf ppf "@[<hv0>if@ %a@ then@ begin@\n%a@\nend%a@]"
        expression e1 expression e2
        (pp_print_option expression
           ~first:"@ else@ begin@\n" ~last:"@\nend") eo
        
  | Pexp_sequence _ ->
      let rec sequence_helper acc = function
        | {pexp_desc=Pexp_sequence(e1,e2);_} ->
            sequence_helper (e1::acc) e2
        | v -> List.rev (v::acc) in
      let lst = sequence_helper [] x in
      fprintf ppf "begin@\n@[<hv>%a@]@\nend"
        (pp_print_list expression ~sep:";@\n") lst
  | Pexp_constraint (e, cto1, cto2) ->
      fprintf ppf "@[<hov2>(%a%a%a)@]"
        expression e
        (pp_print_option core_type ~first:"@ :" ~last:"@ ") cto1
        (pp_print_option core_type ~first:"@ :>") cto2
  | Pexp_when (_e1, _e2) ->  assert false (*FIXME handled already in pattern *)
  | Pexp_send (e, s) ->
      fprintf ppf "@[<hov2>%a#%s@]"
      (fun ppf e -> match e.pexp_desc with
        | Pexp_ident(_) -> expression ppf e;
        | _ -> 
              expression_in_parens ppf e) e s 
  | Pexp_new (li) ->
      fprintf ppf "@[<hov2>new@ %a@]" longident_loc li;
  | Pexp_setinstvar (s, e) ->
      fprintf ppf "@[<hov2>%s@ <-@ %a@]" s.txt expression e
  | Pexp_override l ->
      let string_x_expression ppf (s, e) =
        fprintf ppf "@[<hov2>%s@ =@ %a@]" s.txt expression e in
      fprintf ppf "@[<hov2>{<%a>}@]"
        (pp_print_list string_x_expression  ~sep:";"  )  l;
  | Pexp_letmodule (s, me, e) ->
      fprintf ppf "@[<hov2>let@ module@ %s@ =@ %a@ in@ %a@]" s.txt
        module_expr me  expression e
  | Pexp_assert (e) ->
      fprintf ppf "@[<hov2>assert@ %a@]" expression e 
  | Pexp_assertfalse ->
      fprintf ppf "assert@ false" ;
  | Pexp_lazy (e) ->
      fprintf ppf "@[<hov2>lazy@ %a@]" simple_expr e 
  | Pexp_poly _ -> 
      assert false
  | Pexp_object cs ->
      fprintf ppf "@[<hov2>%a@]" class_structure cs 
  | Pexp_open (lid, e) ->
      fprintf ppf "@[<hov2>let@ open@ %a@ in%a@]" longident_loc lid
        expression  e 
  | _ -> simple_expr ppf x


and value_description ppf x =
  fprintf ppf "@[<hov2>%a%a@]" core_type x.pval_type
    (fun ppf x ->
      if x.pval_prim<>[] then begin
        fprintf ppf "@ =@ %a" 
          (pp_print_list constant_string)
          x.pval_prim ;
      end) x


and type_declaration ppf x = begin
  let  type_variant_leaf ppf  (s, l,gadt, _loc)  = match gadt with
  |None -> 
      fprintf ppf "@[<hov2>|@ %s%a@]" s.txt
        (fun ppf l -> match l with
        | [] -> ()
        | _ -> fprintf ppf "@ of@ %a" (pp_print_list core_type ~sep:"*") l) l
  |Some x ->
      fprintf ppf "@[<hov2>|@ %s:@ %a@]" s.txt
        (pp_print_list core_type ~sep:"@ ->@ ") (l@[x]) in
  fprintf ppf "@[<hov2>%a%a@ %a@]"
    (fun ppf x -> match (x.ptype_manifest,x.ptype_kind,x.ptype_private) with
    | (None,_,Public) -> ()
    | (None,_,Private) -> fprintf ppf "private@ "
    | (Some y, Ptype_abstract,Private) -> 
        fprintf ppf "private@ %a" core_type y;
    | (Some y, _, Private) -> 
        fprintf ppf "%a@ =@ private@ " core_type y 
    | (Some y,Ptype_abstract, Public) -> 
        core_type ppf y;
    | (Some y, _,Public) -> begin
        fprintf ppf "%a@ =@ " core_type y (* manifest types*)
    end) x 
  (fun ppf x -> match x.ptype_kind with  (*here only normal variant types allowed here*)
    | Ptype_variant xs -> 
        fprintf ppf "@[<hov2>@[<hv>%a@]@]"
          (pp_print_list type_variant_leaf) xs
    | Ptype_abstract -> ()
    | Ptype_record l ->
        let type_record_field ppf (s, mf, ct,_) =
          fprintf ppf "@[<hov2>%a@ %s:%a@]" mutable_flag mf s.txt core_type ct in
        fprintf ppf "@[<hov2>{%a}@]"
        (pp_print_list type_record_field ~sep:";" )  l ;
     ) x
    (pp_print_list
       (fun ppf (ct1,ct2,_) ->
         fprintf ppf "@[<hov2>constraint@ %a@ =@ %a@]"
           core_type ct1 core_type ct2 ) (* ~breakfirst:true *) )  x.ptype_cstrs  ;
end
and exception_declaration ppf (s,ed) =
  fprintf ppf "@[<hov2>exception@ %s%a@]" s
    (fun ppf ed -> match ed with
    |[] -> ()
    |_ -> fprintf ppf "@ of@ %a" (pp_print_list ~sep:"*" core_type) ed) ed

and class_type ppf x =  match x.pcty_desc with
  | Pcty_signature cs ->
      class_signature ppf cs;
  | Pcty_constr (li, l) ->
      fprintf ppf "@[<hov2>%a%a@]"
      (fun ppf l -> match l with
      | [] -> ()
      | _  -> fprintf ppf "[%a]@ " (pp_print_list core_type ~sep:"," ) l) l 
      longident_loc li 
  | Pcty_fun (l, co, cl) ->
        fprintf ppf "@[<hov2>(%a@ ->@ %a)@ @]" (* FIXME remove parens later *)
        print_type_with_label (l,co) class_type cl;

and class_signature ppf { pcsig_self = ct; pcsig_fields = l ;_} =
  let class_type_field ppf x =
  match x.pctf_desc with
  | Pctf_inher (ct) ->  
      fprintf ppf "@[<hov2>inherit@ %a@]"
        class_type ct 
  | Pctf_val (s, mf, vf, ct) ->
      fprintf ppf "@[<hov2>val @ %a%a%s@ :@ %a@]"
        mutable_flag mf virtual_flag vf s  core_type  ct 
  | Pctf_virt (s, pf, ct) ->    (* todo: test this *)
      fprintf ppf "@[<hov2>@[<hov2>method@ %a@ virtual@ %s@]@ :@ %a@]"
        private_flag pf s  core_type ct 
  | Pctf_meth (s, pf, ct) ->
      fprintf ppf "@[<hov2>@[<hov2>method@ %a@ %s@]@ :@ %a@]"
        private_flag pf s core_type ct 
  | Pctf_cstr (ct1, ct2) ->
      fprintf ppf "@[<hov2>constraint@ %a@ =@ %a@]"
        core_type ct1 core_type ct2 in 
  fprintf ppf "object%a@\n%a@\nend"
  (fun ppf ct -> match ct.ptyp_desc with
    | Ptyp_any -> ()
    | _ -> fprintf ppf "@ (%a)" core_type ct) ct
  (pp_print_list  (* ~breakfirst:true *) class_type_field) l  ;

and class_type_declaration_list ppf  l =
  let class_type_declaration ppf ({pci_params=(ls,_);pci_name={txt;_};_} as x) =
    fprintf ppf "%a%a%s@ =@ %a" virtual_flag x.pci_virt  class_params_def ls txt
      class_type x.pci_expr in 
  match l with
  | [] -> () 
  | [h] -> fprintf ppf "@[<hov2>class@ type@ %a@]" class_type_declaration   h 
  | _ ->
      fprintf ppf "@[<hov2>class@ type@ %a@]"
        (pp_print_list class_type_declaration ~sep:"@\nand@ ") l 

and class_expr ppf x =
  match x.pcl_desc with
  | Pcl_structure (cs) ->
      class_structure ppf cs ;
  | Pcl_fun (l, eo, p, e) ->
      fprintf ppf "@[<hov2>fun@ %a@ ->@ %a@]"
        pp_print_label_exp (l,eo,p)
        class_expr e
  | Pcl_let (rf, l, ce) ->
      fprintf ppf "@[<hov>let@ %a@ %a@ in@ %a@]"
        rec_flag rf
        pattern_x_expression_def_list  l
        class_expr ce
  | Pcl_apply (ce, l) ->
      fprintf ppf "@[<hov2>(%a@ %a)@]"
      class_expr  ce
      (pp_print_list label_x_expression_param (* ~breakfirst:true *) ) l 
  | Pcl_constr (li, l) ->
      fprintf ppf "@[<hov2>%a%a@]"
      (fun ppf l-> if l <>[] then 
        fprintf ppf "[%a]@ " 
        (pp_print_list core_type  ~sep:"," ) l ) l 
       longident_loc li
  | Pcl_constraint (ce, ct) ->
      fprintf ppf "@[<hov2>(%a@ :@ %a)@]"
        class_expr ce
        class_type ct 
and class_structure ppf { pcstr_pat = p; pcstr_fields =  l } =
  fprintf ppf "object@\n%a%a@ end"
  (fun ppf p -> match p.ppat_desc with
    | Ppat_any -> ();
    | _ -> fprintf ppf "@ " ;
        pattern_in_parens ppf p ) p 
  (pp_print_list class_field  (* ~indent *) (* ~breakfirst:true *)) l 
and override ovf = match ovf with
    Override -> "!"
  | Fresh -> ""
and class_field ppf x =
  match x.pcf_desc with
  | Pcf_inher (ovf, ce, so) ->
      fprintf ppf "@[<hov2>inherit@ %s@ %a%a@]"
        (override ovf)
      class_expr ce
      (fun ppf so -> match so with
        | None -> ();
        | Some (s) -> fprintf ppf "@ as %s" s ) so 
  | Pcf_val (s, mf, ovf, e) ->
      fprintf ppf "@[<hov2>val@ %s@ %a@ %s@ =@ %a@]"
        (override ovf)
        mutable_flag mf
        s.txt 
        expression  e 

  | Pcf_virt (s, pf, ct) ->
      fprintf ppf "@[<hov2>method@ virtual@ %a@ %s@ :@ %a@]"  private_flag pf
        s.txt   core_type  ct
  | Pcf_valvirt (s, mf, ct) ->
      fprintf ppf "@[<hov2>val@ virtual@ %s@ %s@ :@ %a@]"
        (match mf with
        | Mutable -> "mutable "
        | _       -> "")
        s.txt
        core_type  ct
  | Pcf_meth (s, pf, ovf, e) ->
      fprintf ppf "@[<hov2>method@ %s@ %a@ %s@ %a@]"
        (override ovf)
        private_flag pf
        s.txt
        (* FIXME special Pexp_poly handling? move right arguments left *)
      (fun ppf e -> match e.pexp_desc with
        | Pexp_poly (e, ct) ->
            fprintf ppf "%a=@ %a"
              (pp_print_option core_type ~first:":" ) ct
              expression e
        | _ ->
            expression ppf e ) e 
  | Pcf_constr (ct1, ct2) ->
      fprintf ppf "@[<hov2>constraint@ %a@ =@ %a@]" core_type  ct1 core_type  ct2
  | Pcf_init (e) ->
      fprintf ppf "@[<hov2>initializer@ %a@]" expression e 
and module_type ppf x =
  match x.pmty_desc with
  | Pmty_ident li ->
      fprintf ppf "%a" longident_loc li;
  | Pmty_signature (s) ->
      fprintf ppf "@[<hov>sig@ %a@ end@]"
      (pp_print_list signature_item  (* ~breakfirst:true *) (* ~indent *)) s
  | Pmty_functor (s, mt1, mt2) ->
      fprintf ppf "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" s.txt
        module_type mt1  module_type mt2 
  | Pmty_with (mt, l) ->
      let longident_x_with_constraint ppf (li, wc) =
        match wc with
        | Pwith_type ({ptype_params= ls ;_} as td) ->
            fprintf ppf "type@ %a %a =@ %a"
              (pp_print_list pp_print_type_var_option ~sep:"," ~first:"(" ~last:")")
              ls longident_loc li  type_declaration  td 
        | Pwith_module (li2) ->
            fprintf ppf "module %a =@ %a" longident_loc li longident_loc li2;
        | Pwith_typesubst ({ptype_params=ls;_} as td) ->
            fprintf ppf "type@ %a %a :=@ %a"
              (pp_print_list pp_print_type_var_option ~sep:"," ~first:"(" ~last:")")
              ls longident_loc li
              type_declaration  td 
        | Pwith_modsubst (li2) ->
            fprintf ppf "module %a :=@ %a" longident_loc li longident_loc li2 in
      (match l with
      | [] -> fprintf ppf "@[<hov2>%a@]" module_type mt 
      | _ -> fprintf ppf "@[<hov2>(%a@ with@ %a)@]"
            module_type mt (pp_print_list longident_x_with_constraint ~sep:"@ and@ ") l )
  | Pmty_typeof me ->
      fprintf ppf "@[<hov2>module@ type@ of@ %a@]"
      module_expr me 
and signature ppf x =  pp_print_list ~sep:"@\n" signature_item ppf x

and signature_item ppf x :unit= begin
    match x.psig_desc with
    | Psig_type l ->
        type_def_list ppf l
    | Psig_value (s, vd) ->
        fprintf ppf "@[<hov2>%a@]"
          (fun ppf (s,vd) -> 
            let intro = if vd.pval_prim = [] then "val" else "external" in
            if (is_infix (fixity_of_string s.txt)) || List.mem s.txt.[0] prefix_symbols then
              fprintf ppf "%s@ (@ %s@ )@ :@ " intro s.txt                
            else
              fprintf ppf "%s@ %s@ :@ " intro s.txt;
            value_description ppf vd;) (s,vd)
    | Psig_exception (s, ed) ->
          exception_declaration ppf (s.txt,ed)
    | Psig_class (l) ->
        let class_description ppf ({pci_params=(ls,_);pci_name={txt;_};_} as x) =
          fprintf ppf "@[<hv>@[<hov2>class@ %a%a%s@ :@]@ %a@]"  virtual_flag x.pci_virt
            class_params_def ls  txt  class_type x.pci_expr in 
        fprintf ppf "@[<hov2>%a@]"
        (pp_print_list class_description) l 
    | Psig_module (s, mt) ->
        fprintf ppf "@[<hov>module@ %s@ :@ %a@]"
          s.txt
          module_type  mt
    | Psig_open li ->
        fprintf ppf "@[<hov2>open@ %a@]" longident_loc li
    | Psig_include (mt) ->
        fprintf ppf "@[<hov2>include@ %a@]"
        module_type  mt
    | Psig_modtype (s, md) ->
        fprintf ppf "@[<hov2>module@ type@ %s%a@]"
          s.txt
        (fun ppf md -> match md with
          | Pmodtype_abstract -> ()
          | Pmodtype_manifest (mt) ->
              pp_print_space ppf () ;
              fprintf ppf "@ =@ %a"  module_type mt
        ) md 
    | Psig_class_type (l) ->
        class_type_declaration_list ppf l ;
    | Psig_recmodule decls ->
        let rec  string_x_module_type_list ppf ?(first=true) l =
          match l with
          | [] -> () ;
          | (s,mty) :: tl ->
              if not first then
                fprintf ppf "@ @[<hov2>and@ %s:@ %a@]"
                  s.txt module_type mty
              else
                fprintf ppf "@ @[<hov2>module@ rec@ %s:@ %a@]"
                  s.txt module_type mty;
              string_x_module_type_list ppf ~first:false tl  in
        string_x_module_type_list ppf decls
  end
and module_expr ppf x =
  match x.pmod_desc with
  | Pmod_structure (s) ->
      fprintf ppf "@[<hov2>struct@ %a@ end@]"
      (pp_print_list structure_item  (* ~breakfirst:true *) (* ~indent *)) s;
  | Pmod_constraint (me, mt) ->
      fprintf ppf "@[<hov2>(%a@ :@ %a)@]"
        module_expr  me
        module_type mt
  | Pmod_ident (li) ->
      fprintf ppf "%a" longident_loc li;
  | Pmod_functor (s, mt, me) ->
      fprintf ppf "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]"
        s.txt  module_type mt  module_expr me
  | Pmod_apply (me1, me2) ->
      fprintf ppf "@[<hov2>(%a)@ (%a)@]" module_expr me1  module_expr  me2
  | Pmod_unpack e ->
      fprintf ppf "@[<hov2>(val@ %a)@]"  expression  e
and structure ppf x = pp_print_list ~sep:"@\n" structure_item ppf x ;
and structure_item ppf x = begin
    match x.pstr_desc with
    | Pstr_eval (e) ->
        fprintf ppf "@[<hov2>let@ _=@ %a@]" expression e 
    | Pstr_type [] -> assert false
    | Pstr_type l  -> type_def_list ppf l 
    | Pstr_value (rf, l) ->
        fprintf ppf "@[<hov2>let@ %a@ %a@]"
          rec_flag rf
          pattern_x_expression_def_list l ;
    | Pstr_exception (s, ed) ->
        exception_declaration ppf (s.txt,ed)
    | Pstr_module (s, me) ->
        let rec module_helper me = match me.pmod_desc with
        | Pmod_functor(s,mt,me) ->
            fprintf ppf "(%s:%a)"  s.txt  module_type mt ;
            module_helper me
        | _ -> me in 
        fprintf ppf "@[<hov2>module@ %s@ %a@]"
          s.txt
          (fun ppf me ->
            let me = module_helper me  in
            (match me.pmod_desc with
            | Pmod_constraint
                (me,
                 ({pmty_desc=(Pmty_ident (_)
                 | Pmty_signature (_));_} as mt)) ->
                     fprintf ppf "@ :@ %a@ =@ %a@ "  module_type mt module_expr  me 
            | _ ->
                fprintf ppf "@ =@ %a"  module_expr  me 
        )) me 
    | Pstr_open (li) ->
        fprintf ppf "open %a" longident_loc li;
    | Pstr_modtype (s, mt) ->
        fprintf ppf "@[<hov2>module@ type@ %s@ =@ %a@]" s.txt module_type mt 
    | Pstr_class l ->
        let class_declaration ppf  (* for the second will be changed to and FIXME*)
            ({pci_params=(ls,_);
              pci_name={txt;_};
              pci_virt;
              pci_expr={pcl_desc;_};
              pci_variance;_ } as x) = (* FIXME pci_variance *)
          let rec  class_fun_helper ppf e = match e.pcl_desc with
          | Pcl_fun (l, eo, p, e) ->
              pp_print_label_exp ppf (l,eo,p);
              class_fun_helper ppf e
          | _ -> e in 
          fprintf ppf "@[<hov2>@ %a%a%s@ %a@]"  virtual_flag pci_virt class_params_def ls txt 
            (fun ppf _ ->  
              let ce =
                (match pcl_desc with
                | Pcl_fun _ ->
                    class_fun_helper ppf x.pci_expr;
                | _ -> x.pci_expr) in
              let ce =
                (match ce.pcl_desc with
                | Pcl_constraint (ce, ct) ->
                    fprintf ppf ":@ %a@ " class_type  ct ;
                    ce
                | _ -> ce ) in
              fprintf ppf "=@ %a" class_expr ce ) x in
        (match l with
        |[x ] -> fprintf ppf "@[<hov2>class@ %a@]" class_declaration x
        | xs -> fprintf ppf "@[<hov2>class@ %a@]"
              (pp_print_list class_declaration ~sep:"@\nand@ ") xs )
    | Pstr_class_type (l) ->
        class_type_declaration_list ppf l ;
    | Pstr_primitive (s, vd) ->
        let need_parens =
          match s.txt with
          | "or" | "mod" | "land"| "lor" | "lxor" | "lsl" | "lsr" | "asr" -> true
          | _ -> match s.txt.[0] with
              'a'..'z' -> false | _ -> true in
        fprintf ppf "@[<hov2>external@ %s@ :@ %a@]"
          (if need_parens then "( "^s.txt^" )" else s.txt)
          value_description  vd
    | Pstr_include me ->
        fprintf ppf "@[<hov2>include@ %a@]"  module_expr  me 
    | Pstr_exn_rebind (s, li) ->        (* todo: check this *)
        fprintf ppf "@[<hov2>exception@ %s@ =@ %a@]" s.txt longident_loc li 
    | Pstr_recmodule decls -> (* 3.07 *)
        let text_x_modtype_x_module ppf (s, mt, me) =
          fprintf ppf "@[<hov2>and@ %s:%a@ =@ %a@]"
            s.txt module_type mt module_expr me
        in match decls with
        | (s,mt,me):: l2 ->
            fprintf ppf "@[<hv>@[<hov2>module@ rec@ %s:%a@ =@ %a@]@ %a@]"
              s.txt
              module_type mt
              module_expr me 
              (fun ppf l2 -> List.iter (text_x_modtype_x_module ppf) l2) l2 
        | _ -> assert false
  end
and  type_def_list ppf  l =
  let aux ppf (s, ({ptype_params;ptype_kind;ptype_manifest;_} as td )) =
    fprintf ppf "%a%s%a"
      (pp_print_list pp_print_type_var_option ~sep:"," ~first:"(" ~last:")")
      ptype_params s.txt
      (fun ppf td ->begin match ptype_kind, ptype_manifest with
      | Ptype_abstract, None -> ()
      | _ , _ -> fprintf ppf "@ =@ " end;
        fprintf ppf "@ %a" type_declaration td ) td  in 
  match l with
  | [] -> () ;
  | [x] -> fprintf ppf "@[<hov>type@ %a@]" aux x
  | xs -> fprintf ppf "@[<hov>type@ %a@]" (pp_print_list aux ~sep:"and@ ") xs 


and case_list ppf (l:(pattern * expression) list) :unit=
  let aux ppf (p,e) =
    let (e,w) =
      (match e with
        | {pexp_desc = Pexp_when (e1, e2);_} -> (e2, Some (e1))
        | _ -> (e, None)) in
    fprintf ppf "@[<hov2>|@ %a%a@ ->@ %a@]"
        pattern p (pp_print_option expression ~first:"@ when@ ") w expression e in
  fprintf ppf "@[<hov2>%a@]" (pp_print_list aux) l 


(* prints a list of definitions as found in a let statement
   note! breaks "open and close boxes in same function" convention, however
         does always open and close the same number of boxes. (i.e. no "net
         gain or loss" of box depth.                                         *)
and pattern_x_expression_def_list ppf l =
   let pattern_x_expression_def ppf (p, e) =
      let rec pp_print_pexp_function ppf e =
         begin  match e.pexp_desc with 
         | Pexp_function (label,eo,[(p,e')]) ->
             if label="" then  (*normal case single branch *)
                 match e'.pexp_desc with
             | Pexp_when _  -> fprintf ppf "=@ %a" expression e
             | _ -> 
                fprintf ppf "(%a)@ %a" pattern p pp_print_pexp_function e'
             else
                fprintf ppf "%a@ %a" pp_print_label_exp (label,eo,p) pp_print_pexp_function e'
        | Pexp_newtype (str,e') ->
            fprintf ppf "(type@ %s)@ %a" str pp_print_pexp_function e'
        | _ -> fprintf ppf "=@ %a" expression e end in 
        begin match e.pexp_desc with
        | Pexp_when (e1,e2) ->
             fprintf ppf "=@[<hov2>fun@ %a@ when@ %a@ ->@ %a@]" pattern p expression e1 expression e2 
       | _ -> fprintf ppf "%a@ %a" pattern p pp_print_pexp_function e end in 
     begin match l with
     | [] -> ()
     | [x] -> pattern_x_expression_def ppf x 
     | _ ->
       pp_print_list pattern_x_expression_def ~sep:"@ and@ " ppf l  end



(* FIXME *)
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
        Pexp_apply ({pexp_desc=Pexp_ident ({ txt = Ldot (
                Lident(modname), funname) ;_});_},_)
        -> (match modname,funname with
            | "Array","get" -> false;
            | "Array","set" -> false;
            | _,_ -> true) ;
      | Pexp_apply ({pexp_desc=Pexp_ident ({ txt = Lident(funname) ;_});_},_)
        -> (match funname with
            | "!" -> false;
            | _ -> true);
      | Pexp_apply (_,_) -> true;
      | Pexp_match (_,_) -> true;
      | Pexp_tuple (_) -> true ;
      | Pexp_constraint (_,_,_) -> true ;
      | _ -> false) in
   if already_has_parens then
     expression ppf e
   else begin
   fprintf ppf "(%a)" expression e
   end 
and pattern_in_parens ppf p =
  let already_has_parens =
    match p.ppat_desc with
    | Ppat_alias (_,_) -> true
    | Ppat_tuple (_) -> true
    | Ppat_or (_,_) -> true
    | Ppat_constraint (_,_) -> true
    | _ -> false in
   if already_has_parens then
      pattern ppf p
   else begin
   fprintf ppf "(%a)" pattern p 
   end
and directive_argument ppf x =
  (match x with
  | Pdir_none -> ()
  | Pdir_string (s) -> fprintf ppf "@ %S" s
  | Pdir_int (i) -> fprintf ppf "@ %d" i
  | Pdir_ident (li) -> fprintf ppf "@ %a" longident li
  | Pdir_bool (b) -> fprintf ppf "@ %s" (string_of_bool b));;

let toplevel_phrase ppf x =
  match x with
  | Ptop_def (s) ->
   pp_open_hvbox ppf 0;
   pp_print_list structure_item ppf s ;
   pp_close_box ppf ();
  | Ptop_dir (s, da) ->
      pp_open_hovbox ppf indent;
      fprintf ppf "#%s" s;
      directive_argument ppf da;
      pp_close_box ppf () ;;

let expression ppf x =
  fprintf ppf "@[%a@]" expression x 


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
   
