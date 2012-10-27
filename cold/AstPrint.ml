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


(* let indent    = 1 ;; (\* standard indentation increment *\) *)
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

let is_predef_option = function
  | (Ldot (Lident "*predef*","option")) -> true
  | _ -> false
        
type space_formatter = (unit, Format.formatter, unit) format

let override = function
  | Override -> "!"
  | Fresh -> ""
   
class printer  ()= object(self:'self)
  method list : 'a . ?sep:space_formatter -> ?first:space_formatter ->
    ?last:space_formatter -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
      = fun  ?sep ?first  ?last f ppf xs -> 
        let first = match first with Some x -> x |None -> ""
        and last = match last with Some x -> x |None -> ""
        and sep = match sep with Some x -> x |None -> "@ " in
        let aux ppf = function
          | [] -> ()
          | [x] -> f ppf x
          | xs ->
          let rec loop  ppf = function
            | [x] -> f ppf x
            | x::xs -> fprintf ppf ("%a"^^sep) f x ;
                loop ppf xs
            | _ -> assert false in begin
                (* fprintf ppf (first ^^"%a"^^last) loop xs  *)
                fprintf ppf first;
                loop ppf xs;
                fprintf ppf last;
            end in
    aux ppf xs
  method option : 'a. ?first:space_formatter -> ?last:space_formatter ->
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit =
    fun  ?first  ?last f ppf a ->
      let first = match first with Some x -> x | None -> ""
      and last = match last with Some x -> x | None -> "" in
      match a with
      | None -> ()
      | Some x -> begin
          fprintf ppf first;
          f ppf x ;
          fprintf ppf last;  (* fprintf ppf (first^^"%a"^^last) f x *)
      end
  method paren: 'a . bool -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit =
    fun b f ppf x ->
      if b then fprintf ppf "(%a)" f  x
      else f ppf x
  method longident ppf = function
    | Lident s -> fprintf ppf "%s" s
    | Ldot(y,s) -> (match s.[0] with
      | 'a'..'z' | 'A' .. 'Z' ->
          fprintf ppf "%a.%s" self#longident y s
      | _ ->
          fprintf ppf "%a.(@ %s@ )@ " self#longident y s)
    | Lapply (y,s)->
        fprintf ppf "%a(%a)" self#longident y self#longident s
  method longident_loc ppf x = fprintf ppf "%a" self#longident x.txt
  method constant ppf  = function
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
  method  virtual_flag ppf  = function
    | Concrete -> ()
    | Virtual -> fprintf ppf "virtual@ "
  (* trailing space added *)        
  method rec_flag ppf = function
    | Nonrecursive -> ()
    | Recursive | Default -> fprintf ppf "rec@;"
  method direction_flag ppf = function
    | Upto -> fprintf ppf "to@ "
    | Downto -> fprintf ppf "downto@ "
  method private_flag ppf = function
    | Public -> ()
    | Private -> fprintf ppf "private@ "

  method constant_string ppf s = fprintf ppf "%S" s 
  method tyvar ppf str = fprintf ppf "'%s" str
  method string_quot ppf x = fprintf ppf "`%s" x 
  method type_var_option ppf str =
    match str with
    |  None -> () 
    | Some {txt;_} -> self#tyvar ppf txt

  (* c ['a,'b] *)                                                                         
  method class_params_def ppf =  function
   | [] -> ()
   | l ->  fprintf ppf "[%a]" (self#list (fun ppf {txt;_} -> self#tyvar ppf txt) ~sep:",") l 
   
  method type_with_label ppf (label,({ptyp_desc;_}as c) ) =
    match label with
    | "" ->  self#core_type ppf c
    | s  ->
       if s.[0]='?' then 
          match ptyp_desc with
          | Ptyp_constr ({txt;_}, l) -> 
            assert (is_predef_option txt);
            fprintf ppf "%s:%a" s (self#list self#core_type) l 
          | _ -> failwith "invalid input in print_type_with_label"
       else fprintf ppf "%s:%a" s self#core_type c
  method core_type ppf  ({ptyp_desc;_}:Parsetree.core_type) =
      match ptyp_desc with
      | Ptyp_any -> fprintf ppf "_";       
      | Ptyp_var s -> self#tyvar ppf  s; 
      | Ptyp_arrow (l, ct1, ct2) ->
        fprintf ppf "@[<hov2>(%a@ ->@ %a)@ @]" (* FIXME remove parens later *)
          self#type_with_label (l,ct1) self#core_type ct2
      | Ptyp_tuple l ->
        fprintf ppf "@[<hov2>(%a)@]" (self#list self#core_type ~sep:"*") l ;
      | Ptyp_constr (li, l) ->
        fprintf ppf "%a@ %a@ " 
          (self#list ~first:"(" ~last:")" self#core_type ~sep:",")  l self#longident_loc li
      | Ptyp_variant (l, closed, low) ->
          let type_variant_helper ppf x =
            match x with
            | Rtag (l, _, ctl) -> fprintf ppf "@[<hov2>%a%a@]"  self#string_quot l
                (fun ppf l -> match l with
                |[] -> ()
                | _ -> fprintf ppf "@ of@ %a"
                    (self#list self#core_type ~sep:"&")  ctl) ctl
            | Rinherit ct -> self#core_type ppf ct in 
          fprintf ppf "@[<hov2>[%a%a]@]"
                 (fun ppf l -> match l with
                  | [] -> ()
                  | _ -> fprintf ppf "%s@ %a"
                  (match (closed,low) with
                  | (true,None) -> ""
                  | (true,Some _) -> ""
                  | (false,_) -> ">") 
                  (self#list type_variant_helper ~sep:"|") l) l 
                 (fun ppf low -> match low with
                  |Some [] |None -> ()  
                  |Some xs ->
                  fprintf ppf ">@ %a"
                  (self#list self#string_quot) xs) low
      | Ptyp_object l ->
          let  core_field_type ppf {pfield_desc;_} =
            match pfield_desc with
            | Pfield (s, ct) ->
               fprintf ppf "@[<hov2>%s@ :%a@ @]" s self#core_type ct 
            | Pfield_var -> fprintf ppf ".." in
          fprintf ppf "@[<hov2><@ %a@ >@]" (self#list core_field_type ~sep:";") l
      | Ptyp_class (li, l, low) ->   (*FIXME*)
          fprintf ppf "@[<hov2>%a#%a%a@]"
              (self#list self#core_type ~sep:"," ~first:"(" ~last:")") l
              self#longident_loc li
              (fun ppf low -> match low with
              | [] -> ()
              | _ -> fprintf ppf "@ [>@ %a]" (self#list self#string_quot) low) low
      | Ptyp_alias (ct, s) ->               
         fprintf ppf "@[<hov2>(%a @ as@ '%s)@]" self#core_type ct s
      | Ptyp_poly (sl, ct) ->   
          fprintf ppf "@[<hov2>%a%a@]"
            (fun ppf l ->
               fprintf ppf "%a"
                 (fun ppf l -> match l with
                 | [] -> ()
                 | _ ->
                  fprintf ppf "%a@ .@ "
                    (self#list self#tyvar ~sep:"@;")  l) (List.rev l)) sl  self#core_type ct 
      | Ptyp_package (lid, cstrs) ->
         let aux ppf (s, ct) =
            fprintf ppf "type %a@ =@ %a" self#longident_loc s self#core_type ct  in
         match cstrs with
         |[] -> fprintf ppf "@[<hov2>(module@ %a)@]" self#longident_loc lid
         |_ ->  
            fprintf ppf "@[<hov2>(module@ %a@ with@ %a)@]" self#longident_loc lid
            (self#list aux  ~sep:"@ and@ ")  cstrs

   (********************pattern********************)
   method pattern ppf x =
     let rec pattern_list_helper ppf  =
       function
         | {ppat_desc =
            Ppat_construct
              ({ txt = Lident("::") ;_},
               Some ({ppat_desc = Ppat_tuple([pat1; pat2]);_}),
               _);_} ->
                 fprintf ppf "%a::%a"
                   self#pattern  pat1
                   pattern_list_helper pat2
         | p -> self#pattern ppf p in
     let rec pattern_or_helper  cur = function
       |{ppat_desc = Ppat_constant (Const_char a);_}
        -> 
          if Char.code a = Char.code cur + 1 then
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
                self#longident_loc li
                self#pattern_in_parens x
           | None -> fprintf ppf "%a@ "self#longident_loc li )
     | Ppat_any -> fprintf ppf "_";           
     | Ppat_var ({txt = txt;_}) ->
        if (is_infix (fixity_of_string txt)) || List.mem txt.[0] prefix_symbols then
          if txt.[0]='*' then
            fprintf ppf "(@ %s@ )@ " txt
          else
            fprintf ppf "(%s)" txt 
        else
          fprintf ppf "%s" txt;
     | Ppat_alias (p, s) -> fprintf ppf "@[<hov2>(%a@ as@ %s)@]"  self#pattern p  s.txt
     | Ppat_constant (c) -> fprintf ppf "%a" self#constant c;
     | Ppat_tuple (l) ->                      
         fprintf ppf "@[<hov1>(%a)@]"
           (self#list  ~sep:"," self#pattern)  l;
     | Ppat_variant (l, po) ->
         (match po with
         | None ->
             fprintf ppf "`%s" l;
         | Some (p) ->
             fprintf ppf "@[<hov2>(`%s@ %a)@]" l self#pattern p)
     | Ppat_record (l, closed) ->
         let longident_x_pattern ppf (li, p) =
           fprintf ppf "@[<hov2>%a@ =@ %a@]" self#longident_loc li self#pattern p in
         (match closed with
         |Closed -> 
             fprintf ppf "@[<hov2>{%a}@]"
               (self#list longident_x_pattern ~sep:";") l
         | _ -> 
             fprintf ppf "@[<hov2>{%a;_}@]"
               (self#list longident_x_pattern ~sep:";") l)
     | Ppat_array l ->
         fprintf ppf "@[<hov2>[|%a|]@]"  (self#list self#pattern ~sep:";") l 
     | Ppat_or (p1, p2) ->
         (match p1 with
         | {ppat_desc=Ppat_constant (Const_char a);_} -> begin 
             match pattern_or_helper a p2 with
             |Some b -> fprintf ppf "@[<hov2>(%C..%C)@]" a b 
             |None ->   fprintf ppf "@[<hov2>(%a@ |%a)@]"  self#pattern p1  self#pattern p2 end
         | _ -> fprintf ppf "@[<hov2>(%a@ |%a)@]"  self#pattern p1  self#pattern p2 )
     | Ppat_constraint (p, ct) ->
         fprintf ppf "@[<hov2>(%a@ :@ %a)@]" self#pattern p self#core_type ct 
     | Ppat_type li ->
         fprintf ppf "#%a" self#longident_loc li 
     | Ppat_lazy p ->
         fprintf ppf "@[<hov2>(lazy@ %a)@]" self#pattern p 
     | Ppat_unpack (s) ->
         fprintf ppf "(module@ %s)@ " s.txt
  method simple_expr ppf x =
    match x.pexp_desc with
    | Pexp_construct (li, None, _) ->
        fprintf ppf "%a@ " self#longident_loc li
    | Pexp_ident (li) -> 
        let flag = is_infix (fixity_of_longident li)
        || match li.txt with
          | Lident (li) -> List.mem li.[0] prefix_symbols
          | _ -> false in 
        self#paren flag self#longident_loc ppf li 
    | Pexp_constant (c) -> fprintf ppf "%a" self#constant c;
    | Pexp_pack (me) ->
        fprintf ppf "(module@ %a)"  self#module_expr me
    | Pexp_newtype (lid, e) ->
        fprintf ppf "fun@ (type@ %s)@ ->@ %a"  lid  self#expression  e
    | Pexp_tuple (l) ->
        fprintf ppf "@[<hov 1>(%a)@]"  (self#list self#simple_expr  ~sep:",")  l
    | Pexp_variant (l, eo) ->
        fprintf ppf "`%s%a" l (self#option ~first:"@ (" ~last:")" self#expression) eo
    | Pexp_record (l, eo) ->
        let longident_x_expression ppf (li, e) =
          fprintf ppf "@[<hov2>%a@ =@ %a@]" self#longident_loc li self#simple_expr e in 
        fprintf ppf "@[<hov2>{%a%a}@]"
          (self#option ~last:"@ with@ " self#expression) eo
          (self#list longident_x_expression ~sep:";")  l
    | Pexp_array (l) ->
        fprintf ppf "@[<hov2>[|%a|]@]"
          (self#list self#simple_expr ~sep:";") l
    | Pexp_while (e1, e2) ->
        fprintf ppf "@[<hov2>while@ %a@ do@ %a@ done@]"
          self#expression e1 self#expression e2 
    | Pexp_for (s, e1, e2, df, e3) ->
        fprintf ppf "@[<hov2>for@ %s@ =@ %a@ %a@ %a@ do@ %a@ done@]"
          s.txt  self#expression e1  self#direction_flag df self#expression e2 
          self#expression e3
    | _ -> (* complex case delegated to expression *)
        fprintf ppf "(@ %a@ )" self#expression x 
  method label_exp ppf (l,opt,p) =
    if l = "" then
      fprintf ppf "(%a)@ " self#pattern p (*single case pattern parens needed here *)
    else
      if l.[0] = '?' then 
        let len = String.length l - 1 in 
        let rest = String.sub l 1 len in begin
          match p.ppat_desc with
          | Ppat_var {txt;_} when txt = rest ->
              (match opt with
              |Some o -> fprintf ppf "?(%s=%a)@ " rest  self#expression o
              | None -> fprintf ppf "?%s@ " rest)
          | _ -> (match opt with
            | Some o -> fprintf ppf "%s:(%a=%a)@ " l self#pattern p self#expression o
            | None -> fprintf ppf "%s:(%a)@ " l self#pattern p  )
        end
      else
        (match p.ppat_desc with
        | Ppat_var {txt;_} when txt = l ->
            fprintf ppf "~%s@ " l 
        | _ ->  fprintf ppf "~%s:(%a)@ " l self#pattern p )
  method sugar_expr ppf e =
    match e.pexp_desc with 
    | Pexp_apply
        ({pexp_desc=
          Pexp_ident
            {txt= Ldot (Lident (("Array"|"String") as s),"get");_};_},[(_,e1);(_,e2)]) -> begin
              let fmt:(_,_,_)format =
                if s= "Array" then "@[<hov>%a.(%a)@]" else "@[<hov>%a.[%a]@]" in
              fprintf ppf fmt   self#expression e1 self#expression e2;
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
          self#expression e1  self#expression e2  self#expression e3;
        true
    | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident "!";_};_}, [(_,e)]) -> begin
        fprintf ppf "@[<hov>(!(%a))@]" self#expression e;
        true
    end
    | _ -> false
  method expression ppf x =
    match x.pexp_desc with
    | Pexp_let (rf, l, e) ->
        fprintf ppf "@[<2>let %a%a in@;<1 -2>%a@]" (*no identation here, a new line*)
          self#rec_flag rf
          self#pattern_x_expression_def_list l
          self#expression e 
    | Pexp_function (p, eo, l) ->
        ( match l with
        | [(p',e')] ->
            (match e'.pexp_desc with
            | Pexp_when(e1,e2) ->
                fprintf ppf "@[<hov2>(fun@ %a@ when@ %a@ ->@ %a)@]"
                  self#pattern p' self#expression e1 self#expression e2 
            | _ -> 
                fprintf ppf "@[<hov2>(fun@ %a->@ %a)@]" (* FIXME IMPROVE later *)
                  self#label_exp (p,eo,p') self#expression e')
        | _ -> 
            fprintf ppf "@[(function@\n%a)@]" (* a new line *)
              self#case_list  l ;)

    | Pexp_apply (e, l) ->
        if not (self#sugar_expr ppf x) then
          let fixity = (is_infix (fixity_of_exp e)) in
          if not fixity  then
            fprintf ppf "@[<hov2>%a@]" begin fun ppf (e,l) -> 
              fprintf ppf "(%a@ %a)" (fun ppf e ->
                (match e.pexp_desc with
                | Pexp_ident(_) -> self#expression ppf e ;
                | Pexp_send (_,_) -> self#expression ppf e ;
                | _ -> fprintf ppf "@[<hov2>%a@]"  self#expression_in_parens e )) e
                (self#list self#label_x_expression_param)  l 
            end (e,l)
          else 
            (match l with
            | [ arg1; arg2 ] ->
                fprintf ppf "@[<hov2>(%a@ %a@ %a)@]"
                  self#label_x_expression_param  arg1 
                  (fun ppf e -> match e.pexp_desc with
                  | Pexp_ident(li) ->
                      fprintf ppf "%a" self#longident_loc li ;
                  | _ -> self#simple_expr ppf e) e 
                  self#label_x_expression_param arg2
            | _ ->
                fprintf ppf "@[<hov2>(%a %a)@]" self#simple_expr e 
                  (fun ppf l -> self#list self#label_x_expression_param (* ~breakfirst:true *) ppf l)  l)

    | Pexp_match (e, l) ->
        fprintf ppf "begin@;match@;%a@;with@;%a@;end" (*a new line*)
          self#expression e
          self#case_list  l 
    | Pexp_try (e, l) ->
        fprintf ppf "begin@;try@;%a@;with@;@[<hov>%a@]@;end"
          self#expression e  self#case_list l 
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
                fprintf ppf "[%a]" (self#list self#expression ~sep:";") ls 

            | ls,false -> 
                self#list self#expression ppf ls ~sep:"::")
        | Lident ("()") -> fprintf ppf "()" ;
        | _ ->
            fprintf ppf "@[<hov2>%a%a@]" self#longident_loc li
              (self#option self#expression ~first:"@ (" ~last:")") eo)
    | Pexp_field (e, li) ->
        fprintf ppf "@[<hov2>%a.%a@]"
          (fun ppf e -> match e.pexp_desc with
          | Pexp_ident (_) ->
              self#simple_expr ppf e 
          | _ ->
              self#expression_in_parens ppf e ) e self#longident_loc li 
    | Pexp_setfield (e1, li, e2) ->
        fprintf ppf "@[<hov2>%a.%a@ <-@ %a@]"
          (fun ppf e1 -> match e1.pexp_desc with
          | Pexp_ident (_) ->
              self#simple_expr ppf e1 
          | _ ->
              self#expression_in_parens ppf e1) e1
          self#longident_loc li 
          self#expression e2;
    | Pexp_ifthenelse (e1, e2, eo) ->
        fprintf ppf "@[<0>if@;%a@;then begin@;@[<2>%a@]@\nend%a@]"
          self#expression e1 self#expression e2
          (self#option self#expression
             ~first:"@;else begin@;@[<2>" ~last:"@]@\nend") eo
          
    | Pexp_sequence _ ->
        let rec sequence_helper acc = function
          | {pexp_desc=Pexp_sequence(e1,e2);_} ->
              sequence_helper (e1::acc) e2
          | v -> List.rev (v::acc) in
        let lst = sequence_helper [] x in
        fprintf ppf "begin@\n@[<hv>%a@]@\nend"
          (self#list self#expression ~sep:";@\n") lst
    | Pexp_constraint (e, cto1, cto2) ->
        fprintf ppf "@[<hov2>(%a%a%a)@]"
          self#expression e
          (self#option self#core_type ~first:"@ :" ~last:"@ ") cto1
          (self#option self#core_type ~first:"@ :>") cto2
    | Pexp_when (_e1, _e2) ->  assert false (*FIXME handled already in pattern *)
    | Pexp_send (e, s) ->
        fprintf ppf "@[<hov2>%a#%s@]"
          (fun ppf e -> match e.pexp_desc with
          | Pexp_ident(_) -> self#expression ppf e;
          | _ -> 
              self#expression_in_parens ppf e) e s 
    | Pexp_new (li) ->
        fprintf ppf "@[<hov2>new@ %a@]" self#longident_loc li;
    | Pexp_setinstvar (s, e) ->
        fprintf ppf "@[<hov2>%s@ <-@ %a@]" s.txt self#expression e
    | Pexp_override l ->
        let string_x_expression ppf (s, e) =
          fprintf ppf "@[<hov2>%s@ =@ %a@]" s.txt self#expression e in
        fprintf ppf "@[<hov2>{<%a>}@]"
          (self#list string_x_expression  ~sep:";"  )  l;
    | Pexp_letmodule (s, me, e) ->
        fprintf ppf "@[<hov2>let@ module@ %s@ =@ %a@ in@ %a@]" s.txt
          self#module_expr me  self#expression e
    | Pexp_assert (e) ->
        fprintf ppf "@[<hov2>assert@ %a@]" self#expression e 
    | Pexp_assertfalse ->
        fprintf ppf "assert@ false" ;
    | Pexp_lazy (e) ->
        fprintf ppf "@[<hov2>lazy@ %a@]" self#simple_expr e 
    | Pexp_poly _ -> 
        assert false
    | Pexp_object cs ->
        fprintf ppf "@[<hov2>%a@]" self#class_structure cs 
    | Pexp_open (lid, e) ->
        fprintf ppf "@[<2>let open %a in@;%a@]" self#longident_loc lid
          self#expression  e 
    | _ -> self#simple_expr ppf x


  method value_description ppf x =
    fprintf ppf "@[<hov2>%a%a@]" self#core_type x.pval_type
      (fun ppf x ->
        if x.pval_prim<>[] then begin
          fprintf ppf "@ =@ %a" 
            (self#list self#constant_string)
            x.pval_prim ;
        end) x


  method exception_declaration ppf (s,ed) =
    fprintf ppf "@[<hov2>exception@ %s%a@]" s
      (fun ppf ed -> match ed with
      |[] -> ()
      |_ -> fprintf ppf "@ of@ %a" (self#list ~sep:"*" self#core_type) ed) ed

  method class_type ppf x =  match x.pcty_desc with
  | Pcty_signature cs ->
      self#class_signature ppf cs;
  | Pcty_constr (li, l) ->
      fprintf ppf "@[<hov2>%a%a@]"
        (fun ppf l -> match l with
        | [] -> ()
        | _  -> fprintf ppf "[%a]@ " (self#list self#core_type ~sep:"," ) l) l 
        self#longident_loc li 
  | Pcty_fun (l, co, cl) ->
      fprintf ppf "@[<hov2>(%a@ ->@ %a)@ @]" (* FIXME remove parens later *)
        self#type_with_label (l,co) self#class_type cl;

  method class_signature ppf { pcsig_self = ct; pcsig_fields = l ;_} =
    let class_type_field ppf x =
      match x.pctf_desc with
      | Pctf_inher (ct) ->  
          fprintf ppf "inherit@ %a" self#class_type ct 
      | Pctf_val (s, mf, vf, ct) ->
          fprintf ppf "val @ %a%a%s@ :@ %a"
            self#mutable_flag mf self#virtual_flag vf s  self#core_type  ct 
      | Pctf_virt (s, pf, ct) ->    (* todo: test this *)
          fprintf ppf "method@ %a@ virtual@ %s@ :@ %a"
            self#private_flag pf s  self#core_type ct 
      | Pctf_meth (s, pf, ct) ->
          fprintf ppf "method@ %a@ %s@]@ :@ %a"
            self#private_flag pf s self#core_type ct 
      | Pctf_cstr (ct1, ct2) ->
          fprintf ppf "constraint@ %a@ =@ %a"
            self#core_type ct1 self#core_type ct2 in 
    fprintf ppf "object%a@\n%a@;<1 -2>end"
      (fun ppf ct -> match ct.ptyp_desc with
      | Ptyp_any -> ()
      | _ -> fprintf ppf "@ (%a)" self#core_type ct) ct
      (self#list   class_type_field) l  ;

  method class_type_declaration_list ppf  l =
    let class_type_declaration ppf ({pci_params=(ls,_);pci_name={txt;_};_} as x) =
      fprintf ppf "%a%a%s@ =@ %a" self#virtual_flag x.pci_virt  self#class_params_def ls txt
        self#class_type x.pci_expr in 
    match l with
    | [] -> () 
    | [h] -> fprintf ppf "@[<hv2>class@ type@ %a@]" class_type_declaration   h 
    | _ ->
        fprintf ppf "@[<hv2>class@ type@ %a@]"
          (self#list class_type_declaration ~sep:"@\nand@ ") l 

  method class_expr ppf x =
    match x.pcl_desc with
    | Pcl_structure (cs) ->  self#class_structure ppf cs ;
    | Pcl_fun (l, eo, p, e) ->
        fprintf ppf "@[<hov2>fun@ %a@ ->@ %a@]" self#label_exp (l,eo,p)  self#class_expr e
    | Pcl_let (rf, l, ce) ->
        fprintf ppf "@[<hov>let@;%a%a@ in@ %a@]" self#rec_flag rf
          self#pattern_x_expression_def_list  l
          self#class_expr ce
    | Pcl_apply (ce, l) ->
        fprintf ppf "@[<hov2>(%a@ %a)@]"
          self#class_expr  ce
          (self#list self#label_x_expression_param (* ~breakfirst:true *) ) l 
    | Pcl_constr (li, l) ->
        fprintf ppf "@[<hov2>%a%a@]"
          (fun ppf l-> if l <>[] then 
            fprintf ppf "[%a]@ " 
              (self#list self#core_type  ~sep:"," ) l ) l 
          self#longident_loc li
    | Pcl_constraint (ce, ct) ->
        fprintf ppf "@[<hov2>(%a@ :@ %a)@]"
          self#class_expr ce
          self#class_type ct 
  method class_structure ppf { pcstr_pat = p; pcstr_fields =  l } =
    fprintf ppf "object@\n%a%a@ end"
      (fun ppf p -> match p.ppat_desc with
      | Ppat_any -> ();
      | _ -> fprintf ppf "@ " ;
          self#pattern_in_parens ppf p ) p (self#list self#class_field  ) l 
  method class_field ppf x =
    match x.pcf_desc with
    | Pcf_inher (ovf, ce, so) ->
        fprintf ppf "@[<hov2>inherit@ %s@ %a%a@]"
          (override ovf)
          self#class_expr ce
          (fun ppf so -> match so with
          | None -> ();
          | Some (s) -> fprintf ppf "@ as %s" s ) so 
    | Pcf_val (s, mf, ovf, e) ->
        fprintf ppf "@[<hov2>val@ %s@ %a@ %s@ =@ %a@]"
          (override ovf)
          self#mutable_flag mf
          s.txt 
          self#expression  e 

    | Pcf_virt (s, pf, ct) ->
        fprintf ppf "@[<hov2>method@ virtual@ %a@ %s@ :@ %a@]"  self#private_flag pf
          s.txt   self#core_type  ct
    | Pcf_valvirt (s, mf, ct) ->
        fprintf ppf "@[<hov2>val@ virtual@ %s@ %s@ :@ %a@]"
          (match mf with
          | Mutable -> "mutable "
          | _       -> "")
          s.txt
          self#core_type  ct
    | Pcf_meth (s, pf, ovf, e) ->
        fprintf ppf "@[<hov2>method@ %s@ %a@ %s@ %a@]"
          (override ovf)
          self#private_flag pf
          s.txt
          (* FIXME special Pexp_poly handling? move right arguments left *)
          (fun ppf e -> match e.pexp_desc with
          | Pexp_poly (e, ct) ->
              fprintf ppf "%a=@;%a"
                (self#option self#core_type ~first:":" ) ct
                self#expression e
          | _ ->
              self#expression ppf e ) e 
    | Pcf_constr (ct1, ct2) ->
        fprintf ppf "@[<hov2>constraint@ %a@ =@ %a@]" self#core_type  ct1 self#core_type  ct2
    | Pcf_init (e) ->
        fprintf ppf "@[<hov2>initializer@ %a@]" self#expression e 
  method module_type ppf x =
    match x.pmty_desc with
    | Pmty_ident li ->
        fprintf ppf "%a" self#longident_loc li;
    | Pmty_signature (s) ->
        fprintf ppf "@[<hov>sig@ %a@ end@]"
          (self#list self#signature_item  (* ~breakfirst:true *) (* ~indent *)) s
    | Pmty_functor (s, mt1, mt2) ->
        fprintf ppf "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" s.txt
          self#module_type mt1  self#module_type mt2 
    | Pmty_with (mt, l) ->
        let longident_x_with_constraint ppf (li, wc) =
          match wc with
          | Pwith_type ({ptype_params= ls ;_} as td) ->
              fprintf ppf "type@ %a %a =@ %a"
                (self#list self#type_var_option ~sep:"," ~first:"(" ~last:")")
                ls self#longident_loc li  self#type_declaration  td 
          | Pwith_module (li2) ->
              fprintf ppf "module %a =@ %a" self#longident_loc li self#longident_loc li2;
          | Pwith_typesubst ({ptype_params=ls;_} as td) ->
              fprintf ppf "type@ %a %a :=@ %a"
                (self#list self#type_var_option ~sep:"," ~first:"(" ~last:")")
                ls self#longident_loc li
                self#type_declaration  td 
          | Pwith_modsubst (li2) ->
              fprintf ppf "module %a :=@ %a" self#longident_loc li self#longident_loc li2 in
        (match l with
        | [] -> fprintf ppf "@[<hov2>%a@]" self#module_type mt 
        | _ -> fprintf ppf "@[<hov2>(%a@ with@ %a)@]"
              self#module_type mt (self#list longident_x_with_constraint ~sep:"@ and@ ") l )
    | Pmty_typeof me ->
        fprintf ppf "@[<hov2>module@ type@ of@ %a@]"
          self#module_expr me 
  method signature ppf x =  self#list ~sep:"@\n" self#signature_item ppf x

  method signature_item ppf x :unit= begin
    match x.psig_desc with
    | Psig_type l ->
        self#type_def_list ppf l
    | Psig_value (s, vd) ->
        fprintf ppf "@[<hov2>%a@]"
          (fun ppf (s,vd) -> 
            let intro = if vd.pval_prim = [] then "val" else "external" in
            if (is_infix (fixity_of_string s.txt)) || List.mem s.txt.[0] prefix_symbols then
              fprintf ppf "%s@ (@ %s@ )@ :@ " intro s.txt                
            else
              fprintf ppf "%s@ %s@ :@ " intro s.txt;
            self#value_description ppf vd;) (s,vd)
    | Psig_exception (s, ed) ->
        self#exception_declaration ppf (s.txt,ed)
    | Psig_class (l) ->
        let class_description ppf ({pci_params=(ls,_);pci_name={txt;_};_} as x) =
          fprintf ppf "@[<hv>@[<hov2>class@ %a%a%s@ :@]@ %a@]"  self#virtual_flag x.pci_virt
            self#class_params_def ls  txt  self#class_type x.pci_expr in 
        fprintf ppf "@[<hov2>%a@]"
          (self#list class_description) l 
    | Psig_module (s, mt) ->
        fprintf ppf "@[<hov>module@ %s@ :@ %a@]"
          s.txt
          self#module_type  mt
    | Psig_open li ->
        fprintf ppf "@[<hov2>open@ %a@]" self#longident_loc li
    | Psig_include (mt) ->
        fprintf ppf "@[<hov2>include@ %a@]"
          self#module_type  mt
    | Psig_modtype (s, md) ->
        fprintf ppf "@[<hov2>module@ type@ %s%a@]"
          s.txt
          (fun ppf md -> match md with
          | Pmodtype_abstract -> ()
          | Pmodtype_manifest (mt) ->
              pp_print_space ppf () ;
              fprintf ppf "@ =@ %a"  self#module_type mt
          ) md 
    | Psig_class_type (l) ->
        self#class_type_declaration_list ppf l ;
    | Psig_recmodule decls ->
        let rec  string_x_module_type_list ppf ?(first=true) l =
          match l with
          | [] -> () ;
          | (s,mty) :: tl ->
              if not first then
                fprintf ppf "@ @[<hov2>and@ %s:@ %a@]"
                  s.txt self#module_type mty
              else
                fprintf ppf "@ @[<hov2>module@ rec@ %s:@ %a@]"
                  s.txt self#module_type mty;
              string_x_module_type_list ppf ~first:false tl  in
        string_x_module_type_list ppf decls
  end
  method module_expr ppf x =
    match x.pmod_desc with
    | Pmod_structure (s) ->
        fprintf ppf "@[<hov2>struct@ %a@ end@]"
          (self#list self#structure_item  ) s;
    | Pmod_constraint (me, mt) ->
        fprintf ppf "@[<hov2>(%a@ :@ %a)@]"
          self#module_expr  me
          self#module_type mt
    | Pmod_ident (li) ->
        fprintf ppf "%a" self#longident_loc li;
    | Pmod_functor (s, mt, me) ->
        fprintf ppf "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]"
          s.txt  self#module_type mt  self#module_expr me
    | Pmod_apply (me1, me2) ->
        fprintf ppf "@[<hov2>(%a)@ (%a)@]" self#module_expr me1  self#module_expr  me2
    | Pmod_unpack e ->
        fprintf ppf "@[<hov2>(val@ %a)@]"  self#expression  e
  method structure ppf x = self#list ~sep:"@." self#structure_item ppf x ;
  method pattern_x_expression_def_list ppf l =
    let pattern_x_expression_def ppf (p, e) =
      let rec pp_print_pexp_function ppf e =
        begin  match e.pexp_desc with 
        | Pexp_function (label,eo,[(p,e')]) ->
            if label="" then  (*normal case single branch *)
              match e'.pexp_desc with
              | Pexp_when _  -> fprintf ppf "=@ %a" self#expression e
              | _ -> 
                  fprintf ppf "(%a)@ %a" self#pattern p pp_print_pexp_function e'
            else
              fprintf ppf "%a@ %a" self#label_exp (label,eo,p) pp_print_pexp_function e'
        | Pexp_newtype (str,e') ->
            fprintf ppf "(type@ %s)@ %a" str pp_print_pexp_function e'
        | _ -> fprintf ppf "=@ %a" self#expression e end in 
      begin match e.pexp_desc with
      | Pexp_when (e1,e2) ->
          fprintf ppf "=@[<hov2>fun@ %a@ when@ %a@ ->@ %a@]" self#pattern p self#expression e1 self#expression e2 
      | _ -> fprintf ppf "%a@ %a" self#pattern p pp_print_pexp_function e end in 
    begin match l with
    | [] -> ()
    | [x] -> pattern_x_expression_def ppf x 
    | _ ->
        self#list pattern_x_expression_def ~sep:"@ and@ " ppf l  end
    
  method structure_item ppf x = begin
    match x.pstr_desc with
    | Pstr_eval (e) ->
        fprintf ppf "@[<hov2>let@ _=@ %a@]" self#expression e 
    | Pstr_type [] -> assert false
    | Pstr_type l  -> self#type_def_list ppf l 
    | Pstr_value (rf, l) -> fprintf ppf "@[<hov2>let %a%a@]"  self#rec_flag rf self#pattern_x_expression_def_list l
    | Pstr_exception (s, ed) -> self#exception_declaration ppf (s.txt,ed)
    | Pstr_module (s, me) ->
        let rec module_helper me = match me.pmod_desc with
        | Pmod_functor(s,mt,me) ->
            fprintf ppf "(%s:%a)"  s.txt  self#module_type mt ;
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
                fprintf ppf "@ :@ %a@ =@ %a@ "  self#module_type mt self#module_expr  me 
            | _ ->
                fprintf ppf "@ =@ %a"  self#module_expr  me 
            )) me 
    | Pstr_open (li) ->
        fprintf ppf "open %a" self#longident_loc li;
    | Pstr_modtype (s, mt) ->
        fprintf ppf "@[<hov2>module@ type@ %s@ =@ %a@]" s.txt self#module_type mt 
    | Pstr_class l ->
        let class_declaration ppf  (* for the second will be changed to and FIXME*)
            ({pci_params=(ls,_);
              pci_name={txt;_};
              pci_virt;
              pci_expr={pcl_desc;_};
              pci_variance;_ } as x) = (* FIXME pci_variance *)
          let rec  class_fun_helper ppf e = match e.pcl_desc with
          | Pcl_fun (l, eo, p, e) ->
              self#label_exp ppf (l,eo,p);
              class_fun_helper ppf e
          | _ -> e in 
          fprintf ppf "@[<hov2>@ %a%a%s@ %a@]"  self#virtual_flag pci_virt self#class_params_def ls txt 
            (fun ppf _ ->  
              let ce =
                (match pcl_desc with
                | Pcl_fun _ ->
                    class_fun_helper ppf x.pci_expr;
                | _ -> x.pci_expr) in
              let ce =
                (match ce.pcl_desc with
                | Pcl_constraint (ce, ct) ->
                    fprintf ppf ":@ %a@ " self#class_type  ct ;
                    ce
                | _ -> ce ) in
              fprintf ppf "=@ %a" self#class_expr ce ) x in
        (match l with
        |[x ] -> fprintf ppf "@[<hov2>class@ %a@]" class_declaration x
        | xs -> fprintf ppf "@[<hov2>class@ %a@]"
              (self#list class_declaration ~sep:"@\nand@ ") xs )
    | Pstr_class_type (l) ->
        self#class_type_declaration_list ppf l ;
    | Pstr_primitive (s, vd) ->
        let need_parens =
          match s.txt with
          | "or" | "mod" | "land"| "lor" | "lxor" | "lsl" | "lsr" | "asr" -> true
          | _ -> match s.txt.[0] with
              'a'..'z' -> false | _ -> true in
        fprintf ppf "@[<hov2>external@ %s@ :@ %a@]"
          (if need_parens then "( "^s.txt^" )" else s.txt)
          self#value_description  vd
    | Pstr_include me ->
        fprintf ppf "@[<hov2>include@ %a@]"  self#module_expr  me 
    | Pstr_exn_rebind (s, li) ->        (* todo: check this *)
        fprintf ppf "@[<hov2>exception@ %s@ =@ %a@]" s.txt self#longident_loc li 
    | Pstr_recmodule decls -> (* 3.07 *)
        let text_x_modtype_x_module ppf (s, mt, me) =
          fprintf ppf "@[<hov2>and@ %s:%a@ =@ %a@]"
            s.txt self#module_type mt self#module_expr me
        in match decls with
        | (s,mt,me):: l2 ->
            fprintf ppf "@[<hv>@[<hov2>module@ rec@ %s:%a@ =@ %a@]@ %a@]"
              s.txt
              self#module_type mt
              self#module_expr me 
              (fun ppf l2 -> List.iter (text_x_modtype_x_module ppf) l2) l2 
        | _ -> assert false
  end
  (* shared by [Pstr_type,Psig_type]*)    
  method  type_def_list ppf  l =
    let aux ppf (s, ({ptype_params;ptype_kind;ptype_manifest;_} as td )) =
      fprintf ppf "%a %s%a"
        (self#list self#type_var_option ~sep:"," ~first:"(" ~last:")")
        ptype_params s.txt
        (fun ppf td ->begin match ptype_kind, ptype_manifest with
        | Ptype_abstract, None -> ()
        | _ , _ -> fprintf ppf " =@;" end;
          fprintf ppf "%a" self#type_declaration td ) td  in 
    match l with
    | [] -> () ;
    | [x] -> fprintf ppf "@[<2>type %a@]" aux x
    | xs -> fprintf ppf "@[<v>@[<2>type %a"
          (self#list aux ~sep:"@]@,@[<2>and " ~last:"@]@]") xs 
  (* called by type_def_list *)        
  method type_declaration ppf x = begin
    let  type_variant_leaf ppf  (s, l,gadt, _loc)  = match gadt with
    |None -> 
        fprintf ppf "@\n|@;%s%a" s.txt
          (fun ppf l -> match l with
          | [] -> ()
          | _ -> fprintf ppf "@;of@;%a" (self#list self#core_type ~sep:"*") l) l
    |Some x ->
        fprintf ppf "@\n|@;%s:@;%a" s.txt
          (self#list self#core_type ~sep:"@;->@;") (l@[x]) in
    fprintf ppf "%a%a@ %a"
      (fun ppf x -> match (x.ptype_manifest,x.ptype_kind,x.ptype_private) with
      | (None,_,Public) ->  fprintf ppf "@;"
      | (None,_,Private) -> fprintf ppf "private@;"
      | (Some y, Ptype_abstract,Private) -> 
          fprintf ppf "private@;%a" self#core_type y;
      | (Some y, _, Private) -> 
          fprintf ppf "%a = private@;" self#core_type y 
      | (Some y,Ptype_abstract, Public) ->  self#core_type ppf y;
      | (Some y, _,Public) -> begin
          fprintf ppf "%a =@;" self#core_type y (* manifest types*)
      end) x 
      (fun ppf x -> match x.ptype_kind with
        (*here only normal variant types allowed here*)
      | Ptype_variant xs -> 
          fprintf ppf "%a"
            (self#list ~sep:"" type_variant_leaf) xs
      | Ptype_abstract -> ()
      | Ptype_record l ->
          let type_record_field ppf (s, mf, ct,_) =
            fprintf ppf "@[<hov2>%a@ %s:%a@]" self#mutable_flag mf s.txt self#core_type ct in
          fprintf ppf "@[<hov2>{%a}@]"
            (self#list type_record_field ~sep:";" )  l ;
      ) x
      (self#list
         (fun ppf (ct1,ct2,_) ->
           fprintf ppf "@[<hov2>constraint@ %a@ =@ %a@]"
             self#core_type ct1 self#core_type ct2 ))  x.ptype_cstrs  ;
  end

  method case_list ppf (l:(pattern * expression) list) :unit=
    let aux ppf (p,e) =
      let (e,w) =
        (match e with
        | {pexp_desc = Pexp_when (e1, e2);_} -> (e2, Some (e1))
        | _ -> (e, None)) in
      fprintf ppf "| %a%a@;->@;@[<2>@;<2 2>%a@]"
        self#pattern p (self#option self#expression ~first:"@;when@;") w self#expression e in
    self#list aux ~first:"@[<v>@[<2>" ~last:"@]@]" ~sep:"@]@,@[<2>"  ppf l 
    (* fprintf ppf "%a" (self#list aux ~sep:"@\n@;<2 2>") l  *)





(* FIXME *)
   method label_x_expression_param ppf (l,e) =
   match l with
   | ""  -> self#simple_expr ppf e ;
   | lbl ->
   if ((String.get lbl 0) = '?') then begin
   fprintf ppf "%s:" lbl ;
   self#simple_expr ppf e ;
   end else begin
   fprintf ppf "~%s:" lbl ;
   self#simple_expr ppf e ;
   end ;

   method expression_in_parens ppf e =
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
   self#expression ppf e
   else begin
   fprintf ppf "(%a)" self#expression e
   end 
   method pattern_in_parens ppf p =
   let already_has_parens =
   match p.ppat_desc with
   | Ppat_alias (_,_) -> true
   | Ppat_tuple (_) -> true
   | Ppat_or (_,_) -> true
   | Ppat_constraint (_,_) -> true
   | _ -> false in
   if already_has_parens then
   self#pattern ppf p
   else begin
   fprintf ppf "(%a)" self#pattern p 
   end
   method directive_argument ppf x =
   (match x with
   | Pdir_none -> ()
   | Pdir_string (s) -> fprintf ppf "@ %S" s
   | Pdir_int (i) -> fprintf ppf "@ %d" i
   | Pdir_ident (li) -> fprintf ppf "@ %a" self#longident li
   | Pdir_bool (b) -> fprintf ppf "@ %s" (string_of_bool b))

   method toplevel_phrase ppf x =
   match x with
   | Ptop_def (s) ->
   pp_open_hvbox ppf 0;
   self#list self#structure_item ppf s ;
   pp_close_box ppf ();
   | Ptop_dir (s, da) ->
   fprintf ppf "@[<hov2>#%s@ %a@]" s self#directive_argument da 
end;;


let default = new printer ()


let toplevel_phrase ppf x =
  match x with
  | Ptop_def (s) ->fprintf ppf "@[<hov0>%a@]"  (default#list default#structure_item) s 
   (* pp_open_hvbox ppf 0; *)
   (* pp_print_list structure_item ppf s ; *)
   (* pp_close_box ppf (); *)
  | Ptop_dir (s, da) ->
   fprintf ppf "@[<hov2>#%s@ %a@]" s default#directive_argument da 
   (* fprintf ppf "@[<hov2>#%s@ %a@]" s directive_argument da *)

let expression ppf x =
  fprintf ppf "@[%a@]" default#expression x


let string_of_expression x =
  ignore (flush_str_formatter ()) ;
  let ppf = str_formatter in
  default#expression ppf x ;
  flush_str_formatter () ;;

let top_phrase ppf x =
  pp_print_newline ppf () ;
  toplevel_phrase ppf x;
  fprintf ppf ";;" ;
  pp_print_newline ppf ();;

let core_type=default#core_type
let pattern=default#pattern
let signature=default#signature
let structure=default#structure   
