open FAst
open LibUtil
let pp_print_loc _f _loc = ()
let pp_print_string = StdFan.pp_print_string
let pp_print_vid' = Objs.pp_print_vid'
let pp_print_vid = Objs.pp_print_vid
let pp_print_alident = Objs.pp_print_alident
let pp_print_ant = Objs.pp_print_ant
class mapbase =
  object 
    method loc (x : loc) = x
    method string (x : string) = x
    method ant (x : ant) = x
  end
type lident = [ `Lid of (loc* string)] 
and simple_pat =
  [ `Vrn of (loc* string) | `App of (loc* simple_pat* simple_pat)
  | `Lid of (loc* string) | ant | `Com of (loc* simple_pat* simple_pat)
  | `Alias of (loc* simple_pat* lident)
  | `Bar of (loc* simple_pat* simple_pat) | `Str of (loc* string)
  | `Any of loc] 
class map =
  object (self : 'self_type)
    inherit  mapbase
    method lident : lident -> lident=
      fun (`Lid (_a0,_a1))  ->
        let _a0 = self#loc _a0 in
        let _a1 = self#string _a1 in `Lid (_a0, _a1)
    method simple_pat : simple_pat -> simple_pat=
      function
      | `Vrn (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Vrn (_a0, _a1)
      | `App (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#simple_pat _a1 in
          let _a2 = self#simple_pat _a2 in `App (_a0, _a1, _a2)
      | `Lid (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Lid (_a0, _a1)
      | #ant as _a0 -> (self#ant _a0 : ant  :>simple_pat)
      | `Com (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#simple_pat _a1 in
          let _a2 = self#simple_pat _a2 in `Com (_a0, _a1, _a2)
      | `Alias (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#simple_pat _a1 in
          let _a2 = self#lident _a2 in `Alias (_a0, _a1, _a2)
      | `Bar (_a0,_a1,_a2) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#simple_pat _a1 in
          let _a2 = self#simple_pat _a2 in `Bar (_a0, _a1, _a2)
      | `Str (_a0,_a1) ->
          let _a0 = self#loc _a0 in
          let _a1 = self#string _a1 in `Str (_a0, _a1)
      | `Any _a0 -> let _a0 = self#loc _a0 in `Any _a0
  end
let rec pp_print_lident: Format.formatter -> lident -> unit =
  fun fmt  (`Lid (_a0,_a1))  ->
    Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
      pp_print_string _a1
and pp_print_simple_pat: Format.formatter -> simple_pat -> unit =
  fun fmt  ->
    function
    | `Vrn (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Vrn@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `App (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`App@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_simple_pat _a1 pp_print_simple_pat _a2
    | `Lid (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Lid@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | #ant as _a0 -> (pp_print_ant fmt _a0 :>unit)
    | `Com (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Com@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_simple_pat _a1 pp_print_simple_pat _a2
    | `Alias (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Alias@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_simple_pat _a1 pp_print_lident _a2
    | `Bar (_a0,_a1,_a2) ->
        Format.fprintf fmt "@[<1>(`Bar@ %a@ %a@ %a)@]" pp_print_loc _a0
          pp_print_simple_pat _a1 pp_print_simple_pat _a2
    | `Str (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Str@ %a@ %a)@]" pp_print_loc _a0
          pp_print_string _a1
    | `Any _a0 -> Format.fprintf fmt "@[<1>(`Any@ %a)@]" pp_print_loc _a0
let wildcarder =
  object (self)
    inherit  map as super
    method! simple_pat =
      function
      | `Lid (_loc,_) -> `Any _loc
      | `Alias (_loc,p,_) -> self#simple_pat p
      | p -> super#simple_pat p
  end
type name =  {
  exp: exp;
  tvar: string;
  loc: loc} 
type styp =
  [ vid' | `App of (loc* styp* styp)
  | `Quote of (loc* position_flag* alident) | `Self of loc | `Tok of loc
  | `Type of ctyp] 
type entry =  {
  name: name;
  pos: exp option;
  local: bool;
  levels: levels} 
and levels = [ `Group of level list | `Single of level] 
and level =  {
  label: string option;
  assoc: exp option;
  rules: rule list} 
and rule =  {
  prod: symbol list;
  action: exp option} 
and symbol =  {
  text: text;
  styp: styp;
  pattern: pat option} 
and text =
  [ `Slist of (loc* bool* symbol* symbol option)
  | `Snterm of (loc* name* string option) | `Sopt of (loc* text)
  | `Stry of (loc* text) | `Speek of (loc* text) | `Sself of loc
  | `Skeyword of (loc* string) | `Stok of (loc* exp* simple_pat)] 
type action_pattern =
  [ vid | `Com of (loc* action_pattern* action_pattern)
  | `Par of (loc* action_pattern) | `Any of loc] 
let _ = FConfig.antiquotations := true
open Fsyntax
let simple_pat: simple_pat Fgram.t = Fgram.mk "simple_pat"
let _ =
  let grammar_entry_create x = Fgram.mk x in
  let internal_pat: 'internal_pat Fgram.t =
    grammar_entry_create "internal_pat" in
  Fgram.extend_single (simple_pat : 'simple_pat Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "`"; `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
           ("`Vrn (_loc, s)\n",
             (Fgram.mk_action
                (fun (s : 'luident)  _  (_loc : FLoc.t)  ->
                   (`Vrn (_loc, s) : 'simple_pat )))));
        ([`Skeyword "`";
         `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
         `Stoken
           (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
             (`App
                ((`App ((`Vrn "Ant"), (`Bar ((`Str ""), (`Str "anti"))))),
                  `Any)), "`Ant (\"\"| \"anti\",_)")],
          ("`App (_loc, (`Vrn (_loc, v)), (FanUtil.mk_anti _loc ~c:\"pat\" n s))\n",
            (Fgram.mk_action
               (fun (__fan_2 : [> FToken.t])  (v : 'luident)  _ 
                  (_loc : FLoc.t)  ->
                  match __fan_2 with
                  | `Ant ((""|"anti" as n),s) ->
                      (`App
                         (_loc, (`Vrn (_loc, v)),
                           (FanUtil.mk_anti _loc ~c:"pat" n s)) : 'simple_pat )
                  | _ ->
                      failwith
                        "`App (_loc, (`Vrn (_loc, v)), (FanUtil.mk_anti _loc ~c:\"pat\" n s))\n"))));
        ([`Skeyword "`";
         `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
         `Stoken
           (((function | `STR _ -> true | _ -> false)),
             (`App ((`Vrn "STR"), `Any)), "`STR _")],
          ("`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v)))\n",
            (Fgram.mk_action
               (fun (__fan_2 : [> FToken.t])  (s : 'luident)  _ 
                  (_loc : FLoc.t)  ->
                  match __fan_2 with
                  | `STR v ->
                      (`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v))) : 
                      'simple_pat )
                  | _ ->
                      failwith
                        "`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v)))\n"))));
        ([`Skeyword "`";
         `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x)))\n",
            (Fgram.mk_action
               (fun (__fan_2 : [> FToken.t])  (s : 'luident)  _ 
                  (_loc : FLoc.t)  ->
                  match __fan_2 with
                  | `Lid x ->
                      (`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x))) : 
                      'simple_pat )
                  | _ ->
                      failwith
                        "`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x)))\n"))));
        ([`Skeyword "`";
         `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
         `Skeyword "_"],
          ("`App (_loc, (`Vrn (_loc, s)), (`Any _loc))\n",
            (Fgram.mk_action
               (fun _  (s : 'luident)  _  (_loc : FLoc.t)  ->
                  (`App (_loc, (`Vrn (_loc, s)), (`Any _loc)) : 'simple_pat )))));
        ([`Skeyword "`";
         `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
         `Skeyword "(";
         `Slist1sep
           ((`Snterm (Fgram.obj (internal_pat : 'internal_pat Fgram.t ))),
             (`Skeyword ","));
         `Skeyword ")"],
          ("AstLib.appl_of_list ((`Vrn (_loc, s)) :: v)\n",
            (Fgram.mk_action
               (fun _  (v : 'internal_pat list)  _  (s : 'luident)  _ 
                  (_loc : FLoc.t)  ->
                  (AstLib.appl_of_list ((`Vrn (_loc, s)) :: v) : 'simple_pat )))))]));
  Fgram.extend (internal_pat : 'internal_pat Fgram.t )
    (None,
      [((Some "as"), None,
         [([`Sself;
           `Skeyword "as";
           `Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
            ("`Alias (_loc, p1, (`Lid (_loc, s)))\n",
              (Fgram.mk_action
                 (fun (__fan_2 : [> FToken.t])  _  (p1 : 'internal_pat) 
                    (_loc : FLoc.t)  ->
                    match __fan_2 with
                    | `Lid s ->
                        (`Alias (_loc, p1, (`Lid (_loc, s))) : 'internal_pat )
                    | _ -> failwith "`Alias (_loc, p1, (`Lid (_loc, s)))\n"))))]);
      ((Some "|"), None,
        [([`Sself; `Skeyword "|"; `Sself],
           ("`Bar (_loc, p1, p2)\n",
             (Fgram.mk_action
                (fun (p2 : 'internal_pat)  _  (p1 : 'internal_pat) 
                   (_loc : FLoc.t)  -> (`Bar (_loc, p1, p2) : 'internal_pat )))))]);
      ((Some "simple"), None,
        [([`Stoken
             (((function | `STR _ -> true | _ -> false)),
               (`App ((`Vrn "STR"), `Any)), "`STR _")],
           ("`Str (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `STR s -> (`Str (_loc, s) : 'internal_pat )
                   | _ -> failwith "`Str (_loc, s)\n"))));
        ([`Skeyword "_"],
          ("`Any _loc\n",
            (Fgram.mk_action
               (fun _  (_loc : FLoc.t)  -> (`Any _loc : 'internal_pat )))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)),
              (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("`Lid (_loc, x)\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Lid x -> (`Lid (_loc, x) : 'internal_pat )
                  | _ -> failwith "`Lid (_loc, x)\n"))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("p\n",
            (Fgram.mk_action
               (fun _  (p : 'internal_pat)  _  (_loc : FLoc.t)  ->
                  (p : 'internal_pat )))))])])
open Format
let p = fprintf
let rec unparse_simple_pat f (x : simple_pat) =
  match x with
  | `Vrn (_,s) -> p f "`%s" s
  | `App _ ->
      let l = AstLib.list_of_app x [] in
      (match l with
       | (`Vrn _ as x)::[] -> unparse_simple_pat f x
       | (`Vrn _ as x)::v::[] ->
           p f "%a %a" unparse_simple_pat x unparse_simple_pat v
       | (`Vrn _ as x)::rest ->
           p f "%a (%a)" unparse_simple_pat x
             (pp_list unparse_simple_pat ~sep:",") rest
       | _ ->
           (p Format.err_formatter "impossible pattern %a@."
              pp_print_simple_pat x;
            invalid_arg "unparse_simple_pat"))
  | `Com (_,a,b) -> p f "%a, %a" unparse_simple_pat a unparse_simple_pat b
  | `Alias (_,p,_) -> unparse_simple_pat f p
  | `Bar (_,a,b) -> p f "%a| %a" unparse_simple_pat a unparse_simple_pat b
  | `Str (_,s) -> p f "%S" s
  | `Any _ -> p f "_"
  | `Lid (_,s) -> p f "%s" s
  | `Ant (_,{ FanUtil.content = s;_}) -> p f "$%s" s
let string_of_simple_pat = to_string_of_printer unparse_simple_pat