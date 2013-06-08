open FAst

type name =  {
  exp: exp;
  tvar: string;
  loc: loc} 

type styp =
  [ vid' | `App of (loc * styp * styp)
  | `Quote of (loc * position_flag * alident) | `Self of loc | `Tok of loc
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
  [ `Slist of (loc * bool * symbol * symbol option)
  | `Snterm of (loc * name * string option) | `Sopt of (loc * text)
  | `Stry of (loc * text) | `Speek of (loc * text) | `Sself of loc
  | `Skeyword of (loc * string) | `Stok of (loc * exp * FAstN.pat)] 

type used =  
  | Unused
  | UsedScanned
  | UsedNotScanned 

type simple_pat =
  [ `Vrn of (loc * string) | `App of (loc * simple_pat * simple_pat) | 
    vid
  | `Com of (loc * simple_pat * simple_pat)
  | `Alias of (loc * simple_pat * alident)
  | `Bar of (loc * simple_pat * simple_pat) | `Str of (loc * string)
  | `Any of loc | ant] 

type action_pattern =
  [ vid | `Com of (loc * action_pattern * action_pattern)
  | `Par of (loc * action_pattern) | `Any of loc] 

let _ = FConfig.antiquotations := true

open Fsyntax

let simple_pat: simple_pat Fgram.t = Fgram.mk "simple_pat"

let _ =
  let grammar_entry_create x = Fgram.mk x in
  let internal_pat: 'internal_pat Fgram.t =
    grammar_entry_create "internal_pat" in
  begin
    Fgram.extend_single (simple_pat : 'simple_pat Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "`";
            `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
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
                    `Any)))],
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
             (((function | `STR (_,_) -> true | _ -> false)),
               (`App ((`App ((`Vrn "STR"), `Any)), `Any)))],
            ("`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v)))\n",
              (Fgram.mk_action
                 (fun (__fan_2 : [> FToken.t])  (s : 'luident)  _ 
                    (_loc : FLoc.t)  ->
                    match __fan_2 with
                    | `STR (_,v) ->
                        (`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v))) : 
                        'simple_pat )
                    | _ ->
                        failwith
                          "`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v)))\n"))));
          ([`Skeyword "`";
           `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
           `Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)))],
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
             `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
              ("`Alias (_loc, p1, s)\n",
                (Fgram.mk_action
                   (fun (s : 'a_lident)  _  (p1 : 'internal_pat) 
                      (_loc : FLoc.t)  ->
                      (`Alias (_loc, p1, s) : 'internal_pat )))))]);
        ((Some "|"), None,
          [([`Sself; `Skeyword "|"; `Sself],
             ("`Bar (_loc, p1, p2)\n",
               (Fgram.mk_action
                  (fun (p2 : 'internal_pat)  _  (p1 : 'internal_pat) 
                     (_loc : FLoc.t)  ->
                     (`Bar (_loc, p1, p2) : 'internal_pat )))))]);
        ((Some "simple"), None,
          [([`Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`App ((`App ((`Vrn "STR"), `Any)), `Any)))],
             ("`Str (_loc, s)\n",
               (Fgram.mk_action
                  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `STR (_,s) -> (`Str (_loc, s) : 'internal_pat )
                     | _ -> failwith "`Str (_loc, s)\n"))));
          ([`Skeyword "_"],
            ("`Any _loc\n",
              (Fgram.mk_action
                 (fun _  (_loc : FLoc.t)  -> (`Any _loc : 'internal_pat )))));
          ([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`App ((`Vrn "Lid"), `Any)))],
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
  end