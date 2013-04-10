open Ast

type name =  {
  exp: exp;
  tvar: string;
  loc: loc} 

type styp =
  [ `Id of (loc * ident) | `App of (loc * styp * styp)
  | `Quote of (loc * position_flag * alident) | `Self of (loc * string)
  | `Tok of loc | `Type of ctyp] 

type attr = string 

type entry =  {
  name: name;
  pos: exp option;
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
  [ `Smeta of (loc * string list * text list * exp * styp)
  | `Slist of (loc * bool * symbol * symbol option)
  | `Snterm of (loc * name * string option) | `Sopt of (loc * text)
  | `Stry of (loc * text) | `Speek of (loc * text)
  | `Srules of (loc * (text list * exp) list) | `Sself of loc | `Snext of loc
  | `Skeyword of (loc * string) | `Stok of (loc * exp * attr * string)] 

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