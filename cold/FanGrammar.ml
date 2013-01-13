open Ast
type loc = FanLoc.t 
type name =  {
  expr: expr;
  tvar: string;
  loc: loc} 
type styp =
  [ `Id of (loc* ident) | `TyApp of (loc* styp* styp)
  | `Quote of (loc* position_flag* alident meta_option)
  | `Self of (loc* string) | `Tok of loc | `Type of ctyp] 
type attr = string 
type entry =  {
  name: name;
  pos: expr option;
  levels: level list} 
and level =  {
  label: string option;
  assoc: expr option;
  rules: rule list} 
and rule =  {
  prod: symbol list;
  action: expr option} 
and symbol =  {
  text: text;
  styp: styp;
  pattern: patt option} 
and text =
  [ `TXmeta of (loc* string list* text list* expr* styp)
  | `TXlist of (loc* bool* symbol* symbol option)
  | `TXnterm of (loc* name* string option) | `TXopt of (loc* text)
  | `TXtry of (loc* text) | `TXpeek of (loc* text)
  | `TXrules of (loc* (text list* expr) list) | `TXself of loc
  | `TXnext of loc | `TXkwd of (loc* string)
  | `TXtok of (loc* expr* attr* string)] 
type used =  
  | Unused
  | UsedScanned
  | UsedNotScanned 