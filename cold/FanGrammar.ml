open Ast
type name =  {
  expr: expr;
  tvar: string;
  loc: loc} 
type styp =
  [ `Id of (loc* ident) | `App of (loc* styp* styp)
  | `Quote of (loc* position_flag* alident) | `Self of (loc* string)
  | `Tok of loc | `Type of ctyp] 
type attr = string 
type entry =  {
  name: name;
  pos: expr option;
  levels: levels} 
and levels = [ `Group of level list | `Single of level] 
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
  [ `Smeta of (loc* string list* text list* expr* styp)
  | `Slist of (loc* bool* symbol* symbol option)
  | `Snterm of (loc* name* string option) | `Sopt of (loc* text)
  | `Stry of (loc* text) | `Speek of (loc* text)
  | `Srules of (loc* (text list* expr) list) | `Sself of loc | `Snext of loc
  | `Skeyword of (loc* string) | `Stok of (loc* expr* attr* string)] 
type used =  
  | Unused
  | UsedScanned
  | UsedNotScanned 
type simple_patt =
  [ `Vrn of (loc* string) | `App of (loc* simple_patt* simple_patt)
  | `Id of (loc* ident) | `Com of (loc* simple_patt* simple_patt)
  | `Alias of (loc* simple_patt* alident)
  | `Or of (loc* simple_patt* simple_patt) | `Str of (loc* string)
  | `Any of loc | ant] 
type action_pattern =
  [ `Id of (loc* ident) | `Com of (loc* action_pattern* action_pattern)
  | `Tup of (loc* action_pattern) | `Any of loc] 