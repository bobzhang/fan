open FAst
type name =  {
  id: vid;
  tvar: string;
  loc: loc} 
type styp =
  [ vid' | `App of (loc* styp* styp)
  | `Quote of (loc* position_flag* alident) | `Self of loc | `Type of ctyp] 
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
and rule = 
  {
  prod: osymbol list;
  action: exp option;
  mutable env: (locid* exp) list} 
and label = string option 
and kind =  
  | KNone
  | KSome
  | KNormal 
and locid = (loc* string) 
and symbol = 
  {
  text: text;
  styp: styp;
  pattern: (locid* label) list;
  bounds: locid list} 
and 'a decorate =  {
  kind: kind;
  txt: 'a} 
and osymbol = 
  {
  text: text;
  styp: styp;
  pattern: (locid* label) list;
  bounds: locid list;
  outer_pattern: locid option} 
and text =
  [ `List of (loc* bool* symbol* symbol option)
  | `Nterm of (loc* name* string option) | `Try of (loc* text)
  | `Peek of (loc* text) | `Self of loc | `Keyword of (loc* string)
  | `Token of (loc* exp)] 
type entries =  {
  items: entry list;
  gram: vid option;
  safe: bool} 
