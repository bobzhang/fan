open FAst
class primitive =
  object 
    method string _loc (i : string) =
      ((`Str (_loc, (String.escaped i)) : FAst.ep ) : ep )
  end
type word = [ `Any | `A of string | `Empty] 
and data = (string* word) 
class meta =
  object (self : 'self_type)
    inherit  primitive
    method word : 'loc -> word -> FAst.ep=
      fun _loc  ->
        function
        | `Any -> `Vrn (_loc, "Any")
        | `A _a0 -> `App (_loc, (`Vrn (_loc, "A")), (self#string _loc _a0))
        | `Empty -> `Vrn (_loc, "Empty")
    method data : 'loc -> data -> FAst.ep=
      fun _loc  _a0  ->
        (fun _loc  (_a0,_a1)  ->
           `Par
             (_loc,
               (`Com (_loc, (self#string _loc _a0), (self#word _loc _a1)))))
          _loc _a0
  end
let meta_data = new meta
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
  | `Skeyword of (loc* string) | `Stoken of (loc* exp* exp* string)] 
type entries =  {
  items: entry list;
  gram: vid option;
  safe: bool} 