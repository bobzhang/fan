type loc = FanLoc.t 
type ant = [ `Ant of (loc* FanUtil.anti_cxt)] 
type nil = [ `Nil of loc] 
type ant_nil = [ ant | nil] 
type literal =
  [ `Chr of (loc* string) | `Int of (loc* string) | `Int32 of (loc* string)
  | `Int64 of (loc* string) | `Flo of (loc* string)
  | `NativeInt of (loc* string) | `Str of (loc* string)] 
type rec_flag = [ `Recursive of loc | `ReNil of loc | ant] 
type direction_flag = [ `To of loc | `Downto of loc | ant] 
type mutable_flag = [ `Mutable of loc | `MuNil of loc | ant] 
type private_flag = [ `Private of loc | `PrNil of loc | ant] 
type virtual_flag = [ `Virtual of loc | `ViNil of loc | ant] 
type override_flag = [ `Override of loc | `OvNil of loc | ant] 
type row_var_flag = [ `RowVar of loc | `RvNil of loc | ant] 
type position_flag =
  [ `Positive of loc | `Negative of loc | `Normal of loc | ant] 
type meta_bool = [ `True of loc | `False of loc | ant] 
type 'a meta_option = [ `None | `Some of 'a | ant] 
type 'a meta_list = [ `LNil | `LCons of ('a* 'a meta_list) | ant] 
type alident = [ `Lid of (loc* string) | ant] 
type auident = [ `Uid of (loc* string) | ant] 
type aident = [ alident | auident] 
type astring = [ `C of (loc* string) | ant] 
type uident =
  [ `Dot of (loc* uident* uident) | `App of (loc* uident* uident) | auident] 
type ident =
  [ `Dot of (loc* ident* ident) | `App of (loc* ident* ident) | alident
  | auident] 
type dupath = [ `Dot of (loc* dupath* dupath) | auident] 
type dlpath = [ `Dot of (loc* dupath* alident) | alident] 
type sid = [ `Id of (loc* ident)] 
type any = [ `Any of loc] 
type patt =
  [ nil | sid | `App of (loc* patt* patt) | `Vrn of (loc* string)
  | `Com of (loc* patt* patt) | `Sem of (loc* patt* patt)
  | `Tup of (loc* patt) | any | `Record of (loc* rec_patt) | ant | literal
  | `Alias of (loc* patt* alident) | `Array of (loc* patt)
  | `Label of (loc* alident* patt)
  | `PaOlbi of (loc* alident* patt* expr meta_option)
  | `Or of (loc* patt* patt) | `PaRng of (loc* patt* patt)
  | `Constraint of (loc* patt* ctyp) | `ClassPath of (loc* ident)
  | `Lazy of (loc* patt) | `ModuleUnpack of (loc* auident* ctyp meta_option)] 
and rec_patt =
  [ nil | `RecBind of (loc* ident* patt) | `Sem of (loc* rec_patt* rec_patt)
  | any | ant] 