
open LibUtil;
let f = Format.std_formatter;
let rule_list = Gram.mk "rule_list"  ;
{|Gram LOCAL: rule;
  rule_list:
  [ [ "["; "]" 
  | "["; LIST1 rule SEP "|"; "]"  -> pp f "@[loc:%a@]@." FanLoc.print _loc ] ]
  rule:
  [ [`INT (_,_)  ]]  
|};
let f = Format.std_formatter;
Gram.dump f rule_list;
Gram.parse_string rule_list FanLoc.string_loc "[ 3 | 4]" ;
