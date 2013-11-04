

%lang{
name:"plc";
parser:
      prog:
      []

lexer:
      keywords:
      [".";":";"-";",";"(";")";"=";
       "\\=";"is";"=:=";"=\\=";"<";
       "=<";">";">=";"+"; "_";"!";
       "[";"]";"|";"%:";"?";":-"];
      ;
    
transformer:
      ;
    
position:
      stru
      ;
}
