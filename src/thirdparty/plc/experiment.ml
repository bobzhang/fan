


%lang{

name:"Fan.Lang.plc"
  ;

parser:
      {
       prog:
         []
     };

lexer:
      {
       keywords:
         [".";":";"-";",";"(";")";"=";
          "\\=";"is";"=:=";"=\\=";"<";
          "=<";">";">=";"+"; "_";"!";
          "[";"]";"|";"%:";"?";":-"];
       ocaml_style:
         basic;
     }
      ;
    
transformer:
      {
     }
      ;
    
position:
      stru
      ;
}
