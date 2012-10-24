type section =  string 
val mode : (section -> bool) 
val printf : (section -> (('a ,Format.formatter,unit)  format  -> 'a ) ) 