
let mkgh loc = FanLoc.ghostify loc;

let add  txt loc = Location.mkloc txt  loc;

let error loc str = FanLoc.raise loc (Failure str);  
