open Parsetree;
let mk loc d = {ppat_desc = d; ppat_loc =  loc};
let mkgh loc d = {ppat_desc = d; ppat_loc =Ploc.mkgh loc};
