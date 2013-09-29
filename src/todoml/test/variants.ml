

type 'a u = [< `a of int | `b of bool & int & string > `a ]  as 'a

(*
                    Ptyp_variant closed=true
                  [
                    Rtag "a" false
                      [
                        core_type (test/variants.ml[3,2+21]..[3,2+24])
                          Ptyp_constr "int" (test/variants.ml[3,2+21]..[3,2+24])
                          []
                      ]
                    Rtag "b" false
                      [
                        core_type (test/variants.ml[3,2+33]..[3,2+37])
                          Ptyp_constr "bool" (test/variants.ml[3,2+33]..[3,2+37])
                          []
                        core_type (test/variants.ml[3,2+40]..[3,2+43])
                          Ptyp_constr "int" (test/variants.ml[3,2+40]..[3,2+43])
                          []
                        core_type (test/variants.ml[3,2+46]..[3,2+52])
                          Ptyp_constr "string" (test/variants.ml[3,2+46]..[3,2+52])
                          []
                      ]
                  ]
                  Some
                    [
                      "a"
                    ]
 *)      


type 'a u1 = [> `a of int ] as 'a
(*
  Ptyp_variant closed=false
                  [
                    Rtag "a" false
                      [
                        core_type (test/variants.ml[34,1157+22]..[34,1157+25])
                          Ptyp_constr "int" (test/variants.ml[34,1157+22]..[34,1157+25])
                          []
                      ]
                  ]
                  None
 *)      

      
type u2 = [ `a of int]      
(*
                  Ptyp_variant closed=true
                [
                  Rtag "a" false
                    [
                      core_type (test/variants.ml[36,1192+18]..[36,1192+21])
                        Ptyp_constr "int" (test/variants.ml[36,1192+18]..[36,1192+21])
                        []
                    ]
                ]
                None
 *)      



(*
  Full specifications of variant tags are only used for non-exact closed types. 
 *)
type 'a u3 = [< `b | `a of  & int & bool ] as 'a
(*
   Ptyp_variant closed=true
                    [
                      Rtag "b" true
                        []
                      Rtag "a" true
                        [
                          core_type (//toplevel//[1,0+30]..[1,0+33])
                            Ptyp_constr "int" (//toplevel//[1,0+30]..[1,0+33])
                            []
                          core_type (//toplevel//[1,0+36]..[1,0+40])
                            Ptyp_constr "bool" (//toplevel//[1,0+36]..[1,0+40])
                            []
                        ]
                    ]
                    Some
                      []
 *)  

type 'a u4 = [< `b | `a of int & bool ]   as 'a

(*
  Ptyp_variant closed=true
                    [
                      Rtag "b" true
                        []
                      Rtag "a" false
                        [
                          core_type (//toplevel//[1,0+27]..[1,0+30])
                            Ptyp_constr "int" (//toplevel//[1,0+27]..[1,0+30])
                            []
                          core_type (//toplevel//[1,0+33]..[1,0+37])
                            Ptyp_constr "bool" (//toplevel//[1,0+33]..[1,0+37])
                            []
                        ]
                    ]
                    Some
                      []
 *)

  










