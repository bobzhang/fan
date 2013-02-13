


func {:func|
def foo(a, b)
  a * a + 2.0 * a * b + b * b
|};

expr {:expr|3.0 + 4.0 |};

func {:func|
def bar(a) foo(a,4.0)+bar(313.0)
|};



proto {:proto|
extern cos(x)
|};

expr {:expr|
cos(1.234)
|};


(*
  # the_module;
; ModuleID = 'my cool jit'

define double @foo(double %a, double %b) {
entry:
  %multmp = fmul double %a, %a
  %multmp1 = fmul double 2.000000e+00, %a
  %multmp2 = fmul double %multmp1, %b
  %addtmp = fadd double %multmp, %multmp2
  %multmp3 = fmul double %b, %b
  %addtmp4 = fadd double %addtmp, %multmp3
  ret double %addtmp4
}

define double @bar(double %a) {
entry:
  %calltmp = call double @foo(double %a, double 4.000000e+00)
  %calltmp1 = call double @bar(double 3.130000e+02)
  %addtmp = fadd double %calltmp, %calltmp1
  ret double %addtmp
  %calltmp2 = call double @cos(double 1.234000e+00)
}

declare double @cos(double)

 *)
