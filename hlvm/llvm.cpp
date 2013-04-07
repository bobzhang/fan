#include "llvm/Target/TargetOptions.h"
#include <iostream>

extern "C" {
  using namespace std;

  void enable_tail_call_opt() {
    cout << "Enabling TCO" << endl;
    //llvm::PerformTailCallOpt = true;
    llvm::GuaranteedTailCallOpt = true;
  }

}
