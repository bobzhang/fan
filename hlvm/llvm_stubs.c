#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "llvm/Config/config.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

extern void enable_tail_call_opt();

CAMLprim void llvm_enable_tail_call_opt() {
  enable_tail_call_opt();
}
