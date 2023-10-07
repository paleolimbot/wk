
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

SEXP wk_c_handler_void_new(void) {
  return wk_handler_create_xptr(wk_handler_create(), R_NilValue, R_NilValue);
}

SEXP wk_c_handler_addr(SEXP xptr) {
  char buffer[256];
  snprintf(buffer, 256, "%p", (void*)R_ExternalPtrAddr(xptr));
  return Rf_mkString(buffer);
}
