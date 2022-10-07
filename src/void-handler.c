
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

SEXP wk_c_handler_void_new(void) {
  return wk_handler_create_xptr(wk_handler_create(), R_NilValue, R_NilValue);
}

SEXP wk_c_handler_addr(SEXP xptr) {
  SEXP buffer = PROTECT(Rf_allocVector(RAWSXP, 256));
  sprintf((char*) RAW(buffer), "%p", (void*) R_ExternalPtrAddr(xptr));

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar((char*) RAW(buffer)));
  UNPROTECT(2);
  return out;
}
