
#include "wk-v1.h"
#include <Rinternals.h>

SEXP wk_c_handler_void_new() {
  return WKV1_handler_create_xptr(WKV1_handler_create(), R_NilValue, R_NilValue);
}

SEXP wk_c_handler_addr(SEXP xptr) {
  SEXP buffer = PROTECT(Rf_allocVector(RAWSXP, 32));
  sprintf((char*) RAW(buffer), "%p", (void*) R_ExternalPtrAddr(xptr));

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar((char*) RAW(buffer)));
  UNPROTECT(2);
  return out;
}
