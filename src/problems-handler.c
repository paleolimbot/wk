
#include "wk-v1.h"
#include <stdlib.h>
#include <Rinternals.h>

int wk_handler_problems_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  SEXP output = PROTECT(Rf_allocVector(STRSXP, meta->size));
  R_xlen_t n_features = meta->size;
  for (R_xlen_t i = 0; i < n_features; i++) {
    SET_STRING_ELT(output, i, NA_STRING);
  }

  R_SetExternalPtrProtected((SEXP) handler_data, output);
  UNPROTECT(1);
  return WK_CONTINUE;
}

int wk_handler_problems_error(R_xlen_t feat_id, int code, const char* message, void* handler_data) {
  SET_STRING_ELT(R_ExternalPtrProtected((SEXP) handler_data), feat_id, Rf_mkChar(message));
  return WK_ABORT_FEATURE;
}

SEXP wk_handler_problems_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  return R_ExternalPtrProtected((SEXP) handler_data);
}

SEXP wk_c_handler_problems_new() {
  wk_handler_t* handler = wk_handler_create();

  handler->vector_start = &wk_handler_problems_vector_start;
  handler->vector_end = &wk_handler_problems_vector_end;
  handler->error = &wk_handler_problems_error;

  SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
  handler->handler_data = xptr;

  return xptr;
}
