
#include "wk-v1.h"
#include <stdlib.h>
#include <Rinternals.h>

char wk_handler_validation_vector_start(const WKGeometryMeta_t* meta, void* userData) {
  SEXP output = PROTECT(Rf_allocVector(STRSXP, meta->size));
  R_xlen_t nFeatures = meta->size;
  for (R_xlen_t i = 0; i < nFeatures; i++) {
    SET_STRING_ELT(output, i, NA_STRING);
  }

  R_SetExternalPtrProtected((SEXP) userData, output);
  UNPROTECT(1);
  return WK_CONTINUE;
}

char wk_handler_validation_error(R_xlen_t featureId, int code, const char* message, void* userData) {
  SET_STRING_ELT(R_ExternalPtrProtected((SEXP) userData), featureId, Rf_mkChar(message));
  return WK_ABORT_FEATURE;
}

SEXP wk_handler_validation_vector_end(const WKGeometryMeta_t* meta, void* userData) {
  return R_ExternalPtrProtected((SEXP) userData);
}

SEXP wk_c_handler_validation_new() {
  WKHandler_t* handler = wk_handler_create();

  handler->vectorStart = &wk_handler_validation_vector_start;
  handler->vectorEnd = &wk_handler_validation_vector_end;
  handler->error = &wk_handler_validation_error;

  SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
  handler->userData = xptr;

  return xptr;
}
