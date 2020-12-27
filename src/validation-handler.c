
#include "wk-v1.h"
#include <stdlib.h>
#include <Rinternals.h>

typedef struct {
  SEXP xptr;
} ValidationHandlerData_t;

char wk_handler_validation_vector_start(const WKGeometryMeta_t* meta, void* userData) {
  ValidationHandlerData_t* handlerData = (ValidationHandlerData_t*) userData;

  SEXP output = PROTECT(Rf_allocVector(STRSXP, meta->size));
  R_xlen_t nFeatures = meta->size;
  for (R_xlen_t i = 0; i < nFeatures; i++) {
    SET_STRING_ELT(output, i, NA_STRING);
  }

  R_SetExternalPtrProtected(handlerData->xptr, output);
  UNPROTECT(1);
  return WK_CONTINUE;
}

char wk_handler_validation_error(R_xlen_t featureId, int code, const char* message, void* userData) {
  ValidationHandlerData_t* handlerData = (ValidationHandlerData_t*) userData;
  SET_STRING_ELT(R_ExternalPtrProtected(handlerData->xptr), featureId, Rf_mkChar(message));
  return WK_ABORT_FEATURE;
}

SEXP wk_handler_validation_vector_end(const WKGeometryMeta_t* meta, void* userData) {
  ValidationHandlerData_t* handlerData = (ValidationHandlerData_t*) userData;
  return R_ExternalPtrProtected(handlerData->xptr);
}

void wk_handler_validation_finalizer(void* userData) {
  ValidationHandlerData_t* handlerData = (ValidationHandlerData_t*) userData;
  if (handlerData != NULL) {
    free(handlerData);
  }
}

SEXP wk_c_handler_validation_new() {
  WKHandler_t* handler = wk_handler_create();

  handler->vectorStart = &wk_handler_validation_vector_start;
  handler->vectorEnd = &wk_handler_validation_vector_end;
  handler->error = &wk_handler_validation_error;
  handler->finalizer = &wk_handler_validation_finalizer;

  ValidationHandlerData_t* handlerData = (ValidationHandlerData_t*) malloc(sizeof(ValidationHandlerData_t));
  handlerData->xptr = R_NilValue;
  handler->userData = handlerData;

  SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
  handlerData->xptr = xptr;

  return xptr;
}
