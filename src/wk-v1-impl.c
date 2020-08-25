
#include "wk-v1.h"
#include <stdlib.h>

char WKV1_handler_void_vector_start(WKV1_GeometryMeta* meta, void* userData) {
  return 0;
}

SEXP WKV1_handler_void_vector_end(WKV1_GeometryMeta* meta, void* userData) {
  return R_NilValue;
}

char WKV1_handler_void_feature(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
  return 0;
}

char WKV1_handler_void_geometry(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData) {
  return 0;
}

char WKV1_handler_void_ring(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData) {
  return 0;
}

char WKV1_handler_void_coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userData) {
  return 0;
}

char WKV1_handler_void_error(R_xlen_t featureId, const char* message, void* userData) {
  return 1;
}

WKV1_Handler* WKV1_handler_create() {
  WKV1_Handler* handler = (WKV1_Handler*) malloc(sizeof(WKV1_Handler));
  handler->WKAPIVersion = 1;
  handler->userData = NULL;

  handler->vectorStart = &WKV1_handler_void_vector_start;
  handler->vectorEnd = &WKV1_handler_void_vector_end;

  handler->featureStart = &WKV1_handler_void_feature;
  handler->nullFeature = &WKV1_handler_void_feature;
  handler->featureEnd = &WKV1_handler_void_feature;

  handler->geometryStart = &WKV1_handler_void_geometry;
  handler->geometryEnd = &WKV1_handler_void_geometry;

  handler->ringStart = &WKV1_handler_void_ring;
  handler->ringEnd = &WKV1_handler_void_ring;

  handler->coord = &WKV1_handler_void_coord;

  handler->error = &WKV1_handler_void_error;

  return handler;
}

void WKV1_handler_destroy(WKV1_Handler* handler) {
  if (handler != NULL) {
    free(handler);
  }
}

void WKV1_handler_destroy_xptr(SEXP xptr) {
  WKV1_handler_destroy((WKV1_Handler*) R_ExternalPtrAddr(xptr));
}

SEXP WKV1_handler_create_xptr(WKV1_Handler* handler, SEXP tag, SEXP prot) {
  SEXP xptr = R_MakeExternalPtr(handler, tag, prot);
  R_RegisterCFinalizerEx(xptr, &WKV1_handler_destroy_xptr, TRUE);
  return xptr;
}
