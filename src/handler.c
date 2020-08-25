
#include "wk-v1.h"
#include <Rinternals.h>

SEXP wk_c_handler_addr(SEXP xptr) {
  SEXP buffer = PROTECT(Rf_allocVector(RAWSXP, 32));
  sprintf((char*) RAW(buffer), "%p", (void*) R_ExternalPtrAddr(xptr));

  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar((char*) RAW(buffer)));
  UNPROTECT(2);
  return out;
}

SEXP wk_c_handler_void_new() {
  return WKV1_handler_create_xptr(WKV1_handler_create(), R_NilValue, R_NilValue);
}

void wk_handler_debug_print_meta(WKV1_GeometryMeta* meta) {
  switch (meta->geometryType) {
  case WKV1_Point:
    REprintf("POINT");
  case WKV1_LineString:
    REprintf("POINT");
  case WKV1_Polygon:
    REprintf("POINT");
  case WKV1_MultiPoint:
    REprintf("POINT");
  case WKV1_MultiLineString:
    REprintf("POINT");
  case WKV1_MultiPolygon:
    REprintf("POINT");
  case WKV1_GeometryCollection:
    REprintf("POINT");
  default:
    REprintf("<Unknown type>");
  }

  if (meta->hasZ || meta->hasM || meta->hasSrid || meta->hasBounds) {
    REprintf(" ");
  }
  if (meta->hasZ) REprintf("Z");
  if (meta->hasM) REprintf("M");
  if (meta->hasSrid) REprintf("S");
  if (meta->hasBounds) REprintf("B");

  if (meta->hasSize) {
    if (meta->size == 0) {
      REprintf("[EMPTY]");
    } else {
      REprintf("[%d]");
    }
  }

  REprintf(" <%p>", (void*) meta);
}

void wk_handler_debug_print_indent(void* userData) {
  int* intData = (int*) userData;
  for (int i = 0; i < intData[0]; i++) {
    REprintf("  ");
  }
}

void wk_handler_debug_reset(void* userData) {
  int* intData = (int*) userData;
  intData[0] = 0;
}

void wk_handler_debug_indent(void* userData) {
  int* intData = (int*) userData;
  intData[0]++;
}

void wk_handler_debug_dedent(void* userData) {
  int* intData = (int*) userData;
  intData[0]--;
}

char wk_handler_debug_vector_start(WKV1_GeometryMeta* meta, void* userData) {
  wk_handler_debug_reset(userData);
  wk_handler_debug_print_indent(userData);
  REprintf("vectorStart: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  wk_handler_debug_indent(userData);
  return 0;
}

SEXP wk_handler_debug_vector_end(WKV1_GeometryMeta* meta, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("vectorEnd: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  wk_handler_debug_dedent(userData);
  return R_NilValue;
}

char wk_handler_debug_feature_start(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("featureStart: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  wk_handler_debug_indent(userData);
  return 0;
}

char wk_handler_debug_feature_null(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("nullFeature: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  return 0;
}

char wk_handler_debug_feature_end(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  REprintf("featureEnd: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  return 0;
}

char wk_handler_debug_geometry_start(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("geometryStart: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  wk_handler_debug_indent(userData);
  return 0;
}

char wk_handler_debug_geometry_end(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  REprintf("geometryEnd: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  return 0;
}

char wk_handler_debug_ring_start(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("ringStart: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  wk_handler_debug_indent(userData);
  return 0;
}

char wk_handler_debug_ring_end(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  REprintf("ringEnd: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  return 0;
}

char wk_handler_debug_coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("coord: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  return 0;
}

char wk_handler_debug_error(R_xlen_t featureId, const char* message, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("error [i=%d]: %s\n", featureId, message);
  return 1;
}

SEXP wk_c_handler_debug_new() {
  WKV1_Handler* handler = WKV1_handler_create();

  handler->vectorStart = &wk_handler_debug_vector_start;
  handler->vectorEnd = &wk_handler_debug_vector_end;

  handler->featureStart = &wk_handler_debug_feature_start;
  handler->nullFeature = &wk_handler_debug_feature_null;
  handler->featureEnd = &wk_handler_debug_feature_end;

  handler->geometryStart = &wk_handler_debug_geometry_start;
  handler->geometryEnd = &wk_handler_debug_geometry_end;

  handler->ringStart = &wk_handler_debug_ring_start;
  handler->ringEnd = &wk_handler_debug_ring_end;

  handler->coord = &wk_handler_debug_coord;

  SEXP recursiveDepth = PROTECT(Rf_allocVector(INTSXP, 1));
  int* pRecursiveDepth = INTEGER(recursiveDepth);
  pRecursiveDepth[0] = 0;
  handler->userData = pRecursiveDepth;

  SEXP xptr = WKV1_handler_create_xptr(handler, recursiveDepth, R_NilValue);
  UNPROTECT(1);
  return xptr;
}
