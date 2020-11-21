
#include "wk-v1.h"
#include <Rinternals.h>

void wk_handler_debug_print_meta(const WKGeometryMeta* meta) {
  switch (meta->geometryType) {
  case WK_POINT:
    REprintf("POINT");
    break;
  case WK_LINESTRING:
    REprintf("LINESTRING");
    break;
  case WK_POLYGON:
    REprintf("POLYGON");
    break;
  case WK_MULTIPOINT:
    REprintf("MULTIPOINT");
    break;
  case WK_MULTILINESTRING:
    REprintf("MULTILINESTRING");
    break;
  case WK_MULTIPOLYGON:
    REprintf("MULTIPOLYGON");
    break;
  case WK_GEOMETRYCOLLECTION:
    REprintf("GEOMETRYCOLLECTION");
    break;
  default:
    REprintf("<Unknown type / %d>", meta->geometryType);
    break;
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
      REprintf("[%d]", meta->size);
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

char wk_handler_debug_vector_start(const WKGeometryMeta* meta, void* userData) {
  wk_handler_debug_reset(userData);
  wk_handler_debug_print_indent(userData);
  REprintf("vectorStart: ");
  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  wk_handler_debug_indent(userData);
  return WK_CONTINUE;
}

SEXP wk_handler_debug_vector_end(const WKGeometryMeta* meta, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  REprintf("vectorEnd <%p>\n", meta);
  return R_NilValue;
}

char wk_handler_debug_feature_start(const WKGeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("featureStart (%d / %d) <%p>\n", featureId + 1, nFeatures, meta);
  wk_handler_debug_indent(userData);
  return WK_CONTINUE;
}

char wk_handler_debug_feature_null(const WKGeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("nullFeature (%d / %d) <%p>\n", featureId + 1, nFeatures, meta);
  return WK_CONTINUE;
}

char wk_handler_debug_feature_end(const WKGeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  REprintf("featureEnd (%d / %d) <%p>\n", featureId + 1, nFeatures, meta);
  return WK_CONTINUE;
}

char wk_handler_debug_geometry_start(const WKGeometryMeta* meta, uint32_t nParts, uint32_t partId, void* userData) {
  wk_handler_debug_print_indent(userData);

  if (nParts == WK_PART_ID_NONE) {
    REprintf("geometryStart: ");
  } else {
    REprintf("geometryStart (%d / %d): ", partId + 1, nParts);
  }

  wk_handler_debug_print_meta(meta);
  REprintf("\n");
  wk_handler_debug_indent(userData);
  return WK_CONTINUE;
}

char wk_handler_debug_geometry_end(const WKGeometryMeta* meta, uint32_t nParts, uint32_t partId, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);

  if (nParts == WK_PART_ID_NONE) {
    REprintf("geometryEnd <%p> \n", meta);
  } else {
    REprintf("geometryStart (%d / %d) <%p> \n", partId + 1, nParts, meta);
  }

  return WK_CONTINUE;
}

char wk_handler_debug_ring_start(const WKGeometryMeta* meta, uint32_t nRings, uint32_t ringId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("ringStart (%d / %d) <%p>\n", ringId + 1, nRings, meta);
  wk_handler_debug_indent(userData);
  return WK_CONTINUE;
}

char wk_handler_debug_ring_end(const WKGeometryMeta* meta, uint32_t nRings, uint32_t ringId, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  REprintf("ringEnd (%d / %d) <%p>\n", ringId + 1, nRings, meta);
  return WK_CONTINUE;
}

char wk_handler_debug_coord(const WKGeometryMeta* meta, WKCoord coord, uint32_t nCoords, uint32_t coordId, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("coord (%d / %d) <%p> (%f %f", coordId + 1, nCoords, meta, coord.v[0], coord.v[1]);
  if (meta->hasZ || meta->hasM) REprintf(" %f", coord.v[2]);
  if (meta->hasZ && meta->hasM) REprintf(" %f", coord.v[3]);
  REprintf(")\n");
  return WK_CONTINUE;
}

char wk_handler_debug_error(R_xlen_t featureId, int code, const char* message, void* userData) {
  wk_handler_debug_print_indent(userData);
  REprintf("error [i=%d](%d): %s\n", featureId, code, message);
  return WK_STOP;
}

SEXP wk_c_handler_debug_new() {
  WKHandler* handler = wk_handler_create();

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

  handler->error = &wk_handler_debug_error;

  SEXP recursiveDepth = PROTECT(Rf_allocVector(INTSXP, 1));
  int* pRecursiveDepth = INTEGER(recursiveDepth);
  pRecursiveDepth[0] = 0;
  handler->userData = pRecursiveDepth;

  SEXP xptr = wk_handler_create_xptr(handler, recursiveDepth, R_NilValue);
  UNPROTECT(1);
  return xptr;
}
