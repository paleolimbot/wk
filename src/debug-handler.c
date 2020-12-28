
#include "wk-v1.h"
#include <Rinternals.h>

void wk_handler_debug_print_meta(const WKGeometryMeta_t* meta) {
  switch (meta->geometryType) {
  case WK_POINT:
    Rprintf("POINT");
    break;
  case WK_LINESTRING:
    Rprintf("LINESTRING");
    break;
  case WK_POLYGON:
    Rprintf("POLYGON");
    break;
  case WK_MULTIPOINT:
    Rprintf("MULTIPOINT");
    break;
  case WK_MULTILINESTRING:
    Rprintf("MULTILINESTRING");
    break;
  case WK_MULTIPOLYGON:
    Rprintf("MULTIPOLYGON");
    break;
  case WK_GEOMETRYCOLLECTION:
    Rprintf("GEOMETRYCOLLECTION");
    break;
  default:
    Rprintf("<Unknown type / %d>", meta->geometryType);
    break;
  }

  if (meta->hasZ || meta->hasM || (meta->srid != WK_SRID_NONE) || meta->hasBounds) {
    Rprintf(" ");
  }
  if (meta->hasZ) Rprintf("Z");
  if (meta->hasM) Rprintf("M");
  if (meta->srid != WK_SRID_NONE) Rprintf("S");
  if (meta->hasBounds) Rprintf("B");

  if (meta->size != WK_SIZE_UNKNOWN) {
    if (meta->size == 0) {
      Rprintf("[EMPTY]");
    } else {
      Rprintf("[%d]", meta->size);
    }
  }

  Rprintf(" <%p>", (void*) meta);
}

void wk_handler_debug_print_indent(void* userData) {
  int* intData = (int*) userData;
  for (int i = 0; i < intData[0]; i++) {
    Rprintf("  ");
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

char wk_handler_debug_vector_start(const WKGeometryMeta_t* meta, void* userData) {
  wk_handler_debug_reset(userData);
  wk_handler_debug_print_indent(userData);
  Rprintf("vectorStart: ");
  wk_handler_debug_print_meta(meta);
  Rprintf("\n");
  wk_handler_debug_indent(userData);
  return WK_CONTINUE;
}

SEXP wk_handler_debug_vector_end(const WKGeometryMeta_t* meta, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  Rprintf("vectorEnd <%p>\n", meta);
  return R_NilValue;
}

char wk_handler_debug_feature_start(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId,
                                    void* userData) {
  wk_handler_debug_print_indent(userData);
  Rprintf("featureStart (%d / %d) <%p>\n", featureId + 1, nFeatures, meta);
  wk_handler_debug_indent(userData);
  return WK_CONTINUE;
}

char wk_handler_debug_feature_null(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId,
                                   void* userData) {
  wk_handler_debug_print_indent(userData);
  Rprintf("nullFeature (%d / %d) <%p>\n", featureId + 1, nFeatures, meta);
  return WK_CONTINUE;
}

char wk_handler_debug_feature_end(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId,
                                  void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  Rprintf("featureEnd (%d / %d) <%p>\n", featureId + 1, nFeatures, meta);
  return WK_CONTINUE;
}

char wk_handler_debug_geometry_start(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData) {
  wk_handler_debug_print_indent(userData);

  if (nParts == WK_PART_ID_NONE) {
    Rprintf("geometryStart: ");
  } else {
    Rprintf("geometryStart (%d / %d): ", partId + 1, nParts);
  }

  wk_handler_debug_print_meta(meta);
  Rprintf("\n");
  wk_handler_debug_indent(userData);
  return WK_CONTINUE;
}

char wk_handler_debug_geometry_end(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);

  if (nParts == WK_PART_ID_NONE) {
    Rprintf("geometryEnd <%p> \n", meta);
  } else {
    Rprintf("geometryStart (%d / %d) <%p> \n", partId + 1, nParts, meta);
  }

  return WK_CONTINUE;
}

char wk_handler_debug_ring_start(const WKGeometryMeta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, void* userData) {
  wk_handler_debug_print_indent(userData);
  if (size != WK_SIZE_UNKNOWN) {
    Rprintf("ringStart[%d] (%d / %d) <%p>\n", size, ringId + 1, nRings, meta);
  } else {
    Rprintf("ringStart (%d / %d) <%p>\n", ringId + 1, nRings, meta);
  }
  wk_handler_debug_indent(userData);
  return WK_CONTINUE;
}

char wk_handler_debug_ring_end(const WKGeometryMeta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, void* userData) {
  wk_handler_debug_dedent(userData);
  wk_handler_debug_print_indent(userData);
  if (size != WK_SIZE_UNKNOWN) {
    Rprintf("ringEnd[%d] (%d / %d) <%p>\n", size, ringId + 1, nRings, meta);
  } else {
    Rprintf("ringEnd (%d / %d) <%p>\n", ringId + 1, nRings, meta);
  }
  return WK_CONTINUE;
}

char wk_handler_debug_coord(const WKGeometryMeta_t* meta, WKCoord_t coord, uint32_t nCoords, uint32_t coordId,
                            void* userData) {
  wk_handler_debug_print_indent(userData);
  Rprintf("coord (%d / %d) <%p> (%f %f", coordId + 1, nCoords, meta, coord.v[0], coord.v[1]);
  if (meta->hasZ || meta->hasM) Rprintf(" %f", coord.v[2]);
  if (meta->hasZ && meta->hasM) Rprintf(" %f", coord.v[3]);
  Rprintf(")\n");
  return WK_CONTINUE;
}

char wk_handler_debug_error(R_xlen_t featureId, int code, const char* message, void* userData) {
  wk_handler_debug_print_indent(userData);
  Rprintf("error [i=%d](%d): %s\n", featureId, code, message);
  return WK_ABORT;
}

void wk_handler_debug_vector_finally(void* userData) {
  Rprintf("vectorFinally()\n");
}

SEXP wk_c_handler_debug_new() {
  WKHandler_t* handler = wk_handler_create();

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

  handler->vectorFinally = &wk_handler_debug_vector_finally;

  SEXP recursiveDepth = PROTECT(Rf_allocVector(INTSXP, 1));
  int* pRecursiveDepth = INTEGER(recursiveDepth);
  pRecursiveDepth[0] = 0;
  handler->userData = pRecursiveDepth;

  SEXP xptr = wk_handler_create_xptr(handler, recursiveDepth, R_NilValue);
  UNPROTECT(1);
  return xptr;
}
