
#include "wk-v1.h"

void wk_handler_destroy_xptr(SEXP xptr) {
  wk_handler_destroy((WKHandler_t*) R_ExternalPtrAddr(xptr));
}

char wk_handler_void_vector_start(const WKGeometryMeta_t* meta, void* userData) {
  return WK_CONTINUE;
}

SEXP wk_handler_void_vector_end(const WKGeometryMeta_t* meta, void* userData) {
  return R_NilValue;
}

char wk_handler_void_feature(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
  return WK_CONTINUE;
}

char wk_handler_void_geometry(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData) {
  return WK_CONTINUE;
}

char wk_handler_void_ring(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId, void* userData) {
  return WK_CONTINUE;
}

char wk_handler_void_coord(const WKGeometryMeta_t* meta, const WKCoord_t coord, uint32_t nCoords, uint32_t coordId, void* userData) {
  return WK_CONTINUE;
}

char wk_handler_void_error(R_xlen_t featureId, int code, const char* message, void* userData) {
  return WK_STOP;
}

void wk_handler_void_finalizer(void* userData) {

}
