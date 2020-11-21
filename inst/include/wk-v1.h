
#ifndef WK_V1_H_INCLUDED
#define WK_V1_H_INCLUDED

#include <stdint.h> // for uint_32_t
#include <stdlib.h> // for malloc()
#include <Rinternals.h>

#define WK_CONTINUE 0
#define WK_STOP 1
#define WK_DEFAULT_ERROR_CODE 0
#define WK_NO_ERROR_CODE -1
#define WK_PART_ID_NONE UINT32_MAX

enum WKV1_GeometryType {
  WK_GEOMETRY = 0,
  WK_POINT = 1,
  WK_LINESTRING = 2,
  WK_POLYGON = 3,
  WK_MULTIPOINT = 4,
  WK_MULTILINESTRING = 5,
  WK_MULTIPOLYGON = 6,
  WK_GEOMETRYCOLLECTION = 7
};

typedef union {
  double v[4];
} WKCoord_t;

typedef struct {
  int geometryType;
  int recursiveLevel;
  char hasZ;
  char hasM;
  char hasSrid;
  char hasSize;
  char hasBounds;
  uint32_t size;
  uint32_t srid;
  WKCoord_t boundsMin;
  WKCoord_t boundsMax;
  void* userData;
} WKGeometryMeta_t;

#define WK_META_RESET(meta, geometryType_)                   \
  meta.geometryType = geometryType_;                           \
  meta.recursiveLevel = 0;                                     \
  meta.hasZ = 0;                                               \
  meta.hasM = 0;                                               \
  meta.hasSrid = 0;                                            \
  meta.hasSize = 0;                                            \
  meta.hasBounds = 0;

#define WK_META_SET_SIZE(meta, size_)                        \
  meta.hasSize = 1;                                            \
  meta.size = size_;

#define WK_META_SET_SRID(meta, srid_)                        \
  meta.hasSrid = 1;                                            \
  meta.srid = srid_;

typedef struct {
  int WKAPIVersion;
  void* userData;
  char (*vectorStart)(const WKGeometryMeta_t* meta, void* userData);
  char (*featureStart)(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  char (*nullFeature)(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  char (*geometryStart)(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData);
  char (*ringStart)(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId, void* userData);
  char (*coord)(const WKGeometryMeta_t* meta, const WKCoord_t coord, uint32_t nCoords, uint32_t coordId, void* userData);
  char (*ringEnd)(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId, void* userData);
  char (*geometryEnd)(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData);
  char (*featureEnd)(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  SEXP (*vectorEnd)(const WKGeometryMeta_t* meta, void* userData);
  char (*error)(R_xlen_t featureId, int code, const char* message, void* userData);
  void (*finalizer)(void* userData);
} WKHandler_t;

#ifdef __cplusplus
extern "C" {
#endif

// implementation for void handler functions in wk-v1-impl.c
char wk_handler_void_vector_start(const WKGeometryMeta_t* meta, void* userData);
SEXP wk_handler_void_vector_end(const WKGeometryMeta_t* meta, void* userData);
char wk_handler_void_feature(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
char wk_handler_void_geometry(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData);
char wk_handler_void_ring(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId, void* userData);
char wk_handler_void_coord(const WKGeometryMeta_t* meta, const WKCoord_t coord, uint32_t nCoords, uint32_t coordId, void* userData);
char wk_handler_void_error(R_xlen_t featureId, int code, const char* message, void* userData);
void wk_handler_void_finalizer(void* userData);

inline WKHandler_t* wk_handler_create() {
  WKHandler_t* handler = (WKHandler_t*) malloc(sizeof(WKHandler_t));
  handler->WKAPIVersion = 1;
  handler->userData = NULL;

  handler->vectorStart = &wk_handler_void_vector_start;
  handler->vectorEnd = &wk_handler_void_vector_end;

  handler->featureStart = &wk_handler_void_feature;
  handler->nullFeature = &wk_handler_void_feature;
  handler->featureEnd = &wk_handler_void_feature;

  handler->geometryStart = &wk_handler_void_geometry;
  handler->geometryEnd = &wk_handler_void_geometry;

  handler->ringStart = &wk_handler_void_ring;
  handler->ringEnd = &wk_handler_void_ring;

  handler->coord = &wk_handler_void_coord;

  handler->error = &wk_handler_void_error;
  handler->finalizer = &wk_handler_void_finalizer;

  return handler;
}

// implementation in wk-v1-impl.c
void wk_handler_destroy_xptr(SEXP xptr);

inline SEXP wk_handler_create_xptr(WKHandler_t* handler, SEXP tag, SEXP prot) {
  SEXP xptr = R_MakeExternalPtr(handler, tag, prot);
  R_RegisterCFinalizerEx(xptr, &wk_handler_destroy_xptr, TRUE);
  return xptr;
}

inline void wk_handler_destroy(WKHandler_t* handler) {
  if (handler != NULL) {
    free(handler);
  }
}

inline SEXP wk_error_sentinel(int code, const char* message) {
  const char* names[] = {"code", "message", ""};
  SEXP sentinel = PROTECT(Rf_mkNamed(VECSXP, names));
  Rf_setAttrib(sentinel, Rf_install("class"), Rf_mkString("wk_error_sentinel"));
  SEXP codeSEXP = PROTECT(Rf_allocVector(INTSXP, 1));
  INTEGER(codeSEXP)[0] = code;
  SET_VECTOR_ELT(sentinel, 0, codeSEXP);

  SET_VECTOR_ELT(sentinel, 1, Rf_mkString(message));
  UNPROTECT(2);
  return sentinel;
}

#ifdef __cplusplus
} // extern "C" {
#endif

#endif
