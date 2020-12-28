
#ifndef WK_V1_H_INCLUDED
#define WK_V1_H_INCLUDED

#include <stdint.h> // for uint_32_t
#include <Rinternals.h>

#define WK_CONTINUE 0
#define WK_ABORT 1
#define WK_ABORT_FEATURE 2
#define WK_DEFAULT_ERROR_CODE 0
#define WK_NO_ERROR_CODE -1
#define WK_PART_ID_NONE UINT32_MAX
#define WK_SIZE_UNKNOWN UINT32_MAX

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
  char dirty;
  void* userData;
  char (*vectorStart)(const WKGeometryMeta_t* meta, void* userData);
  char (*featureStart)(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  char (*nullFeature)(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  char (*geometryStart)(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData);
  char (*ringStart)(const WKGeometryMeta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, void* userData);
  char (*coord)(const WKGeometryMeta_t* meta, const WKCoord_t coord, uint32_t nCoords, uint32_t coordId,
                void* userData);
  char (*ringEnd)(const WKGeometryMeta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, void* userData);
  char (*geometryEnd)(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData);
  char (*featureEnd)(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  SEXP (*vectorEnd)(const WKGeometryMeta_t* meta, void* userData);
  char (*error)(R_xlen_t featureId, int code, const char* message, void* userData);
  void (*vectorFinally)(void* userData);
  void (*finalizer)(void* userData);
} WKHandler_t;

#ifdef __cplusplus
extern "C" {
#endif

// implementations in wk-v1-impl.c, which must be included exactly once in an R package
WKHandler_t* wk_handler_create();
SEXP wk_handler_create_xptr(WKHandler_t* handler, SEXP tag, SEXP prot);
void wk_handler_destroy(WKHandler_t* handler);
SEXP wk_handler_run_xptr(SEXP (*readFunction)(SEXP readData, WKHandler_t* handler), SEXP readData, SEXP xptr);
SEXP wk_error_sentinel(int code, const char* message);

#ifdef __cplusplus
} // extern "C" {
#endif

#endif
