
#ifndef WK_V1_H_INCLUDED
#define WK_V1_H_INCLUDED

#include <stdint.h> // for uint_32_t
#include <Rinternals.h>

#define WK_CONTINUE 0
#define WK_ABORT 1
#define WK_ABORT_FEATURE 2

#define WK_DEFAULT_ERROR_CODE 0
#define WK_NO_ERROR_CODE -1

#define WK_FLAG_HAS_BOUNDS 1
#define WK_FLAG_HAS_Z 2
#define WK_FLAG_HAS_M 4
#define WK_FLAG_DIMS_UNKNOWN 8

#define WK_PART_ID_NONE UINT32_MAX
#define WK_SIZE_UNKNOWN UINT32_MAX
#define WK_SRID_NONE UINT32_MAX

enum WKGeometryType {
  WK_GEOMETRY = 0,
  WK_POINT = 1,
  WK_LINESTRING = 2,
  WK_POLYGON = 3,
  WK_MULTIPOINT = 4,
  WK_MULTILINESTRING = 5,
  WK_MULTIPOLYGON = 6,
  WK_GEOMETRYCOLLECTION = 7
};

typedef struct {
  double v[4];
} wk_coord_t;

typedef struct {
  void* meta_data;
  uint32_t geometryType;
  uint32_t flags;
  uint32_t size;
  uint32_t srid;
  wk_coord_t boundsMin;
  wk_coord_t boundsMax;
} wk_meta_t;

#define WK_META_RESET(meta, geometryType_)                     \
  meta.meta_data = NULL;                                        \
  meta.geometryType = geometryType_;                           \
  meta.flags = 0;                                              \
  meta.srid = WK_SRID_NONE;                                    \
  meta.size = WK_SIZE_UNKNOWN

typedef struct {
  int WKAPIVersion;
  char dirty;
  void* handler_data;
  char (*vectorStart)(const wk_meta_t* meta, void* handler_data);
  char (*featureStart)(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id, void* handler_data);
  char (*nullFeature)(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id, void* handler_data);
  char (*geometryStart)(const wk_meta_t* meta, uint32_t nParts, uint32_t partId, void* handler_data);
  char (*ringStart)(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, void* handler_data);
  char (*coord)(const wk_meta_t* meta, const wk_coord_t coord, uint32_t nCoords, uint32_t coordId,
                void* handler_data);
  char (*ringEnd)(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, void* handler_data);
  char (*geometryEnd)(const wk_meta_t* meta, uint32_t nParts, uint32_t partId, void* handler_data);
  char (*featureEnd)(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id, void* handler_data);
  SEXP (*vectorEnd)(const wk_meta_t* meta, void* handler_data);
  char (*error)(R_xlen_t feat_id, int code, const char* message, void* handler_data);
  void (*vectorFinally)(void* handler_data);
  void (*finalizer)(void* handler_data);
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
