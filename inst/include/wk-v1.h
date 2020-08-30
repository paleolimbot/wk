
#ifndef WK_V1_H_INCLUDED
#define WK_V1_H_INCLUDED

#include <Rinternals.h>

#define WKV1_CONTINUE 0
#define WKV1_STOP 1
#define WKV1_DEFAULT_ERROR_CODE 0
#define WKV1_NO_ERROR_CODE -1

enum WKV1_GeometryType {
  WKV1_InvalidGeometryType = 0,
  WKV1_Point = 1,
  WKV1_LineString = 2,
  WKV1_Polygon = 3,
  WKV1_MultiPoint = 4,
  WKV1_MultiLineString = 5,
  WKV1_MultiPolygon = 6,
  WKV1_GeometryCollection = 7
};

typedef struct { double x, y; } WKV1_CoordXY;
typedef struct { double x, y, z; } WKV1_CoordXYZ;
typedef struct { double x, y, m; } WKV1_CoordXYM;
typedef struct { double x, y, z, m; } WKV1_CoordXYZM;

typedef union {
  double v[4];
  WKV1_CoordXY xy;
  WKV1_CoordXYZ xyz;
  WKV1_CoordXYM xym;
  WKV1_CoordXYZM xyzm;
} WKV1_Coord;

typedef struct {
  int geometryType;
  int recursiveLevel;
  char hasZ;
  char hasM;
  char hasSrid;
  char hasSize;
  char hasBounds;
  unsigned int size;
  unsigned int srid;
  WKV1_Coord boundsMin;
  WKV1_Coord boundsMax;
  void* userData;
} WKV1_GeometryMeta;

#define WKV1_META_RESET(meta, geometryType_)                   \
  meta.geometryType = geometryType_;                           \
  meta.recursiveLevel = 0;                                     \
  meta.hasZ = 0;                                               \
  meta.hasM = 0;                                               \
  meta.hasSrid = 0;                                            \
  meta.hasSize = 0;                                            \
  meta.hasBounds = 0;

#define WKV1_META_SET_SIZE(meta, size_)                        \
  meta.hasSize = 1;                                            \
  meta.size = size_;

#define WKV1_META_SET_SRID(meta, srid_)                        \
  meta.hasSrid = 1;                                            \
  meta.srid = srid_;

typedef struct {
  int WKAPIVersion;
  void* userData;
  char (*vectorStart)(WKV1_GeometryMeta* meta, void* userData);
  char (*featureStart)(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  char (*nullFeature)(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  char (*geometryStart)(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData);
  char (*ringStart)(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData);
  char (*coord)(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userData);
  char (*ringEnd)(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData);
  char (*geometryEnd)(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData);
  char (*featureEnd)(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData);
  SEXP (*vectorEnd)(WKV1_GeometryMeta* meta, void* userData);
  char (*error)(R_xlen_t featureId, int code, const char* message, void* userData);
  void (*finalizer)(void* userData);
} WKV1_Handler;

#ifdef __cplusplus
extern "C" {
#endif

WKV1_Handler* WKV1_handler_create();
void WKV1_handler_destroy(WKV1_Handler* handler);
void WKV1_handler_destroy_xptr(SEXP xptr);
SEXP WKV1_handler_create_xptr(WKV1_Handler* handler, SEXP tag, SEXP prot);
SEXP WKV1_error_sentinel(int code, const char* message);

#ifdef __cplusplus
} // extern "C" {
#endif

#endif
