
#ifndef WK_V1_H_INCLUDED
#define WK_V1_H_INCLUDED

#ifndef __cplusplus
# include <stddef.h> /* for size_t definition */
#else
# include <cstddef>
using std::size_t;
#endif

#define WKV1_BUFFER_SIZE 1024

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

typedef struct {
  char (*vectorStart)(WKV1_GeometryMeta* meta, void* userData);
  char (*featureStart)(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData);
  char (*nullFeature)(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData);
  char (*geometryStart)(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData);
  char (*ringStart)(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData);
  char (*coord)(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userData);
  char (*ringEnd)(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData);
  char (*geometryEnd)(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData);
  char (*featureEnd)(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData);
  char (*vectorEnd)(WKV1_GeometryMeta* meta, void* userData);
  void (*finalize)(void* userData);
  char lastErrorMessage[WKV1_BUFFER_SIZE];
} WKV1_Handler;

inline char WKV1_handler_void_vector(WKV1_GeometryMeta* meta, void* userData) {
  return 0;
}

inline char WKV1_handler_void_null(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData) {
  return 0;
}

inline char WKV1_handler_void_feature(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData) {
  return 0;
}

inline char WKV1_handler_void_geometry(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData) {
  return 0;
}

inline char WKV1_handler_void_ring(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData) {
  return 0;
}

inline char WKV1_handler_void_coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userData) {
  return 0;
}

inline void WKV1_handler_void_finalize(void* userData) {}

inline WKV1_Handler WKV1_handler_create_void() {
  WKV1_Handler handler;
  handler.vectorStart = &WKV1_handler_void_vector;
  handler.vectorEnd = &WKV1_handler_void_vector;

  handler.featureStart = &WKV1_handler_void_feature;
  handler.nullFeature = &WKV1_handler_void_null;
  handler.featureEnd = &WKV1_handler_void_feature;

  handler.geometryStart = &WKV1_handler_void_geometry;
  handler.geometryEnd = &WKV1_handler_void_geometry;

  handler.ringStart = &WKV1_handler_void_ring;
  handler.ringEnd = &WKV1_handler_void_ring;

  handler.coord = &WKV1_handler_void_coord;

  handler.finalize = &WKV1_handler_void_finalize;

  return handler;
}

#endif
