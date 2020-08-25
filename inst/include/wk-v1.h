
#ifndef WK_V1_H_INCLUDED
#define WK_V1_H_INCLUDED

#include <Rinternals.h>

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
  char (*error)(R_xlen_t featureId, const char* message, void* userData);
} WKV1_Handler;

#ifdef __cplusplus
extern "C" {
#endif

WKV1_Handler* WKV1_handler_create();
void WKV1_handler_destroy(WKV1_Handler* handler);
void WKV1_handler_destroy_xptr(SEXP xptr);
SEXP WKV1_handler_create_xptr(WKV1_Handler* handler, SEXP tag, SEXP prot);

#ifdef __cplusplus

} // extern "C" {

class WKV1_HandlerClass {
public:
  char vectorStart(WKV1_GeometryMeta* meta) {
    return 0;
  }

  char featureStart(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return 0;
  }

  char nullFeature(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return 0;
  }

  char geometryStart(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId) {
    return 0;
  }

  char ringStart(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId) {
    return 0;
  }

  char coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId) {
    return 0;
  }

  char ringEnd(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId) {
    return 0;
  }

  char geometryEnd(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId) {
    return 0;
  }

  char featureEnd(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return 0;
  }

  SEXP vectorEnd(WKV1_GeometryMeta* meta) {
    return R_NilValue;
  }

  char error(R_xlen_t featureId, const char* message) {
    return 1;
  }
};


template <class HandlerType>
class WKV1_HandlerFactory {
public:

  static WKV1_Handler* c_handler(HandlerType* userData) {
    WKV1_Handler* handler = WKV1_handler_create();
    handler->userData = userData;

    handler->vectorStart = &vectorStart;
    handler->vectorEnd = &vectorEnd;

    handler->featureStart = &featureStart;
    handler->nullFeature = &nullFeature;
    handler->featureEnd = &featureEnd;

    handler->geometryStart = &geometryStart;
    handler->geometryEnd = &geometryEnd;

    handler->ringStart = &ringStart;
    handler->ringEnd = &ringEnd;

    handler->coord = &coord;

    handler->error = &error;

    return handler;
  }

  static SEXP sexp_handler(HandlerType* userData) {
    WKV1_Handler* handler = c_handler(userData);
    SEXP xptr = R_MakeExternalPtr(handler, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(xptr, &deleteSEXPHandler, TRUE);
    return xptr;
  }

private:
  static void deleteSEXPHandler(SEXP xptr) {
    WKV1_Handler* handler = (WKV1_Handler*) R_ExternalPtrAddr(xptr);
    deleteUserData(handler->userData);
    WKV1_handler_destroy(handler);
  }

  static void deleteUserData(void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    if (userData != NULL) {
      delete userData;
    }
  }

  static char vectorStart(WKV1_GeometryMeta* meta, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->vectorStart(meta);
    } catch (std::exception& e) {
      userData->error(-1, e.what());
      return 1;
    }
  }

  static char featureStart(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->featureStart(meta, nFeatures, featureId);
    } catch (std::exception& e) {
      userData->error(featureId, e.what());
      return 1;
    }
  }

  static char nullFeature(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->nullFeature(meta, nFeatures, featureId);
    } catch (std::exception& e) {
      userData->error(featureId, e.what());
      return 1;
    }
  }

  static char geometryStart(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->geometryStart(meta, nParts, partId);
    } catch (std::exception& e) {
      userData->error(e.what());
      return 1;
    }
  }

  static char ringStart(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->ringStart(meta, nRings, ringId);
    } catch (std::exception& e) {
      userData->error(e.what());
      return 1;
    }
  }

  static char coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->coord(meta, coord, nCoords, coordId);
    } catch (std::exception& e) {
      userData->error(e.what());
      return 1;
    }
  }

  static char ringEnd(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->ringEnd(meta, nRings, ringId);
    } catch (std::exception& e) {
      userData->error(e.what());
      return 1;
    }
  }

  static char geometryEnd(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->geometryEnd(meta, nParts, partId);
    } catch (std::exception& e) {
      userData->error(e.what());
      return 1;
    }
  }

  static char featureEnd(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->featureEnd(meta, nFeatures, featureId);
    } catch (std::exception& e) {
      userData->error(e.what());
      return 1;
    }
  }

  static SEXP vectorEnd(WKV1_GeometryMeta* meta, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->vectorEnd(meta);
    } catch (std::exception& e) {
      if (userData->error(-1, e.what()) != 0) {
        Rf_error(e.what());
      } else {
        return R_NilValue;
      }
    }
  }

  static char error(R_xlen_t featureId, const char* message, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->error(featureId, message);
    } catch (std::exception& e) {
      return 1;
    }
  }
};

#endif

#endif
