
#ifndef WK_V1_H_INCLUDED
#define WK_V1_H_INCLUDED

#include <Rinternals.h>

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
  int geometryType = WKV1_InvalidGeometryType;
  int recursiveLevel = 0;
  char hasZ = 0;
  char hasM = 0;
  char hasSrid = 0;
  char hasSize  = 0;
  char hasBounds = 0;
  unsigned int size;
  unsigned int srid;
  WKV1_Coord boundsMin;
  WKV1_Coord boundsMax;
  void* userData = NULL;
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
  int WKAPIVersion = 1;
  char (*vectorStart)(WKV1_GeometryMeta* meta, void* userData);
  char (*featureStart)(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData);
  char (*nullFeature)(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData);
  char (*geometryStart)(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData);
  char (*ringStart)(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData);
  char (*coord)(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userData);
  char (*ringEnd)(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData);
  char (*geometryEnd)(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData);
  char (*featureEnd)(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData);
  SEXP (*vectorEnd)(WKV1_GeometryMeta* meta, void* userData);
  void* userData = NULL;
} WKV1_Handler;

inline char WKV1_handler_void_vector_start(WKV1_GeometryMeta* meta, void* userData) {
  return 0;
}

inline SEXP WKV1_handler_void_vector_end(WKV1_GeometryMeta* meta, void* userData) {
  return R_NilValue;
}

inline char WKV1_handler_void_feature(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, void* userData) {
  return 0;
}

inline char WKV1_handler_void_geometry(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userData) {
  return 0;
}

inline char WKV1_handler_void_ring(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userData) {
  return 0;
}

inline char WKV1_handler_void_coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userData) {
  return 0;
}

inline WKV1_Handler* WKV1_handler_create_void() {
  WKV1_Handler* handler = (WKV1_Handler*) malloc(sizeof(WKV1_Handler));

  handler->vectorStart = &WKV1_handler_void_vector_start;
  handler->vectorEnd = &WKV1_handler_void_vector_end;

  handler->featureStart = &WKV1_handler_void_feature;
  handler->nullFeature = &WKV1_handler_void_feature;
  handler->featureEnd = &WKV1_handler_void_feature;

  handler->geometryStart = &WKV1_handler_void_geometry;
  handler->geometryEnd = &WKV1_handler_void_geometry;

  handler->ringStart = &WKV1_handler_void_ring;
  handler->ringEnd = &WKV1_handler_void_ring;

  handler->coord = &WKV1_handler_void_coord;

  return handler;
}

inline void WKV1_handler_destroy(WKV1_Handler* handler) {
  free(handler);
}

#ifdef __cplusplus

#include <string.h>

class WKV1_HandlerClass {
public:
  char vectorStart(WKV1_GeometryMeta* meta) {
    return 0;
  }

  char featureStart(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId) {
    return 0;
  }

  char nullFeature(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId) {
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

  char featureEnd(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId) {
    return 0;
  }

  SEXP vectorEnd(WKV1_GeometryMeta* meta) {
    return R_NilValue;
  }

  std::string lastErrorMessage() {
    return lastErrorMessage_;
  }

  void setLastErrorMesssage(std::string message) {
    this->lastErrorMessage_ = message;
  }

private:
  std::string lastErrorMessage_;
};


template <class HandlerType>
class WKV1_HandlerFactory {
public:

  static WKV1_Handler* c_handler(HandlerType* userData) {
    WKV1_Handler* handler = WKV1_handler_create_void();
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

    return handler;
  }

  static SEXP sexp_handler(HandlerType* userData) {
    WKV1_Handler* handler = c_handler(userData);
    SEXP xptr = R_MakeExternalPtr(handler, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(xptr, &deleteUserDataXPtr, TRUE);
    return xptr;
  }

private:
  static void deleteUserDataXPtr(SEXP xptr) {
    deleteUserData(R_ExternalPtrAddr(xptr));
  }

  static void deleteUserData(HandlerType* userData) {
    if (userData != NULL) {
      delete userData;
    }
  }

  static char vectorStart(WKV1_GeometryMeta* meta, HandlerType* userData) {
    try {
      return userData->vectorStart(meta);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  static char featureStart(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, HandlerType* userData) {
    try {
      return userData->featureStart(meta, nFeatures, featureId);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  static char nullFeature(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, HandlerType* userData) {
    try {
      return userData->nullFeature(meta, nFeatures, featureId);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  static char geometryStart(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, HandlerType* userData) {
    try {
      return userData->geometryStart(meta, nParts, partId);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  static char ringStart(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, HandlerType* userData) {
    try {
      return userData->ringStart(meta, nRings, ringId);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  static char coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, HandlerType* userData) {
    try {
      return userData->coord(meta, coord, nCoords, coordId);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  static char ringEnd(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, HandlerType* userData) {
    try {
      return userData->ringEnd(meta, nRings, ringId);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  static char geometryEnd(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, HandlerType* userData) {
    try {
      return userData->geometryEnd(meta, nParts, partId);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  static char featureEnd(WKV1_GeometryMeta* meta, size_t nFeatures, size_t featureId, HandlerType* userData) {
    try {
      return userData->featureEnd(meta, nFeatures, featureId);
    } catch (std::exception& e) {
      userData->setLastErrorMessage(e.what());
      return 1;
    }
  }

  SEXP vectorEnd(WKV1_GeometryMeta* meta, HandlerType* userData) {
    try {
      return userData->vectorEnd(meta);
    } catch (std::exception& e) {
      SEXP tryError = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(tryError, 0, Rf_mkChar(e.what()));
      Rf_setAttrib(tryError, Rf_install("class"), Rf_mkString("wk_v1_error"));
      userData->setLastErrorMessage(e.what());
      UNPROTECT(1);
      return tryError;
    }
  }
};

#endif

#endif
