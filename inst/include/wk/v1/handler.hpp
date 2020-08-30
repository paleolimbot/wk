
#ifndef WKV1_HANDLER_HPP
#define WKV1_HANDLER_HPP

#include "wk-v1.h"

class WKV1HandlerException: public std::runtime_error {
public:
  int code;

  WKV1HandlerException(const char* message):
    std::runtime_error(message), code(WKV1_DEFAULT_ERROR_CODE) {}
  WKV1HandlerException(const char* message, int code):
    std::runtime_error(message), code(code) {}
};

class WKV1VoidHandler {
public:
  R_xlen_t lastFeatureId;
  int lastErrorCode;
  std::string lastErrorMessage;

  WKV1VoidHandler(): lastFeatureId(-1), lastErrorCode(WKV1_NO_ERROR_CODE) {}

  virtual ~WKV1VoidHandler() {}

  char vectorStart(WKV1_GeometryMeta* meta) {
    return WKV1_CONTINUE;
  }

  char featureStart(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return WKV1_CONTINUE;
  }

  char nullFeature(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return WKV1_CONTINUE;
  }

  char geometryStart(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId) {
    return WKV1_CONTINUE;
  }

  char ringStart(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId) {
    return WKV1_CONTINUE;
  }

  char coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId) {
    return WKV1_CONTINUE;
  }

  char ringEnd(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId) {
    return WKV1_CONTINUE;
  }

  char geometryEnd(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId) {
    return WKV1_CONTINUE;
  }

  char featureEnd(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return WKV1_CONTINUE;
  }

  SEXP vectorEnd(WKV1_GeometryMeta* meta) {
    return R_NilValue;
  }

  char error(R_xlen_t featureId, int code, const char* message) {
    return WKV1_STOP;
  }
};


template <class HandlerType>
class WKV1HandlerFactory {
public:

  static WKV1_Handler* create(HandlerType* userData) {
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

  static SEXP create_xptr(HandlerType* userData) {
    WKV1_Handler* handler = create(userData);
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
    } catch (WKV1HandlerException& e) {
      return userData->error(-1, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static char featureStart(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    userData->lastFeatureId = featureId;
    try {
      return userData->featureStart(meta, nFeatures, featureId);
    } catch (WKV1HandlerException& e) {
      return userData->error(featureId, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static char nullFeature(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->nullFeature(meta, nFeatures, featureId);
    } catch (WKV1HandlerException& e) {
      return userData->error(featureId, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static char geometryStart(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->geometryStart(meta, nParts, partId);
    } catch (WKV1HandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static char ringStart(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->ringStart(meta, nRings, ringId);
    } catch (WKV1HandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static char coord(WKV1_GeometryMeta* meta, WKV1_Coord coord, unsigned int nCoords, unsigned int coordId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->coord(meta, coord, nCoords, coordId);
    } catch (WKV1HandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static char ringEnd(WKV1_GeometryMeta* meta, unsigned int nRings, unsigned int ringId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->ringEnd(meta, nRings, ringId);
    } catch (WKV1HandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static char geometryEnd(WKV1_GeometryMeta* meta, unsigned int nParts, unsigned int partId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->geometryEnd(meta, nParts, partId);
    } catch (WKV1HandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static char featureEnd(WKV1_GeometryMeta* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->featureEnd(meta, nFeatures, featureId);
    } catch (WKV1HandlerException& e) {
      return userData->error(featureId, e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }

  static SEXP vectorEnd(WKV1_GeometryMeta* meta, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->vectorEnd(meta);
    } catch (WKV1HandlerException& e) {
      // if the error handler returns WKV1_CONTINUE here, it is unclear
      // what should be returned (hence returning an error sentinel regardless)
      return WKV1_error_sentinel(e.code, e.what());
    } catch (std::exception& e) {
      return WKV1_error_sentinel(WKV1_DEFAULT_ERROR_CODE, e.what());
    }
  }

  static char error(R_xlen_t featureId, int code, const char* message, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      char result = userData->error(featureId, code, message);
      if (result == WKV1_STOP) {
        // set the last error
      }
      return result;
    } catch (std::exception& e) {
      return WKV1_STOP;
    }
  }
};

#endif
