
#ifndef WK_V1_HPP_INCLUDED
#define WK_V1_HPP_INCLUDED

#include "cpp11/external_pointer.hpp"
#include "cpp11/protect.hpp"
#include <string>
#include <stdexcept>
#include "wk-v1.h"

class WKParseException: public std::runtime_error {
public:
  WKParseException(int code, std::string message): std::runtime_error(message), exceptionCode(code) {}
  WKParseException(std::string message): std::runtime_error(message), exceptionCode(WK_DEFAULT_ERROR_CODE) {}

  int code() {
    return this->exceptionCode;
  }

private:
  int exceptionCode;
};

class WKHandler {
public:

  WKHandler(WKHandler_t* handler): handler(handler) {}

  ~WKHandler() {
    this->handler->finalizer(this->handler->userData);
  }

  char vectorStart(const WKGeometryMeta_t* meta) {
    return cpp11::safe[handler->vectorStart](meta, handler->userData);
  }

  char featureStart(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return cpp11::safe[handler->featureStart](meta, nFeatures, featureId, handler->userData);
  }

  char nullFeature(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return cpp11::safe[handler->nullFeature](meta, nFeatures, featureId, handler->userData);
  }

  char geometryStart(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId) {
    return cpp11::safe[handler->geometryStart](meta, nParts, partId, handler->userData);
  }

  char ringStart(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId) {
    return cpp11::safe[handler->ringStart](meta, nRings, ringId, handler->userData);
  }

  char coord(const WKGeometryMeta_t* meta, WKCoord_t coord, uint32_t nCoords, uint32_t coordId) {
    return cpp11::safe[handler->coord](meta, coord, nCoords, coordId, handler->userData);
  }

  char ringEnd(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId) {
    return cpp11::safe[handler->ringEnd](meta, nRings, ringId, handler->userData);
  }

  char geometryEnd(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId) {
    return cpp11::safe[handler->geometryEnd](meta, nParts, partId, handler->userData);
  }

  char featureEnd(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return cpp11::safe[handler->featureEnd](meta, nFeatures, featureId, handler->userData);
  }

  SEXP vectorEnd(const WKGeometryMeta_t* meta) {
    return cpp11::safe[handler->vectorEnd](meta, handler->userData);
  }

  char error(R_xlen_t featureId, int code, const char* message) {
    return cpp11::safe[handler->error](featureId, code, message, handler->userData);
  }

private:
  WKHandler_t* handler;
};


// ---- the class one should extend when writing handlers in C++ ---

class WKHandlerException: public std::runtime_error {
public:
  int code;

  WKHandlerException(const char* message):
    std::runtime_error(message), code(WK_DEFAULT_ERROR_CODE) {}
  WKHandlerException(const char* message, int code):
    std::runtime_error(message), code(code) {}
};

class WKVoidHandler {
public:
  R_xlen_t lastFeatureId;
  int lastErrorCode;
  std::string lastErrorMessage;

  WKVoidHandler(): lastFeatureId(-1), lastErrorCode(WK_NO_ERROR_CODE) {}

  virtual ~WKVoidHandler() {}

  virtual char vectorStart(const WKGeometryMeta_t* meta) {
    return WK_CONTINUE;
  }

  virtual char featureStart(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return WK_CONTINUE;
  }

  virtual char nullFeature(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return WK_CONTINUE;
  }

  virtual char geometryStart(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId) {
    return WK_CONTINUE;
  }

  virtual char ringStart(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId) {
    return WK_CONTINUE;
  }

  virtual char coord(const WKGeometryMeta_t* meta, WKCoord_t coord, uint32_t nCoords, uint32_t coordId) {
    return WK_CONTINUE;
  }

  virtual char ringEnd(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId) {
    return WK_CONTINUE;
  }

  virtual char geometryEnd(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId) {
    return WK_CONTINUE;
  }

  virtual char featureEnd(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId) {
    return WK_CONTINUE;
  }

  virtual SEXP vectorEnd(const WKGeometryMeta_t* meta) {
    return R_NilValue;
  }

  virtual char error(R_xlen_t featureId, int code, const char* message) {
    return WK_ABORT;
  }

  virtual void finalize() noexcept {

  }
};


template <class HandlerType>
class WKHandlerFactory {
public:

  static WKHandler_t* create(HandlerType* userData) {
    WKHandler_t* handler = wk_handler_create();
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
    WKHandler_t* handler = create(userData);
    SEXP xptr = R_MakeExternalPtr(handler, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(xptr, &deleteSEXPHandler, TRUE);
    return xptr;
  }

private:
  static void deleteSEXPHandler(SEXP xptr) {
    WKHandler_t* handler = (WKHandler_t*) R_ExternalPtrAddr(xptr);
    deleteUserData(handler->userData);
    wk_handler_destroy(handler);
  }

  static void deleteUserData(void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    if (userData != NULL) {
      delete userData;
    }
  }

  static char vectorStart(const WKGeometryMeta_t* meta, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->vectorStart(meta);
    } catch (WKHandlerException& e) {
      return userData->error(-1, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char featureStart(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    userData->lastFeatureId = featureId;
    try {
      return userData->featureStart(meta, nFeatures, featureId);
    } catch (WKHandlerException& e) {
      return userData->error(featureId, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char nullFeature(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->nullFeature(meta, nFeatures, featureId);
    } catch (WKHandlerException& e) {
      return userData->error(featureId, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char geometryStart(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->geometryStart(meta, nParts, partId);
    } catch (WKHandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char ringStart(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->ringStart(meta, nRings, ringId);
    } catch (WKHandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char coord(const WKGeometryMeta_t* meta, WKCoord_t coord, uint32_t nCoords, uint32_t coordId,
                    void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->coord(meta, coord, nCoords, coordId);
    } catch (WKHandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char ringEnd(const WKGeometryMeta_t* meta, uint32_t nRings, uint32_t ringId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->ringEnd(meta, nRings, ringId);
    } catch (WKHandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char geometryEnd(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->geometryEnd(meta, nParts, partId);
    } catch (WKHandlerException& e) {
      return userData->error(userData->lastFeatureId, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char featureEnd(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->featureEnd(meta, nFeatures, featureId);
    } catch (WKHandlerException& e) {
      return userData->error(featureId, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static SEXP vectorEnd(const WKGeometryMeta_t* meta, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      return userData->vectorEnd(meta);
    } catch (WKHandlerException& e) {
      // if the error handler returns WK_CONTINUE here, it is unclear
      // what should be returned (hence returning an error sentinel regardless)
      return wk_error_sentinel(e.code, e.what());
    } catch (std::exception& e) {
      return wk_error_sentinel(WK_DEFAULT_ERROR_CODE, e.what());
    }
  }

  static char error(R_xlen_t featureId, int code, const char* message, void* userDataPtr) {
    HandlerType* userData = (HandlerType*) userDataPtr;
    try {
      char result = userData->error(featureId, code, message);
      if (result == WK_ABORT) {
        // set the last error
      }
      return result;
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static void finalize(void* userDataPtr) noexcept {
    HandlerType* userData = (HandlerType*) userDataPtr;
    userData->finalize();
  }
};

#endif
