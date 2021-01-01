
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

  // The constructor and deleter are replacements for the run_handler_xptr() function.
  // Instead, the scope of the WKHandler is used to guarantee that (1) the handler
  // is not being re-used and (2) vectorFinalize() is called and is called
  // as soon as possible.
  WKHandler(WKHandler_t* handler): handler(handler) {
    if (handler->dirty) {
      throw std::runtime_error("Can't re-use a wk_handler");
    } else {
      handler->dirty = 1;
    }
  }

  ~WKHandler() {
    handler->vectorFinally(handler->handler_data);
  }

  char vectorStart(const wk_meta_t* meta) {
    return cpp11::safe[handler->vectorStart](meta, handler->handler_data);
  }

  char featureStart(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    return cpp11::safe[handler->featureStart](meta, n_features, feat_id, handler->handler_data);
  }

  char nullFeature(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    return cpp11::safe[handler->nullFeature](meta, n_features, feat_id, handler->handler_data);
  }

  char geometryStart(const wk_meta_t* meta, uint32_t nParts, uint32_t partId) {
    return cpp11::safe[handler->geometryStart](meta, nParts, partId, handler->handler_data);
  }

  char ringStart(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId) {
    return cpp11::safe[handler->ringStart](meta, size, nRings, ringId, handler->handler_data);
  }

  char coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t nCoords, uint32_t coordId) {
    return cpp11::safe[handler->coord](meta, coord, nCoords, coordId, handler->handler_data);
  }

  char ringEnd(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId) {
    return cpp11::safe[handler->ringEnd](meta, size, nRings, ringId, handler->handler_data);
  }

  char geometryEnd(const wk_meta_t* meta, uint32_t nParts, uint32_t partId) {
    return cpp11::safe[handler->geometryEnd](meta, nParts, partId, handler->handler_data);
  }

  char featureEnd(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    return cpp11::safe[handler->featureEnd](meta, n_features, feat_id, handler->handler_data);
  }

  SEXP vectorEnd(const wk_meta_t* meta) {
    return cpp11::safe[handler->vectorEnd](meta, handler->handler_data);
  }

  char error(R_xlen_t feat_id, int code, const char* message) {
    return cpp11::safe[handler->error](feat_id, code, message, handler->handler_data);
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
  R_xlen_t lastfeat_id;
  int lastErrorCode;
  std::string lastErrorMessage;

  WKVoidHandler(): lastfeat_id(-1), lastErrorCode(WK_NO_ERROR_CODE) {}

  virtual ~WKVoidHandler() {}

  void setError(int code, std::string message) noexcept {
    this->lastErrorCode = code;
    this->lastErrorMessage = message;
  }

  bool hasError() noexcept {
    return this->lastErrorCode != WK_NO_ERROR_CODE;
  }

  virtual char vectorStart(const wk_meta_t* meta) {
    return WK_CONTINUE;
  }

  virtual char featureStart(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    return WK_CONTINUE;
  }

  virtual char nullFeature(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    return WK_CONTINUE;
  }

  virtual char geometryStart(const wk_meta_t* meta, uint32_t nParts, uint32_t partId) {
    return WK_CONTINUE;
  }

  virtual char ringStart(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId) {
    return WK_CONTINUE;
  }

  virtual char coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t nCoords, uint32_t coordId) {
    return WK_CONTINUE;
  }

  virtual char ringEnd(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId) {
    return WK_CONTINUE;
  }

  virtual char geometryEnd(const wk_meta_t* meta, uint32_t nParts, uint32_t partId) {
    return WK_CONTINUE;
  }

  virtual char featureEnd(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    return WK_CONTINUE;
  }

  virtual SEXP vectorEnd(const wk_meta_t* meta) {
    return R_NilValue;
  }

  virtual void vectorFinally() noexcept {
     
  }

  virtual char error(R_xlen_t feat_id, int code, const char* message) noexcept {
    this->setError(code, message);
    return WK_ABORT;
  }

  virtual void finalize() noexcept {

  }
};


template <class HandlerType>
class WKHandlerFactory {
public:

  static WKHandler_t* create(HandlerType* handler_data) {
    WKHandler_t* handler = wk_handler_create();
    handler->handler_data = handler_data;

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

    handler->vectorFinally = &vectorFinally;
    handler->finalizer = &finalizer;

    return handler;
  }

  static SEXP create_xptr(HandlerType* handler_data) {
    WKHandler_t* handler = create(handler_data);
    return wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
  }

private:

  static void finalizer(void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    if (handler_data != NULL) {
      delete handler_data;
    }
  }

  static char vectorStart(const wk_meta_t* meta, void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    try {
      return handler_data->vectorStart(meta);
    } catch (WKHandlerException& e) {
      return handler_data->error(-1, e.code, e.what());
    } catch (std::exception& e) {
      return WK_ABORT;
    }
  }

  static char featureStart(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id, 
                           void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    handler_data->lastfeat_id = feat_id;
    try {
      return handler_data->featureStart(meta, n_features, feat_id);
    } catch (WKHandlerException& e) {
      handler_data->setError(e.code, e.what());
    } catch (std::exception& e) {
      handler_data->setError(WK_DEFAULT_ERROR_CODE, e.what());
    }

    return WK_ABORT;
  }

  static char nullFeature(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id, void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    try {
      return handler_data->nullFeature(meta, n_features, feat_id);
    } catch (WKHandlerException& e) {
      handler_data->setError(e.code, e.what());
    } catch (std::exception& e) {
      handler_data->setError(WK_DEFAULT_ERROR_CODE, e.what());
    }

    return WK_ABORT;
  }

  static char geometryStart(const wk_meta_t* meta, uint32_t nParts, uint32_t partId, void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    try {
      return handler_data->geometryStart(meta, nParts, partId);
    } catch (WKHandlerException& e) {
      handler_data->setError(e.code, e.what());
    } catch (std::exception& e) {
      handler_data->setError(WK_DEFAULT_ERROR_CODE, e.what());
    }

    return WK_ABORT;
  }

  static char ringStart(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, 
                        void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    try {
      return handler_data->ringStart(meta, size, nRings, ringId);
    } catch (WKHandlerException& e) {
      handler_data->setError(e.code, e.what());
    } catch (std::exception& e) {
      handler_data->setError(WK_DEFAULT_ERROR_CODE, e.what());
    }

    return WK_ABORT;
  }

  static char coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t nCoords, uint32_t coordId,
                    void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    try {
      return handler_data->coord(meta, coord, nCoords, coordId);
    } catch (WKHandlerException& e) {
      handler_data->setError(e.code, e.what());
    } catch (std::exception& e) {
      handler_data->setError(WK_DEFAULT_ERROR_CODE, e.what());
    }

    return WK_ABORT;
  }

  static char ringEnd(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, 
                      void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    try {
      return handler_data->ringEnd(meta, size, nRings, ringId);
    } catch (WKHandlerException& e) {
      handler_data->setError(e.code, e.what());
    } catch (std::exception& e) {
      handler_data->setError(WK_DEFAULT_ERROR_CODE, e.what());
    }

    return WK_ABORT;
  }

  static char geometryEnd(const wk_meta_t* meta, uint32_t nParts, uint32_t partId, void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    try {
      return handler_data->geometryEnd(meta, nParts, partId);
    } catch (WKHandlerException& e) {
      handler_data->setError(e.code, e.what());
    } catch (std::exception& e) {
      handler_data->setError(WK_DEFAULT_ERROR_CODE, e.what());
    }

    return WK_ABORT;
  }

  static char featureEnd(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id, void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    try {
      return handler_data->featureEnd(meta, n_features, feat_id);
    } catch (WKHandlerException& e) {
      handler_data->setError(e.code, e.what());
    } catch (std::exception& e) {
      handler_data->setError(WK_DEFAULT_ERROR_CODE, e.what());
    }

    return WK_ABORT;
  }

  static SEXP vectorEnd(const wk_meta_t* meta, void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    if (handler_data->hasError()) {
      return wk_error_sentinel(handler_data->lastErrorCode, handler_data->lastErrorMessage.c_str());
    }

    try {
      return handler_data->vectorEnd(meta);
    } catch (WKHandlerException& e) {
      return wk_error_sentinel(e.code, e.what());
    } catch (std::exception& e) {
      return wk_error_sentinel(WK_DEFAULT_ERROR_CODE, e.what());
    }
  }

  static void vectorFinally(void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    handler_data->vectorFinally();
  }

  static char error(R_xlen_t feat_id, int code, const char* message, void* handler_dataPtr) noexcept {
    HandlerType* handler_data = (HandlerType*) handler_dataPtr;
    return handler_data->error(feat_id, code, message);
  }
};

#endif
