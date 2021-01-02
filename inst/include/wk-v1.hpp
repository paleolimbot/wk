
#ifndef WK_V1_HPP_INCLUDED
#define WK_V1_HPP_INCLUDED

#include "cpp11/external_pointer.hpp"
#include "cpp11/protect.hpp"
#include "cpp11/declarations.hpp"
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
  WKHandler(wk_handler_t* handler): handler(handler) {
    if (handler->dirty) {
      throw std::runtime_error("Can't re-use a wk_handler");
    } else {
      handler->dirty = 1;
    }
  }

  ~WKHandler() {
    handler->vector_finally(handler->handler_data);
  }

  char vector_start(const wk_meta_t* meta) {
    return cpp11::safe[handler->vector_start](meta, handler->handler_data);
  }

  char feature_start(const wk_meta_t* meta, R_xlen_t feat_id) {
    return cpp11::safe[handler->feature_start](meta, feat_id, handler->handler_data);
  }

  char null_feature(const wk_meta_t* meta, R_xlen_t feat_id) {
    return cpp11::safe[handler->null_feature](meta, feat_id, handler->handler_data);
  }

  char geometry_start(const wk_meta_t* meta, uint32_t partId) {
    return cpp11::safe[handler->geometry_start](meta, partId, handler->handler_data);
  }

  char ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ringId) {
    return cpp11::safe[handler->ring_start](meta, size, ringId, handler->handler_data);
  }

  char coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t coord_id) {
    return cpp11::safe[handler->coord](meta, coord, coord_id, handler->handler_data);
  }

  char ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ringId) {
    return cpp11::safe[handler->ring_end](meta, size, ringId, handler->handler_data);
  }

  char geometry_end(const wk_meta_t* meta, uint32_t partId) {
    return cpp11::safe[handler->geometry_end](meta, partId, handler->handler_data);
  }

  char feature_end(const wk_meta_t* meta, R_xlen_t feat_id) {
    return cpp11::safe[handler->feature_end](meta, feat_id, handler->handler_data);
  }

  SEXP vector_end(const wk_meta_t* meta) {
    return cpp11::safe[handler->vector_end](meta, handler->handler_data);
  }

  char error(R_xlen_t feat_id, int code, const char* message) {
    return cpp11::safe[handler->error](feat_id, code, message, handler->handler_data);
  }

private:
  wk_handler_t* handler;
};


// ---- the class one should extend when writing handlers in C++ ---
class WKVoidHandler {
public:
  WKVoidHandler() {}
  virtual ~WKVoidHandler() {}

  virtual char vector_start(const wk_meta_t* meta) {
    return WK_CONTINUE;
  }

  virtual char feature_start(const wk_meta_t* meta, R_xlen_t feat_id) {
    return WK_CONTINUE;
  }

  virtual char null_feature(const wk_meta_t* meta, R_xlen_t feat_id) {
    return WK_CONTINUE;
  }

  virtual char geometry_start(const wk_meta_t* meta, uint32_t part_id) {
    return WK_CONTINUE;
  }

  virtual char ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    return WK_CONTINUE;
  }

  virtual char coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t coord_id) {
    return WK_CONTINUE;
  }

  virtual char ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    return WK_CONTINUE;
  }

  virtual char geometry_end(const wk_meta_t* meta, uint32_t part_id) {
    return WK_CONTINUE;
  }

  virtual char feature_end(const wk_meta_t* meta, R_xlen_t feat_id) {
    return WK_CONTINUE;
  }

  virtual SEXP vector_end(const wk_meta_t* meta) {
    return R_NilValue;
  }

  virtual void vector_finally() {
     
  }

  virtual char error(R_xlen_t feat_id, int code, const char* message) {
    cpp11::stop(message);
  }

  virtual void finalize() noexcept {

  }
};

// Need our own BEGIN_CPP11 and END_CPP11 because we don't always return an SEXP
// and the macro contains 'return R_NilValue' which causes a compiler error
// https://github.com/r-lib/cpp11/blob/master/inst/include/cpp11/declarations.hpp
#define WK_BEGIN_CPP11            \
  SEXP err = R_NilValue;          \
  const size_t ERROR_SIZE = 8192; \
  char buf[ERROR_SIZE] = "";      \
  try {
#define WK_END_CPP11(_ret)                                     \
  }                                                            \
  catch (cpp11::unwind_exception & e) {                        \
    err = e.token;                                             \
  }                                                            \
  catch (std::exception & e) {                                 \
    strncpy(buf, e.what(), ERROR_SIZE - 1);                    \
  }                                                            \
  catch (...) {                                                \
    strncpy(buf, "C++ error (unknown cause)", ERROR_SIZE - 1); \
  }                                                            \
  if (buf[0] != '\0') {                                        \
    Rf_errorcall(R_NilValue, "%s", buf);                       \
  } else if (err != R_NilValue) {                              \
    CPP11_UNWIND                                               \
  }                                                            \
  return _ret;

template <class HandlerType>
class WKHandlerFactory {
public:

  static wk_handler_t* create(HandlerType* handler_data) {
    wk_handler_t* handler = wk_handler_create();
    handler->handler_data = handler_data;

    handler->vector_start = &vector_start;
    handler->vector_end = &vector_end;

    handler->feature_start = &feature_start;
    handler->null_feature = &null_feature;
    handler->feature_end = &feature_end;

    handler->geometry_start = &geometry_start;
    handler->geometry_end = &geometry_end;

    handler->ring_start = &ring_start;
    handler->ring_end = &ring_end;

    handler->coord = &coord;

    handler->error = &error;

    handler->vector_finally = &vector_finally;
    handler->finalizer = &finalizer;

    return handler;
  }

  static SEXP create_xptr(HandlerType* handler_data) {
    wk_handler_t* handler = create(handler_data);
    return wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
  }

private:

  static void finalizer(void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    if (cpp_handler != NULL) {
      delete cpp_handler;
    }
  }

  static char vector_start(const wk_meta_t* meta, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->vector_start(meta);
    WK_END_CPP11(WK_ABORT)
  }

  static char feature_start(const wk_meta_t* meta, R_xlen_t feat_id, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->feature_start(meta, feat_id);
    WK_END_CPP11(WK_ABORT)
  }

  static char null_feature(const wk_meta_t* meta, R_xlen_t feat_id, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->null_feature(meta, feat_id);
    WK_END_CPP11(WK_ABORT)
  }

  static char geometry_start(const wk_meta_t* meta, uint32_t partId, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->geometry_start(meta, partId);
    WK_END_CPP11(WK_ABORT)
  }

  static char ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ringId, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->ring_start(meta, size, ringId);
    WK_END_CPP11(WK_ABORT)
  }

  static char coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t coord_id, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->coord(meta, coord, coord_id);
    WK_END_CPP11(WK_ABORT)
  }

  static char ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ringId, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->ring_end(meta, size, ringId);
    WK_END_CPP11(WK_ABORT)
  }

  static char geometry_end(const wk_meta_t* meta, uint32_t partId, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->geometry_end(meta, partId);
    WK_END_CPP11(WK_ABORT)
  }

  static char feature_end(const wk_meta_t* meta, R_xlen_t feat_id, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->feature_end(meta, feat_id);
    WK_END_CPP11(WK_ABORT)
  }

  static SEXP vector_end(const wk_meta_t* meta, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->vector_end(meta);
    WK_END_CPP11(R_NilValue)
  }

  static void vector_finally(void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    cpp_handler->vector_finally();
    WK_END_CPP11()
  }

  static char error(R_xlen_t feat_id, int code, const char* message, void* handler_data) noexcept {
    WK_BEGIN_CPP11
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    return cpp_handler->error(feat_id, code, message);
    WK_END_CPP11(WK_ABORT)
  }
};

#endif
