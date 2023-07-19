
#ifndef WK_V1_HANDLER_HPP_INCLUDED
#define WK_V1_HANDLER_HPP_INCLUDED

#define R_NO_REMAP
#include "wk-v1.h"
#include <stdexcept>
#include <cstring>

// This is an internal C++ class for instances where it's easier to use
// something in the C++ (>=11) standard library. It is safe to throw
// exceptions in handler methods (which are caught and converted to
// an Rf_error()); it is safe to longjmp from handler methods provided
// that the method has been written in such a way that nothing is
// stack-allocated that has a non-trivial destructor. As noted below,
// you can get around this by declaring an object with a non-trivial
// destructor as a member of the class, which always gets deleted
// by the external pointer finalizer.
class WKVoidHandler {
public:
  WKVoidHandler() {
    memset(this->internal_error_message, 0, 8192);
  }
  virtual ~WKVoidHandler() {}

  // # nocov start

  virtual void initialize(int* dirty) {
    if (*dirty) {
      Rf_error("Can't re-use this wk_handler");
    }

    *dirty = 1;
  }

  virtual int vector_start(const wk_vector_meta_t* meta) {
    return WK_CONTINUE;
  }

  virtual int feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id) {
    return WK_CONTINUE;
  }

  virtual int null_feature() {
    return WK_CONTINUE;
  }

  virtual int geometry_start(const wk_meta_t* meta, uint32_t part_id) {
    return WK_CONTINUE;
  }

  virtual int ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    return WK_CONTINUE;
  }

  virtual int coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id) {
    return WK_CONTINUE;
  }

  virtual int ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    return WK_CONTINUE;
  }

  virtual int geometry_end(const wk_meta_t* meta, uint32_t part_id) {
    return WK_CONTINUE;
  }

  virtual int feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id) {
    return WK_CONTINUE;
  }

  virtual SEXP vector_end(const wk_vector_meta_t* meta) {
    return R_NilValue;
  }

  virtual void deinitialize() {
     
  }

  virtual int error(const char* message) {
    Rf_error("%s", message);
  }

  // # nocov end

  char internal_error_message[8192];
};

// The previous version of this macro also handled cpp11::unwind_exception
// throws; however, in this simplified version, we just handle regular
// exceptions and require that users consider the longjmp-y-ness of
// their handler methods. Because handlers are always cleaned up via
// deinitialize/finalize, C++ handlers can declare anything with a
// non-trivial destructor as a handler class member rather than
// a stack-allocated variable.
#define WK_V1_HANDLER_BEGIN_CPP11                              \
  cpp_handler->internal_error_message[0] = '\0';               \
  try {
#define WK_V1_HANDLER_END_CPP11(_error_return)                 \
  } catch (std::exception & e) {                               \
    strncpy(cpp_handler->internal_error_message, e.what(), 8192 - 1); \
  } catch (...) {                                              \
    strncpy(cpp_handler->internal_error_message, "C++ error (unknown cause)", 8192 - 1); \
  }                                                            \
  if (cpp_handler->internal_error_message[0] != '\0') {        \
    Rf_error("%s", cpp_handler->internal_error_message);       \
  }                                                            \
  return _error_return;

template <class HandlerType>
class WKHandlerFactory {
public:

  static wk_handler_t* create(HandlerType* handler_data) {
    wk_handler_t* handler = wk_handler_create();
    handler->handler_data = handler_data;

    handler->initialize = &initialize;
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

    handler->deinitialize = &deinitialize;
    handler->finalizer = &finalizer;

    return handler;
  }

  static SEXP create_xptr(HandlerType* handler_data, SEXP tag = R_NilValue, SEXP prot = R_NilValue) {
    wk_handler_t* handler = create(handler_data);
    return wk_handler_create_xptr(handler, tag, prot);
  }

private:

  static void finalizer(void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    if (cpp_handler != NULL) {
      delete cpp_handler;
    }
  }

  static void initialize(int* dirty, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->initialize(dirty);
    WK_V1_HANDLER_END_CPP11() // # nocov
  }

  static int vector_start(const wk_vector_meta_t* meta, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->vector_start(meta);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static int feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->feature_start(meta, feat_id);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static int null_feature(void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->null_feature();
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static int geometry_start(const wk_meta_t* meta, uint32_t partId, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->geometry_start(meta, partId);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static int ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ringId, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->ring_start(meta, size, ringId);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static int coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->coord(meta, coord, coord_id);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static int ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ringId, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->ring_end(meta, size, ringId);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static int geometry_end(const wk_meta_t* meta, uint32_t partId, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->geometry_end(meta, partId);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static int feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->feature_end(meta, feat_id);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }

  static SEXP vector_end(const wk_vector_meta_t* meta, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->vector_end(meta);
    WK_V1_HANDLER_END_CPP11(R_NilValue) // # nocov
  }

  static void deinitialize(void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->deinitialize();
    WK_V1_HANDLER_END_CPP11() // # nocov
  }

  static int error(const char* message, void* handler_data) noexcept {
    HandlerType* cpp_handler = (HandlerType*) handler_data;
    WK_V1_HANDLER_BEGIN_CPP11
    return cpp_handler->error(message);
    WK_V1_HANDLER_END_CPP11(WK_ABORT) // # nocov
  }
};

#endif
