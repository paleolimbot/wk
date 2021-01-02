
#include "wk-v1.h"
#include <stdlib.h>

char wk_handler_void_vector_start(const wk_meta_t* meta, void* handler_data) {
  return WK_CONTINUE;
}

SEXP wk_handler_void_vector_end(const wk_meta_t* meta, void* handler_data) {
  return R_NilValue;
}

char wk_handler_void_feature(const wk_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  return WK_CONTINUE;
}

char wk_handler_void_geometry(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  return WK_CONTINUE;
}

char wk_handler_void_ring(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  return WK_CONTINUE;
}

char wk_handler_void_coord(const wk_meta_t* meta, const wk_coord_t coord, uint32_t coord_id, void* handler_data) {
  return WK_CONTINUE;
}

char wk_handler_void_error(R_xlen_t feat_id, int code, const char* message, void* handler_data) {
  Rf_error(message);
  return WK_ABORT;
}

void wk_handler_void_finalizer(void* handler_data) {

}

wk_handler_t* wk_handler_create() {
  wk_handler_t* handler = (wk_handler_t*) malloc(sizeof(wk_handler_t));
  handler->api_version = 1;
  handler->dirty = 0;
  handler->handler_data = NULL;

  handler->vector_start = &wk_handler_void_vector_start;
  handler->vector_end = &wk_handler_void_vector_end;

  handler->feature_start = &wk_handler_void_feature;
  handler->null_feature = &wk_handler_void_feature;
  handler->feature_end = &wk_handler_void_feature;

  handler->geometry_start = &wk_handler_void_geometry;
  handler->geometry_end = &wk_handler_void_geometry;

  handler->ring_start = &wk_handler_void_ring;
  handler->ring_end = &wk_handler_void_ring;

  handler->coord = &wk_handler_void_coord;

  handler->error = &wk_handler_void_error;
  handler->vector_finally = &wk_handler_void_finalizer;
  handler->finalizer = &wk_handler_void_finalizer;

  return handler;
}

void wk_handler_destroy(wk_handler_t* handler) {
  if (handler != NULL) {
    handler->finalizer(handler->handler_data);
    free(handler);
  }
}

void wk_handler_destroy_xptr(SEXP xptr) {
  wk_handler_destroy((wk_handler_t*) R_ExternalPtrAddr(xptr));
}

SEXP wk_handler_create_xptr(wk_handler_t* handler, SEXP tag, SEXP prot) {
  SEXP xptr = R_MakeExternalPtr(handler, tag, prot);
  R_RegisterCFinalizerEx(xptr, &wk_handler_destroy_xptr, FALSE);
  return xptr;
}

struct wk_handler_run_data {
  SEXP (*readFunction)(SEXP readData, wk_handler_t* handler);
  SEXP readData;
  wk_handler_t* handler;
};

void wk_handler_run_cleanup(void* data) {
  struct wk_handler_run_data* runData = (struct wk_handler_run_data*) data;
  runData->handler->vector_finally(runData->handler->handler_data);
}

SEXP wk_handler_run_internal(void* data) {
  struct wk_handler_run_data* runData = (struct wk_handler_run_data*) data;

  if (runData->handler->api_version != 1) {
    Rf_error("Can't run a wk_handler with api_version '%d'", runData->handler->api_version);
  }

  if (runData->handler->dirty) {
    Rf_error("Can't re-use a wk_handler");
  } else {
    runData->handler->dirty = 1;
  }

  return runData->readFunction(runData->readData, runData->handler);
}

SEXP wk_handler_run_xptr(SEXP (*readFunction)(SEXP readData, wk_handler_t* handler), SEXP readData, SEXP xptr) {
  wk_handler_t* handler = (wk_handler_t*) R_ExternalPtrAddr(xptr);
  struct wk_handler_run_data runData = { readFunction, readData, handler };
  return R_ExecWithCleanup(&wk_handler_run_internal, &runData, &wk_handler_run_cleanup, &runData);
}
