
#include "wk-v1.h"
#include <stdlib.h>

void wk_default_handler_initialize(int* dirty, void* handler_data) {
  if (*dirty) {
    Rf_error("Can't re-use this wk_handler");
  }

  *dirty = 1;
}

int wk_default_handler_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  return WK_CONTINUE;
}

SEXP wk_default_handler_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  return R_NilValue;
}

int wk_default_handler_feature(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_default_handler_geometry(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_default_handler_ring(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_default_handler_coord(const wk_meta_t* meta, const wk_coord_t coord, uint32_t coord_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_default_handler_error(const char* message, void* handler_data) {
  Rf_error(message);
  return WK_ABORT;
}

void wk_default_handler_finalizer(void* handler_data) {

}

wk_handler_t* wk_handler_create() {
  wk_handler_t* handler = (wk_handler_t*) malloc(sizeof(wk_handler_t));
  handler->api_version = 1;
  handler->dirty = 0;
  handler->handler_data = NULL;

  handler->initialize = &wk_default_handler_initialize;
  handler->vector_start = &wk_default_handler_vector_start;
  handler->vector_end = &wk_default_handler_vector_end;

  handler->feature_start = &wk_default_handler_feature;
  handler->null_feature = &wk_default_handler_feature;
  handler->feature_end = &wk_default_handler_feature;

  handler->geometry_start = &wk_default_handler_geometry;
  handler->geometry_end = &wk_default_handler_geometry;

  handler->ring_start = &wk_default_handler_ring;
  handler->ring_end = &wk_default_handler_ring;

  handler->coord = &wk_default_handler_coord;

  handler->error = &wk_default_handler_error;
  handler->deinitialize = &wk_default_handler_finalizer;
  handler->finalizer = &wk_default_handler_finalizer;

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
  SEXP (*readFunction)(SEXP read_data, wk_handler_t* handler);
  SEXP read_data;
  wk_handler_t* handler;
};

void wk_handler_run_cleanup(void* data) {
  struct wk_handler_run_data* run_data = (struct wk_handler_run_data*) data;
  run_data->handler->deinitialize(run_data->handler->handler_data);
}

SEXP wk_handler_run_internal(void* data) {
  struct wk_handler_run_data* run_data = (struct wk_handler_run_data*) data;

  if (run_data->handler->api_version != 1) {
    // # nocov start
    Rf_error("Can't run a wk_handler with api_version '%d'", run_data->handler->api_version);
    // # nocov end
  }

  run_data->handler->initialize(&(run_data->handler->dirty), run_data->handler->handler_data);

  return run_data->readFunction(run_data->read_data, run_data->handler);
}

SEXP wk_handler_run_xptr(SEXP (*readFunction)(SEXP read_data, wk_handler_t* handler),
                         SEXP read_data, SEXP xptr) {
  wk_handler_t* handler = (wk_handler_t*) R_ExternalPtrAddr(xptr);
  struct wk_handler_run_data run_data = { readFunction, read_data, handler };
  return R_ExecWithCleanup(&wk_handler_run_internal, &run_data, &wk_handler_run_cleanup, &run_data);
}
