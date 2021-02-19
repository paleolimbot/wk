#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

typedef struct {
  wk_handler_t* next;
} identity_filter_t;

void wk_identity_filter_initialize(int* dirty, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  *dirty = 1;
  identity_filter->next->initialize(&identity_filter->next->dirty, identity_filter->next->handler_data);
}

int wk_identity_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->vector_start(meta, identity_filter->next->handler_data);
}

SEXP wk_identity_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->vector_end(meta, identity_filter->next->handler_data);;
}

int wk_identity_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->feature_start(meta, feat_id, identity_filter->next->handler_data);
}

int wk_identity_filter_feature_null(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->null_feature(meta, feat_id, identity_filter->next->handler_data);
}

int wk_identity_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->feature_end(meta, feat_id, identity_filter->next->handler_data);
}

int wk_identity_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->geometry_start(meta, part_id, identity_filter->next->handler_data);
}

int wk_identity_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->geometry_end(meta, part_id, identity_filter->next->handler_data);
}

int wk_identity_filter_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->ring_start(meta, size, ring_id, identity_filter->next->handler_data);
}

int wk_identity_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->ring_end(meta, size, ring_id, identity_filter->next->handler_data);
}

int wk_identity_filter_coord(const wk_meta_t* meta, const wk_coord_t coord, uint32_t coord_id, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->coord(meta, coord, coord_id, identity_filter->next->handler_data);
}

int wk_identity_filter_error(R_xlen_t feat_id, int code, const char* message, void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  return identity_filter->next->error(feat_id, code, message, identity_filter->next->handler_data);
}

void wk_identity_filter_deinitialize(void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  identity_filter->next->deinitialize(identity_filter->next->handler_data);
}

void wk_identity_filter_finalize(void* handler_data) {
  identity_filter_t* identity_filter = (identity_filter_t*) handler_data;
  if (identity_filter != NULL) {
    // finalizer for identity_filter->next is run by the externalptr finalizer
    // and should not be called here
    free(identity_filter);
  }
}

SEXP wk_c_identity_filter_new(SEXP handler_xptr) {
  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_identity_filter_initialize;
  handler->vector_start = &wk_identity_filter_vector_start;
  handler->vector_end = &wk_identity_filter_vector_end;

  handler->feature_start = &wk_identity_filter_feature_start;
  handler->null_feature = &wk_identity_filter_feature_null;
  handler->feature_end = &wk_identity_filter_feature_end;

  handler->geometry_start = &wk_identity_filter_geometry_start;
  handler->geometry_end = &wk_identity_filter_geometry_end;

  handler->ring_start = &wk_identity_filter_ring_start;
  handler->ring_end = &wk_identity_filter_ring_end;

  handler->coord = &wk_identity_filter_coord;

  handler->error = &wk_identity_filter_error;

  handler->deinitialize = &wk_identity_filter_deinitialize;
  handler->finalizer = &wk_identity_filter_finalize;

  identity_filter_t* identity_filter = (identity_filter_t*) malloc(sizeof(identity_filter_t));

  identity_filter->next = R_ExternalPtrAddr(handler_xptr);
  if (identity_filter->next->api_version != 1) {
    Rf_error("Can't run a wk_handler with api_version '%d'", identity_filter->next->api_version);
  }

  handler->handler_data = identity_filter;

  // include the external pointer as a tag for this external pointer
  // which guarnatees that it will not be garbage collected until
  // this object is garbage collected
  return wk_handler_create_xptr(handler, handler_xptr, R_NilValue);
}
