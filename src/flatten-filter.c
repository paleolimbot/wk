#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

typedef struct {
  wk_handler_t* next;
  wk_vector_meta_t vector_meta;
  int feature_id;
  int add_details;
  SEXP details;
  R_xlen_t details_size;
} flatten_filter_t;

#define HANDLE_OR_RETURN(expr)                                 \
  result = expr;                                               \
  if (result != WK_CONTINUE) return result

#define META_IS_COLLECTION(meta) \
  ((meta->geometry_type == WK_GEOMETRY) || (meta->geometry_type == WK_MULTIPOINT) || (meta->geometry_type == WK_MULTILINESTRING) || (meta->geometry_type == WK_MULTIPOLYGON))

void wk_flatten_filter_initialize(int* dirty, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  *dirty = 1;
  flatten_filter->next->initialize(&flatten_filter->next->dirty, flatten_filter->next->handler_data);
}

int wk_flatten_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;

  flatten_filter->feature_id = -1;

  memcpy(&(flatten_filter->vector_meta), meta, sizeof(wk_vector_meta_t));
  if (META_IS_COLLECTION(meta)) {
    flatten_filter->vector_meta.size = WK_VECTOR_SIZE_UNKNOWN;
  }

  return flatten_filter->next->vector_start(meta, flatten_filter->next->handler_data);
}

SEXP wk_flatten_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->vector_end(meta, flatten_filter->next->handler_data);;
}

int wk_flatten_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_flatten_filter_feature_null(void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  int result;
  flatten_filter->feature_id++;
  HANDLE_OR_RETURN(flatten_filter->next->feature_start(&(flatten_filter->vector_meta), flatten_filter->feature_id, flatten_filter->next->handler_data));
  HANDLE_OR_RETURN(flatten_filter->next->null_feature(flatten_filter->next->handler_data));
  return flatten_filter->next->feature_end(&(flatten_filter->vector_meta), flatten_filter->feature_id, flatten_filter->next->handler_data);
}

int wk_flatten_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_flatten_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  int result;
  if (!META_IS_COLLECTION(meta)) {
    flatten_filter->feature_id++;
    HANDLE_OR_RETURN(flatten_filter->next->feature_start(&(flatten_filter->vector_meta), flatten_filter->feature_id, flatten_filter->next->handler_data));
    return flatten_filter->next->geometry_start(meta, WK_PART_ID_NONE, flatten_filter->next->handler_data);
  } else {
    return WK_CONTINUE;
  }
}

int wk_flatten_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  int result;
  if (!META_IS_COLLECTION(meta)) {
    HANDLE_OR_RETURN(flatten_filter->next->geometry_end(meta, WK_PART_ID_NONE, flatten_filter->next->handler_data));
    return flatten_filter->next->feature_end(&(flatten_filter->vector_meta), flatten_filter->feature_id, flatten_filter->next->handler_data);
  } else {
    return WK_CONTINUE;
  }
}

int wk_flatten_filter_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->ring_start(meta, size, ring_id, flatten_filter->next->handler_data);
}

int wk_flatten_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->ring_end(meta, size, ring_id, flatten_filter->next->handler_data);
}

int wk_flatten_filter_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->coord(meta, coord, coord_id, flatten_filter->next->handler_data);
}

int wk_flatten_filter_error(const char* message, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->error(message, flatten_filter->next->handler_data);
}

void wk_flatten_filter_deinitialize(void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  flatten_filter->next->deinitialize(flatten_filter->next->handler_data);
}

void wk_flatten_filter_finalize(void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  if (flatten_filter != NULL) {
    // finalizer for flatten_filter->next is run by the externalptr finalizer
    // and should not be called here
    free(flatten_filter);
  }
}

SEXP wk_c_flatten_filter_new(SEXP handler_xptr, SEXP add_details) {
  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_flatten_filter_initialize;
  handler->vector_start = &wk_flatten_filter_vector_start;
  handler->vector_end = &wk_flatten_filter_vector_end;

  handler->feature_start = &wk_flatten_filter_feature_start;
  handler->null_feature = &wk_flatten_filter_feature_null;
  handler->feature_end = &wk_flatten_filter_feature_end;

  handler->geometry_start = &wk_flatten_filter_geometry_start;
  handler->geometry_end = &wk_flatten_filter_geometry_end;

  handler->ring_start = &wk_flatten_filter_ring_start;
  handler->ring_end = &wk_flatten_filter_ring_end;

  handler->coord = &wk_flatten_filter_coord;

  handler->error = &wk_flatten_filter_error;

  handler->deinitialize = &wk_flatten_filter_deinitialize;
  handler->finalizer = &wk_flatten_filter_finalize;

  flatten_filter_t* flatten_filter = (flatten_filter_t*) malloc(sizeof(flatten_filter_t));
  if (flatten_filter == NULL) {
    wk_handler_destroy(handler); // # nocov
    Rf_error("Failed to alloc handler data"); // # nocov
  }

  flatten_filter->next = R_ExternalPtrAddr(handler_xptr);
  if (flatten_filter->next->api_version != 1) {
    Rf_error("Can't run a wk_handler with api_version '%d'", flatten_filter->next->api_version); // # nocov
  }

  WK_VECTOR_META_RESET(flatten_filter->vector_meta, WK_GEOMETRY);
  flatten_filter->add_details = LOGICAL(add_details)[0];
  flatten_filter->details = R_NilValue;
  flatten_filter->details_size = 0;
  flatten_filter->feature_id = 0;

  handler->handler_data = flatten_filter;

  // include the external pointer as a tag for this external pointer
  // which guarnatees that it will not be garbage collected until
  // this object is garbage collected
  return wk_handler_create_xptr(handler, handler_xptr, R_NilValue);
}

