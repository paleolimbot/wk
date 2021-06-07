
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

#define HANDLE_OR_RETURN(expr)                                 \
  result = expr;                                               \
  if (result != WK_CONTINUE) return result

typedef struct {
  wk_handler_t* next;
  wk_vector_meta_t vector_meta;
  wk_meta_t meta;
  int feature_coord_id;
  R_xlen_t coord_id;
} vertex_filter_t;

void wk_vertex_filter_initialize(int* dirty, void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;
  *dirty = 1;
  vertex_filter->next->initialize(&vertex_filter->next->dirty, vertex_filter->next->handler_data);
}

int wk_vertex_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;

  memcpy(&(vertex_filter->vector_meta), meta, sizeof(wk_vector_meta_t));
  if (meta->geometry_type != WK_POINT) {
    vertex_filter->vector_meta.size = WK_SIZE_UNKNOWN;
  }
  vertex_filter->vector_meta.geometry_type = WK_POINT;

  return vertex_filter->next->vector_start(meta, vertex_filter->next->handler_data);
}

int wk_vertex_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;
  vertex_filter->feature_coord_id = 0;
  return WK_CONTINUE;
}

int wk_vertex_filter_feature_null(void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;
  return vertex_filter->next->null_feature(vertex_filter->next->handler_data);
}

int wk_vertex_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  // theoretically, append vertex count to a vector here
  return WK_CONTINUE;
}

int wk_vertex_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;

  memcpy(&(vertex_filter->meta), meta, sizeof(wk_meta_t));
  vertex_filter->meta.geometry_type = WK_POINT;
  vertex_filter->meta.flags &= ~WK_FLAG_HAS_BOUNDS;
  vertex_filter->meta.size = WK_SIZE_UNKNOWN;

  return WK_CONTINUE;
}

int wk_vertex_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_vertex_filter_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_vertex_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_vertex_filter_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id, void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;
  
  int result;
  HANDLE_OR_RETURN(vertex_filter->next->feature_start(&(vertex_filter->vector_meta), vertex_filter->coord_id, vertex_filter->next->handler_data));
  HANDLE_OR_RETURN(vertex_filter->next->geometry_start(&(vertex_filter->meta), WK_PART_ID_NONE, vertex_filter->next->handler_data));
  HANDLE_OR_RETURN(vertex_filter->next->coord(&(vertex_filter->meta), coord, 0, vertex_filter->next->handler_data));
  HANDLE_OR_RETURN(vertex_filter->next->geometry_end(&(vertex_filter->meta), WK_PART_ID_NONE, vertex_filter->next->handler_data));
  HANDLE_OR_RETURN(vertex_filter->next->feature_end(&(vertex_filter->vector_meta), vertex_filter->coord_id, vertex_filter->next->handler_data));

  vertex_filter->coord_id++;
  vertex_filter->feature_coord_id++;

  return WK_CONTINUE;
}

SEXP wk_vertex_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;
  // theoretically could add the sizes of each feature here
  return vertex_filter->next->vector_end(meta, vertex_filter->next->handler_data);;
}

int wk_vertex_filter_error(const char* message, void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;
  return vertex_filter->next->error(message, vertex_filter->next->handler_data);
}

void wk_vertex_filter_deinitialize(void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;
  vertex_filter->next->deinitialize(vertex_filter->next->handler_data);
}

void wk_vertex_filter_finalize(void* handler_data) {
  vertex_filter_t* vertex_filter = (vertex_filter_t*) handler_data;
  if (vertex_filter != NULL) {
    // finalizer for vertex_filter->next is run by the externalptr finalizer
    // and should not be called here
    free(vertex_filter);
  }
}

SEXP wk_c_vertex_filter_new(SEXP handler_xptr) {
  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_vertex_filter_initialize;
  handler->vector_start = &wk_vertex_filter_vector_start;
  handler->vector_end = &wk_vertex_filter_vector_end;

  handler->feature_start = &wk_vertex_filter_feature_start;
  handler->null_feature = &wk_vertex_filter_feature_null;
  handler->feature_end = &wk_vertex_filter_feature_end;

  handler->geometry_start = &wk_vertex_filter_geometry_start;
  handler->geometry_end = &wk_vertex_filter_geometry_end;

  handler->ring_start = &wk_vertex_filter_ring_start;
  handler->ring_end = &wk_vertex_filter_ring_end;

  handler->coord = &wk_vertex_filter_coord;

  handler->error = &wk_vertex_filter_error;

  handler->deinitialize = &wk_vertex_filter_deinitialize;
  handler->finalizer = &wk_vertex_filter_finalize;

  vertex_filter_t* vertex_filter = (vertex_filter_t*) malloc(sizeof(vertex_filter_t));
  if (vertex_filter == NULL) {
    wk_handler_destroy(handler); // # nocov
    Rf_error("Failed to alloc handler data"); // # nocov
  }

  vertex_filter->next = R_ExternalPtrAddr(handler_xptr);
  if (vertex_filter->next->api_version != 1) {
    Rf_error("Can't run a wk_handler with api_version '%d'", vertex_filter->next->api_version); // # nocov
  }

  vertex_filter->coord_id = 0;
  vertex_filter->feature_coord_id = 0;

  handler->handler_data = vertex_filter;

  // include the external pointer as a tag for this external pointer
  // which guarnatees that it will not be garbage collected until
  // this object is garbage collected
  return wk_handler_create_xptr(handler, handler_xptr, R_NilValue);
}
