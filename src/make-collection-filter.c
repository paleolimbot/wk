#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include "altrep.h"

#define HANDLE_OR_RETURN(expr)                                 \
    result = expr;                                             \
    if (result == WK_ABORT_FEATURE) { \
      Rf_error("wk_collection_filter() does not support WK_ABORT_FEATURE"); \
    } \
    if (result != WK_CONTINUE) return result

typedef struct {
  wk_handler_t* next;
  int geometry_type_out;
  R_xlen_t feature_id;
  SEXP feature_id_sexp;
#ifndef HAS_ALTREP
  int* feature_id_spec;
#endif
  R_xlen_t n_feature_id_spec;
  int last_feature_id_spec;
  int is_new_feature;
  R_xlen_t feature_id_out;
  uint32_t part_id;
  wk_meta_t meta;
  wk_vector_meta_t vector_meta;
} collection_filter_t;

static inline int wk_collection_start(collection_filter_t* collection_filter) {
  int result;
  collection_filter->feature_id_out++;
  HANDLE_OR_RETURN(collection_filter->next->feature_start(&(collection_filter->vector_meta), collection_filter->feature_id_out, collection_filter->next->handler_data));
  HANDLE_OR_RETURN(collection_filter->next->geometry_start(&(collection_filter->meta), WK_PART_ID_NONE, collection_filter->next->handler_data));
  collection_filter->part_id = 0;
  return WK_CONTINUE;
}

static inline int wk_collection_end(collection_filter_t* collection_filter) {
  int result;
  HANDLE_OR_RETURN(collection_filter->next->geometry_end(&(collection_filter->meta), WK_PART_ID_NONE, collection_filter->next->handler_data));
  HANDLE_OR_RETURN(collection_filter->next->feature_end(&(collection_filter->vector_meta), collection_filter->feature_id_out, collection_filter->next->handler_data));
  return WK_CONTINUE;
}

void wk_collection_filter_initialize(int* dirty, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;
  *dirty = 1;
  collection_filter->next->initialize(&collection_filter->next->dirty, collection_filter->next->handler_data);
}

int wk_collection_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;

  collection_filter->feature_id = -1;
  collection_filter->feature_id_out = -1;
  memcpy(&(collection_filter->vector_meta), meta, sizeof(wk_vector_meta_t));
  collection_filter->vector_meta.geometry_type = collection_filter->geometry_type_out;
  collection_filter->vector_meta.size = WK_VECTOR_SIZE_UNKNOWN;
  WK_META_RESET(collection_filter->meta, collection_filter->geometry_type_out);

  return collection_filter->next->vector_start(&(collection_filter->vector_meta), collection_filter->next->handler_data);
}

SEXP wk_collection_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;

  // if there weren't any features we need to start one
  int result = WK_CONTINUE;
  if (collection_filter->feature_id_out == -1) {
    collection_filter->meta.size = 0;
    result = wk_collection_start(collection_filter);
  }

  if (result != WK_ABORT) {
    wk_collection_end(collection_filter);
  }

  return collection_filter->next->vector_end(&(collection_filter->vector_meta), collection_filter->next->handler_data);
}

int wk_collection_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;

  collection_filter->feature_id++;
  R_xlen_t spec_i = collection_filter->feature_id % collection_filter->n_feature_id_spec;
#ifdef HAS_ALTREP
  int feature_id_spec = INTEGER_ELT(collection_filter->feature_id_sexp, spec_i);
#else
  int feature_id_spec = collection_filter->feature_id_spec[spec_i];
#endif
  int feature_id_spec_changed = feature_id_spec != collection_filter->last_feature_id_spec;
  collection_filter->last_feature_id_spec = feature_id_spec;

  collection_filter->is_new_feature = feature_id_spec_changed || (collection_filter->feature_id == 0);

  return WK_CONTINUE;
}

int wk_collection_filter_feature_null(void* handler_data) {
  return WK_CONTINUE;
}

int wk_collection_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_collection_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;
  int result;

  // ensure collection_filter->part_id == 0 for is_new_feature & part_id == WK_PART_ID_NONE
  int inc_part_id = !collection_filter->is_new_feature && part_id == WK_PART_ID_NONE;

  if (collection_filter->is_new_feature) {
    if (collection_filter->feature_id_out >= 0) {
      HANDLE_OR_RETURN(wk_collection_end(collection_filter));
    }

    collection_filter->meta.flags = meta->flags;
    collection_filter->meta.flags &= ~WK_FLAG_HAS_BOUNDS;
    collection_filter->meta.precision = meta->precision;
    collection_filter->meta.srid = meta->srid;

    HANDLE_OR_RETURN(wk_collection_start(collection_filter));
    collection_filter->is_new_feature = 0;
  }

  if (part_id == WK_PART_ID_NONE) {
    part_id = collection_filter->part_id;
    collection_filter->part_id += inc_part_id;
  }

  return collection_filter->next->geometry_start(meta, part_id, collection_filter->next->handler_data);
}

int wk_collection_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;

  if (part_id == WK_PART_ID_NONE) {
    part_id = collection_filter->part_id;
  }

  int result;
  HANDLE_OR_RETURN(collection_filter->next->geometry_end(meta, part_id, collection_filter->next->handler_data));
  return WK_CONTINUE;
}

int wk_collection_filter_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;
  int result;
  HANDLE_OR_RETURN(collection_filter->next->ring_start(meta, size, ring_id, collection_filter->next->handler_data));
  return WK_CONTINUE;
}

int wk_collection_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;
  int result;
  HANDLE_OR_RETURN(collection_filter->next->ring_end(meta, size, ring_id, collection_filter->next->handler_data));
  return WK_CONTINUE;
}

int wk_collection_filter_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;
  int result;
  HANDLE_OR_RETURN(collection_filter->next->coord(meta, coord, coord_id, collection_filter->next->handler_data));
  return WK_CONTINUE;
}

int wk_collection_filter_error(const char* message, void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;
  int result;
  HANDLE_OR_RETURN(collection_filter->next->error(message, collection_filter->next->handler_data));
  return WK_CONTINUE;
}

void wk_collection_filter_deinitialize(void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;
  collection_filter->next->deinitialize(collection_filter->next->handler_data);
}

void wk_collection_filter_finalize(void* handler_data) {
  collection_filter_t* collection_filter = (collection_filter_t*) handler_data;
  if (collection_filter != NULL) {
    // finalizer for collection_filter->next is run by the externalptr finalizer
    // and should not be called here
    free(collection_filter);
  }
}

SEXP wk_c_collection_filter_new(SEXP handler_xptr, SEXP geometry_type, SEXP feature_id) {
#ifndef HAS_ALTREP
  int* feature_id_spec = INTEGER(feature_id);
#endif
  int geometry_type_int = INTEGER(geometry_type)[0];

  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_collection_filter_initialize;
  handler->vector_start = &wk_collection_filter_vector_start;
  handler->vector_end = &wk_collection_filter_vector_end;

  handler->feature_start = &wk_collection_filter_feature_start;
  handler->null_feature = &wk_collection_filter_feature_null;
  handler->feature_end = &wk_collection_filter_feature_end;

  handler->geometry_start = &wk_collection_filter_geometry_start;
  handler->geometry_end = &wk_collection_filter_geometry_end;

  handler->ring_start = &wk_collection_filter_ring_start;
  handler->ring_end = &wk_collection_filter_ring_end;

  handler->coord = &wk_collection_filter_coord;

  handler->error = &wk_collection_filter_error;

  handler->deinitialize = &wk_collection_filter_deinitialize;
  handler->finalizer = &wk_collection_filter_finalize;

  collection_filter_t* collection_filter = (collection_filter_t*) malloc(sizeof(collection_filter_t));
  if (collection_filter == NULL) {
    wk_handler_destroy(handler); // # nocov
    Rf_error("Failed to alloc handler data"); // # nocov
  }

  collection_filter->next = (wk_handler_t*) R_ExternalPtrAddr(handler_xptr);
  if (collection_filter->next->api_version != 1) {
    wk_handler_destroy(handler); // # nocov
    free(collection_filter); // # nocov
    Rf_error("Invalid API version in collection_filter"); // # nocov
  }

  collection_filter->geometry_type_out = geometry_type_int;
  collection_filter->part_id = 0;
  collection_filter->feature_id = -1;
  collection_filter->feature_id_out = 0;
  collection_filter->feature_id_sexp = feature_id;
#ifndef HAS_ALTREP
  collection_filter->feature_id_spec = feature_id_spec;
#endif
  collection_filter->n_feature_id_spec = Rf_xlength(feature_id);
  collection_filter->is_new_feature = 0;
  collection_filter->last_feature_id_spec = NA_INTEGER;

  handler->handler_data = collection_filter;

  // We need both the external pointer SEXP and the feature_id SEXP
  // to be valid for the lifetime of this object
  return wk_handler_create_xptr(handler, handler_xptr, feature_id);
}
