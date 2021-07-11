#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

typedef struct {
  wk_handler_t* next;
  int recursion_depth;
  int recursion_depth_out;
  int recursion_depth_threshold;
  wk_vector_meta_t vector_meta;
  int feature_id;
  int feature_id_out;
  int add_details;
  SEXP details;
  int* details_ptr[1];
  R_xlen_t details_size;
} flatten_filter_t;

#define HANDLE_OR_RETURN(expr)                                 \
    result = expr;                                             \
    if (result == WK_ABORT_FEATURE) { \
      Rf_error("wk_flatten_filter() does not support WK_ABORT_FEATURE"); \
    } \
    if (result != WK_CONTINUE) return result

#define META_IS_COLLECTION(meta) \
  ((meta->geometry_type == WK_GEOMETRY) || \
    (meta->geometry_type == WK_MULTIPOINT) || \
    (meta->geometry_type == WK_MULTILINESTRING) || \
    (meta->geometry_type == WK_MULTIPOLYGON) || \
    (meta->geometry_type == WK_GEOMETRYCOLLECTION))

static inline int wk_flatten_filter_keep(flatten_filter_t* flatten_filter, const wk_meta_t* meta) {
  int is_collection = META_IS_COLLECTION(meta);
  int recursion_level_above_threshold = flatten_filter->recursion_depth >= flatten_filter->recursion_depth_threshold;
  return !is_collection || recursion_level_above_threshold;
}

static inline void wk_flatten_filter_init_details(flatten_filter_t* flatten_filter, R_xlen_t initial_size) {
  if (!flatten_filter->add_details) {
    return;
  }

  if (initial_size == WK_VECTOR_SIZE_UNKNOWN) {
    initial_size = 1024;
  }

  flatten_filter->feature_id = -1;

  if (flatten_filter->details != R_NilValue) {
    R_ReleaseObject(flatten_filter->details); // # nocov
  }

  const char* names[] = {"feature_id", ""};
  flatten_filter->details = PROTECT(Rf_mkNamed(VECSXP, names));
  R_PreserveObject(flatten_filter->details);
  UNPROTECT(1);

  flatten_filter->details_size = initial_size;
  for (int i = 0; i < 1; i++) {
    SEXP item = PROTECT(Rf_allocVector(INTSXP, flatten_filter->details_size));
    SET_VECTOR_ELT(flatten_filter->details, i, item);
    flatten_filter->details_ptr[i] = INTEGER(item);
    UNPROTECT(1);
  }
}

static inline void wk_flatten_filter_append_details(flatten_filter_t* flatten_filter) {
  if (flatten_filter->details == R_NilValue) {
    return;
  }

  if (flatten_filter->feature_id_out >= flatten_filter->details_size) {
    R_xlen_t new_size = flatten_filter->details_size * 2 + 1;
    for (int i = 0; i < 1; i++) {
      SEXP new_item = PROTECT(Rf_allocVector(INTSXP, new_size));
      memcpy(INTEGER(new_item), INTEGER(VECTOR_ELT(flatten_filter->details, i)), flatten_filter->details_size * sizeof(int));
      SET_VECTOR_ELT(flatten_filter->details, i, new_item);
      flatten_filter->details_ptr[i] = INTEGER(new_item);
      UNPROTECT(1);
    }

    flatten_filter->details_size = new_size;
  }

  flatten_filter->details_ptr[0][flatten_filter->feature_id_out] = flatten_filter->feature_id + 1;
}

static inline void wk_flatten_filter_finalize_details(flatten_filter_t* flatten_filter) {
  if (flatten_filter->details == R_NilValue) {
    return;
  }

  flatten_filter->feature_id_out++;

  if (flatten_filter->feature_id_out != flatten_filter->details_size) {
    for (int i = 0; i < 1; i++) {
      SEXP new_item = PROTECT(Rf_allocVector(INTSXP, flatten_filter->feature_id_out));
      memcpy(INTEGER(new_item), INTEGER(VECTOR_ELT(flatten_filter->details, i)), flatten_filter->feature_id_out * sizeof(int));
      SET_VECTOR_ELT(flatten_filter->details, i, new_item);
      UNPROTECT(1);
    }

    flatten_filter->details_size = flatten_filter->feature_id_out;
  }
}

void wk_flatten_filter_initialize(int* dirty, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  *dirty = 1;
  flatten_filter->next->initialize(&flatten_filter->next->dirty, flatten_filter->next->handler_data);
}

int wk_flatten_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;

  flatten_filter->feature_id_out = -1;
  flatten_filter->recursion_depth_out = 0;

  memcpy(&(flatten_filter->vector_meta), meta, sizeof(wk_vector_meta_t));
  if (flatten_filter->recursion_depth_threshold > 0) {
    if (META_IS_COLLECTION(meta)) {
      flatten_filter->vector_meta.size = WK_VECTOR_SIZE_UNKNOWN;
    }

    if (meta->geometry_type == WK_MULTIPOINT) {
      flatten_filter->vector_meta.geometry_type = WK_POINT;
    } else if (meta->geometry_type == WK_MULTILINESTRING) {
      flatten_filter->vector_meta.geometry_type = WK_LINESTRING;
    } else if (meta->geometry_type == WK_MULTIPOLYGON) {
      flatten_filter->vector_meta.geometry_type = WK_POLYGON;
    } else if (meta->geometry_type == WK_GEOMETRYCOLLECTION) {
      flatten_filter->vector_meta.geometry_type = WK_GEOMETRY;
    }
  }

  wk_flatten_filter_init_details(flatten_filter, flatten_filter->vector_meta.size);

  return flatten_filter->next->vector_start(&(flatten_filter->vector_meta), flatten_filter->next->handler_data);
}

SEXP wk_flatten_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  SEXP result = PROTECT(flatten_filter->next->vector_end(&(flatten_filter->vector_meta), flatten_filter->next->handler_data));
  if (result != R_NilValue) {
    wk_flatten_filter_finalize_details(flatten_filter);
    Rf_setAttrib(result, Rf_install("wk_details"), flatten_filter->details);
  }
  UNPROTECT(1);
  return result;
}

int wk_flatten_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  flatten_filter->feature_id++;
  flatten_filter->recursion_depth = 0;
  return WK_CONTINUE;
}

int wk_flatten_filter_feature_null(void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  int result;
  
  flatten_filter->feature_id_out++;
  wk_flatten_filter_append_details(flatten_filter);

  HANDLE_OR_RETURN(flatten_filter->next->feature_start(&(flatten_filter->vector_meta), flatten_filter->feature_id_out, flatten_filter->next->handler_data));
  result = flatten_filter->next->null_feature(flatten_filter->next->handler_data);
  if (result != WK_CONTINUE) {
    return result;
  }
  return flatten_filter->next->feature_end(&(flatten_filter->vector_meta), flatten_filter->feature_id_out, flatten_filter->next->handler_data);
}

int wk_flatten_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_flatten_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  int result;

  int keep = wk_flatten_filter_keep(flatten_filter, meta);
  flatten_filter->recursion_depth++;
  flatten_filter->recursion_depth_out += keep;

  if (keep) {
    uint32_t part_id_out;
    if (flatten_filter->recursion_depth_out > 1) {
      part_id_out = part_id;
    } else {
      part_id_out = WK_PART_ID_NONE;
      flatten_filter->feature_id_out++;
      wk_flatten_filter_append_details(flatten_filter);
      HANDLE_OR_RETURN(flatten_filter->next->feature_start(&(flatten_filter->vector_meta), flatten_filter->feature_id_out, flatten_filter->next->handler_data));
    }
    
    HANDLE_OR_RETURN(flatten_filter->next->geometry_start(meta, part_id_out, flatten_filter->next->handler_data));
  }

  return WK_CONTINUE;
}

int wk_flatten_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  int result;

  flatten_filter->recursion_depth--;
  int keep = wk_flatten_filter_keep(flatten_filter, meta);
  flatten_filter->recursion_depth_out -= keep;

  if (keep) {
    uint32_t part_id_out = flatten_filter->recursion_depth_out > 0 ? part_id : WK_PART_ID_NONE;
    HANDLE_OR_RETURN(flatten_filter->next->geometry_end(meta, part_id_out, flatten_filter->next->handler_data));

    if (flatten_filter->recursion_depth_out == 0) {
      HANDLE_OR_RETURN(flatten_filter->next->feature_end(&(flatten_filter->vector_meta), flatten_filter->feature_id_out, flatten_filter->next->handler_data));
    }
  }

  return WK_CONTINUE;
}

int wk_flatten_filter_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->ring_start(meta, size, ring_id, flatten_filter->next->handler_data);
}

int wk_flatten_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->ring_end(meta, size, ring_id, flatten_filter->next->handler_data);
}

int wk_flatten_filter_coord(const wk_meta_t* meta, const double* coord, uint32_t feature_id_out, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->coord(meta, coord, feature_id_out, flatten_filter->next->handler_data);
}

int wk_flatten_filter_error(const char* message, void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  return flatten_filter->next->error(message, flatten_filter->next->handler_data);
}

void wk_flatten_filter_deinitialize(void* handler_data) {
  flatten_filter_t* flatten_filter = (flatten_filter_t*) handler_data;
  if (flatten_filter->details != R_NilValue) {
    R_ReleaseObject(flatten_filter->details);
    flatten_filter->details = R_NilValue;
  }
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

SEXP wk_c_flatten_filter_new(SEXP handler_xptr, SEXP max_depth, SEXP add_details) {
  int max_depth_int = INTEGER(max_depth)[0];
  int add_details_int = LOGICAL(add_details)[0];

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
  flatten_filter->add_details = add_details_int;
  flatten_filter->recursion_depth_threshold = max_depth_int;
  flatten_filter->recursion_depth = 0;
  flatten_filter->recursion_depth_out = 0;
  flatten_filter->details = R_NilValue;
  flatten_filter->details_size = 0;
  flatten_filter->feature_id = 0;
  flatten_filter->feature_id_out = 0;

  handler->handler_data = flatten_filter;

  // include the external pointer as a tag for this external pointer
  // which guarnatees that it will not be garbage collected until
  // this object is garbage collected
  return wk_handler_create_xptr(handler, handler_xptr, R_NilValue);
}

