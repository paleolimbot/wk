#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

#define HANDLE_OR_RETURN(expr)                                 \
    result = expr;                                             \
    if (result == WK_ABORT_FEATURE) { \
      Rf_error("wk_polygon_filter() does not support WK_ABORT_FEATURE"); \
    } \
    if (result != WK_CONTINUE) return result

typedef struct {
  wk_handler_t* next;
  R_xlen_t feature_id;
  int* feature_id_spec;
  int* ring_id_spec;
  R_xlen_t n_feature_id_spec;
  R_xlen_t n_ring_id_spec;
  int last_feature_id_spec;
  int last_ring_id_spec;
  int is_new_feature;
  int is_new_ring;
  R_xlen_t feature_id_out;
  R_xlen_t ring_id;
  uint32_t coord_id;
  double first_coord[4];
  double last_coord[4];
  int last_coord_size;
  wk_meta_t meta;
  wk_vector_meta_t vector_meta;
} polygon_filter_t;

static inline int wk_polygon_start(polygon_filter_t* polygon_filter) {
  int result;
  polygon_filter->feature_id_out++;
  HANDLE_OR_RETURN(polygon_filter->next->feature_start(&(polygon_filter->vector_meta), polygon_filter->feature_id_out, polygon_filter->next->handler_data));
  HANDLE_OR_RETURN(polygon_filter->next->geometry_start(&(polygon_filter->meta), WK_PART_ID_NONE, polygon_filter->next->handler_data));
  polygon_filter->ring_id = -1;
  return WK_CONTINUE;
}

static inline int wk_ring_start(polygon_filter_t* polygon_filter) {
  int result;
  // keep a copy of the first coordinate so that we can check for a closed loop
  memcpy(polygon_filter->first_coord, polygon_filter->last_coord, 4 * sizeof(double));

  polygon_filter->ring_id++;
  HANDLE_OR_RETURN(polygon_filter->next->ring_start(&(polygon_filter->meta), WK_SIZE_UNKNOWN, polygon_filter->ring_id, polygon_filter->next->handler_data));
  polygon_filter->coord_id = 0;
  return WK_CONTINUE;
}

static inline int wk_ring_end(polygon_filter_t* polygon_filter) {
  int result;
  
  // close the loop if necessary
  for (int i = 0; i < polygon_filter->last_coord_size; i++) {
      if (polygon_filter->last_coord[i] != polygon_filter->first_coord[i]) {
        HANDLE_OR_RETURN(polygon_filter->next->coord(&(polygon_filter->meta), polygon_filter->first_coord, polygon_filter->coord_id, polygon_filter->next->handler_data));
        break;
      }
  }

  HANDLE_OR_RETURN(polygon_filter->next->ring_end(&(polygon_filter->meta), WK_SIZE_UNKNOWN, polygon_filter->ring_id, polygon_filter->next->handler_data));
  return WK_CONTINUE;
}

static inline int wk_polygon_end(polygon_filter_t* polygon_filter) {
  int result;
  HANDLE_OR_RETURN(polygon_filter->next->geometry_end(&(polygon_filter->meta), WK_PART_ID_NONE, polygon_filter->next->handler_data));
  HANDLE_OR_RETURN(polygon_filter->next->feature_end(&(polygon_filter->vector_meta), polygon_filter->feature_id_out, polygon_filter->next->handler_data));
  return WK_CONTINUE;
}

void wk_polygon_filter_initialize(int* dirty, void* handler_data) {
  polygon_filter_t* polygon_filter = (polygon_filter_t*) handler_data;
  *dirty = 1;
  polygon_filter->next->initialize(&polygon_filter->next->dirty, polygon_filter->next->handler_data);
}

int wk_polygon_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  polygon_filter_t* polygon_filter = (polygon_filter_t*) handler_data;

  polygon_filter->feature_id = -1;
  polygon_filter->feature_id_out = -1;
  memcpy(&(polygon_filter->vector_meta), meta, sizeof(wk_vector_meta_t));
  polygon_filter->vector_meta.geometry_type = WK_POLYGON;
  polygon_filter->vector_meta.size = WK_VECTOR_SIZE_UNKNOWN;
  WK_META_RESET(polygon_filter->meta, WK_POLYGON);
  
  return polygon_filter->next->vector_start(&(polygon_filter->vector_meta), polygon_filter->next->handler_data);
}

int wk_polygon_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  polygon_filter_t* polygon_filter = (polygon_filter_t*) handler_data;

  polygon_filter->feature_id++;
  R_xlen_t spec_i = polygon_filter->feature_id % polygon_filter->n_feature_id_spec;

  int feature_id_spec = polygon_filter->feature_id_spec[spec_i];
  int feature_id_spec_changed = feature_id_spec != polygon_filter->last_feature_id_spec;
  polygon_filter->last_feature_id_spec = feature_id_spec;

  spec_i = polygon_filter->feature_id % polygon_filter->n_ring_id_spec;
  int ring_id_spec = polygon_filter->ring_id_spec[spec_i];
  int ring_id_spec_changed = ring_id_spec != polygon_filter->last_ring_id_spec;
  polygon_filter->last_ring_id_spec = ring_id_spec;

  polygon_filter->is_new_feature = feature_id_spec_changed || (polygon_filter->feature_id == 0);
  polygon_filter->is_new_ring = polygon_filter->is_new_feature || ring_id_spec_changed;

  return WK_CONTINUE;
}

int wk_polygon_filter_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id, void* handler_data) {
  polygon_filter_t* polygon_filter = (polygon_filter_t*) handler_data;
  int result;

  // We always need to keep a copy of the last coordinate because we
  // need to check for closed rings and the ring end method gets called
  // at the *next* coordinate where there's a new feature.
  polygon_filter->last_coord_size = 2 + 
    ((meta->flags & WK_FLAG_HAS_Z) != 0) + 
    ((meta->flags & WK_FLAG_HAS_M) != 0);
  memset(polygon_filter->last_coord, 0, 4 * sizeof(double));
  memcpy(polygon_filter->last_coord, coord, polygon_filter->last_coord_size * sizeof(double));

  // maybe need to close the ring before starting a new feature/ring
  if (polygon_filter->is_new_ring && polygon_filter->feature_id > 0) {
    HANDLE_OR_RETURN(wk_ring_end(polygon_filter));
  }

  if (polygon_filter->is_new_feature) {
    if (polygon_filter->feature_id > 0) {
      HANDLE_OR_RETURN(wk_polygon_end(polygon_filter));
    }
    
    polygon_filter->meta.flags = meta->flags;
    polygon_filter->meta.flags &= ~WK_FLAG_HAS_BOUNDS;
    polygon_filter->meta.precision = meta->precision;
    polygon_filter->meta.srid = meta->srid;

    HANDLE_OR_RETURN(wk_polygon_start(polygon_filter));
    polygon_filter->is_new_feature = 0;
  } else {
    // check dimensions against current meta because handlers make the assumption
    // that all coordinates passed have the same dimension for a single geometry
    int diff_z = (polygon_filter->meta.flags & WK_FLAG_HAS_Z) ^ (meta->flags & WK_FLAG_HAS_Z);
    int diff_m = (polygon_filter->meta.flags & WK_FLAG_HAS_M) ^ (meta->flags & WK_FLAG_HAS_M);
    int diff_srid = polygon_filter->meta.srid != meta->srid;
    if (diff_z || diff_m || diff_srid) {
        Rf_error("Can't create polygon using geometries with differing dimensions or SRID");
    }
  }

  if (polygon_filter->is_new_ring) {
    HANDLE_OR_RETURN(wk_ring_start(polygon_filter));
    polygon_filter->is_new_ring = 0;
  }

  HANDLE_OR_RETURN(polygon_filter->next->coord(&(polygon_filter->meta), coord, polygon_filter->coord_id, polygon_filter->next->handler_data));
  polygon_filter->coord_id++;
  return WK_CONTINUE;
}

SEXP wk_polygon_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  polygon_filter_t* polygon_filter = (polygon_filter_t*) handler_data;

  // if there weren't any features we need to start one
  int result = WK_CONTINUE;
  if (polygon_filter->feature_id_out == -1) {
    polygon_filter->meta.size = 0;
    result = wk_polygon_start(polygon_filter);
  }

  if (result != WK_ABORT) {
    if (polygon_filter->ring_id >= 0) {
      result = wk_ring_end(polygon_filter);
    }

    if (result != WK_ABORT) {
      wk_polygon_end(polygon_filter);
    }
  }
  
  return polygon_filter->next->vector_end(&(polygon_filter->vector_meta), polygon_filter->next->handler_data);
}

int wk_polygon_filter_feature_null(void* handler_data) {
  return WK_CONTINUE;
}

int wk_polygon_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_polygon_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_polygon_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_polygon_filter_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_polygon_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_polygon_filter_error(const char* message, void* handler_data) {
  polygon_filter_t* polygon_filter = (polygon_filter_t*) handler_data;
  return polygon_filter->next->error(message, polygon_filter->next->handler_data);
}

void wk_polygon_filter_deinitialize(void* handler_data) {
  polygon_filter_t* polygon_filter = (polygon_filter_t*) handler_data;
  polygon_filter->next->deinitialize(polygon_filter->next->handler_data);
}

void wk_polygon_filter_finalize(void* handler_data) {
  polygon_filter_t* polygon_filter = (polygon_filter_t*) handler_data;
  if (polygon_filter != NULL) {
    free(polygon_filter);
  }
}

SEXP wk_c_polygon_filter_new(SEXP handler_xptr, SEXP feature_id, SEXP ring_id) {
  int* feature_id_spec = INTEGER(feature_id);
  int* ring_id_spec = INTEGER(ring_id);

  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_polygon_filter_initialize;
  handler->vector_start = &wk_polygon_filter_vector_start;
  handler->vector_end = &wk_polygon_filter_vector_end;

  handler->feature_start = &wk_polygon_filter_feature_start;
  handler->null_feature = &wk_polygon_filter_feature_null;
  handler->feature_end = &wk_polygon_filter_feature_end;

  handler->geometry_start = &wk_polygon_filter_geometry_start;
  handler->geometry_end = &wk_polygon_filter_geometry_end;

  handler->ring_start = &wk_polygon_filter_ring_start;
  handler->ring_end = &wk_polygon_filter_ring_end;

  handler->coord = &wk_polygon_filter_coord;

  handler->error = &wk_polygon_filter_error;

  handler->deinitialize = &wk_polygon_filter_deinitialize;
  handler->finalizer = &wk_polygon_filter_finalize;

  polygon_filter_t* polygon_filter = (polygon_filter_t*) malloc(sizeof(polygon_filter_t));
  if (polygon_filter == NULL) {
    wk_handler_destroy(handler); // # nocov
    Rf_error("Failed to alloc handler data"); // # nocov
  }

  polygon_filter->next = R_ExternalPtrAddr(handler_xptr);
  if (polygon_filter->next->api_version != 1) {
    wk_handler_destroy(handler); // # nocov
    free(polygon_filter); // # nocov
    Rf_error("Can't run a wk_handler with api_version '%d'", polygon_filter->next->api_version); // # nocov
  }

  polygon_filter->coord_id = 0;
  polygon_filter->ring_id = 0;
  polygon_filter->feature_id = -1;
  polygon_filter->feature_id_out = 0;
  polygon_filter->feature_id_spec = feature_id_spec;
  polygon_filter->ring_id_spec = ring_id_spec;
  polygon_filter->n_feature_id_spec = Rf_xlength(feature_id);
  polygon_filter->n_ring_id_spec = Rf_xlength(ring_id);
  polygon_filter->is_new_feature = 0;
  polygon_filter->is_new_ring = 0;
  polygon_filter->last_feature_id_spec = NA_INTEGER;
  polygon_filter->last_ring_id_spec = NA_INTEGER;

  handler->handler_data = polygon_filter;

  // We need the external pointer SEXP, the feature_id SEXP,
  // and the ring_id SEEXP to be valid for the lifetime of this object
  SEXP id_spec = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(id_spec, 0, feature_id);
  SET_VECTOR_ELT(id_spec, 1, ring_id);
  SEXP filter_xptr = PROTECT(wk_handler_create_xptr(handler, handler_xptr, id_spec));
  UNPROTECT(2);
  return filter_xptr;
}
