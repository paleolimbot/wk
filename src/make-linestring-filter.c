#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "altrep.h"
#include "wk-v1.h"

#define HANDLE_OR_RETURN(expr)                                            \
  result = expr;                                                          \
  if (result == WK_ABORT_FEATURE) {                                       \
    Rf_error("wk_linestring_filter() does not support WK_ABORT_FEATURE"); \
  }                                                                       \
  if (result != WK_CONTINUE) return result

typedef struct {
  wk_handler_t* next;
  R_xlen_t feature_id;
  SEXP feature_id_sexp;
#ifndef HAS_ALTREP
  int* feature_id_spec;
#endif
  R_xlen_t n_feature_id_spec;
  int last_feature_id_spec;
  int is_new_feature;
  R_xlen_t feature_id_out;
  uint32_t coord_id;
  wk_meta_t meta;
  wk_vector_meta_t vector_meta;
} linestring_filter_t;

static inline int wk_linestring_start(linestring_filter_t* linestring_filter) {
  int result;
  linestring_filter->feature_id_out++;
  HANDLE_OR_RETURN(linestring_filter->next->feature_start(
      &(linestring_filter->vector_meta), linestring_filter->feature_id_out,
      linestring_filter->next->handler_data));
  HANDLE_OR_RETURN(
      linestring_filter->next->geometry_start(&(linestring_filter->meta), WK_PART_ID_NONE,
                                              linestring_filter->next->handler_data));
  linestring_filter->coord_id = 0;
  return WK_CONTINUE;
}

static inline int wk_linestring_end(linestring_filter_t* linestring_filter) {
  int result;
  HANDLE_OR_RETURN(
      linestring_filter->next->geometry_end(&(linestring_filter->meta), WK_PART_ID_NONE,
                                            linestring_filter->next->handler_data));
  HANDLE_OR_RETURN(linestring_filter->next->feature_end(
      &(linestring_filter->vector_meta), linestring_filter->feature_id_out,
      linestring_filter->next->handler_data));
  return WK_CONTINUE;
}

void wk_linestring_filter_initialize(int* dirty, void* handler_data) {
  linestring_filter_t* linestring_filter = (linestring_filter_t*)handler_data;
  *dirty = 1;
  linestring_filter->next->initialize(&linestring_filter->next->dirty,
                                      linestring_filter->next->handler_data);
}

int wk_linestring_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  linestring_filter_t* linestring_filter = (linestring_filter_t*)handler_data;

  linestring_filter->feature_id = -1;
  linestring_filter->feature_id_out = -1;
  memcpy(&(linestring_filter->vector_meta), meta, sizeof(wk_vector_meta_t));
  linestring_filter->vector_meta.geometry_type = WK_LINESTRING;
  linestring_filter->vector_meta.size = WK_VECTOR_SIZE_UNKNOWN;
  WK_META_RESET(linestring_filter->meta, WK_LINESTRING);

  return linestring_filter->next->vector_start(&(linestring_filter->vector_meta),
                                               linestring_filter->next->handler_data);
}

int wk_linestring_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id,
                                       void* handler_data) {
  linestring_filter_t* linestring_filter = (linestring_filter_t*)handler_data;

  linestring_filter->feature_id++;
  R_xlen_t spec_i = linestring_filter->feature_id % linestring_filter->n_feature_id_spec;
#ifdef HAS_ALTREP
  int feature_id_spec = INTEGER_ELT(linestring_filter->feature_id_sexp, spec_i);
#else
  int feature_id_spec = linestring_filter->feature_id_spec[spec_i];
#endif
  int feature_id_spec_changed =
      feature_id_spec != linestring_filter->last_feature_id_spec;
  linestring_filter->last_feature_id_spec = feature_id_spec;

  linestring_filter->is_new_feature =
      feature_id_spec_changed || (linestring_filter->feature_id == 0);

  return WK_CONTINUE;
}

int wk_linestring_filter_coord(const wk_meta_t* meta, const double* coord,
                               uint32_t coord_id, void* handler_data) {
  linestring_filter_t* linestring_filter = (linestring_filter_t*)handler_data;
  int result;

  if (linestring_filter->is_new_feature) {
    if (linestring_filter->feature_id_out >= 0) {
      HANDLE_OR_RETURN(wk_linestring_end(linestring_filter));
    }

    linestring_filter->meta.flags = meta->flags;
    linestring_filter->meta.flags &= ~WK_FLAG_HAS_BOUNDS;
    linestring_filter->meta.precision = meta->precision;
    linestring_filter->meta.srid = meta->srid;

    HANDLE_OR_RETURN(wk_linestring_start(linestring_filter));
    linestring_filter->is_new_feature = 0;
  } else {
    // check dimensions againist current meta because handlers make the assumption
    // that all coordinates passed have the same dimension for a single geometry
    int diff_z =
        (linestring_filter->meta.flags & WK_FLAG_HAS_Z) ^ (meta->flags & WK_FLAG_HAS_Z);
    int diff_m =
        (linestring_filter->meta.flags & WK_FLAG_HAS_M) ^ (meta->flags & WK_FLAG_HAS_M);
    int diff_srid = linestring_filter->meta.srid != meta->srid;
    if (diff_z || diff_m || diff_srid) {
      Rf_error(
          "Can't create linestring using geometries with differing dimensions or SRID");
    }
  }

  HANDLE_OR_RETURN(linestring_filter->next->coord(&(linestring_filter->meta), coord,
                                                  linestring_filter->coord_id,
                                                  linestring_filter->next->handler_data));
  linestring_filter->coord_id++;
  return WK_CONTINUE;
}

SEXP wk_linestring_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  linestring_filter_t* linestring_filter = (linestring_filter_t*)handler_data;

  // if there weren't any features we need to start one
  int result = WK_CONTINUE;
  if (linestring_filter->feature_id_out == -1) {
    linestring_filter->meta.size = 0;
    result = wk_linestring_start(linestring_filter);
  }

  if (result != WK_ABORT) {
    wk_linestring_end(linestring_filter);
  }

  return linestring_filter->next->vector_end(&(linestring_filter->vector_meta),
                                             linestring_filter->next->handler_data);
}

int wk_linestring_filter_feature_null(void* handler_data) { return WK_CONTINUE; }

int wk_linestring_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id,
                                     void* handler_data) {
  return WK_CONTINUE;
}

int wk_linestring_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id,
                                        void* handler_data) {
  return WK_CONTINUE;
}

int wk_linestring_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id,
                                      void* handler_data) {
  return WK_CONTINUE;
}

int wk_linestring_filter_ring_start(const wk_meta_t* meta, uint32_t size,
                                    uint32_t ring_id, void* handler_data) {
  return WK_CONTINUE;
}

int wk_linestring_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id,
                                  void* handler_data) {
  return WK_CONTINUE;
}

int wk_linestring_filter_error(const char* message, void* handler_data) {
  linestring_filter_t* linestring_filter = (linestring_filter_t*)handler_data;
  int result;
  HANDLE_OR_RETURN(
      linestring_filter->next->error(message, linestring_filter->next->handler_data));
  return WK_CONTINUE;
}

void wk_linestring_filter_deinitialize(void* handler_data) {
  linestring_filter_t* linestring_filter = (linestring_filter_t*)handler_data;
  linestring_filter->next->deinitialize(linestring_filter->next->handler_data);
}

void wk_linestring_filter_finalize(void* handler_data) {
  linestring_filter_t* linestring_filter = (linestring_filter_t*)handler_data;
  if (linestring_filter != NULL) {
    // finalizer for linestring_filter->next is run by the externalptr finalizer
    // and should not be called here
    free(linestring_filter);
  }
}

SEXP wk_c_linestring_filter_new(SEXP handler_xptr, SEXP feature_id) {
#ifndef HAS_ALTREP
  int* feature_id_spec = INTEGER(feature_id);
#endif

  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_linestring_filter_initialize;
  handler->vector_start = &wk_linestring_filter_vector_start;
  handler->vector_end = &wk_linestring_filter_vector_end;

  handler->feature_start = &wk_linestring_filter_feature_start;
  handler->null_feature = &wk_linestring_filter_feature_null;
  handler->feature_end = &wk_linestring_filter_feature_end;

  handler->geometry_start = &wk_linestring_filter_geometry_start;
  handler->geometry_end = &wk_linestring_filter_geometry_end;

  handler->ring_start = &wk_linestring_filter_ring_start;
  handler->ring_end = &wk_linestring_filter_ring_end;

  handler->coord = &wk_linestring_filter_coord;

  handler->error = &wk_linestring_filter_error;

  handler->deinitialize = &wk_linestring_filter_deinitialize;
  handler->finalizer = &wk_linestring_filter_finalize;

  linestring_filter_t* linestring_filter =
      (linestring_filter_t*)malloc(sizeof(linestring_filter_t));
  if (linestring_filter == NULL) {
    wk_handler_destroy(handler);               // # nocov
    Rf_error("Failed to alloc handler data");  // # nocov
  }

  linestring_filter->next = (wk_handler_t*)R_ExternalPtrAddr(handler_xptr);
  if (linestring_filter->next->api_version != 1) {
    wk_handler_destroy(handler);  // # nocov
    free(linestring_filter);
    Rf_error("Invalid API version in linestring_filter");  // # nocov
  }

  linestring_filter->coord_id = 0;
  linestring_filter->feature_id = -1;
  linestring_filter->feature_id_out = 0;
  linestring_filter->feature_id_sexp = feature_id;
#ifndef HAS_ALTREP
  linestring_filter->feature_id_spec = feature_id_spec;
#endif
  linestring_filter->n_feature_id_spec = Rf_xlength(feature_id);
  linestring_filter->is_new_feature = 0;
  linestring_filter->last_feature_id_spec = NA_INTEGER;

  handler->handler_data = linestring_filter;

  // We need both the external pointer SEXP and the feature_id SEXP
  // to be valid for the lifetime of this object
  return wk_handler_create_xptr(handler, handler_xptr, feature_id);
}
