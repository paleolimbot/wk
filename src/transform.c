#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include "wk-v1.h"

#define MAX_LEVELS 32

typedef struct {
  wk_handler_t* next;
  wk_trans_t* trans;
  wk_meta_t meta[MAX_LEVELS];
  wk_vector_meta_t vector_meta;
  int recursive_level;
  R_xlen_t feature_id;
  double xyzm_in[4];
  double xyzm_out[4];
  double coord_out[4];
} trans_filter_t;

void wk_trans_filter_initialize(int* dirty, void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  *dirty = 1;
  trans_filter->next->initialize(&trans_filter->next->dirty,
                                 trans_filter->next->handler_data);
}

int wk_trans_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;

  memcpy(&(trans_filter->vector_meta), meta, sizeof(wk_vector_meta_t));

  // bounds are no longer valid
  trans_filter->vector_meta.flags &= ~WK_FLAG_HAS_BOUNDS;

  // set the output dimensions NA_INTEGER means "leave alone"
  int dims_maybe_unknown = 0;
  if (trans_filter->trans->use_z == 1) {
    trans_filter->vector_meta.flags |= WK_FLAG_HAS_Z;
  } else if (trans_filter->trans->use_z == 0) {
    trans_filter->vector_meta.flags &= ~WK_FLAG_HAS_Z;
  } else {
    dims_maybe_unknown = 1;
  }

  if (trans_filter->trans->use_m == 1) {
    trans_filter->vector_meta.flags |= WK_FLAG_HAS_M;
  } else if (trans_filter->trans->use_m == 0) {
    trans_filter->vector_meta.flags &= ~WK_FLAG_HAS_M;
  } else {
    dims_maybe_unknown = 1;
  }

  if (!dims_maybe_unknown) {
    trans_filter->vector_meta.flags &= ~WK_FLAG_DIMS_UNKNOWN;
  }

  trans_filter->feature_id = -1;

  return trans_filter->next->vector_start(&(trans_filter->vector_meta),
                                          trans_filter->next->handler_data);
}

SEXP wk_trans_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  trans_filter->trans->vector_end(trans_filter->trans->trans_data);
  return trans_filter->next->vector_end(&(trans_filter->vector_meta),
                                        trans_filter->next->handler_data);
}

int wk_trans_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id,
                                  void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  trans_filter->recursive_level = -1;
  trans_filter->feature_id++;
  return trans_filter->next->feature_start(&(trans_filter->vector_meta), feat_id,
                                           trans_filter->next->handler_data);
}

int wk_trans_filter_feature_null(void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  return trans_filter->next->null_feature(trans_filter->next->handler_data);
}

int wk_trans_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id,
                                void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  return trans_filter->next->feature_end(&(trans_filter->vector_meta), feat_id,
                                         trans_filter->next->handler_data);
}

int wk_trans_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id,
                                   void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;

  trans_filter->recursive_level++;
  if (trans_filter->recursive_level >= MAX_LEVELS) {
    Rf_error("Too many recursive levels for wk_transform_filter()");
  }

  wk_meta_t* new_meta = trans_filter->meta + trans_filter->recursive_level;
  memcpy(new_meta, meta, sizeof(wk_meta_t));
  new_meta->flags &= ~WK_FLAG_HAS_BOUNDS;
  if (trans_filter->trans->use_z == 1) {
    new_meta->flags |= WK_FLAG_HAS_Z;
  } else if (trans_filter->trans->use_z == 0) {
    new_meta->flags &= ~WK_FLAG_HAS_Z;
  }

  if (trans_filter->trans->use_m == 1) {
    new_meta->flags |= WK_FLAG_HAS_M;
  } else if (trans_filter->trans->use_m == 0) {
    new_meta->flags &= ~WK_FLAG_HAS_M;
  }

  return trans_filter->next->geometry_start(new_meta, part_id,
                                            trans_filter->next->handler_data);
}

int wk_trans_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id,
                                 void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  wk_meta_t* new_meta = trans_filter->meta + trans_filter->recursive_level;
  trans_filter->recursive_level--;
  return trans_filter->next->geometry_end(new_meta, part_id,
                                          trans_filter->next->handler_data);
}

int wk_trans_filter_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id,
                               void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  wk_meta_t* new_meta = trans_filter->meta + trans_filter->recursive_level;
  return trans_filter->next->ring_start(new_meta, size, ring_id,
                                        trans_filter->next->handler_data);
}

int wk_trans_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id,
                             void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  wk_meta_t* new_meta = trans_filter->meta + trans_filter->recursive_level;
  return trans_filter->next->ring_end(new_meta, size, ring_id,
                                      trans_filter->next->handler_data);
}

int wk_trans_filter_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id,
                          void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  wk_meta_t* new_meta = trans_filter->meta + trans_filter->recursive_level;

  trans_filter->xyzm_in[0] = coord[0];
  trans_filter->xyzm_in[1] = coord[1];
  if (meta->flags & WK_FLAG_HAS_Z && meta->flags & WK_FLAG_HAS_M) {
    trans_filter->xyzm_in[2] = coord[2];
    trans_filter->xyzm_in[3] = coord[3];
  } else if (meta->flags & WK_FLAG_HAS_Z) {
    trans_filter->xyzm_in[2] = coord[2];
    trans_filter->xyzm_in[3] = R_NaN;
  } else if (meta->flags & WK_FLAG_HAS_M) {
    trans_filter->xyzm_in[2] = R_NaN;
    trans_filter->xyzm_in[3] = coord[2];
  } else {
    trans_filter->xyzm_in[2] = R_NaN;
    trans_filter->xyzm_in[3] = R_NaN;
  }

  int result =
      trans_filter->trans->trans(trans_filter->feature_id, trans_filter->xyzm_in,
                                 trans_filter->xyzm_out, trans_filter->trans->trans_data);

  if (result != WK_CONTINUE) {
    return result;
  }

  trans_filter->coord_out[0] = trans_filter->xyzm_out[0];
  trans_filter->coord_out[1] = trans_filter->xyzm_out[1];
  if (new_meta->flags & WK_FLAG_HAS_Z && new_meta->flags & WK_FLAG_HAS_M) {
    trans_filter->coord_out[2] = trans_filter->xyzm_out[2];
    trans_filter->coord_out[3] = trans_filter->xyzm_out[3];
  } else if (new_meta->flags & WK_FLAG_HAS_Z) {
    trans_filter->coord_out[2] = trans_filter->xyzm_out[2];
  } else if (new_meta->flags & WK_FLAG_HAS_M) {
    trans_filter->coord_out[2] = trans_filter->xyzm_out[3];
  }

  return trans_filter->next->coord(new_meta, trans_filter->coord_out, coord_id,
                                   trans_filter->next->handler_data);
}

int wk_trans_filter_error(const char* message, void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  return trans_filter->next->error(message, trans_filter->next->handler_data);
}

void wk_trans_filter_deinitialize(void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  trans_filter->next->deinitialize(trans_filter->next->handler_data);
}

void wk_trans_filter_finalize(void* handler_data) {
  trans_filter_t* trans_filter = (trans_filter_t*)handler_data;
  if (trans_filter != NULL) {
    // finalizer for trans_filter->next is run by the externalptr finalizer
    // and should not be called here
    free(trans_filter);
  }
}

SEXP wk_c_trans_filter_new(SEXP handler_xptr, SEXP trans_xptr) {
  wk_trans_t* trans = (wk_trans_t*)R_ExternalPtrAddr(trans_xptr);

  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_trans_filter_initialize;
  handler->vector_start = &wk_trans_filter_vector_start;
  handler->vector_end = &wk_trans_filter_vector_end;

  handler->feature_start = &wk_trans_filter_feature_start;
  handler->null_feature = &wk_trans_filter_feature_null;
  handler->feature_end = &wk_trans_filter_feature_end;

  handler->geometry_start = &wk_trans_filter_geometry_start;
  handler->geometry_end = &wk_trans_filter_geometry_end;

  handler->ring_start = &wk_trans_filter_ring_start;
  handler->ring_end = &wk_trans_filter_ring_end;

  handler->coord = &wk_trans_filter_coord;

  handler->error = &wk_trans_filter_error;

  handler->deinitialize = &wk_trans_filter_deinitialize;
  handler->finalizer = &wk_trans_filter_finalize;

  trans_filter_t* trans_filter = (trans_filter_t*)malloc(sizeof(trans_filter_t));
  if (trans_filter == NULL) {
    wk_handler_destroy(handler);               // # nocov
    Rf_error("Failed to alloc handler data");  // # nocov
  }

  trans_filter->next = R_ExternalPtrAddr(handler_xptr);
  if (trans_filter->next->api_version != 1) {
    Rf_error("Can't run a wk_handler with api_version '%d'",
             trans_filter->next->api_version);  // # nocov
  }

  trans_filter->trans = trans;

  handler->handler_data = trans_filter;

  // include the external pointers as tags for this external pointer
  // which guarnatees that they will not be garbage collected until
  // this object is garbage collected
  return wk_handler_create_xptr(handler, handler_xptr, trans_xptr);
}
