
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <memory.h>
#include <stdlib.h>
#include "wk-v1.h"

typedef struct {
  SEXP result;
  // caching the underlying pointers results in a slight speedup
  double* result_ptr[4];
  R_xlen_t result_size;
  R_xlen_t feat_id;
  int has_coord;
  uint32_t flags;
} xy_writer_t;

static inline SEXP xy_writer_alloc_result(R_xlen_t size, int32_t flags) {
  const char* names[] = {"x", "y", "z", "m", ""};
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
  SET_VECTOR_ELT(result, 0, Rf_allocVector(REALSXP, size));
  SET_VECTOR_ELT(result, 1, Rf_allocVector(REALSXP, size));

  if (flags & WK_FLAG_HAS_Z) {
    SET_VECTOR_ELT(result, 2, Rf_allocVector(REALSXP, size));
  } else {
    SET_VECTOR_ELT(result, 2, R_NilValue);
  }

  if (flags & WK_FLAG_HAS_M) {
    SET_VECTOR_ELT(result, 3, Rf_allocVector(REALSXP, size));
  } else {
    SET_VECTOR_ELT(result, 3, R_NilValue);
  }

  UNPROTECT(1);
  return result;
}

static inline SEXP xy_writer_realloc_result(SEXP result, R_xlen_t new_size,
                                            int32_t flags) {
  SEXP new_result = PROTECT(xy_writer_alloc_result(new_size, flags));

  R_xlen_t size_cpy;
  if (Rf_xlength(VECTOR_ELT(result, 0)) < new_size) {
    size_cpy = Rf_xlength(VECTOR_ELT(result, 0));
  } else {
    size_cpy = new_size;
  }

  for (int i = 0; i < 4; i++) {
    if (VECTOR_ELT(result, i) == R_NilValue) {
      continue;
    }

    memcpy(REAL(VECTOR_ELT(new_result, i)), REAL(VECTOR_ELT(result, i)),
           sizeof(double) * size_cpy);
  }

  UNPROTECT(1);
  return new_result;
}

static inline void xy_writer_append_empty(xy_writer_t* writer) {
  if (writer->feat_id >= writer->result_size) {
    SEXP new_result = PROTECT(xy_writer_realloc_result(
        writer->result, writer->result_size * 2 + 1, writer->flags));
    R_ReleaseObject(writer->result);
    writer->result = new_result;
    R_PreserveObject(writer->result);
    UNPROTECT(1);
    writer->result_size = writer->result_size * 2 + 1;
    for (int i = 0; i < 4; i++) {
      if (VECTOR_ELT(writer->result, i) == R_NilValue) {
        writer->result_ptr[i] = NULL;
      } else {
        writer->result_ptr[i] = REAL(VECTOR_ELT(writer->result, i));
      }
    }
  }

  for (int i = 0; i < 4; i++) {
    if (writer->result_ptr[i]) {
      writer->result_ptr[i][writer->feat_id] = R_NaN;
    }
  }
  writer->feat_id++;
}

int xy_writer_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  xy_writer_t* data = (xy_writer_t*)handler_data;

  if (data->result != R_NilValue) {
    Rf_error("Destination vector was already allocated");  // # nocov
  }

  if (meta->flags & WK_FLAG_HAS_Z) {
    data->flags |= WK_FLAG_HAS_Z;
  }

  if (meta->flags & WK_FLAG_HAS_M) {
    data->flags |= WK_FLAG_HAS_M;
  }

  if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
    data->result = PROTECT(xy_writer_alloc_result(1024, data->flags));
    data->result_size = 1024;
  } else {
    data->result = PROTECT(xy_writer_alloc_result(meta->size, data->flags));
    data->result_size = meta->size;
  }

  R_PreserveObject(data->result);
  UNPROTECT(1);

  for (int i = 0; i < 4; i++) {
    if (VECTOR_ELT(data->result, i) == R_NilValue) {
      data->result_ptr[i] = NULL;
    } else {
      data->result_ptr[i] = REAL(VECTOR_ELT(data->result, i));
    }
  }

  data->feat_id = 0;

  return WK_CONTINUE;
}

int xy_writer_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id,
                            void* handler_data) {
  xy_writer_t* data = (xy_writer_t*)handler_data;
  data->has_coord = 0;
  xy_writer_append_empty(data);
  return WK_CONTINUE;
}

int xy_writer_null_feature(void* handler_data) {
  xy_writer_t* data = (xy_writer_t*)handler_data;
  for (int i = 0; i < 4; i++) {
    if (data->result_ptr[i]) {
      data->result_ptr[i][data->feat_id - 1] = NA_REAL;
    }
  }

  return WK_ABORT_FEATURE;
}

int xy_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id,
                             void* handler_data) {
  xy_writer_t* data = (xy_writer_t*)handler_data;

  // EMPTY and any set of features that (could) contain a single point work with this
  // handler! (error otherwise)
  if (meta->size != 0 && meta->geometry_type != WK_POINT &&
      meta->geometry_type != WK_MULTIPOINT &&
      meta->geometry_type != WK_GEOMETRYCOLLECTION) {
    Rf_error("[%ld] Can't convert geometry with type '%d' to coordinate",
             (long)data->feat_id + 1, (int)meta->geometry_type);
  }

  // keep track of zm flags to possibly trim output
  data->flags |= meta->flags;

  // make sure we've allocated the output for ZM dimensions if they were just added
  if (meta->flags & WK_FLAG_HAS_Z && data->result_ptr[2] == NULL) {
    SET_VECTOR_ELT(data->result, 2, Rf_allocVector(REALSXP, data->result_size));
    data->result_ptr[2] = REAL(VECTOR_ELT(data->result, 2));
    for (R_xlen_t i = 0; i < data->feat_id; i++) {
      data->result_ptr[2][i] = NA_REAL;
    }
  }

  if (meta->flags & WK_FLAG_HAS_M && data->result_ptr[3] == NULL) {
    SET_VECTOR_ELT(data->result, 3, Rf_allocVector(REALSXP, data->result_size));
    data->result_ptr[3] = REAL(VECTOR_ELT(data->result, 3));
    for (R_xlen_t i = 0; i < data->feat_id; i++) {
      data->result_ptr[3][i] = NA_REAL;
    }
  }

  return WK_CONTINUE;
}

int xy_writer_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id,
                    void* handler_data) {
  xy_writer_t* data = (xy_writer_t*)handler_data;

  if (data->has_coord) {
    Rf_error("[%ld] Feature contains more than one coordinate.", (long)data->feat_id);
  } else {
    data->has_coord = 1;
  }

  data->result_ptr[0][data->feat_id - 1] = coord[0];
  data->result_ptr[1][data->feat_id - 1] = coord[1];

  if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
    data->result_ptr[2][data->feat_id - 1] = coord[2];
    data->result_ptr[3][data->feat_id - 1] = coord[3];
  } else if (meta->flags & WK_FLAG_HAS_Z) {
    data->result_ptr[2][data->feat_id - 1] = coord[2];
  } else if (meta->flags & WK_FLAG_HAS_M) {
    data->result_ptr[3][data->feat_id - 1] = coord[2];
  }

  return WK_CONTINUE;
}

SEXP xy_writer_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  xy_writer_t* data = (xy_writer_t*)handler_data;

  R_xlen_t final_size = data->feat_id;
  if (final_size != data->result_size) {
    SEXP new_result =
        PROTECT(xy_writer_realloc_result(data->result, final_size, data->flags));
    R_ReleaseObject(data->result);
    data->result = new_result;
    R_PreserveObject(data->result);
    UNPROTECT(1);
  }

  if ((data->flags & WK_FLAG_HAS_Z) && (data->flags & WK_FLAG_HAS_M)) {
    SEXP xy_class = PROTECT(Rf_allocVector(STRSXP, 5));
    SET_STRING_ELT(xy_class, 0, Rf_mkChar("wk_xyzm"));
    SET_STRING_ELT(xy_class, 1, Rf_mkChar("wk_xyz"));
    SET_STRING_ELT(xy_class, 2, Rf_mkChar("wk_xym"));
    SET_STRING_ELT(xy_class, 3, Rf_mkChar("wk_xy"));
    SET_STRING_ELT(xy_class, 4, Rf_mkChar("wk_rcrd"));

    Rf_setAttrib(data->result, R_ClassSymbol, xy_class);
    UNPROTECT(1);
    return data->result;

  } else if (data->flags & WK_FLAG_HAS_Z) {
    const char* xyz_names[] = {"x", "y", "z", ""};
    SEXP xyz = PROTECT(Rf_mkNamed(VECSXP, xyz_names));
    for (int i = 0; i < 3; i++) {
      SET_VECTOR_ELT(xyz, i, VECTOR_ELT(data->result, i));
    }

    SEXP xy_class = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(xy_class, 0, Rf_mkChar("wk_xyz"));
    SET_STRING_ELT(xy_class, 1, Rf_mkChar("wk_xy"));
    SET_STRING_ELT(xy_class, 2, Rf_mkChar("wk_rcrd"));

    Rf_setAttrib(xyz, R_ClassSymbol, xy_class);
    UNPROTECT(2);
    return xyz;

  } else if (data->flags & WK_FLAG_HAS_M) {
    const char* xym_names[] = {"x", "y", "m", ""};
    SEXP xym = PROTECT(Rf_mkNamed(VECSXP, xym_names));
    SET_VECTOR_ELT(xym, 0, VECTOR_ELT(data->result, 0));
    SET_VECTOR_ELT(xym, 1, VECTOR_ELT(data->result, 1));
    SET_VECTOR_ELT(xym, 2, VECTOR_ELT(data->result, 3));

    SEXP xy_class = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(xy_class, 0, Rf_mkChar("wk_xym"));
    SET_STRING_ELT(xy_class, 1, Rf_mkChar("wk_xy"));
    SET_STRING_ELT(xy_class, 2, Rf_mkChar("wk_rcrd"));

    Rf_setAttrib(xym, R_ClassSymbol, xy_class);
    UNPROTECT(2);
    return xym;
  } else {
    const char* xy_names[] = {"x", "y", ""};
    SEXP xy = PROTECT(Rf_mkNamed(VECSXP, xy_names));
    for (int i = 0; i < 2; i++) {
      SET_VECTOR_ELT(xy, i, VECTOR_ELT(data->result, i));
    }

    SEXP xy_class = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(xy_class, 0, Rf_mkChar("wk_xy"));
    SET_STRING_ELT(xy_class, 1, Rf_mkChar("wk_rcrd"));

    Rf_setAttrib(xy, R_ClassSymbol, xy_class);
    UNPROTECT(2);
    return xy;
  }
}

void xy_writer_deinitialize(void* handler_data) {
  xy_writer_t* data = (xy_writer_t*)handler_data;
  if (data->result != R_NilValue) {
    R_ReleaseObject(data->result);
    data->result = R_NilValue;
  }
}

void xy_writer_finalize(void* handler_data) {
  xy_writer_t* data = (xy_writer_t*)handler_data;
  if (data != NULL) {
    free(data);
  }
}

SEXP wk_c_xy_writer_new(void) {
  wk_handler_t* handler = wk_handler_create();

  handler->vector_start = &xy_writer_vector_start;
  handler->feature_start = &xy_writer_feature_start;
  handler->null_feature = &xy_writer_null_feature;
  handler->geometry_start = &xy_writer_geometry_start;
  handler->coord = &xy_writer_coord;
  handler->vector_end = &xy_writer_vector_end;
  handler->deinitialize = &xy_writer_deinitialize;
  handler->finalizer = &xy_writer_finalize;

  xy_writer_t* data = (xy_writer_t*)malloc(sizeof(xy_writer_t));
  if (data == NULL) {
    wk_handler_destroy(handler);               // # nocov
    Rf_error("Failed to alloc handler data");  // # nocov
  }

  data->feat_id = 0;
  data->has_coord = 0;
  data->result = R_NilValue;
  data->flags = 0;

  handler->handler_data = data;

  SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
  return xptr;
}
