

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include "altrep.h"
#include "wk-v1.h"

#define REAL_NA(val) (ISNA(val) || ISNAN(val))

#define HANDLE_OR_RETURN(expr) \
  result = expr;               \
  if (result != WK_CONTINUE) return result

#define HANDLE_CONTINUE_OR_BREAK(expr) \
  result = expr;                       \
  if (result == WK_ABORT_FEATURE)      \
    continue;                          \
  else if (result == WK_ABORT)         \
  break

int wk_crc_handle_single(wk_handler_t* handler, const wk_meta_t* meta, double x, double y,
                         double r, double segs_per_circle) {
  int result;
  double coord[4];
  double angle;

  for (int i = 0; i < segs_per_circle; i++) {
    angle = i / segs_per_circle * PI * 2.0;
    coord[0] = x + r * cos(angle);
    coord[1] = y + r * sin(angle);
    HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
  }

  // re-export the first coordinate (i = 0) identically
  // to ensure that the loops are closed with no floating-point error
  angle = 0 / segs_per_circle * PI * 2.0;
  coord[0] = x + r * cos(angle);
  coord[1] = y + r * sin(angle);
  HANDLE_OR_RETURN(handler->coord(meta, coord, segs_per_circle, handler->handler_data));

  return WK_CONTINUE;
}

SEXP wk_read_crc(SEXP data_coords, wk_handler_t* handler) {
  SEXP data = VECTOR_ELT(data_coords, 0);
  int* segs_per_circle = INTEGER(VECTOR_ELT(data_coords, 1));
  int segs_per_circle_len = Rf_length(VECTOR_ELT(data_coords, 1));

  if (!Rf_inherits(data, "wk_crc")) {
    Rf_error("Object does not inherit from 'wk_crc'");
  }

  R_xlen_t n_features = Rf_xlength(VECTOR_ELT(data, 0));
  double* data_ptr[3];
  R_xlen_t data_ptr_i = 0;

#ifdef HAS_ALTREP
  SEXP altrep_buffer = PROTECT(Rf_allocVector(REALSXP, ALTREP_CHUNK_SIZE * 4));
  for (int j = 0; j < 3; j++) {
    data_ptr[j] = REAL(altrep_buffer) + (ALTREP_CHUNK_SIZE * j);
  }
#else
  for (int j = 0; j < 3; j++) {
    data_ptr[j] = REAL(VECTOR_ELT(data, j));
  }
#endif

  wk_vector_meta_t vector_meta;
  WK_VECTOR_META_RESET(vector_meta, WK_POLYGON);
  vector_meta.size = n_features;

  if (handler->vector_start(&vector_meta, handler->handler_data) == WK_CONTINUE) {
    int result, n_segs;
    double cx, cy, radius;
    wk_meta_t meta;
    WK_META_RESET(meta, WK_POLYGON);
    meta.flags = vector_meta.flags | WK_FLAG_HAS_BOUNDS;

    for (R_xlen_t i = 0; i < n_features; i++) {
      if (((i + 1) % 1000) == 0) R_CheckUserInterrupt();

      HANDLE_CONTINUE_OR_BREAK(
          handler->feature_start(&vector_meta, i, handler->handler_data));

#ifdef HAS_ALTREP
      data_ptr_i = i % ALTREP_CHUNK_SIZE;
      if (data_ptr_i == 0) {
        for (int j = 0; j < 3; j++) {
          REAL_GET_REGION(VECTOR_ELT(data, j), i, ALTREP_CHUNK_SIZE, data_ptr[j]);
        }
      }
#else
      data_ptr_i = i;
#endif

      cx = data_ptr[0][i];
      cy = data_ptr[1][i];
      radius = data_ptr[2][i];
      n_segs = segs_per_circle[i % segs_per_circle_len];

      int circle_empty = REAL_NA(cx) || REAL_NA(cy) || REAL_NA(radius);

      if (circle_empty) {
        meta.size = 0;
      } else {
        meta.size = 1;
      }

      meta.bounds_min[0] = cx - radius;
      meta.bounds_min[1] = cy - radius;
      meta.bounds_max[0] = cx + radius;
      meta.bounds_max[1] = cy + radius;

      HANDLE_CONTINUE_OR_BREAK(
          handler->geometry_start(&meta, WK_PART_ID_NONE, handler->handler_data));
      if (!circle_empty) {
        HANDLE_CONTINUE_OR_BREAK(
            handler->ring_start(&meta, n_segs + 1, 0, handler->handler_data));
        HANDLE_CONTINUE_OR_BREAK(
            wk_crc_handle_single(handler, &meta, cx, cy, radius, n_segs));
        HANDLE_CONTINUE_OR_BREAK(
            handler->ring_end(&meta, n_segs + 1, 0, handler->handler_data));
      }
      HANDLE_CONTINUE_OR_BREAK(
          handler->geometry_end(&meta, WK_PART_ID_NONE, handler->handler_data));

      if (handler->feature_end(&vector_meta, i, handler->handler_data) == WK_ABORT) {
        break;
      }
    }
  }

#ifdef HAS_ALTREP
  UNPROTECT(1);
#endif

  SEXP result = PROTECT(handler->vector_end(&vector_meta, handler->handler_data));
  UNPROTECT(1);
  return result;
}

SEXP wk_c_read_crc(SEXP data, SEXP handler_xptr, SEXP n_segs) {
  SEXP data_coords = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(data_coords, 0, data);
  SET_VECTOR_ELT(data_coords, 1, n_segs);
  SEXP result = PROTECT(wk_handler_run_xptr(&wk_read_crc, data_coords, handler_xptr));
  UNPROTECT(2);
  return result;
}
