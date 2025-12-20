
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <memory.h>
#include "wk-v1.h"

#define HANDLE_OR_RETURN(expr) \
  result = expr;               \
  if (result != WK_CONTINUE) return result

#define HANDLE_CONTINUE_OR_BREAK(expr) \
  result = expr;                       \
  if (result == WK_ABORT_FEATURE)      \
    continue;                          \
  else if (result == WK_ABORT)         \
  break

int wk_sfc_read_sfg(SEXP x, wk_handler_t* handler, uint32_t part_id, double precision);
int wk_sfc_read_point(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id);
int wk_sfc_read_linestring(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                           uint32_t part_id);
int wk_sfc_read_polygon(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id);
int wk_sfc_read_multipoint(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                           uint32_t part_id);
int wk_sfc_read_multilinestring(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                                uint32_t part_id);
int wk_sfc_read_multipolygon(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                             uint32_t part_id);
int wk_sfc_read_geometrycollection(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                                   uint32_t part_id);
void wk_update_meta_from_sfg(SEXP x, wk_meta_t* meta);
void wk_update_vector_meta_from_sfc(SEXP x, wk_vector_meta_t* vector_meta);
double wk_sfc_precision(SEXP x);

SEXP wk_c_read_sfc_impl(SEXP data, wk_handler_t* handler) {
  R_xlen_t n_features = Rf_xlength(data);

  wk_vector_meta_t vector_meta;
  WK_VECTOR_META_RESET(vector_meta, WK_GEOMETRY);
  vector_meta.size = n_features;
  wk_update_vector_meta_from_sfc(data, &vector_meta);
  double precision = wk_sfc_precision(data);

  if (handler->vector_start(&vector_meta, handler->handler_data) != WK_ABORT) {
    int result;
    SEXP item;
    for (R_xlen_t i = 0; i < n_features; i++) {
      if (((i + 1) % 1000) == 0) R_CheckUserInterrupt();

      HANDLE_CONTINUE_OR_BREAK(
          handler->feature_start(&vector_meta, i, handler->handler_data));

      item = VECTOR_ELT(data, i);
      if (item == R_NilValue) {
        HANDLE_CONTINUE_OR_BREAK(handler->null_feature(handler->handler_data));
      } else {
        HANDLE_CONTINUE_OR_BREAK(
            wk_sfc_read_sfg(item, handler, WK_PART_ID_NONE, precision));
      }

      if (handler->feature_end(&vector_meta, i, handler->handler_data) == WK_ABORT) {
        break;
      }
    }
  }

  return handler->vector_end(&vector_meta, handler->handler_data);
}

SEXP wk_c_read_sfc(SEXP data, SEXP handler_xptr) {
  return wk_handler_run_xptr(&wk_c_read_sfc_impl, data, handler_xptr);
}

int wk_sfc_read_sfg(SEXP x, wk_handler_t* handler, uint32_t part_id, double precision) {
  wk_meta_t meta;
  WK_META_RESET(meta, WK_GEOMETRY);
  wk_update_meta_from_sfg(x, &meta);
  meta.precision = precision;

  if (Rf_inherits(x, "POINT")) {
    return wk_sfc_read_point(x, handler, &meta, part_id);
  } else if (Rf_inherits(x, "LINESTRING")) {
    return wk_sfc_read_linestring(x, handler, &meta, part_id);
  } else if (Rf_inherits(x, "POLYGON")) {
    return wk_sfc_read_polygon(x, handler, &meta, part_id);
  } else if (Rf_inherits(x, "MULTIPOINT")) {
    return wk_sfc_read_multipoint(x, handler, &meta, part_id);
  } else if (Rf_inherits(x, "MULTILINESTRING")) {
    return wk_sfc_read_multilinestring(x, handler, &meta, part_id);
  } else if (Rf_inherits(x, "MULTIPOLYGON")) {
    return wk_sfc_read_multipolygon(x, handler, &meta, part_id);
  } else if (Rf_inherits(x, "GEOMETRYCOLLECTION")) {
    return wk_sfc_read_geometrycollection(x, handler, &meta, part_id);
  } else if (Rf_inherits(x, "sfg")) {
    Rf_error("Unsupported sfg type");
  } else {
    Rf_error("Element of sfc list must inherit from 'sfg'");
  }

  // should never be reached
  return WK_ABORT;  // # nocov
}

int wk_sfc_read_point(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id) {
  int result;
  meta->geometry_type = WK_POINT;
  meta->size = 0;

  double* values = REAL(x);
  int coord_size = Rf_length(x);
  for (int i = 0; i < coord_size; i++) {
    if (!ISNA(values[i]) && !ISNAN(values[i])) {
      meta->size = 1;
      break;
    }
  }

  HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));

  if (meta->size) {
    double coord[4];
    memcpy(coord, REAL(x), sizeof(double) * coord_size);
    HANDLE_OR_RETURN(handler->coord(meta, coord, 0, handler->handler_data));
  }

  HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));
  return WK_CONTINUE;
}

int wk_sfc_read_linestring(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                           uint32_t part_id) {
  int result;
  meta->geometry_type = WK_LINESTRING;
  meta->size = Rf_nrows(x);
  int coord_size = Rf_ncols(x);

  HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));

  double coord[4];
  double* coords = REAL(x);
  for (uint32_t i = 0; i < meta->size; i++) {
    for (int j = 0; j < coord_size; j++) {
      coord[j] = coords[j * meta->size + i];
    }
    HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
  }

  HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));
  return WK_CONTINUE;
}

int wk_sfc_read_polygon(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                        uint32_t part_id) {
  int result;
  meta->geometry_type = WK_POLYGON;
  meta->size = Rf_xlength(x);

  HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));

  SEXP ring;
  for (uint32_t ring_id = 0; ring_id < meta->size; ring_id++) {
    ring = VECTOR_ELT(x, ring_id);
    uint32_t ring_size = Rf_nrows(ring);
    int coord_size = Rf_ncols(ring);

    HANDLE_OR_RETURN(
        handler->ring_start(meta, meta->size, ring_id, handler->handler_data));

    double coord[4];
    double* coords = REAL(ring);
    for (uint32_t i = 0; i < ring_size; i++) {
      for (int j = 0; j < coord_size; j++) {
        coord[j] = coords[j * ring_size + i];
      }
      HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
    }

    HANDLE_OR_RETURN(handler->ring_end(meta, meta->size, ring_id, handler->handler_data));
  }

  HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));
  return WK_CONTINUE;
}

int wk_sfc_read_multipoint(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                           uint32_t part_id) {
  int result;
  meta->geometry_type = WK_MULTIPOINT;
  meta->size = Rf_nrows(x);
  int coord_size = Rf_ncols(x);

  wk_meta_t child_meta;
  WK_META_RESET(child_meta, WK_POINT);
  child_meta.size = 1;
  child_meta.flags = meta->flags;

  HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));

  double coord[4];
  double* coords = REAL(x);
  for (uint32_t i = 0; i < meta->size; i++) {
    for (int j = 0; j < coord_size; j++) {
      coord[j] = coords[j * meta->size + i];
    }
    HANDLE_OR_RETURN(handler->geometry_start(&child_meta, i, handler->handler_data));
    HANDLE_OR_RETURN(handler->coord(&child_meta, coord, 0, handler->handler_data));
    HANDLE_OR_RETURN(handler->geometry_end(&child_meta, i, handler->handler_data));
  }

  HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));
  return WK_CONTINUE;
}

int wk_sfc_read_multilinestring(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                                uint32_t part_id) {
  int result;
  meta->geometry_type = WK_MULTILINESTRING;
  wk_meta_t child_meta;
  WK_META_RESET(child_meta, WK_LINESTRING);
  child_meta.flags = meta->flags;
  meta->size = Rf_xlength(x);

  HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));
  for (uint32_t child_part_id = 0; child_part_id < meta->size; child_part_id++) {
    HANDLE_OR_RETURN(wk_sfc_read_linestring(VECTOR_ELT(x, child_part_id), handler,
                                            &child_meta, child_part_id));
  }
  HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));

  return WK_CONTINUE;
}

int wk_sfc_read_multipolygon(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                             uint32_t part_id) {
  int result;
  meta->geometry_type = WK_MULTIPOLYGON;
  wk_meta_t child_meta;
  WK_META_RESET(child_meta, WK_POLYGON);
  child_meta.flags = meta->flags;
  meta->size = Rf_xlength(x);

  HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));
  for (uint32_t child_part_id = 0; child_part_id < meta->size; child_part_id++) {
    HANDLE_OR_RETURN(wk_sfc_read_polygon(VECTOR_ELT(x, child_part_id), handler,
                                         &child_meta, child_part_id));
  }
  HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));

  return WK_CONTINUE;
}

int wk_sfc_read_geometrycollection(SEXP x, wk_handler_t* handler, wk_meta_t* meta,
                                   uint32_t part_id) {
  int result;
  meta->geometry_type = WK_GEOMETRYCOLLECTION;
  meta->size = Rf_xlength(x);

  HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));
  for (uint32_t child_part_id = 0; child_part_id < meta->size; child_part_id++) {
    HANDLE_OR_RETURN(wk_sfc_read_sfg(VECTOR_ELT(x, child_part_id), handler, child_part_id,
                                     meta->precision));
  }
  HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));

  return WK_CONTINUE;
}

void wk_update_meta_from_sfg(SEXP x, wk_meta_t* meta) {
  if (Rf_inherits(x, "XY")) {
    // don't need to do anything here; default meta is xy
  } else if (Rf_inherits(x, "XYZ")) {
    meta->flags |= WK_FLAG_HAS_Z;
  } else if (Rf_inherits(x, "XYM")) {
    meta->flags |= WK_FLAG_HAS_M;
  } else if (Rf_inherits(x, "XYZM")) {
    meta->flags |= WK_FLAG_HAS_Z;
    meta->flags |= WK_FLAG_HAS_M;
  } else if (Rf_inherits(x, "sfg")) {
    Rf_error("Can't guess dimensions from class of 'sfg'");
  }
}

void wk_update_vector_meta_from_sfc(SEXP x, wk_vector_meta_t* vector_meta) {
  // provide geometry type based on class
  if (Rf_inherits(x, "sfc_POINT")) {
    vector_meta->geometry_type = WK_POINT;
  } else if (Rf_inherits(x, "sfc_LINESTRING")) {
    vector_meta->geometry_type = WK_LINESTRING;
  } else if (Rf_inherits(x, "sfc_POLYGON")) {
    vector_meta->geometry_type = WK_POLYGON;
  } else if (Rf_inherits(x, "sfc_MULTIPOINT")) {
    vector_meta->geometry_type = WK_MULTIPOINT;
  } else if (Rf_inherits(x, "sfc_MULTILINESTRING")) {
    vector_meta->geometry_type = WK_MULTILINESTRING;
  } else if (Rf_inherits(x, "sfc_MULTIPOLYGON")) {
    vector_meta->geometry_type = WK_MULTIPOLYGON;
  } else if (Rf_inherits(x, "sfc_GEOMETRYCOLLECTION")) {
    vector_meta->geometry_type = WK_GEOMETRYCOLLECTION;
  } else {
    vector_meta->geometry_type = WK_GEOMETRY;
  }

  // if z or m coords are present, ranges are provided
  SEXP z_range;
  PROTECT(z_range = Rf_getAttrib(x, Rf_install("z_range")));
  if (z_range != R_NilValue) {
    vector_meta->flags |= WK_FLAG_HAS_Z;
  }

  SEXP m_range;
  PROTECT(m_range = Rf_getAttrib(x, Rf_install("m_range")));
  if (m_range != R_NilValue) {
    vector_meta->flags |= WK_FLAG_HAS_M;
  }

  // sfc objects come with a cached bbox
  // This appears to always be xmin, ymin, xmax, ymax
  // when attached to an sfc object
  SEXP bbox;
  PROTECT(bbox = Rf_getAttrib(x, Rf_install("bbox")));
  if ((Rf_xlength(x) > 0) && (bbox != R_NilValue)) {
    vector_meta->bounds_min[0] = REAL(bbox)[0];
    vector_meta->bounds_min[1] = REAL(bbox)[1];
    vector_meta->bounds_max[0] = REAL(bbox)[2];
    vector_meta->bounds_max[1] = REAL(bbox)[3];

    vector_meta->flags |= WK_FLAG_HAS_BOUNDS;
  }

  // Also include ZM values in the provided ranges
  if ((z_range != R_NilValue) && (m_range != R_NilValue)) {
    vector_meta->bounds_min[2] = REAL(z_range)[1];
    vector_meta->bounds_max[2] = REAL(z_range)[2];
    vector_meta->bounds_min[3] = REAL(m_range)[1];
    vector_meta->bounds_max[3] = REAL(m_range)[2];
  } else if (z_range != R_NilValue) {
    vector_meta->bounds_min[2] = REAL(z_range)[1];
    vector_meta->bounds_max[2] = REAL(z_range)[2];
  } else if (m_range != R_NilValue) {
    vector_meta->bounds_min[2] = REAL(m_range)[1];
    vector_meta->bounds_max[2] = REAL(m_range)[2];
  }
  UNPROTECT(3);
}

double wk_sfc_precision(SEXP x) {
  SEXP prec;
  PROTECT(prec = Rf_getAttrib(x, Rf_install("precision")));
  if ((TYPEOF(prec) == INTSXP) && (Rf_length(prec) == 1)) {
    UNPROTECT(1);
    return INTEGER(prec)[0];
  } else if ((TYPEOF(prec) == REALSXP) && (Rf_length(prec) == 1)) {
    UNPROTECT(1);
    return REAL(prec)[0];
  } else {
    UNPROTECT(1);
    return WK_PRECISION_NONE;
  }
}
