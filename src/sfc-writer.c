
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include "wk-v1.h"

#define SFC_FLAGS_NOT_YET_DEFINED UINT32_MAX
#define SFC_GEOMETRY_TYPE_NOT_YET_DEFINED -1
#define SFC_MAX_RECURSION_DEPTH 32
#define SFC_WRITER_GEOM_LENGTH SFC_MAX_RECURSION_DEPTH + 2
#define SFC_INITIAL_SIZE_IF_UNKNOWN 32
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

typedef struct {
  // Flag to promote all simple geometries to multi
  int promote_multi;

  // output vector list()
  SEXP sfc;
  // container list() geometries
  SEXP geom[SFC_WRITER_GEOM_LENGTH];
  // keep track of recursion level and number of parts seen in a geometry
  int64_t recursion_level;
  R_xlen_t part_id[SFC_WRITER_GEOM_LENGTH];

  // the current coordinate sequence and information about
  // where we are in the coordinate sequence
  SEXP coord_seq;
  int coord_size;
  uint32_t coord_id;
  int coord_seq_rows;

  // attr(sfc, "bbox"): xmin, ymin, xmax, ymax
  double bbox[4];
  // attr(sfc, "z_range"): zmin, zmax
  double z_range[2];
  // attr(sfc, "m_range"): mmin, mmax
  double m_range[2];
  // attr(sfc, "precision")
  double precision;
  // used to tell if all items are the same type for output class
  int geometry_type;
  // when all elements are empty, sfc holds the classes of these objects
  // so in addition to knowing the common geometry type, we need to know
  // all types that were encountered in the off chance that they are all empty
  // using a bitwise OR  with (1 << (wk geometry type))
  int all_geometry_types;
  // used to enforce requirement that all sub geometries to have the same dimensions
  uint32_t flags;
  // attr(sfc, "n_empty")
  R_xlen_t n_empty;
  // sfc views NULL as equivalent to EMPTY, but we can skip this replacement if
  // there were not any NULLs (almost 100% of the time)
  int any_null;
  // needed to access feat_id in geometry handlers
  R_xlen_t feat_id;
} sfc_writer_t;

sfc_writer_t* sfc_writer_new(int promote_multi) {
  sfc_writer_t* writer = (sfc_writer_t*)malloc(sizeof(sfc_writer_t));
  if (writer == NULL) {
    return NULL;  // # nocov
  }

  writer->promote_multi = promote_multi;

  writer->sfc = R_NilValue;
  for (int i = 0; i < SFC_WRITER_GEOM_LENGTH; i++) {
    writer->geom[i] = R_NilValue;
    writer->part_id[i] = 0;
  }
  writer->recursion_level = 0;

  writer->coord_seq = R_NilValue;
  writer->coord_id = -1;
  writer->coord_size = 2;
  writer->coord_seq_rows = -1;

  writer->bbox[0] = R_PosInf;
  writer->bbox[1] = R_PosInf;
  writer->bbox[2] = R_NegInf;
  writer->bbox[3] = R_NegInf;

  writer->z_range[0] = R_PosInf;
  writer->z_range[1] = R_NegInf;

  writer->m_range[0] = R_PosInf;
  writer->m_range[1] = R_NegInf;

  writer->precision = R_PosInf;

  writer->geometry_type = SFC_GEOMETRY_TYPE_NOT_YET_DEFINED;
  writer->all_geometry_types = 0;
  writer->flags = SFC_FLAGS_NOT_YET_DEFINED;
  writer->n_empty = 0;
  writer->any_null = 0;
  writer->feat_id = 0;

  return writer;
}

int sfc_writer_is_nesting_geometrycollection(sfc_writer_t* writer) {
  return (writer->recursion_level > 0) &&
         Rf_inherits(writer->geom[writer->recursion_level - 1], "GEOMETRYCOLLECTION");
}

int sfc_writer_is_nesting_multipoint(sfc_writer_t* writer) {
  return Rf_inherits(writer->coord_seq, "MULTIPOINT");
}

static inline int sfc_double_all_na_or_nan(int n_values, const double* values) {
  for (int i = 0; i < n_values; i++) {
    if (!ISNA(values[i]) && !ISNAN(values[i])) {
      return 0;
    }
  }

  return 1;
}

// this is intended to replicate NA_crs_
SEXP sfc_na_crs(void) {
  const char* crs_names[] = {"input", "wkt", ""};
  SEXP crs = PROTECT(Rf_mkNamed(VECSXP, crs_names));
  SEXP crs_input = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(crs_input, 0, NA_STRING);
  SET_VECTOR_ELT(crs, 0, crs_input);
  UNPROTECT(1);
  SEXP crs_wkt = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(crs_wkt, 0, NA_STRING);
  SET_VECTOR_ELT(crs, 1, crs_wkt);
  UNPROTECT(1);
  Rf_setAttrib(crs, R_ClassSymbol, Rf_mkString("crs"));
  UNPROTECT(1);
  return crs;
}

SEXP sfc_writer_empty_sfg(int geometry_type, uint32_t flags) {
  SEXP result = R_NilValue;

  int coord_size;
  if ((flags & WK_FLAG_HAS_Z) && (flags & WK_FLAG_HAS_M)) {
    coord_size = 4;
  } else if ((flags & WK_FLAG_HAS_Z) || (flags & WK_FLAG_HAS_M)) {
    coord_size = 3;
  } else {
    coord_size = 2;
  }

  switch (geometry_type) {
    case WK_POINT:
      result = PROTECT(Rf_allocVector(REALSXP, coord_size));
      for (int i = 0; i < coord_size; i++) {
        REAL(result)[i] = NA_REAL;
      }
      break;
    case WK_LINESTRING:
      result = PROTECT(Rf_allocMatrix(REALSXP, 0, coord_size));
      break;
    case WK_POLYGON:
      result = PROTECT(Rf_allocVector(VECSXP, 0));
      break;
    case WK_MULTIPOINT:
      result = PROTECT(Rf_allocMatrix(REALSXP, 0, coord_size));
      break;
    case WK_MULTILINESTRING:
      result = PROTECT(Rf_allocVector(VECSXP, 0));
      break;
    case WK_MULTIPOLYGON:
      result = PROTECT(Rf_allocVector(VECSXP, 0));
      break;
    case WK_GEOMETRYCOLLECTION:
      result = PROTECT(Rf_allocVector(VECSXP, 0));
      break;
    default:
      Rf_error("Can't generate empty 'sfg' for geometry type '%d'",
               geometry_type);  // # nocov
  }

  UNPROTECT(1);
  return result;
}

SEXP sfc_writer_promote_multi(SEXP item, int geometry_type, uint32_t flags,
                              uint32_t size) {
  int coord_size;
  if ((flags & WK_FLAG_HAS_Z) && (flags & WK_FLAG_HAS_M)) {
    coord_size = 4;
  } else if ((flags & WK_FLAG_HAS_Z) || (flags & WK_FLAG_HAS_M)) {
    coord_size = 3;
  } else {
    coord_size = 2;
  }

  switch (geometry_type) {
    case WK_POINT: {
      if (size > 0) {
        SEXP result = PROTECT(Rf_allocMatrix(REALSXP, 1, coord_size));
        memcpy(REAL(result), REAL(item), coord_size * sizeof(double));
        UNPROTECT(1);
        return result;
      } else {
        return Rf_allocMatrix(REALSXP, 0, coord_size);
      }
    }
    case WK_LINESTRING:
    case WK_POLYGON: {
      if (size > 0) {
        SEXP result = PROTECT(Rf_allocVector(VECSXP, 1));
        Rf_setAttrib(item, R_ClassSymbol, R_NilValue);
        SET_VECTOR_ELT(result, 0, item);
        UNPROTECT(1);
        return result;
      } else {
        return Rf_allocVector(VECSXP, 0);
      }
    }

    default:
      return item;
  }
}

void sfc_writer_maybe_add_class_to_sfg(sfc_writer_t* writer, SEXP item, int geometry_type,
                                       uint32_t flags) {
  if (writer->recursion_level == 0 || sfc_writer_is_nesting_geometrycollection(writer)) {
    // in the form XY(ZM), GEOM_TYPE, sfg
    SEXP class = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(class, 2, Rf_mkChar("sfg"));

    if ((flags & WK_FLAG_HAS_Z) && (flags & WK_FLAG_HAS_M)) {
      SET_STRING_ELT(class, 0, Rf_mkChar("XYZM"));
    } else if (flags & WK_FLAG_HAS_Z) {
      SET_STRING_ELT(class, 0, Rf_mkChar("XYZ"));
    } else if (flags & WK_FLAG_HAS_M) {
      SET_STRING_ELT(class, 0, Rf_mkChar("XYM"));
    } else {
      SET_STRING_ELT(class, 0, Rf_mkChar("XY"));
    }

    switch (geometry_type) {
      case WK_POINT:
        SET_STRING_ELT(class, 1, Rf_mkChar("POINT"));
        break;
      case WK_LINESTRING:
        SET_STRING_ELT(class, 1, Rf_mkChar("LINESTRING"));
        break;
      case WK_POLYGON:
        SET_STRING_ELT(class, 1, Rf_mkChar("POLYGON"));
        break;
      case WK_MULTIPOINT:
        SET_STRING_ELT(class, 1, Rf_mkChar("MULTIPOINT"));
        break;
      case WK_MULTILINESTRING:
        SET_STRING_ELT(class, 1, Rf_mkChar("MULTILINESTRING"));
        break;
      case WK_MULTIPOLYGON:
        SET_STRING_ELT(class, 1, Rf_mkChar("MULTIPOLYGON"));
        break;
      case WK_GEOMETRYCOLLECTION:
        SET_STRING_ELT(class, 1, Rf_mkChar("GEOMETRYCOLLECTION"));
        break;
      default:
        Rf_error("Can't generate class 'sfg' for geometry type '%d'",
                 geometry_type);  // # nocov
    }

    Rf_setAttrib(item, R_ClassSymbol, class);
    UNPROTECT(1);
  }
}

void sfc_writer_update_dimensions(sfc_writer_t* writer, const wk_meta_t* meta,
                                  uint32_t size) {
  if (size > 0) {
    if (writer->flags == SFC_FLAGS_NOT_YET_DEFINED) {
      writer->flags = meta->flags;
    } else if (writer->flags != meta->flags) {
      Rf_error("Can't convert geometries with incompatible dimensions to 'sfc'");
    }
  }
}

void sfc_writer_update_vector_attributes(sfc_writer_t* writer, const wk_meta_t* meta,
                                         int geometry_type, uint32_t size) {
  // all geometry types specifically matters for when everything is EMPTY
  writer->all_geometry_types = writer->all_geometry_types | (1 << (geometry_type - 1));

  // these matter even for EMPTY
  if (writer->geometry_type == SFC_GEOMETRY_TYPE_NOT_YET_DEFINED) {
    writer->geometry_type = geometry_type;
  } else if (writer->geometry_type != geometry_type) {
    writer->geometry_type = WK_GEOMETRY;
  }

  // update empty count
  writer->n_empty += size == 0;

  // update dimensions
  sfc_writer_update_dimensions(writer, meta, size);

  // update precision
  writer->precision = MIN(writer->precision, meta->precision);
}

void sfc_writer_update_ranges(sfc_writer_t* writer, const wk_meta_t* meta,
                              const double* coord) {
  writer->bbox[0] = MIN(writer->bbox[0], coord[0]);
  writer->bbox[1] = MIN(writer->bbox[1], coord[1]);
  writer->bbox[2] = MAX(writer->bbox[2], coord[0]);
  writer->bbox[3] = MAX(writer->bbox[3], coord[1]);

  if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
    writer->z_range[0] = MIN(writer->z_range[0], coord[2]);
    writer->z_range[1] = MAX(writer->z_range[1], coord[2]);
    writer->m_range[0] = MIN(writer->m_range[0], coord[3]);
    writer->m_range[1] = MAX(writer->m_range[1], coord[3]);
  } else if (meta->flags & WK_FLAG_HAS_Z) {
    writer->z_range[0] = MIN(writer->z_range[0], coord[2]);
    writer->z_range[1] = MAX(writer->z_range[1], coord[2]);
  } else if (meta->flags & WK_FLAG_HAS_M) {
    writer->m_range[0] = MIN(writer->m_range[0], coord[2]);
    writer->m_range[1] = MAX(writer->m_range[1], coord[2]);
  }
}

SEXP sfc_writer_alloc_coord_seq(uint32_t size_hint, int coord_size) {
  if (size_hint == WK_SIZE_UNKNOWN) {
    size_hint = SFC_INITIAL_SIZE_IF_UNKNOWN;
  }

  return Rf_allocMatrix(REALSXP, size_hint, coord_size);
}

SEXP sfc_writer_realloc_coord_seq(SEXP coord_seq, uint32_t new_size) {
  uint32_t current_size = Rf_nrows(coord_seq);
  int coord_size = Rf_ncols(coord_seq);

  SEXP new_coord_seq = PROTECT(Rf_allocMatrix(REALSXP, new_size, coord_size));

  double* old_values = REAL(coord_seq);
  double* new_values = REAL(new_coord_seq);

  for (int j = 0; j < coord_size; j++) {
    memcpy(new_values + (j * new_size), old_values + (j * current_size),
           sizeof(double) * current_size);
  }

  if (Rf_inherits(coord_seq, "sfg")) {
    SEXP class = PROTECT(Rf_getAttrib(coord_seq, R_ClassSymbol));
    Rf_setAttrib(new_coord_seq, R_ClassSymbol, class);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return new_coord_seq;
}

SEXP sfc_writer_finalize_coord_seq(SEXP coord_seq, uint32_t final_size) {
  uint32_t current_size = Rf_nrows(coord_seq);
  int coord_size = Rf_ncols(coord_seq);

  SEXP new_coord_seq = PROTECT(Rf_allocMatrix(REALSXP, final_size, coord_size));

  double* old_values = REAL(coord_seq);
  double* new_values = REAL(new_coord_seq);

  for (int j = 0; j < coord_size; j++) {
    memcpy(new_values + (j * final_size), old_values + (j * current_size),
           sizeof(double) * final_size);
  }

  if (Rf_inherits(coord_seq, "sfg")) {
    SEXP class = PROTECT(Rf_getAttrib(coord_seq, R_ClassSymbol));
    Rf_setAttrib(new_coord_seq, R_ClassSymbol, class);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return new_coord_seq;
}

SEXP sfc_writer_alloc_geom(uint32_t size_hint) {
  if (size_hint == WK_SIZE_UNKNOWN) {
    size_hint = SFC_INITIAL_SIZE_IF_UNKNOWN;
  }
  return Rf_allocVector(VECSXP, size_hint);
}

SEXP sfc_writer_realloc_geom(SEXP geom, R_xlen_t new_size) {
  R_xlen_t current_size = Rf_xlength(geom);

  SEXP new_geom = PROTECT(Rf_allocVector(VECSXP, new_size));
  for (R_xlen_t i = 0; i < current_size; i++) {
    SET_VECTOR_ELT(new_geom, i, VECTOR_ELT(geom, i));
  }

  if (Rf_inherits(geom, "sfg")) {
    SEXP class = PROTECT(Rf_getAttrib(geom, R_ClassSymbol));
    Rf_setAttrib(new_geom, R_ClassSymbol, class);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return new_geom;
}

SEXP sfc_writer_finalize_geom(SEXP geom, R_xlen_t final_size) {
  SEXP new_geom = PROTECT(Rf_allocVector(VECSXP, final_size));
  for (R_xlen_t i = 0; i < final_size; i++) {
    SET_VECTOR_ELT(new_geom, i, VECTOR_ELT(geom, i));
  }

  if (Rf_inherits(geom, "sfg")) {
    SEXP class = PROTECT(Rf_getAttrib(geom, R_ClassSymbol));
    Rf_setAttrib(new_geom, R_ClassSymbol, class);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return new_geom;
}

static inline void sfc_writer_sfc_append(sfc_writer_t* writer, SEXP value) {
  R_xlen_t current_size = Rf_xlength(writer->sfc);
  if (writer->feat_id >= current_size) {
    SEXP new_result = PROTECT(Rf_allocVector(VECSXP, current_size * 2 + 1));
    for (R_xlen_t i = 0; i < current_size; i++) {
      SET_VECTOR_ELT(new_result, i, VECTOR_ELT(writer->sfc, i));
    }
    R_ReleaseObject(writer->sfc);
    writer->sfc = new_result;
    R_PreserveObject(writer->sfc);
    UNPROTECT(1);
  }

  SET_VECTOR_ELT(writer->sfc, writer->feat_id, value);
  writer->feat_id++;
}

static inline void sfc_writer_sfc_finalize(sfc_writer_t* writer) {
  R_xlen_t current_size = Rf_xlength(writer->sfc);
  if (writer->feat_id != current_size) {
    SEXP new_result = PROTECT(Rf_allocVector(VECSXP, writer->feat_id));
    for (R_xlen_t i = 0; i < writer->feat_id; i++) {
      SET_VECTOR_ELT(new_result, i, VECTOR_ELT(writer->sfc, i));
    }
    R_ReleaseObject(writer->sfc);
    writer->sfc = new_result;
    R_PreserveObject(writer->sfc);
    UNPROTECT(1);
  }
}

int sfc_writer_vector_start(const wk_vector_meta_t* vector_meta, void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;

  if (writer->sfc != R_NilValue) {
    Rf_error("Destination vector was already allocated");  // # nocov
  }

  if (vector_meta->size == WK_VECTOR_SIZE_UNKNOWN) {
    writer->sfc = PROTECT(Rf_allocVector(VECSXP, 1024));
  } else {
    writer->sfc = PROTECT(Rf_allocVector(VECSXP, vector_meta->size));
  }

  R_PreserveObject(writer->sfc);
  UNPROTECT(1);

  writer->feat_id = 0;

  return WK_CONTINUE;
}

int sfc_writer_feature_start(const wk_vector_meta_t* vector_meta, R_xlen_t feat_id,
                             void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;
  writer->recursion_level = 0;
  return WK_CONTINUE;
}

int sfc_writer_null_feature(void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;
  // sfc doesn't do NULLs and replaces them with GEOMETRYCOLLECTION EMPTY
  // however, as the dimensions have to align among features we asign a NULL here and fix
  // in vector_end()
  writer->any_null = 1;
  sfc_writer_sfc_append(writer, R_NilValue);
  return WK_ABORT_FEATURE;
}

int sfc_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id,
                              void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;

  // ignore start of POINT nested in MULTIPOINT
  int nesting_multipoint = sfc_writer_is_nesting_multipoint(writer);
  if (meta->geometry_type == WK_POINT && nesting_multipoint) {
    return WK_CONTINUE;
  } else if (nesting_multipoint) {
    Rf_error("Expected geometry type nested within MULTIPOINT to be a POINT");
  }

  if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
    writer->coord_size = 4;
  } else if ((meta->flags & WK_FLAG_HAS_Z) || (meta->flags & WK_FLAG_HAS_M)) {
    writer->coord_size = 3;
  } else {
    writer->coord_size = 2;
  }

  // there isn't quite enough information here yet for points, which can
  // be considered empty if coordinates are NA
  if ((writer->recursion_level == 0) && (meta->geometry_type != WK_POINT) &&
      !writer->promote_multi) {
    sfc_writer_update_vector_attributes(writer, meta, meta->geometry_type, meta->size);
  } else if ((writer->recursion_level < 0) ||
             (writer->recursion_level >= SFC_MAX_RECURSION_DEPTH)) {
    Rf_error("Invalid recursion depth whilst parsing 'sfg': %d", writer->recursion_level);
  }

  // if POINT, LINESTRING, or MULTIPOINT
  // replace coordinate sequence with a fresh one
  // otherwise, create a list() container and push it to the writer->geom[] stack
  switch (meta->geometry_type) {
    case WK_POINT:
      if (writer->coord_seq != R_NilValue) R_ReleaseObject(writer->coord_seq);
      writer->coord_seq = PROTECT(Rf_allocVector(REALSXP, writer->coord_size));

      // empty point is NA, NA ...
      if (meta->size == 0) {
        for (int i = 0; i < writer->coord_size; i++) {
          REAL(writer->coord_seq)[i] = NA_REAL;
        }
      }

      sfc_writer_maybe_add_class_to_sfg(writer, writer->coord_seq, meta->geometry_type,
                                        meta->flags);
      R_PreserveObject(writer->coord_seq);
      UNPROTECT(1);
      writer->coord_id = 0;
      writer->coord_seq_rows = 1;
      break;
    case WK_LINESTRING:
    case WK_MULTIPOINT:
      if (writer->coord_seq != R_NilValue) R_ReleaseObject(writer->coord_seq);
      writer->coord_seq =
          PROTECT(sfc_writer_alloc_coord_seq(meta->size, writer->coord_size));

      sfc_writer_maybe_add_class_to_sfg(writer, writer->coord_seq, meta->geometry_type,
                                        meta->flags);
      R_PreserveObject(writer->coord_seq);
      UNPROTECT(1);
      writer->coord_id = 0;
      writer->coord_seq_rows = Rf_nrows(writer->coord_seq);
      break;
    case WK_POLYGON:
    case WK_MULTILINESTRING:
    case WK_MULTIPOLYGON:
    case WK_GEOMETRYCOLLECTION:
      if (writer->geom[writer->recursion_level] != R_NilValue) {
        R_ReleaseObject(writer->geom[writer->recursion_level]);
      }

      writer->geom[writer->recursion_level] = PROTECT(sfc_writer_alloc_geom(meta->size));
      sfc_writer_maybe_add_class_to_sfg(writer, writer->geom[writer->recursion_level],
                                        meta->geometry_type, meta->flags);
      R_PreserveObject(writer->geom[writer->recursion_level]);
      UNPROTECT(1);
      writer->part_id[writer->recursion_level] = 0;
      break;
    default:
      Rf_error("Can't convert geometry type '%d' to sfg",
               meta->geometry_type);  // # nocov
      break;
  }

  writer->recursion_level++;
  return WK_CONTINUE;
}

int sfc_writer_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id,
                          void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;

  if (writer->coord_seq != NULL) {
    R_ReleaseObject(writer->coord_seq);
  }

  writer->coord_seq = PROTECT(sfc_writer_alloc_coord_seq(size, writer->coord_size));
  R_PreserveObject(writer->coord_seq);
  UNPROTECT(1);
  writer->coord_id = 0;
  writer->coord_seq_rows = Rf_nrows(writer->coord_seq);

  writer->recursion_level++;
  return WK_CONTINUE;
}

int sfc_writer_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id,
                     void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;

  // This point might be EMPTY, in which case it will cause the ranges to be all NaN
  if ((meta->geometry_type != WK_POINT) ||
      (!sfc_double_all_na_or_nan(writer->coord_size, coord))) {
    sfc_writer_update_ranges(writer, meta, coord);
  }

  // realloc the coordinate sequence if necessary
  if (writer->coord_id >= writer->coord_seq_rows) {
    SEXP new_coord_seq = PROTECT(
        sfc_writer_realloc_coord_seq(writer->coord_seq, writer->coord_id * 1.5 + 1));
    R_ReleaseObject(writer->coord_seq);
    writer->coord_seq = new_coord_seq;
    R_PreserveObject(writer->coord_seq);
    UNPROTECT(1);
    writer->coord_seq_rows = Rf_nrows(writer->coord_seq);
  }

  double* current_values = REAL(writer->coord_seq);
  for (int i = 0; i < writer->coord_size; i++) {
    current_values[i * writer->coord_seq_rows + writer->coord_id] = coord[i];
  }

  writer->coord_id++;
  return WK_CONTINUE;
}

int sfc_writer_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id,
                        void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;

  writer->recursion_level--;
  if (writer->recursion_level < 0) {
    Rf_error("Recursion level underflowed");  // # nocov
  }

  SEXP geom;
  if (writer->coord_id < Rf_nrows(writer->coord_seq)) {
    geom = PROTECT(sfc_writer_finalize_coord_seq(writer->coord_seq, writer->coord_id));
  } else {
    geom = PROTECT(writer->coord_seq);
  }

  R_ReleaseObject(writer->coord_seq);
  writer->coord_seq = R_NilValue;

  // may need to reallocate the container
  R_xlen_t container_len = Rf_xlength(writer->geom[writer->recursion_level - 1]);
  if (ring_id >= container_len) {
    SEXP new_geom = PROTECT(sfc_writer_realloc_geom(
        writer->geom[writer->recursion_level - 1], container_len * 1.5 + 1));
    R_ReleaseObject(writer->geom[writer->recursion_level - 1]);
    writer->geom[writer->recursion_level - 1] = new_geom;
    R_PreserveObject(writer->geom[writer->recursion_level - 1]);
    UNPROTECT(1);
  }

  SET_VECTOR_ELT(writer->geom[writer->recursion_level - 1], ring_id, geom);
  writer->part_id[writer->recursion_level - 1]++;
  UNPROTECT(1);

  return WK_CONTINUE;
}

int sfc_writer_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;

  // ignore end of POINT nested in MULTIPOINT
  int nesting_multipoint = sfc_writer_is_nesting_multipoint(writer);
  if ((meta->geometry_type == WK_POINT) && nesting_multipoint) {
    return WK_CONTINUE;
  }

  writer->recursion_level--;
  if (writer->recursion_level < 0) {
    Rf_error("Recursion level underflowed");  // # nocov
  }

  SEXP geom;
  switch (meta->geometry_type) {
    case WK_POINT:
      geom = PROTECT(writer->coord_seq);
      R_ReleaseObject(writer->coord_seq);
      writer->coord_seq = R_NilValue;
      break;
    case WK_LINESTRING:
    case WK_MULTIPOINT:
      if (writer->coord_id < Rf_nrows(writer->coord_seq)) {
        geom =
            PROTECT(sfc_writer_finalize_coord_seq(writer->coord_seq, writer->coord_id));
      } else {
        geom = PROTECT(writer->coord_seq);
      }
      R_ReleaseObject(writer->coord_seq);
      writer->coord_seq = R_NilValue;
      break;
    case WK_POLYGON:
    case WK_MULTILINESTRING:
    case WK_MULTIPOLYGON:
    case WK_GEOMETRYCOLLECTION:
      if (writer->part_id[writer->recursion_level] <
          Rf_xlength(writer->geom[writer->recursion_level])) {
        geom =
            PROTECT(sfc_writer_finalize_geom(writer->geom[writer->recursion_level],
                                             writer->part_id[writer->recursion_level]));
      } else {
        geom = PROTECT(writer->geom[writer->recursion_level]);
      }

      // R_ReleaseObject() is called on `geom` in finalize() or
      // when it is replaced in geometry_start()
      break;
    default:
      Rf_error("Can't convert geometry type '%d' to sfg",
               meta->geometry_type);  // # nocov
      break;                          // # nocov
  }

  // Top-level geometries have their dimensions checked but nested must be as well
  if ((writer->recursion_level) > 0 && (meta->geometry_type == WK_POINT)) {
    int all_na = sfc_double_all_na_or_nan(writer->coord_size, REAL(geom));
    sfc_writer_update_dimensions(writer, meta, meta->size && !all_na);
  } else if (writer->recursion_level > 0) {
    sfc_writer_update_dimensions(writer, meta, meta->size);
  }

  // if we're above a top-level geometry, this geometry needs to be added to the parent
  // otherwise, it needs to be added to sfc
  if (writer->recursion_level > 0) {
    // may need to reallocate the container
    R_xlen_t container_len = Rf_xlength(writer->geom[writer->recursion_level - 1]);
    if (part_id >= container_len) {
      SEXP new_geom = PROTECT(sfc_writer_realloc_geom(
          writer->geom[writer->recursion_level - 1], container_len * 1.5 + 1));
      R_ReleaseObject(writer->geom[writer->recursion_level - 1]);
      writer->geom[writer->recursion_level - 1] = new_geom;
      R_PreserveObject(writer->geom[writer->recursion_level - 1]);
      UNPROTECT(1);
    }

    SET_VECTOR_ELT(writer->geom[writer->recursion_level - 1], part_id, geom);
    writer->part_id[writer->recursion_level - 1]++;
  } else if (meta->geometry_type == WK_POINT) {
    // at the top level, we have to check again if all point coordinates are NA
    // because this is 'empty' for the purposes of sfc
    // We didn't update this earlier because we didn't know if the point was
    // empty yet or not!
    int all_na = sfc_double_all_na_or_nan(writer->coord_size, REAL(geom));

    // Promote geometry to multi if necessary
    if (writer->promote_multi) {
      SEXP item_to_append = PROTECT(
          sfc_writer_promote_multi(geom, WK_POINT, meta->flags, meta->size && !all_na));

      sfc_writer_maybe_add_class_to_sfg(writer, item_to_append, WK_MULTIPOINT,
                                        meta->flags);
      sfc_writer_update_vector_attributes(writer, meta, WK_MULTIPOINT,
                                          meta->size && !all_na);

      sfc_writer_sfc_append(writer, item_to_append);
      UNPROTECT(1);
    } else {
      sfc_writer_update_vector_attributes(writer, meta, WK_POINT, meta->size && !all_na);
      sfc_writer_sfc_append(writer, geom);
    }
  } else if (writer->promote_multi) {
    SEXP item_to_append = PROTECT(
        sfc_writer_promote_multi(geom, meta->geometry_type, meta->flags, meta->size));

    int geometry_type_to_append = meta->geometry_type <= WK_POLYGON
                                      ? meta->geometry_type + 3L
                                      : meta->geometry_type;
    sfc_writer_maybe_add_class_to_sfg(writer, item_to_append, geometry_type_to_append,
                                      meta->flags);
    sfc_writer_update_vector_attributes(writer, meta, geometry_type_to_append,
                                        meta->size);

    sfc_writer_sfc_append(writer, item_to_append);
    UNPROTECT(1);
  } else {
    sfc_writer_sfc_append(writer, geom);
  }

  UNPROTECT(1);
  return WK_CONTINUE;
}

SEXP sfc_writer_vector_end(const wk_vector_meta_t* vector_meta, void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;

  sfc_writer_sfc_finalize(writer);

  // replace NULLs with EMPTY of an appropriate type
  if (writer->any_null) {
    wk_meta_t meta;

    if (writer->geometry_type == WK_GEOMETRY ||
        writer->geometry_type == SFC_GEOMETRY_TYPE_NOT_YET_DEFINED) {
      WK_META_RESET(meta, WK_GEOMETRYCOLLECTION);
      // also update the type list for attr(sfc, "classes")
      writer->all_geometry_types =
          writer->all_geometry_types | (1 << (WK_GEOMETRYCOLLECTION - 1));
    } else {
      WK_META_RESET(meta, writer->geometry_type);
    }

    if (writer->flags != SFC_FLAGS_NOT_YET_DEFINED) {
      meta.flags = writer->flags;
    }

    if (writer->geometry_type == SFC_GEOMETRY_TYPE_NOT_YET_DEFINED) {
      writer->geometry_type = WK_GEOMETRYCOLLECTION;
    }

    meta.size = 0;
    writer->recursion_level = 0;
    SEXP empty = PROTECT(sfc_writer_empty_sfg(meta.geometry_type, meta.flags));
    sfc_writer_maybe_add_class_to_sfg(writer, empty, meta.geometry_type, meta.flags);

    for (R_xlen_t i = 0; i < Rf_xlength(writer->sfc); i++) {
      if (VECTOR_ELT(writer->sfc, i) == R_NilValue) {
        writer->n_empty++;
        SET_VECTOR_ELT(writer->sfc, i, empty);
      }
    }

    UNPROTECT(1);
  }

  // attr(sfc, "precision")
  SEXP precision;
  if (writer->precision == R_PosInf) {
    precision = PROTECT(Rf_ScalarReal(0.0));
  } else {
    precision = PROTECT(Rf_ScalarReal(writer->precision));
  }
  Rf_setAttrib(writer->sfc, Rf_install("precision"), precision);
  UNPROTECT(1);

  // attr(sfc, "bbox")
  const char* bbox_names[] = {"xmin", "ymin", "xmax", "ymax", ""};
  SEXP bbox = PROTECT(Rf_mkNamed(REALSXP, bbox_names));
  Rf_setAttrib(bbox, R_ClassSymbol, Rf_mkString("bbox"));

  // the bounding box may or may not have a crs attribute
  // when all features are empty
  if (Rf_xlength(writer->sfc) == writer->n_empty) {
    SEXP na_crs = PROTECT(sfc_na_crs());
    Rf_setAttrib(bbox, Rf_install("crs"), na_crs);
    UNPROTECT(1);
  }

  // if the bounding box was never updated, set it to NAs
  if (writer->bbox[0] == R_PosInf) {
    writer->bbox[0] = NA_REAL;
    writer->bbox[1] = NA_REAL;
    writer->bbox[2] = NA_REAL;
    writer->bbox[3] = NA_REAL;
  }
  memcpy(REAL(bbox), writer->bbox, sizeof(double) * 4);
  Rf_setAttrib(writer->sfc, Rf_install("bbox"), bbox);
  UNPROTECT(1);

  // attr(sfc, "z_range"), attr(sfc, "m_range")
  if (writer->flags == SFC_FLAGS_NOT_YET_DEFINED) {
    writer->flags = 0;
  }

  if (writer->flags & WK_FLAG_HAS_Z) {
    // if the z_range was never updated, set it to NAs
    if (writer->z_range[0] == R_PosInf) {
      writer->z_range[0] = NA_REAL;
      writer->z_range[1] = NA_REAL;
    }

    const char* z_range_names[] = {"zmin", "zmax", ""};
    SEXP z_range = PROTECT(Rf_mkNamed(REALSXP, z_range_names));
    Rf_setAttrib(z_range, R_ClassSymbol, Rf_mkString("z_range"));
    memcpy(REAL(z_range), writer->z_range, sizeof(double) * 2);
    Rf_setAttrib(writer->sfc, Rf_install("z_range"), z_range);
    UNPROTECT(1);
  }

  if (writer->flags & WK_FLAG_HAS_M) {
    // if the m_range was never updated, set it to NAs
    if (writer->m_range[0] == R_PosInf) {
      writer->m_range[0] = NA_REAL;
      writer->m_range[1] = NA_REAL;
    }

    const char* m_range_names[] = {"mmin", "mmax", ""};
    SEXP m_range = PROTECT(Rf_mkNamed(REALSXP, m_range_names));
    Rf_setAttrib(m_range, R_ClassSymbol, Rf_mkString("m_range"));
    memcpy(REAL(m_range), writer->m_range, sizeof(double) * 2);
    Rf_setAttrib(writer->sfc, Rf_install("m_range"), m_range);
    UNPROTECT(1);
  }

  // attr(sfc, "crs")
  // this should be handled in R; however, inserting a placeholder here
  // because the print() method for sfc will error otherwise
  SEXP na_crs = PROTECT(sfc_na_crs());
  Rf_setAttrib(writer->sfc, Rf_install("crs"), na_crs);
  UNPROTECT(1);

  // attr(sfc, "n_empty")
  SEXP n_empty = PROTECT(Rf_ScalarInteger(writer->n_empty));
  Rf_setAttrib(writer->sfc, Rf_install("n_empty"), n_empty);
  UNPROTECT(1);

  // class(sfc)
  SEXP class = PROTECT(Rf_allocVector(STRSXP, 2));
  switch (writer->geometry_type) {
    case WK_POINT:
      SET_STRING_ELT(class, 0, Rf_mkChar("sfc_POINT"));
      break;
    case WK_LINESTRING:
      SET_STRING_ELT(class, 0, Rf_mkChar("sfc_LINESTRING"));
      break;
    case WK_POLYGON:
      SET_STRING_ELT(class, 0, Rf_mkChar("sfc_POLYGON"));
      break;
    case WK_MULTIPOINT:
      SET_STRING_ELT(class, 0, Rf_mkChar("sfc_MULTIPOINT"));
      break;
    case WK_MULTILINESTRING:
      SET_STRING_ELT(class, 0, Rf_mkChar("sfc_MULTILINESTRING"));
      break;
    case WK_MULTIPOLYGON:
      SET_STRING_ELT(class, 0, Rf_mkChar("sfc_MULTIPOLYGON"));
      break;
    case WK_GEOMETRYCOLLECTION:
      SET_STRING_ELT(class, 0, Rf_mkChar("sfc_GEOMETRYCOLLECTION"));
      break;
    default:
      SET_STRING_ELT(class, 0, Rf_mkChar("sfc_GEOMETRY"));
      break;
  }
  SET_STRING_ELT(class, 1, Rf_mkChar("sfc"));
  Rf_setAttrib(writer->sfc, R_ClassSymbol, class);
  UNPROTECT(1);

  // attr(sfc, "classes") (only for sfc_GEOMETRY)
  // This is class(x[[i]])[[2]] for each sfg in the output sfc
  R_xlen_t out_length = Rf_xlength(writer->sfc);
  if (writer->geometry_type == WK_GEOMETRY || out_length == 0) {
    SEXP classes = PROTECT(Rf_allocVector(STRSXP, out_length));
    for (R_xlen_t i = 0; i < out_length; i++) {
      SEXP item = VECTOR_ELT(writer->sfc, i);
      SEXP cls = Rf_getAttrib(item, R_ClassSymbol);
      SET_STRING_ELT(classes, i, STRING_ELT(cls, 1));
    }

    Rf_setAttrib(writer->sfc, Rf_install("classes"), classes);
    UNPROTECT(1);
  }

  return writer->sfc;
}

void sfc_writer_deinitialize(void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;

  if (writer->sfc != R_NilValue) {
    R_ReleaseObject(writer->sfc);
    writer->sfc = R_NilValue;
  }

  for (int i = 0; i < (SFC_WRITER_GEOM_LENGTH); i++) {
    if (writer->geom[i] != R_NilValue) {
      R_ReleaseObject(writer->geom[i]);
      writer->geom[i] = R_NilValue;
    }
  }

  if (writer->coord_seq != R_NilValue) {
    R_ReleaseObject(writer->coord_seq);
    writer->coord_seq = R_NilValue;
  }
}

void sfc_writer_finalize(void* handler_data) {
  sfc_writer_t* writer = (sfc_writer_t*)handler_data;
  if (writer != NULL) {
    free(writer);
  }
}

SEXP wk_c_sfc_writer_new(SEXP promote_multi_sexp) {
  int promote_multi = LOGICAL(promote_multi_sexp)[0];

  wk_handler_t* handler = wk_handler_create();

  handler->finalizer = &sfc_writer_finalize;
  handler->vector_start = &sfc_writer_vector_start;
  handler->feature_start = &sfc_writer_feature_start;
  handler->null_feature = &sfc_writer_null_feature;
  handler->geometry_start = &sfc_writer_geometry_start;
  handler->ring_start = &sfc_writer_ring_start;
  handler->coord = &sfc_writer_coord;
  handler->ring_end = &sfc_writer_ring_end;
  handler->geometry_end = &sfc_writer_geometry_end;
  handler->vector_end = &sfc_writer_vector_end;
  handler->deinitialize = &sfc_writer_deinitialize;

  handler->handler_data = sfc_writer_new(promote_multi);
  if (handler->handler_data == NULL) {
    wk_handler_destroy(handler);               // # nocov
    Rf_error("Failed to alloc handler data");  // # nocov
  }

  SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
  return xptr;
}
