
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <memory.h>
#include <stdlib.h>
#include "wk-v1.h"

// IS_BIG_ENDIAN, IS_LITTLE_ENDIAN, bswap_32(), bswap_64()
#include "port.h"

#define EWKB_Z_BIT 0x80000000
#define EWKB_M_BIT 0x40000000
#define EWKB_SRID_BIT 0x20000000

#define WKB_MAX_RECURSION_DEPTH 32

typedef struct {
  SEXP result;
  int swap_endian;
  unsigned char endian;
  unsigned char* buffer;
  size_t size;
  size_t offset;
  size_t current_size_offset[WKB_MAX_RECURSION_DEPTH + 3];
  uint32_t current_size[WKB_MAX_RECURSION_DEPTH + 3];
  size_t recursion_level;
  R_xlen_t feat_id;
} wkb_writer_t;

static inline unsigned char wkb_writer_platform_endian(void) {
#ifdef IS_LITTLE_ENDIAN
  return 1;
#else
  return 0;
#endif
}

static inline uint32_t wkb_writer_encode_type(const wk_meta_t* meta,
                                              int recursion_level) {
  uint32_t out = meta->geometry_type;
  if (meta->flags & WK_FLAG_HAS_Z) out |= EWKB_Z_BIT;
  if (meta->flags & WK_FLAG_HAS_M) out |= EWKB_M_BIT;
  if (recursion_level == 0) {
    if (meta->srid != WK_SRID_NONE) out |= EWKB_SRID_BIT;
  }
  return out;
}

wkb_writer_t* wkb_writer_new(size_t buffer_size, unsigned char endian) {
  unsigned char* buffer = malloc(buffer_size);
  if (buffer == NULL) {
    return NULL;  // # nocov
  }

  wkb_writer_t* writer = (wkb_writer_t*)malloc(sizeof(wkb_writer_t));
  if (writer == NULL) {
    free(buffer);  // # nocov
    return NULL;   // # nocov
  }

  writer->endian = endian;
#ifdef IS_LITTLE_ENDIAN
  writer->swap_endian = endian != 1;
#else
  writer->swap_endian = endian != 0;
#endif
  writer->result = R_NilValue;
  writer->buffer = buffer;
  writer->size = buffer_size;
  writer->offset = 0;
  writer->recursion_level = 0;
  writer->feat_id = 0;
  return writer;
}

static inline void wkb_writer_ensure_space(wkb_writer_t* writer, size_t item) {
  if ((writer->offset + item) >= writer->size) {
    unsigned char* new_buffer = realloc(writer->buffer, writer->size * 2);
    if (new_buffer == NULL) {
      Rf_error("Can't reallocate buffer of size %ld", (long)writer->size * 2);  // # nocov
    }

    writer->buffer = new_buffer;
    writer->size = writer->size * 2;
  }
}

static inline void wkb_write_uchar(wkb_writer_t* writer, const unsigned char value) {
  wkb_writer_ensure_space(writer, sizeof(unsigned char));
  memcpy(writer->buffer + writer->offset, &value, sizeof(unsigned char));
  writer->offset += sizeof(unsigned char);
}

static inline void wkb_write_uint_offset(wkb_writer_t* writer, const uint32_t value,
                                         size_t offset) {
  if (writer->swap_endian) {
    uint32_t swapped = bswap_32(value);
    memcpy(writer->buffer + offset, &swapped, sizeof(uint32_t));
  } else {
    memcpy(writer->buffer + offset, &value, sizeof(uint32_t));
  }
}

static inline void wkb_write_uint(wkb_writer_t* writer, const uint32_t value) {
  wkb_writer_ensure_space(writer, sizeof(uint32_t));

  if (writer->swap_endian) {
    uint32_t swapped = bswap_32(value);
    memcpy(writer->buffer + writer->offset, &swapped, sizeof(uint32_t));
  } else {
    memcpy(writer->buffer + writer->offset, &value, sizeof(uint32_t));
  }

  writer->offset += sizeof(uint32_t);
}

static inline void wkb_write_doubles(wkb_writer_t* writer, const double* value,
                                     uint32_t n) {
  wkb_writer_ensure_space(writer, sizeof(double) * n);

  if (writer->swap_endian) {
    uint64_t swappable, swapped;
    for (uint32_t i = 0; i < n; i++) {
      memcpy(&swappable, value + i, sizeof(double));
      swapped = bswap_64(swappable);
      memcpy(writer->buffer + writer->offset, &swapped, sizeof(double));
      writer->offset += sizeof(double);
    }
  } else {
    for (uint32_t i = 0; i < n; i++) {
      memcpy(writer->buffer + writer->offset, value + i, sizeof(double));
      writer->offset += sizeof(double);
    }
  }
}

static inline void wkb_writer_result_append(wkb_writer_t* writer, SEXP value) {
  R_xlen_t current_size = Rf_xlength(writer->result);
  if (writer->feat_id >= current_size) {
    SEXP new_result = PROTECT(Rf_allocVector(VECSXP, current_size * 2 + 1));
    for (R_xlen_t i = 0; i < current_size; i++) {
      SET_VECTOR_ELT(new_result, i, VECTOR_ELT(writer->result, i));
    }
    R_ReleaseObject(writer->result);
    writer->result = new_result;
    R_PreserveObject(writer->result);
    UNPROTECT(1);
  }

  SET_VECTOR_ELT(writer->result, writer->feat_id, value);
  writer->feat_id++;
}

static inline void wkb_writer_result_finalize(wkb_writer_t* writer) {
  R_xlen_t current_size = Rf_xlength(writer->result);
  if (writer->feat_id != current_size) {
    SEXP new_result = PROTECT(Rf_allocVector(VECSXP, writer->feat_id));
    for (R_xlen_t i = 0; i < writer->feat_id; i++) {
      SET_VECTOR_ELT(new_result, i, VECTOR_ELT(writer->result, i));
    }
    R_ReleaseObject(writer->result);
    writer->result = new_result;
    R_PreserveObject(writer->result);
    UNPROTECT(1);
  }
}

int wkb_writer_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  if (writer->result != R_NilValue) {
    Rf_error("Destination vector was already allocated");  // # nocov
  }

  if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
    writer->result = PROTECT(Rf_allocVector(VECSXP, 1024));
  } else {
    writer->result = PROTECT(Rf_allocVector(VECSXP, meta->size));
  }
  R_PreserveObject(writer->result);
  UNPROTECT(1);

  writer->feat_id = 0;

  return WK_CONTINUE;
}

int wkb_writer_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id,
                             void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  writer->offset = 0;
  writer->recursion_level = 0;
  return WK_CONTINUE;
}

int wkb_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id,
                              void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  if (writer->recursion_level > 0) {
    writer->current_size[writer->recursion_level - 1]++;
  }

  wkb_write_uchar(writer, writer->endian);
  wkb_write_uint(writer, wkb_writer_encode_type(meta, writer->recursion_level));
  if (writer->recursion_level == 0 && (meta->srid != WK_SRID_NONE)) {
    wkb_write_uint(writer, meta->srid);
  }

  if (meta->geometry_type != WK_POINT) {
    if (writer->recursion_level >= WKB_MAX_RECURSION_DEPTH) {
      Rf_error("Can't write WKB with maximum recursion depth greater than %d",
               WKB_MAX_RECURSION_DEPTH);
    }

    // reserve space for the size and record where it is
    writer->current_size_offset[writer->recursion_level] = writer->offset;
    writer->current_size[writer->recursion_level] = 0;
    wkb_write_uint(writer, 0);
  }

  // handle empty point as nan nan here (coord() will not get called)
  if (meta->geometry_type == WK_POINT && meta->size == 0) {
    int coord_size = 2;
    if (meta->flags & WK_FLAG_HAS_Z) coord_size++;
    if (meta->flags & WK_FLAG_HAS_M) coord_size++;
    double empty_coord[4];
    empty_coord[0] = NAN;
    empty_coord[1] = NAN;
    empty_coord[2] = NAN;
    empty_coord[3] = NAN;
    wkb_write_doubles(writer, empty_coord, coord_size);
  }

  writer->recursion_level++;

  return WK_CONTINUE;
}

int wkb_writer_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  writer->recursion_level--;
  if (meta->geometry_type != WK_POINT) {
    wkb_write_uint_offset(writer, writer->current_size[writer->recursion_level],
                          writer->current_size_offset[writer->recursion_level]);
  }
  return WK_CONTINUE;
}

int wkb_writer_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id,
                          void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  writer->current_size[writer->recursion_level - 1]++;

  if (writer->recursion_level >= WKB_MAX_RECURSION_DEPTH) {
    Rf_error("Can't write WKB with maximum recursion depth greater than %d",
             WKB_MAX_RECURSION_DEPTH);
  }

  writer->current_size_offset[writer->recursion_level] = writer->offset;
  writer->current_size[writer->recursion_level] = 0;
  wkb_write_uint(writer, 0);
  writer->recursion_level++;

  return WK_CONTINUE;
}

int wkb_writer_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id,
                        void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  writer->recursion_level--;
  wkb_write_uint_offset(writer, writer->current_size[writer->recursion_level],
                        writer->current_size_offset[writer->recursion_level]);
  return WK_CONTINUE;
}

int wkb_writer_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id,
                     void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  writer->current_size[writer->recursion_level - 1]++;
  int n_dim =
      2 + ((meta->flags & WK_FLAG_HAS_Z) != 0) + ((meta->flags & WK_FLAG_HAS_M) != 0);
  wkb_write_doubles(writer, coord, n_dim);
  return WK_CONTINUE;
}

int wkb_writer_feature_null(void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  wkb_writer_result_append(writer, R_NilValue);
  return WK_ABORT_FEATURE;
}

int wkb_writer_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id,
                           void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  SEXP item = PROTECT(Rf_allocVector(RAWSXP, writer->offset));
  memcpy(RAW(item), writer->buffer, writer->offset);
  wkb_writer_result_append(writer, item);
  UNPROTECT(1);
  return WK_CONTINUE;
}

SEXP wkb_writer_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;

  wkb_writer_result_finalize(writer);

  SEXP wkb_class = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(wkb_class, 0, Rf_mkChar("wk_wkb"));
  SET_STRING_ELT(wkb_class, 1, Rf_mkChar("wk_vctr"));
  Rf_setAttrib(writer->result, R_ClassSymbol, wkb_class);
  UNPROTECT(1);

  return writer->result;
}

void wkb_writer_deinitialize(void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  if (writer->result != R_NilValue) {
    R_ReleaseObject(writer->result);
    writer->result = R_NilValue;
  }
}

void wkb_writer_finalize(void* handler_data) {
  wkb_writer_t* writer = (wkb_writer_t*)handler_data;
  if (writer != NULL) {
    free(writer->buffer);
    free(writer);
  }
}

SEXP wk_c_wkb_writer_new(SEXP buffer_size_sexp, SEXP endian_sexp) {
  int endian = INTEGER(endian_sexp)[0];
  int buffer_size = INTEGER(buffer_size_sexp)[0];

  if (endian == NA_INTEGER) {
    endian = wkb_writer_platform_endian();
  } else if (endian) {
    endian = 1;
  }

  // If the initial buffer is too small, illegal reads can occur
  // and cause R to crash. The smallest value that doesn't cause a
  // crash is probably much less than 1024, but since this alloc
  // only happens once, we set the minimum size to 1024 here.
  if (buffer_size < 1024) {
    buffer_size = 1024;
  }

  wk_handler_t* handler = wk_handler_create();

  handler->vector_start = &wkb_writer_vector_start;
  handler->feature_start = &wkb_writer_feature_start;
  handler->geometry_start = &wkb_writer_geometry_start;
  handler->ring_start = &wkb_writer_ring_start;
  handler->coord = &wkb_writer_coord;
  handler->ring_end = &wkb_writer_ring_end;
  handler->geometry_end = &wkb_writer_geometry_end;
  handler->null_feature = &wkb_writer_feature_null;
  handler->feature_end = &wkb_writer_feature_end;
  handler->vector_end = &wkb_writer_vector_end;
  handler->deinitialize = &wkb_writer_deinitialize;
  handler->finalizer = &wkb_writer_finalize;

  handler->handler_data = wkb_writer_new(buffer_size, endian);
  if (handler->handler_data == NULL) {
    wk_handler_destroy(handler);               // # nocov
    Rf_error("Failed to alloc handler data");  // # nocov
  }

  SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
  return xptr;
}
