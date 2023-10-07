
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <memory.h>
#include <stdarg.h>
#include <stdint.h>
#include "altrep.h"
#include "wk-v1.h"

#define WK_DEFAULT_ERROR_CODE 0
#define WK_NO_ERROR_CODE -1

// IS_BIG_ENDIAN, IS_LITTLE_ENDIAN, bswap_32(), bswap_64()
#include "port.h"

#define EWKB_Z_BIT 0x80000000
#define EWKB_M_BIT 0x40000000
#define EWKB_SRID_BIT 0x20000000

typedef struct {
  wk_handler_t* handler;
  R_xlen_t feat_id;
  SEXP buffer_sexp;
  R_xlen_t buffer_sexp_i;
#ifdef HAS_ALTREP_RAW
  unsigned char buffer[ALTREP_CHUNK_SIZE];
#else
  unsigned char* buffer;
#endif
  size_t size;
  size_t offset;
  char swap_endian;
  int error_code;
  char error_buf[1024];
} wkb_reader_t;

int wkb_read_geometry(wkb_reader_t* reader, uint32_t part_id);
int wkb_read_endian(wkb_reader_t* reader, unsigned char* value);
int wkb_read_geometry_type(wkb_reader_t* reader, wk_meta_t* meta);
int wkb_read_uint(wkb_reader_t* reader, uint32_t* value);
int wkb_read_coordinates(wkb_reader_t* reader, const wk_meta_t* meta, uint32_t n_coords,
                         int n_dim);
void wkb_read_set_errorf(wkb_reader_t* reader, const char* error_buf, ...);

static inline int wkb_read_check_buffer(wkb_reader_t* reader, R_xlen_t bytes) {
  R_xlen_t bytes_to_keep = reader->size - reader->offset;
  if ((bytes_to_keep - bytes) >= 0) {
    return WK_CONTINUE;
  }

#ifdef HAS_ALTREP_RAW
  // with ALTREP, we try to refill the buffer

  // We can do this without a memmove() by just issuing slightly overlapping
  // RAW_GET_REGION() calls, but there are some cases where this might cause
  // an altrep implementation to seek backwards in a file which is slow.
  if (bytes_to_keep > 0) {
    memmove(reader->buffer, reader->buffer + reader->offset, bytes_to_keep);
  }

  R_xlen_t new_bytes =
      RAW_GET_REGION(reader->buffer_sexp, reader->buffer_sexp_i,
                     ALTREP_CHUNK_SIZE - bytes_to_keep, reader->buffer + bytes_to_keep);
  reader->offset = 0;
  reader->buffer_sexp_i += new_bytes;
  reader->size = bytes_to_keep + new_bytes;
#else
  // without ALTREP, reader->size is the full length of the RAW() buffer, so we've
  // hit the end of it
  reader->size = 0;
  reader->buffer_sexp_i += reader->offset;
#endif

  if (reader->size == 0) {
    wkb_read_set_errorf(reader, "Unexpected end of buffer at %d bytes",
                        reader->buffer_sexp_i);
    return WK_ABORT_FEATURE;
  }

  return WK_CONTINUE;
}

#define HANDLE_OR_RETURN(expr) \
  result = expr;               \
  if (result != WK_CONTINUE) return result

#define HANDLE_CONTINUE_OR_BREAK(expr) \
  result = expr;                       \
  if (result == WK_ABORT_FEATURE)      \
    continue;                          \
  else if (result == WK_ABORT)         \
  break

int wkb_read_geometry(wkb_reader_t* reader, uint32_t part_id) {
  int result;

  unsigned char endian;
  HANDLE_OR_RETURN(wkb_read_endian(reader, &endian));

#ifdef IS_LITTLE_ENDIAN
  reader->swap_endian = endian != 1;
#else
  reader->swap_endian = endian != 0;
#endif

  wk_meta_t meta;
  WK_META_RESET(meta, WK_GEOMETRY);
  HANDLE_OR_RETURN(wkb_read_geometry_type(reader, &meta));
  int n_dim =
      2 + ((meta.flags & WK_FLAG_HAS_Z) != 0) + ((meta.flags & WK_FLAG_HAS_M) != 0);

  HANDLE_OR_RETURN(
      reader->handler->geometry_start(&meta, part_id, reader->handler->handler_data));

  switch (meta.geometry_type) {
    case WK_POINT:
    case WK_LINESTRING:
      HANDLE_OR_RETURN(wkb_read_coordinates(reader, &meta, meta.size, n_dim));
      break;
    case WK_POLYGON:
      for (uint32_t i = 0; i < meta.size; i++) {
        uint32_t n_coords;
        HANDLE_OR_RETURN(wkb_read_uint(reader, &n_coords));
        HANDLE_OR_RETURN(reader->handler->ring_start(&meta, n_coords, i,
                                                     reader->handler->handler_data));
        HANDLE_OR_RETURN(wkb_read_coordinates(reader, &meta, n_coords, n_dim));
        HANDLE_OR_RETURN(
            reader->handler->ring_end(&meta, n_coords, i, reader->handler->handler_data));
      }
      break;
    case WK_MULTIPOINT:
    case WK_MULTILINESTRING:
    case WK_MULTIPOLYGON:
    case WK_GEOMETRYCOLLECTION:
      for (uint32_t i = 0; i < meta.size; i++) {
        HANDLE_OR_RETURN(wkb_read_geometry(reader, i));
      }
      break;
    default:
      wkb_read_set_errorf(reader, "Unrecognized geometry type code '%d'",
                          meta.geometry_type);
      return WK_ABORT_FEATURE;
  }

  return reader->handler->geometry_end(&meta, part_id, reader->handler->handler_data);
}

int wkb_read_endian(wkb_reader_t* reader, unsigned char* value) {
  int result;
  HANDLE_OR_RETURN(wkb_read_check_buffer(reader, sizeof(unsigned char)));
  memcpy(value, reader->buffer + reader->offset, sizeof(unsigned char));
  reader->offset += sizeof(unsigned char);
  return WK_CONTINUE;
}

int wkb_read_uint(wkb_reader_t* reader, uint32_t* value) {
  int result;
  HANDLE_OR_RETURN(wkb_read_check_buffer(reader, sizeof(uint32_t)));
  if (reader->swap_endian) {
    uint32_t swappable;
    memcpy(&swappable, reader->buffer + reader->offset, sizeof(uint32_t));
    reader->offset += sizeof(uint32_t);
    *value = bswap_32(swappable);
  } else {
    memcpy(value, reader->buffer + reader->offset, sizeof(uint32_t));
    reader->offset += sizeof(uint32_t);
  }

  return WK_CONTINUE;
}

int wkb_read_geometry_type(wkb_reader_t* reader, wk_meta_t* meta) {
  int result;
  uint32_t geometry_type;
  HANDLE_OR_RETURN(wkb_read_uint(reader, &geometry_type));

  if (geometry_type & EWKB_Z_BIT) {
    meta->flags |= WK_FLAG_HAS_Z;
  }

  if (geometry_type & EWKB_M_BIT) {
    meta->flags |= WK_FLAG_HAS_M;
  }

  if (geometry_type & EWKB_SRID_BIT) {
    HANDLE_OR_RETURN(wkb_read_uint(reader, &(meta->srid)));
  }

  geometry_type = geometry_type & 0x0000ffff;

  if (geometry_type >= 3000) {
    meta->geometry_type = geometry_type - 3000;
    meta->flags |= WK_FLAG_HAS_Z;
    meta->flags |= WK_FLAG_HAS_M;
  } else if (geometry_type >= 2000) {
    meta->geometry_type = geometry_type - 2000;
    meta->flags |= WK_FLAG_HAS_M;
  } else if (geometry_type >= 1000) {
    meta->geometry_type = geometry_type - 1000;
    meta->flags |= WK_FLAG_HAS_Z;
  } else {
    meta->geometry_type = geometry_type;
  }

  if (meta->geometry_type == WK_POINT) {
    meta->size = 1;
  } else {
    HANDLE_OR_RETURN(wkb_read_uint(reader, &(meta->size)));
  }

  return WK_CONTINUE;
}

int wkb_read_coordinates(wkb_reader_t* reader, const wk_meta_t* meta, uint32_t n_coords,
                         int n_dim) {
  double coord[4];
  int result;

  if (reader->swap_endian) {
    uint64_t swappable, swapped;
    for (uint32_t i = 0; i < n_coords; i++) {
      HANDLE_OR_RETURN(wkb_read_check_buffer(reader, sizeof(uint64_t) * n_dim));

      for (int j = 0; j < n_dim; j++) {
        memcpy(&swappable, reader->buffer + reader->offset, sizeof(uint64_t));
        reader->offset += sizeof(double);

        swapped = bswap_64(swappable);
        memcpy(coord + j, &swapped, sizeof(double));
      }

      HANDLE_OR_RETURN(
          reader->handler->coord(meta, coord, i, reader->handler->handler_data));
    }
  } else {
    // seems to be slightly faster than memcpy(coord, ..., coord_size)
    uint64_t swappable;
    for (uint32_t i = 0; i < n_coords; i++) {
      HANDLE_OR_RETURN(wkb_read_check_buffer(reader, sizeof(uint64_t) * n_dim));
      for (int j = 0; j < n_dim; j++) {
        memcpy(&swappable, reader->buffer + reader->offset, sizeof(uint64_t));
        reader->offset += sizeof(double);
        memcpy(coord + j, &swappable, sizeof(double));
      }

      HANDLE_OR_RETURN(
          reader->handler->coord(meta, coord, i, reader->handler->handler_data));
    }
  }

  return WK_CONTINUE;
}

void wkb_read_set_errorf(wkb_reader_t* reader, const char* error_buf, ...) {
  reader->error_code = WK_DEFAULT_ERROR_CODE;
  va_list args;
  va_start(args, error_buf);
  vsnprintf(reader->error_buf, 1024, error_buf, args);
  va_end(args);
}

SEXP wkb_read_wkb(SEXP data, wk_handler_t* handler) {
  R_xlen_t n_features = Rf_xlength(data);

  wk_vector_meta_t vector_meta;
  WK_VECTOR_META_RESET(vector_meta, WK_GEOMETRY);
  vector_meta.size = n_features;
  vector_meta.flags |= WK_FLAG_DIMS_UNKNOWN;

  if (handler->vector_start(&vector_meta, handler->handler_data) == WK_CONTINUE) {
    int result;
    SEXP item;
    wkb_reader_t reader;
    reader.handler = handler;
    memset(reader.error_buf, 0, 1024);

    for (R_xlen_t i = 0; i < n_features; i++) {
      // each feature could be huge, so check frequently
      if (((i + 1) % 1000) == 0) R_CheckUserInterrupt();

      reader.feat_id = i;
      item = VECTOR_ELT(data, i);

      HANDLE_CONTINUE_OR_BREAK(
          handler->feature_start(&vector_meta, i, handler->handler_data));

      if (item == R_NilValue) {
        HANDLE_CONTINUE_OR_BREAK(handler->null_feature(handler->handler_data));
      } else {
        reader.buffer_sexp = item;
        reader.buffer_sexp_i = 0;
        reader.offset = 0;

#ifdef HAS_ALTREP_RAW
        reader.size = 0;
#else
        reader.size = Rf_xlength(item);
        reader.buffer = RAW(item);
#endif
        reader.error_code = WK_NO_ERROR_CODE;
        reader.error_buf[0] = '\0';

        result = wkb_read_geometry(&reader, WK_PART_ID_NONE);
        if (result == WK_ABORT_FEATURE && reader.error_code != WK_NO_ERROR_CODE) {
          result = handler->error(reader.error_buf, handler->handler_data);
        }

        if (result == WK_ABORT_FEATURE) {
          continue;
        } else if (result == WK_ABORT) {
          break;
        }
      }

      if (handler->feature_end(&vector_meta, i, handler->handler_data) == WK_ABORT) {
        break;
      }
    }
  }

  SEXP result = PROTECT(handler->vector_end(&vector_meta, handler->handler_data));
  UNPROTECT(1);
  return result;
}

SEXP wk_c_read_wkb(SEXP data, SEXP handler_xptr) {
  return wk_handler_run_xptr(&wkb_read_wkb, data, handler_xptr);
}
