
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <memory.h>
#include <stdint.h>
#include <stdarg.h>

// IS_BIG_ENDIAN, IS_LITTLE_ENDIAN, bswap_32(), bswap_64()
#include "port.h"

#define EWKB_Z_BIT 0x80000000
#define EWKB_M_BIT 0x40000000
#define EWKB_SRID_BIT 0x20000000

typedef struct {
  R_xlen_t feat_id;
  unsigned char* buffer;
  size_t size;
  size_t offset;
  char swap_endian;
  int error_code;
  char error_buf[1024];
} wkb_reader_t;

void wkb_parse_geometry_type(uint32_t geometry_type, wk_meta_t* meta);
void wkb_set_errorf(wkb_reader_t* reader, const char* error_buf, ...);
int wkb_read_geometry(const wk_handler_t* handler, wkb_reader_t* reader, uint32_t part_id);
int wkb_read_endian(const wk_handler_t* handler, wkb_reader_t* reader);
int wkb_read_uint(const wk_handler_t* handler, wkb_reader_t* reader, uint32_t* value);
int wkb_read_coordinates(const wk_handler_t* handler, wkb_reader_t* reader, const wk_meta_t* meta,
                         uint32_t nCoords);
int wkb_check_buffer(const wk_handler_t* handler, wkb_reader_t* reader, size_t bytes);
unsigned char wkb_platform_endian();
void memcpyrev(void* dst, unsigned char* src, size_t n);


#define HANDLE_OR_RETURN(expr)                                 \
  result = expr;                                               \
  if (result != WK_CONTINUE) return result

#define HANDLE_CONTINUE_OR_BREAK(expr)                         \
  result = expr;                                               \
  if (result == WK_ABORT_FEATURE) continue; else if (result == WK_ABORT) break

// parses both EWKB flags and the 1000-style WKB types
void wkb_parse_geometry_type(uint32_t geometry_type, wk_meta_t* meta) {
  if (geometry_type & EWKB_Z_BIT) {
    meta->flags |= WK_FLAG_HAS_Z;
  }

  if (geometry_type & EWKB_M_BIT) {
    meta->flags |= WK_FLAG_HAS_M;
  }

  geometry_type = geometry_type & 0x0000ffff;

  if (geometry_type >= 3000) {
    meta->geometry_type = geometry_type - 3000;
    meta->flags |= WK_FLAG_HAS_Z;
    meta->flags |= WK_FLAG_HAS_M;
  } else  if (geometry_type >= 2000) {
    meta->geometry_type = geometry_type - 2000;
    meta->flags |= WK_FLAG_HAS_M;
  } else if (geometry_type >= 1000) {
    meta->geometry_type = geometry_type - 1000;
    meta->flags |= WK_FLAG_HAS_Z;
  } else {
    meta->geometry_type = geometry_type;
  }
}


void wkb_set_errorf(wkb_reader_t* reader, const char* error_buf, ...) {
  reader->error_code = WK_DEFAULT_ERROR_CODE;
  va_list args;
  va_start(args, error_buf);
  vsnprintf(reader->error_buf, 1024, error_buf, args);
  va_end(args);
}

int wkb_read_geometry(const wk_handler_t* handler, wkb_reader_t* reader,
                       uint32_t part_id) {
  int result;
  HANDLE_OR_RETURN(wkb_read_endian(handler, reader));

  uint32_t geometry_type;
  HANDLE_OR_RETURN(wkb_read_uint(handler, reader, &geometry_type));

  wk_meta_t meta;
  WK_META_RESET(meta, WK_GEOMETRY);
  wkb_parse_geometry_type(geometry_type, &meta);
  if (meta.geometry_type < WK_POINT || meta.geometry_type > WK_GEOMETRYCOLLECTION) {
    wkb_set_errorf(reader, "Unrecognized geometry type code '%d'", meta.geometry_type);
    return WK_ABORT_FEATURE;
  }

  if ((geometry_type & EWKB_SRID_BIT) != 0) {
    HANDLE_OR_RETURN(wkb_read_uint(handler, reader, &(meta.srid)));
  }

  if (meta.geometry_type == WK_POINT) {
    meta.size = 1;
  } else {
    HANDLE_OR_RETURN(wkb_read_uint(handler, reader, &(meta.size)));
  }

  HANDLE_OR_RETURN(handler->geometry_start(&meta, part_id, handler->handler_data));

  switch (meta.geometry_type) {
  case WK_POINT:
  case WK_LINESTRING:
    HANDLE_OR_RETURN(wkb_read_coordinates(handler, reader, &meta, meta.size));
    break;
  case WK_POLYGON:
    for (uint32_t i = 0; i < meta.size; i++) {
      uint32_t nCoords;
      HANDLE_OR_RETURN(wkb_read_uint(handler, reader, &nCoords));
      HANDLE_OR_RETURN(handler->ring_start(&meta, nCoords, i, handler->handler_data));
      HANDLE_OR_RETURN(wkb_read_coordinates(handler, reader, &meta, nCoords));
      HANDLE_OR_RETURN(handler->ring_end(&meta, nCoords, i, handler->handler_data));
    }
    break;
  case WK_MULTIPOINT:
  case WK_MULTILINESTRING:
  case WK_MULTIPOLYGON:
  case WK_GEOMETRYCOLLECTION:
    for (uint32_t i = 0; i < meta.size; i++) {
      HANDLE_OR_RETURN(wkb_read_geometry(handler, reader, i));
    }
    break;
  default:
    // this should never be reached as the type is checked above
    wkb_set_errorf(reader, "Unrecognized geometry type code '%d'", meta.geometry_type); // # nocov
    return WK_ABORT_FEATURE; // # nocov
  }

  return handler->geometry_end(&meta, part_id, handler->handler_data);
}

inline int wkb_read_endian(const wk_handler_t* handler, wkb_reader_t* reader) {
  if (wkb_check_buffer(handler, reader, 1) == WK_CONTINUE) {
    unsigned char value;
    memcpy(&value, &(reader->buffer[reader->offset]), 1);
    reader->offset += 1;
    reader->swap_endian = value != wkb_platform_endian();
    return WK_CONTINUE;
  } else {
    return WK_ABORT_FEATURE;
  }
}

inline int wkb_read_uint(const wk_handler_t* handler, wkb_reader_t* reader, uint32_t* value) {
  if (wkb_check_buffer(handler, reader, sizeof(uint32_t)) == WK_CONTINUE) {
    if (reader->swap_endian) {
      memcpyrev(value, &(reader->buffer[reader->offset]), sizeof(uint32_t));
      reader->offset += sizeof(uint32_t);
    } else {
      memcpy(value, &(reader->buffer[reader->offset]), sizeof(uint32_t));
      reader->offset += sizeof(uint32_t);
    }

    return WK_CONTINUE;
  } else {
    return WK_ABORT_FEATURE;
  }
}

inline int wkb_read_coordinates(const wk_handler_t* handler, wkb_reader_t* reader,
                                 const wk_meta_t* meta, uint32_t nCoords) {
  int nDim = 2 + ((meta->flags & WK_FLAG_HAS_Z )!= 0) + ((meta->flags & WK_FLAG_HAS_M) != 0);
  size_t coordSize = nDim * sizeof(double);

  if (wkb_check_buffer(handler, reader, coordSize * nCoords) != WK_CONTINUE) {
    return WK_ABORT_FEATURE;
  }

  wk_coord_t coord;
  int result;

  if (reader->swap_endian) {
    for (uint32_t i = 0; i < nCoords; i++) {
      for (int j = 0; j < nDim; j++) {
        memcpyrev(&(coord.v[j]), &(reader->buffer[reader->offset]), sizeof(double));
        reader->offset += sizeof(double);
      }

      HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
    }
  } else {
    for (uint32_t i = 0; i < nCoords; i++) {
      memcpy(&coord, &(reader->buffer[reader->offset]), coordSize);
      reader->offset += coordSize;
      HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
    }
  }

  return WK_CONTINUE;
}

inline int wkb_check_buffer(const wk_handler_t* handler, wkb_reader_t* reader, size_t bytes) {
  if ((reader->offset + bytes) <= reader->size) {
    return WK_CONTINUE;
  } else {
    wkb_set_errorf(reader, "Unexpected end of buffer (%d/%d)", reader->offset + bytes, reader->size);
    return WK_ABORT_FEATURE;
  }
}

inline unsigned char wkb_platform_endian() {
  const int one = 1;
  unsigned char *cp = (unsigned char *) &one;
  return (char) *cp;
}

inline void memcpyrev(void* dst, unsigned char* src, size_t n) {
  unsigned char* dstChar = (unsigned char*) dst;
  for (size_t i = 0; i < n; i++) {
    memcpy(&(dstChar[n - i - 1]), &(src[i]), 1);
  }
}

SEXP wkb_read_wkb(SEXP data, wk_handler_t* handler) {
  R_xlen_t n_features = Rf_xlength(data);

  wk_vector_meta_t vectorMeta;
  WK_VECTOR_META_RESET(vectorMeta, WK_GEOMETRY);
  vectorMeta.size = n_features;
  vectorMeta.flags |= WK_FLAG_DIMS_UNKNOWN;

  if (handler->vector_start(&vectorMeta, handler->handler_data) == WK_CONTINUE) {
    int result;
    SEXP item;
    wkb_reader_t buffer;

    for (R_xlen_t i = 0; i < n_features; i++) {
      if (((i + 1) % 1000) == 0) R_CheckUserInterrupt();
      
      buffer.feat_id = i;
      item = VECTOR_ELT(data, i);

      HANDLE_CONTINUE_OR_BREAK(handler->feature_start(&vectorMeta, i, handler->handler_data));

      if (item == R_NilValue) {
        HANDLE_CONTINUE_OR_BREAK(handler->null_feature(&vectorMeta, i, handler->handler_data));
      } else {
        buffer.buffer = RAW(item);
        buffer.size = Rf_xlength(item);
        buffer.offset = 0;
        buffer.error_code = WK_NO_ERROR_CODE;
        memset(buffer.error_buf, 0, 1024);

        result = wkb_read_geometry(handler, &buffer, WK_PART_ID_NONE);
        if (result == WK_ABORT_FEATURE && buffer.error_code != WK_NO_ERROR_CODE) {
          result = handler->error(i, buffer.error_code, buffer.error_buf, handler->handler_data);
        }

        if (result == WK_ABORT_FEATURE) {
          continue;
        } else if (result == WK_ABORT) {
          break;
        }
      }

      if (handler->feature_end(&vectorMeta, i, handler->handler_data) == WK_ABORT) {
        break;
      }
    }
  }

  SEXP result = PROTECT(handler->vector_end(&vectorMeta, handler->handler_data));
  UNPROTECT(1);
  return result;
}

SEXP wk_c_read_wkb(SEXP data, SEXP handlerXptr) {
  return wk_handler_run_xptr(&wkb_read_wkb, data, handlerXptr);
}
