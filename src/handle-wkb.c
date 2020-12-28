
#include "wk-v1.h"
#include <memory.h>
#include <stdint.h>
#include <stdarg.h>
#include <Rinternals.h>

#define EWKB_Z_BIT    0x80000000
#define EWKB_M_BIT    0x40000000
#define EWKB_SRID_BIT 0x20000000

#define HANDLE_OR_RETURN(expr)                                 \
  result = expr;                                               \
  if (result != WK_CONTINUE) return result

typedef struct {
  R_xlen_t featureId;
  unsigned char* buffer;
  size_t size;
  size_t offset;
  char swapEndian;
  int errorCode;
  char errorMessage[1024];
} WKBBuffer_t;

void wkb_set_errorf(WKBBuffer_t* buffer, const char* errorMessage, ...) {
  buffer->errorCode = WK_DEFAULT_ERROR_CODE;
  va_list args;
  va_start(args, errorMessage);
  vsnprintf(buffer->errorMessage, 1024, errorMessage, args);
  va_end(args);
}

SEXP wkb_read_wkb(SEXP data, WKHandler_t* handler);
char wkb_read_geometry(const WKHandler_t* handler, WKBBuffer_t* buffer, uint32_t nParts, uint32_t partId);

char wkb_read_endian(const WKHandler_t* handler, WKBBuffer_t* buffer);
char wkb_read_uint(const WKHandler_t* handler, WKBBuffer_t* buffer, uint32_t* value);
char wkb_read_coordinates(const WKHandler_t* handler, WKBBuffer_t* buffer, const WKGeometryMeta_t* meta,
                          uint32_t nCoords);
char wkb_check_buffer(const WKHandler_t* handler, WKBBuffer_t* buffer, size_t bytes);
unsigned char wkb_platform_endian();
void memcpyrev(void* dst, unsigned char* src, size_t n);

SEXP wk_c_read_wkb(SEXP data, SEXP handlerXptr) {
  return wk_handler_run_xptr(&wkb_read_wkb, data, handlerXptr);
}

SEXP wkb_read_wkb(SEXP data, WKHandler_t* handler) {
  R_xlen_t nFeatures = Rf_xlength(data);

  WKGeometryMeta_t vectorMeta;
  WK_META_RESET(vectorMeta, WK_GEOMETRY);
  WK_META_SET_SIZE(vectorMeta, nFeatures);

  if (handler->vectorStart(&vectorMeta, handler->userData) == WK_CONTINUE) {
    SEXP item;
    WKBBuffer_t buffer;

    for (R_xlen_t i = 0; i < nFeatures; i++) {
      item = VECTOR_ELT(data, i);

      if (handler->featureStart(&vectorMeta, nFeatures, i, handler->userData)  != WK_CONTINUE) {
        break;
      }

      if ((item == R_NilValue) &&
          (handler->nullFeature(&vectorMeta, nFeatures, i, handler->userData) != WK_CONTINUE)) {
        break;
      }

      buffer.featureId = i;
      buffer.buffer = RAW(item);
      buffer.size = Rf_xlength(item);
      buffer.offset = 0;
      buffer.errorCode = WK_NO_ERROR_CODE;
      memset(buffer.errorMessage, 0, 1024);

      char result = wkb_read_geometry(handler, &buffer, WK_PART_ID_NONE, WK_PART_ID_NONE);

      if (result == WK_ABORT_FEATURE && buffer.errorCode != WK_NO_ERROR_CODE) {
        result = handler->error(i, buffer.errorCode, buffer.errorMessage, handler->userData);
      }

      if (result == WK_ABORT) {
        break;
      }

      if (handler->featureEnd(&vectorMeta, nFeatures, i, handler->userData) == WK_ABORT) {
        break;
      }
    }
  }

  SEXP result = PROTECT(handler->vectorEnd(&vectorMeta, handler->userData));
  UNPROTECT(1);
  return result;
}

char wkb_read_geometry(const WKHandler_t* handler, WKBBuffer_t* buffer,
                       uint32_t nParts, uint32_t partId) {
  if (wkb_read_endian(handler, buffer) != WK_CONTINUE) {
    return WK_ABORT_FEATURE;
  }

  uint32_t geometryType;
  if (wkb_read_uint(handler, buffer, &geometryType) != WK_CONTINUE) {
    return WK_ABORT_FEATURE;
  }

  WKGeometryMeta_t meta;
  WK_META_RESET(meta, geometryType & 0x000000ff)
  meta.hasZ = (geometryType & EWKB_Z_BIT) != 0;
  meta.hasM = (geometryType & EWKB_M_BIT) != 0;
  meta.hasSrid = (geometryType & EWKB_SRID_BIT) != 0;

  if (meta.hasSrid &&
      (wkb_read_uint(handler, buffer, &(meta.srid)) != WK_CONTINUE)) {
    return WK_ABORT_FEATURE;
  }

  meta.hasSize = 1;
  if (meta.geometryType == WK_POINT) {
    meta.size = 1;
  } else if (wkb_read_uint(handler, buffer, &(meta.size)) != WK_CONTINUE) {
    return WK_ABORT_FEATURE;
  }

  char result;
  HANDLE_OR_RETURN(handler->geometryStart(&meta, WK_PART_ID_NONE, WK_PART_ID_NONE, handler->userData));

  switch (meta.geometryType) {
  case WK_POINT:
  case WK_LINESTRING:
    HANDLE_OR_RETURN(wkb_read_coordinates(handler, buffer, &meta, meta.size));
    break;
  case WK_POLYGON:
    for (uint32_t i = 0; i < meta.size; i++) {
      uint32_t nCoords;
      HANDLE_OR_RETURN(wkb_read_uint(handler, buffer, &nCoords));
      HANDLE_OR_RETURN(handler->ringStart(&meta, nCoords, meta.size, i, handler->userData));
      HANDLE_OR_RETURN(wkb_read_coordinates(handler, buffer, &meta, nCoords));
      HANDLE_OR_RETURN(handler->ringEnd(&meta, nCoords, meta.size, i, handler->userData));
    }
    break;
  case WK_MULTIPOINT:
  case WK_MULTILINESTRING:
  case WK_MULTIPOLYGON:
  case WK_GEOMETRYCOLLECTION:
    for (uint32_t i = 0; i < meta.size; i++) {
      HANDLE_OR_RETURN(wkb_read_geometry(handler, buffer, meta.size, i));
    }
    break;
  default:
    wkb_set_errorf(buffer, "Unrecognized geometry type code: %d", meta.geometryType);
    return WK_ABORT_FEATURE;
  }

  return handler->geometryEnd(&meta, WK_PART_ID_NONE, WK_PART_ID_NONE, handler->userData);
}

inline char wkb_read_endian(const WKHandler_t* handler, WKBBuffer_t* buffer) {
  if (wkb_check_buffer(handler, buffer, 1) == WK_CONTINUE) {
    unsigned char value;
    memcpy(&value, &(buffer->buffer[buffer->offset]), 1);
    buffer->offset += 1;
    buffer->swapEndian = value != wkb_platform_endian();
    return WK_CONTINUE;
  } else {
    return WK_ABORT_FEATURE;
  }
}

inline char wkb_read_uint(const WKHandler_t* handler, WKBBuffer_t* buffer, uint32_t* value) {
  if (wkb_check_buffer(handler, buffer, sizeof(uint32_t)) == WK_CONTINUE) {
    if (buffer->swapEndian) {
      memcpyrev(value, &(buffer->buffer[buffer->offset]), sizeof(uint32_t));
      buffer->offset += sizeof(uint32_t);
    } else {
      memcpy(value, &(buffer->buffer[buffer->offset]), sizeof(uint32_t));
      buffer->offset += sizeof(uint32_t);
    }

    return WK_CONTINUE;
  } else {
    return WK_ABORT_FEATURE;
  }
}

inline char wkb_read_coordinates(const WKHandler_t* handler, WKBBuffer_t* buffer,
                                 const WKGeometryMeta_t* meta, uint32_t nCoords) {
  int nDim = 2 + (meta->hasZ != 0) + (meta->hasM != 0);
  size_t coordSize = nDim * sizeof(double);

  if (wkb_check_buffer(handler, buffer, coordSize * nCoords) != WK_CONTINUE) {
    return WK_ABORT_FEATURE;
  }

  WKCoord_t coord;
  char result;

  if (buffer->swapEndian) {
    for (uint32_t i = 0; i < nCoords; i++) {
      for (int j = 0; j < nDim; j++) {
        memcpyrev(&(coord.v[j]), &(buffer->buffer[buffer->offset]), sizeof(double));
        buffer->offset += sizeof(double);
      }

      HANDLE_OR_RETURN(handler->coord(meta, coord, nCoords, i, handler->userData));
    }
  } else {
    for (uint32_t i = 0; i < nCoords; i++) {
      memcpy(&coord, &(buffer->buffer[buffer->offset]), coordSize);
      buffer->offset += coordSize;
      HANDLE_OR_RETURN(handler->coord(meta, coord, nCoords, i, handler->userData));
    }
  }

  return WK_CONTINUE;
}

inline char wkb_check_buffer(const WKHandler_t* handler, WKBBuffer_t* buffer, size_t bytes) {
  if ((buffer->offset + bytes) <= buffer->size) {
    return WK_CONTINUE;
  } else {
    wkb_set_errorf(buffer, "Unexpected end of buffer (%d/%d)", buffer->offset + bytes, buffer->size);
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
