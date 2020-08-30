
#include "wk-v1.h"
#include <memory.h>
#include <stdint.h>
#include <Rinternals.h>

#define EWKB_Z_BIT    0x80000000
#define EWKB_M_BIT    0x40000000
#define EWKB_SRID_BIT 0x20000000

typedef struct {
  R_xlen_t featureId;
  unsigned char* buffer;
  size_t size;
  size_t offset;
  char swapEndian;
} WKBBuffer;

char wkb_read_geometry(const WKV1_Handler* handler, WKBBuffer* buffer, uint32_t nParts, uint32_t partId, int recursiveLevel);

char wkb_read_endian(const WKV1_Handler* handler, WKBBuffer* buffer);
char wkb_read_uint(const WKV1_Handler* handler, WKBBuffer* buffer, uint32_t* value);
char wkb_read_coordinates(const WKV1_Handler* handler, WKBBuffer* buffer, const WKV1_GeometryMeta* meta, uint32_t nCoords);
char wkb_check_buffer(const WKV1_Handler* handler, WKBBuffer* buffer, size_t bytes);
unsigned char wkb_platform_endian();
void memcpyrev(void* dst, unsigned char* src, size_t n);

SEXP wk_c_read_wkb(SEXP data, SEXP handlerXptr) {
  R_xlen_t nFeatures = Rf_xlength(data);
  WKV1_Handler* handler = (WKV1_Handler*) R_ExternalPtrAddr(handlerXptr);

  WKV1_GeometryMeta vectorMeta;
  WKV1_META_RESET(vectorMeta, WKV1_InvalidGeometryType);
  WKV1_META_SET_SIZE(vectorMeta, nFeatures);

  if (handler->vectorStart(&vectorMeta, handler->userData) == WKV1_CONTINUE) {
    SEXP item;
    WKBBuffer buffer;

    for (R_xlen_t i = 0; i < nFeatures; i++) {
      item = VECTOR_ELT(data, i);

      if (handler->featureStart(&vectorMeta, nFeatures, i, handler->userData)  != WKV1_CONTINUE) {
        break;
      }

      if ((item == R_NilValue) &&
          (handler->nullFeature(&vectorMeta, nFeatures, i, handler->userData) != WKV1_CONTINUE)) {
        break;
      }

      buffer.featureId = i;
      buffer.buffer = RAW(item);
      buffer.size = Rf_xlength(item);
      buffer.offset = 0;
      if (wkb_read_geometry(handler, &buffer, WKV1_PART_ID_NONE, WKV1_PART_ID_NONE, 0) != WKV1_CONTINUE) {
        break;
      }

      if (handler->featureEnd(&vectorMeta, nFeatures, i, handler->userData) != WKV1_CONTINUE) {
        break;
      }
    }
  }

  SEXP result = PROTECT(handler->vectorEnd(&vectorMeta, handler->userData));
  handler->finalizer(handler->userData);
  UNPROTECT(1);
  return result;
}

char wkb_read_geometry(const WKV1_Handler* handler, WKBBuffer* buffer,
                       uint32_t nParts, uint32_t partId, int recursiveLevel) {
  if (wkb_read_endian(handler, buffer) != WKV1_CONTINUE) {
    return WKV1_STOP;
  }

  uint32_t geometryType;
  if (wkb_read_uint(handler, buffer, &geometryType) != WKV1_CONTINUE) {
    return WKV1_STOP;
  }

  WKV1_GeometryMeta meta;
  WKV1_META_RESET(meta, geometryType & 0x000000ff)
  meta.recursiveLevel = recursiveLevel;
  meta.hasZ = (geometryType & EWKB_Z_BIT) != 0;
  meta.hasM = (geometryType & EWKB_M_BIT) != 0;
  meta.hasSrid = (geometryType & EWKB_SRID_BIT) != 0;

  if (meta.hasSrid &&
      (wkb_read_uint(handler, buffer, &(meta.srid)) != WKV1_CONTINUE)) {
    return WKV1_STOP;
  }

  meta.hasSize = 1;
  if (meta.geometryType == WKV1_Point) {
    meta.size = 1;
  } else if (wkb_read_uint(handler, buffer, &(meta.size)) != WKV1_CONTINUE) {
    return WKV1_STOP;
  }

  if (handler->geometryStart(&meta, WKV1_PART_ID_NONE, WKV1_PART_ID_NONE, handler->userData) != WKV1_CONTINUE) {
    return WKV1_STOP;
  }

  switch (meta.geometryType) {
  case WKV1_Point:
  case WKV1_LineString:
    if (wkb_read_coordinates(handler, buffer, &meta, meta.size) != WKV1_CONTINUE) {
      return WKV1_STOP;
    }
    break;
  case WKV1_Polygon:
    for (uint32_t i = 0; i < meta.size; i++) {
      if (handler->ringStart(&meta, meta.size, i, handler->userData) != WKV1_CONTINUE) {
        return WKV1_STOP;
      }

      uint32_t nCoords;
      if (wkb_read_uint(handler, buffer, &nCoords) != WKV1_CONTINUE) {
        return WKV1_STOP;
      }

      if (wkb_read_coordinates(handler, buffer, &meta, nCoords) != WKV1_CONTINUE) {
        return WKV1_STOP;
      }

      if (handler->ringEnd(&meta, meta.size, i, handler->userData) != WKV1_CONTINUE) {
        return WKV1_STOP;
      }
    }
    break;
  case WKV1_MultiPoint:
  case WKV1_MultiLineString:
  case WKV1_MultiPolygon:
  case WKV1_GeometryCollection:
    for (uint32_t i = 0; i < meta.size; i++) {
      if (wkb_read_geometry(handler, buffer, meta.size, i, meta.recursiveLevel + 1) != WKV1_CONTINUE) {
        return WKV1_STOP;
      }
    }
    break;
  default:
    return handler->error(
        buffer->featureId,
        WKV1_DEFAULT_ERROR_CODE,
        "Unrecognized geometry type code",
        handler->userData
    );
  }

  return handler->geometryEnd(&meta, WKV1_PART_ID_NONE, WKV1_PART_ID_NONE, handler->userData);
}

inline char wkb_read_endian(const WKV1_Handler* handler, WKBBuffer* buffer) {
  if (wkb_check_buffer(handler, buffer, 1) == WKV1_CONTINUE) {
    unsigned char value;
    memcpy(&value, &(buffer->buffer[buffer->offset]), 1);
    buffer->offset += 1;
    buffer->swapEndian = value != wkb_platform_endian();
    return WKV1_CONTINUE;
  } else {
    return WKV1_STOP;
  }
}

inline char wkb_read_uint(const WKV1_Handler* handler, WKBBuffer* buffer, uint32_t* value) {
  if (wkb_check_buffer(handler, buffer, sizeof(uint32_t)) == WKV1_CONTINUE) {
    if (buffer->swapEndian) {
      memcpyrev(value, &(buffer->buffer[buffer->offset]), sizeof(uint32_t));
      buffer->offset += sizeof(uint32_t);
    } else {
      memcpy(value, &(buffer->buffer[buffer->offset]), sizeof(uint32_t));
      buffer->offset += sizeof(uint32_t);
    }

    return WKV1_CONTINUE;
  } else {
    return WKV1_STOP;
  }
}

inline char wkb_read_coordinates(const WKV1_Handler* handler, WKBBuffer* buffer, const WKV1_GeometryMeta* meta, uint32_t nCoords) {
  int nDim = 2 + (meta->hasZ != 0) + (meta->hasM != 0);
  size_t coordSize = nDim * sizeof(double);

  if (wkb_check_buffer(handler, buffer, coordSize * nCoords) != WKV1_CONTINUE) {
    return WKV1_STOP;
  }

  WKV1_Coord coord;

  if (buffer->swapEndian) {
    for (uint32_t i = 0; i < nCoords; i++) {
      for (int j = 0; j < nDim; j++) {
        memcpyrev(&(coord.v[j]), &(buffer->buffer[buffer->offset]), sizeof(double));
        buffer->offset += sizeof(double);
      }

      if (handler->coord(meta, coord, nCoords, i, handler->userData) != WKV1_CONTINUE) {
        return WKV1_STOP;
      }
    }
  } else {
    for (uint32_t i = 0; i < nCoords; i++) {
      memcpy(&coord, &(buffer->buffer[buffer->offset]), coordSize);
      buffer->offset += coordSize;
      if (handler->coord(meta, coord, nCoords, i, handler->userData) != WKV1_CONTINUE) {
        return WKV1_STOP;
      }
    }
  }

  return WKV1_CONTINUE;
}

inline char wkb_check_buffer(const WKV1_Handler* handler, WKBBuffer* buffer, size_t bytes) {
  if ((buffer->offset + bytes) <= buffer->size) {
    return WKV1_CONTINUE;
  } else {
    return handler->error(
        buffer->featureId,
        WKV1_DEFAULT_ERROR_CODE,
        "Unexpected end of buffer",
        handler->userData
    );
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
