
#include "wk-v1.h"
#include <memory.h>
#include <stdint.h>
#include <Rinternals.h>

#define SWAP_UINT32(x) (((x) >> 24) | (((x) & 0x00FF0000) >> 8) | (((x) & 0x0000FF00) << 8) | ((x) << 24))
#define SWAP_UINT64(x)                                         \
x = (x & 0x00000000FFFFFFFF) << 32 | (x & 0xFFFFFFFF00000000) >> 32;\
x = (x & 0x0000FFFF0000FFFF) << 16 | (x & 0xFFFF0000FFFF0000) >> 16;\
x = (x & 0x00FF00FF00FF00FF) << 8  | (x & 0xFF00FF00FF00FF00) >> 8;

typedef struct {
  R_xlen_t featureId;
  char* buffer;
  size_t size;
  size_t offset;
  char swapEndian;
} WKBBuffer;

char wkb_read_feature(WKV1_Handler handler, SEXP item);
char wkb_read_geometry(WKV1_Handler handler, WKBBuffer* buffer);

char wkb_read_point();
char wkb_read_coordinates();
char wkb_read_polygon();
char wkb_read_collection();
char wkb_read_char(WKV1_Handler* handler, WKBBuffer* buffer, unsigned char* value);
char wkb_read_uint(WKV1_Handler* handler, WKBBuffer* buffer, unsigned int* value);
char wkb_read_coordinates(WKV1_Handler* handler, WKBBuffer* buffer, WKV1_GeometryMeta* meta);
char wkb_check_buffer(WKV1_Handler* handler, WKBBuffer* buffer, size_t bytes);
unsigned char wkb_platform_endian();

SEXP wk_c_read_wkb(SEXP data, SEXP handlerXptr) {
  R_xlen_t size = Rf_xlength(data);
  WKV1_Handler* handler = (WKV1_Handler*) R_ExternalPtrAddr(handlerXptr);

  WKV1_GeometryMeta vectorMeta;
  WKV1_META_RESET(vectorMeta, WKV1_InvalidGeometryType);
  WKV1_META_SET_SIZE(vectorMeta, size);

  if (handler->vectorStart(&vectorMeta, handler->userData) == WKV1_CONTINUE) {
    SEXP item;
    unsigned char* buffer;
    size_t offset;

    for (R_xlen_t i = 0; i < size; i++) {
      item = VECTOR_ELT(data, i);
    }
  }

  SEXP result = PROTECT(handler->vectorEnd(&vectorMeta, handler->userData));
  handler->finalizer(handler->userData);
  UNPROTECT(1);
  return result;
}

char wkb_read_endian(WKV1_Handler* handler, WKBBuffer* buffer) {
  if (wkb_check_buffer(handler, buffer, 1) == WKV1_CONTINUE) {
    unsigned char value;
    memcpy(&value, buffer->buffer, 1);
    buffer->offset += 1;
    buffer->swapEndian = value != wkb_platform_endian();
    return WKV1_CONTINUE;
  } else {
    return WKV1_STOP;
  }
}


char wkb_read_uint(WKV1_Handler* handler, WKBBuffer* buffer, unsigned int* value) {
  if (wkb_check_buffer(handler, buffer, sizeof(unsigned int)) == WKV1_CONTINUE) {
    memcpy(value, buffer->buffer, sizeof(unsigned int));
    buffer->offset += sizeof(unsigned int);
    if (buffer->swapEndian) {
      SWAP_UINT32(*value);
    }

    return WKV1_CONTINUE;
  } else {
    return WKV1_STOP;
  }
}

char wkb_read_coordinates(WKV1_Handler* handler, WKBBuffer* buffer, WKV1_GeometryMeta* meta, unsigned int nCoords) {
  int nDim = 2 + (meta->hasZ != 0) + (meta->hasM != 0);
  size_t coordSize = nDim * sizeof(double);
  if (wkb_check_buffer(handler, buffer, coordSize * nCoords) == WKV1_CONTINUE) {
    WKV1_Coord coord;

    if (buffer->swapEndian) {
      for (uint32_t i = 0; i < nCoords; i++) {
        memcpy(&coord, buffer->buffer, coordSize);
        buffer->offset += coordSize;

        for (int j = 0; j < nDim; j++) {
          uint64_t val;
          memcpy(&val, &(coord.v[j]), sizeof(double));
          SWAP_UINT64(val);
          memcpy(&(coord.v[j]), &val, sizeof(double));
        }

        if (handler->coord(meta, coord, nCoords, i, handler->userData) != WKV1_CONTINUE) {
          return WKV1_STOP;
        }
      }
    } else {
      for (uint32_t i = 0; i < nCoords; i++) {
        memcpy(&coord, buffer->buffer, coordSize);
        buffer->offset += coordSize;
        if (handler->coord(meta, coord, nCoords, i, handler->userData) != WKV1_CONTINUE) {
          return WKV1_STOP;
        }
      }
    }

    return WKV1_CONTINUE;
  } else {
    return WKV1_STOP;
  }
}

char wkb_check_buffer(WKV1_Handler* handler, WKBBuffer* buffer, size_t bytes) {
  if ((buffer->offset + bytes) < buffer->size) {
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

unsigned char wkb_platform_endian() {
  const int one = 1;
  unsigned char *cp = (unsigned char *) &one;
  return (char) *cp;
}
