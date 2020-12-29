
#include "wk-v1.h"
#include <stdlib.h>
#include <memory.h>
#include <Rinternals.h>

#define EWKB_Z_BIT    0x80000000
#define EWKB_M_BIT    0x40000000
#define EWKB_SRID_BIT 0x20000000

typedef struct {
    SEXP result;
    unsigned char endian;
    unsigned char* buffer;
    size_t size;
    size_t offset;
} WKBWriteBuffer_t;

unsigned char wkb_writer_platform_endian() {
    const int one = 1;
    unsigned char *cp = (unsigned char *) &one;
    return (char) *cp;
}

uint32_t wkb_writer_encode_type(const WKGeometryMeta_t* meta) {
    uint32_t out = meta->geometryType;
    if (meta->hasZ) out |= EWKB_Z_BIT;
    if (meta->hasM) out |= EWKB_M_BIT;
    if (meta->srid != WK_SRID_NONE) out |= EWKB_SRID_BIT;
    return out;
  }

WKBWriteBuffer_t* wkb_write_buffer_new(size_t size) {
    unsigned char* buffer = malloc(size);
    if (buffer == NULL) {
        Rf_error("Can't allocate buffer of size %d", size); // # nocov
    }

    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) malloc(sizeof(WKBWriteBuffer_t));
    if (writeBuffer == NULL) {
        free(buffer); // # nocov
        Rf_error("Can't allocate WKBWriteBuffer_t"); // # nocov
    }

    writeBuffer->endian = wkb_writer_platform_endian();
    writeBuffer->result =  R_NilValue;
    writeBuffer->buffer = buffer;
    writeBuffer->size = size;
    writeBuffer->offset = 0;
    return writeBuffer;
}

void wkb_write_buffer_ensure_size(WKBWriteBuffer_t* writeBuffer, size_t size) {
    Rprintf("Attempting to realloc to size %d\n", size);

    unsigned char* newBuffer = realloc(writeBuffer->buffer, size);
    if (newBuffer == NULL) {
        Rf_error("Can't reallocate buffer of size %d", size);
    }

    writeBuffer->buffer = newBuffer;
    writeBuffer->size = size;
}

char wkb_write_buffer_has_space(WKBWriteBuffer_t* writeBuffer, size_t item) {
    return (writeBuffer->offset + item) <= writeBuffer->size;
}

void wkb_write_buffer_ensure_space(WKBWriteBuffer_t* writeBuffer, size_t item) {
    if (!wkb_write_buffer_has_space(writeBuffer, item)) {
        wkb_write_buffer_ensure_size(writeBuffer, writeBuffer->size * 2);
    }
}

void wkb_write_uint(WKBWriteBuffer_t* writeBuffer, const uint32_t value) {
    wkb_write_buffer_ensure_space(writeBuffer, sizeof(uint32_t));
    memcpy(writeBuffer->buffer + writeBuffer->offset, &value, sizeof(uint32_t));
    writeBuffer->offset += sizeof(uint32_t);
}

void wkb_write_uchar(WKBWriteBuffer_t* writeBuffer, const unsigned char value) {
    wkb_write_buffer_ensure_space(writeBuffer, sizeof(unsigned char));
    memcpy(writeBuffer->buffer + writeBuffer->offset, &value, sizeof(unsigned char));
    writeBuffer->offset += sizeof(unsigned char);
}

void wkb_write_doubles(WKBWriteBuffer_t* writeBuffer, const double* value, uint32_t nValues) {
    wkb_write_buffer_ensure_space(writeBuffer, sizeof(double) * nValues);
    memcpy(writeBuffer->buffer + writeBuffer->offset, value, sizeof(double) * nValues);
    writeBuffer->offset += sizeof(double) * nValues;
}

char wkb_writer_vector_start(const WKGeometryMeta_t* meta, void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    writeBuffer->result = Rf_allocVector(VECSXP, meta->size);
    R_PreserveObject(writeBuffer->result);
    return WK_CONTINUE;
}

char wkb_writer_feature_start(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    writeBuffer->offset = 0;
    return WK_CONTINUE;
}

char wkb_writer_geometry_start(const WKGeometryMeta_t* meta, uint32_t nParts, uint32_t partId, void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    wkb_write_uchar(writeBuffer, writeBuffer->endian);
    wkb_write_uint(writeBuffer, wkb_writer_encode_type(meta));
    if (meta->geometryType != WK_POINT) {
        wkb_write_uint(writeBuffer, meta->size);
    }
    return WK_CONTINUE;
}

char wkb_writer_ring_start(const WKGeometryMeta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId, void* userData) {
  WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
  wkb_write_uint(writeBuffer, size);
  return WK_CONTINUE;
}

char wkb_writer_coord(const WKGeometryMeta_t* meta, const WKCoord_t coord, uint32_t nCoords, uint32_t coordId,
                      void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    wkb_write_doubles(writeBuffer, coord.v, 2 + meta->hasZ + meta->hasM);
    return WK_CONTINUE;
}

char wkb_writer_feature_null(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    SET_VECTOR_ELT(writeBuffer->result, featureId, R_NilValue);
    return WK_ABORT_FEATURE;
}

char wkb_writer_feature_end(const WKGeometryMeta_t* meta, R_xlen_t nFeatures, R_xlen_t featureId, void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    SEXP item = PROTECT(Rf_allocVector(RAWSXP, writeBuffer->offset));
    memcpy(RAW(item), writeBuffer->buffer, writeBuffer->offset);
    SET_VECTOR_ELT(writeBuffer->result, featureId, item);
    UNPROTECT(1);
    return WK_CONTINUE;
}

SEXP wkb_writer_vector_end(const WKGeometryMeta_t* meta, void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    return writeBuffer->result;
}

void wkb_writer_vector_finally(void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    if (writeBuffer->result != R_NilValue) {
        R_ReleaseObject(writeBuffer->result);
    }
}

void wkb_writer_finalize(void* userData) {
    WKBWriteBuffer_t* writeBuffer = (WKBWriteBuffer_t*) userData;
    if (writeBuffer != NULL) {
        free(writeBuffer->buffer);
        free(writeBuffer);
    }
}

SEXP wk_c_wkb_writer_new() {
    WKHandler_t* handler = wk_handler_create();

    handler->vectorStart = &wkb_writer_vector_start;
    handler->featureStart = &wkb_writer_feature_start;
    handler->geometryStart = &wkb_writer_geometry_start;
    handler->ringStart = &wkb_writer_ring_start;
    handler->coord = &wkb_writer_coord;
    handler->nullFeature = &wkb_writer_feature_null;
    handler->featureEnd = &wkb_writer_feature_end;
    handler->vectorEnd = &wkb_writer_vector_end;
    handler->vectorFinally = &wkb_writer_vector_finally;
    handler->finalizer = &wkb_writer_finalize;

    handler->userData = wkb_write_buffer_new(1024);
    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}
