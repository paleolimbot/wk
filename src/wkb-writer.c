
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>
#include <memory.h>

#define EWKB_Z_BIT    0x80000000
#define EWKB_M_BIT    0x40000000
#define EWKB_SRID_BIT 0x20000000

#define WKB_MAX_RECURSION_DEPTH 32

typedef struct {
    SEXP result;
    unsigned char endian;
    unsigned char* buffer;
    size_t size;
    size_t offset;
    size_t current_size_offset[WKB_MAX_RECURSION_DEPTH + 3];
    uint32_t current_size[WKB_MAX_RECURSION_DEPTH + 3];
    size_t recursion_level;
} wkb_write_buffer_t;

unsigned char wkb_writer_platform_endian() {
    const int one = 1;
    unsigned char* cp = (unsigned char*) &one;
    return (char) *cp;
}

uint32_t wkb_writer_encode_type(const wk_meta_t* meta, int recursion_level) {
    uint32_t out = meta->geometry_type;
    if (meta->flags & WK_FLAG_HAS_Z) out |= EWKB_Z_BIT;
    if (meta->flags & WK_FLAG_HAS_M) out |= EWKB_M_BIT;
    if (recursion_level == 0) {
        if (meta->srid != WK_SRID_NONE) out |= EWKB_SRID_BIT;
    }
    return out;
  }

wkb_write_buffer_t* wkb_write_buffer_new(size_t size) {
    unsigned char* buffer = malloc(size);
    if (buffer == NULL) {
        Rf_error("Can't allocate buffer of size %d", size); // # nocov
    }

    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) malloc(sizeof(wkb_write_buffer_t));
    if (write_buffer == NULL) {
        free(buffer); // # nocov
        Rf_error("Can't allocate wkb_write_buffer_t"); // # nocov
    }

    write_buffer->endian = wkb_writer_platform_endian();
    write_buffer->result =  R_NilValue;
    write_buffer->buffer = buffer;
    write_buffer->size = size;
    write_buffer->offset = 0;
    write_buffer->recursion_level = 0;
    return write_buffer;
}

void wkb_write_buffer_ensure_size(wkb_write_buffer_t* write_buffer, size_t size) {
    unsigned char* new_buffer = realloc(write_buffer->buffer, size);
    if (new_buffer == NULL) {
        Rf_error("Can't reallocate buffer of size %d", size);
    }

    write_buffer->buffer = new_buffer;
    write_buffer->size = size;
}

int wkb_write_buffer_has_space(wkb_write_buffer_t* write_buffer, size_t item) {
    return (write_buffer->offset + item) <= write_buffer->size;
}

void wkb_write_buffer_ensure_space(wkb_write_buffer_t* write_buffer, size_t item) {
    if (!wkb_write_buffer_has_space(write_buffer, item)) {
        wkb_write_buffer_ensure_size(write_buffer, write_buffer->size * 2);
    }
}

void wkb_write_uint_offset(wkb_write_buffer_t* write_buffer, const uint32_t value, size_t offset) {
    wkb_write_buffer_ensure_space(write_buffer, sizeof(uint32_t));
    memcpy(write_buffer->buffer + offset, &value, sizeof(uint32_t));
}

void wkb_write_uint(wkb_write_buffer_t* write_buffer, const uint32_t value) {
    wkb_write_uint_offset(write_buffer, value, write_buffer->offset);
    write_buffer->offset += sizeof(uint32_t);
}

void wkb_write_uchar(wkb_write_buffer_t* write_buffer, const unsigned char value) {
    wkb_write_buffer_ensure_space(write_buffer, sizeof(unsigned char));
    memcpy(write_buffer->buffer + write_buffer->offset, &value, sizeof(unsigned char));
    write_buffer->offset += sizeof(unsigned char);
}

void wkb_write_doubles(wkb_write_buffer_t* write_buffer, const double* value, uint32_t nValues) {
    wkb_write_buffer_ensure_space(write_buffer, sizeof(double) * nValues);
    memcpy(write_buffer->buffer + write_buffer->offset, value, sizeof(double) * nValues);
    write_buffer->offset += sizeof(double) * nValues;
}

int wkb_writer_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    write_buffer->result = Rf_allocVector(VECSXP, meta->size);
    R_PreserveObject(write_buffer->result);
    return WK_CONTINUE;
}

int wkb_writer_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    write_buffer->offset = 0;
    write_buffer->recursion_level = 0;
    return WK_CONTINUE;
}

int wkb_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    if (write_buffer->recursion_level > 0) {
        write_buffer->current_size[write_buffer->recursion_level - 1]++;
    }

    wkb_write_uchar(write_buffer, write_buffer->endian);
    wkb_write_uint(write_buffer, wkb_writer_encode_type(meta, write_buffer->recursion_level));
    if (write_buffer->recursion_level == 0 && (meta->srid != WK_SRID_NONE)) {
        wkb_write_uint(write_buffer, meta->srid);
    }

    if (meta->geometry_type != WK_POINT) {
        if (write_buffer->recursion_level >= WKB_MAX_RECURSION_DEPTH) {
            Rf_error(
                "Can't write WKB with maximum recursion depth greater than %d",
                WKB_MAX_RECURSION_DEPTH
            );
        }

        // reserve space for the size and record where it is
        write_buffer->current_size_offset[write_buffer->recursion_level] = write_buffer->offset;
        write_buffer->current_size[write_buffer->recursion_level] = 0;
        wkb_write_uint(write_buffer, 0);
    }

    // handle empty point as nan nan here (coord() will not get called)
    if (meta->geometry_type == WK_POINT && meta->size == 0) {
        int coord_size = 2;
        if (meta->flags & WK_FLAG_HAS_Z) coord_size++;
        if (meta->flags & WK_FLAG_HAS_Z) coord_size++;
        double empty_coord[4];
        empty_coord[0] = NAN;
        empty_coord[1] = NAN;
        empty_coord[2] = NAN;
        empty_coord[3] = NAN;
        wkb_write_doubles(write_buffer, empty_coord, coord_size);
    }

    write_buffer->recursion_level++;

    return WK_CONTINUE;
}

int wkb_writer_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    write_buffer->recursion_level--;
    if (meta->geometry_type != WK_POINT) {
        wkb_write_uint_offset(
            write_buffer,
            write_buffer->current_size[write_buffer->recursion_level],
            write_buffer->current_size_offset[write_buffer->recursion_level]
        );
    }
    return WK_CONTINUE;
}

int wkb_writer_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    write_buffer->current_size[write_buffer->recursion_level - 1]++;

    if (write_buffer->recursion_level >= WKB_MAX_RECURSION_DEPTH) {
        Rf_error(
            "Can't write WKB with maximum recursion depth greater than %d",
            WKB_MAX_RECURSION_DEPTH
        );
    }

    write_buffer->current_size_offset[write_buffer->recursion_level] = write_buffer->offset;
    write_buffer->current_size[write_buffer->recursion_level] = 0;
    wkb_write_uint(write_buffer, 0);
    write_buffer->recursion_level++;

    return WK_CONTINUE;
}

int wkb_writer_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    write_buffer->recursion_level--;
    wkb_write_uint_offset(
        write_buffer,
        write_buffer->current_size[write_buffer->recursion_level],
        write_buffer->current_size_offset[write_buffer->recursion_level]
    );
    return WK_CONTINUE;
}

int wkb_writer_coord(const wk_meta_t* meta, const wk_coord_t coord, uint32_t coord_id,
                      void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    write_buffer->current_size[write_buffer->recursion_level - 1]++;
    int coordSize = 2;
    if (meta->flags & WK_FLAG_HAS_Z) coordSize++;
    if (meta->flags & WK_FLAG_HAS_M) coordSize++;
    wkb_write_doubles(write_buffer, coord.v, coordSize);
    return WK_CONTINUE;
}

int wkb_writer_feature_null(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    SET_VECTOR_ELT(write_buffer->result, feat_id, R_NilValue);
    return WK_ABORT_FEATURE;
}

int wkb_writer_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    SEXP item = PROTECT(Rf_allocVector(RAWSXP, write_buffer->offset));
    memcpy(RAW(item), write_buffer->buffer, write_buffer->offset);
    SET_VECTOR_ELT(write_buffer->result, feat_id, item);
    UNPROTECT(1);
    return WK_CONTINUE;
}

SEXP wkb_writer_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    SEXP wkb_class = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(wkb_class, 0, Rf_mkChar("wk_wkb"));
    SET_STRING_ELT(wkb_class, 1, Rf_mkChar("wk_vctr"));
    Rf_setAttrib(write_buffer->result, R_ClassSymbol, wkb_class);
    UNPROTECT(1);
    return write_buffer->result;
}

void wkb_writer_deinitialize(void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    if (write_buffer->result != R_NilValue) {
        R_ReleaseObject(write_buffer->result);
        write_buffer->result = R_NilValue;
    }
}

void wkb_writer_finalize(void* handler_data) {
    wkb_write_buffer_t* write_buffer = (wkb_write_buffer_t*) handler_data;
    if (write_buffer != NULL) {
        free(write_buffer->buffer);
        free(write_buffer);
    }
}

SEXP wk_c_wkb_writer_new() {
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

    handler->handler_data = wkb_write_buffer_new(1024);
    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}
