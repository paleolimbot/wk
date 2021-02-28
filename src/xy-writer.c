
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>
#include <memory.h>

#define XY_SIZE_DEFAULT 128

typedef struct {
    SEXP result;
    R_xlen_t result_size;
    R_xlen_t feat_id;
    int has_coord;
    uint32_t flags;
    double* x;
    double* y;
    double* z;
    double* m;
} xy_writer_t;

static inline SEXP xy_writer_alloc_result(R_xlen_t size) {
    const char* names[] = {"x", "y", "z", "m", ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(result, 0, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(result, 1, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(result, 2, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(result, 3, Rf_allocVector(REALSXP, size));

    UNPROTECT(1);
    return result;
}

static inline SEXP xy_writer_realloc_result(SEXP result, R_xlen_t new_size) {
    SEXP new_result = PROTECT(xy_writer_alloc_result(new_size));

    R_xlen_t size_cpy;
    if (Rf_xlength(VECTOR_ELT(result, 0)) < new_size) {
        size_cpy = Rf_xlength(VECTOR_ELT(result, 0)); // reduce size
    } else {
        size_cpy = new_size; // increase size
    }

    for (int i = 0; i < 4; i ++) {
        memcpy(
            REAL(VECTOR_ELT(new_result, i)), 
            REAL(VECTOR_ELT(result, i)), 
            sizeof(double) * size_cpy
        );
    }

    UNPROTECT(1);
    return result;
}

static inline void xy_writer_append_empty(xy_writer_t* writer) {
    if (writer->feat_id >= writer->result_size) {
        SEXP new_result = PROTECT(xy_writer_realloc_result(writer->result, writer->result_size * 2 + 1));
        R_ReleaseObject(writer->result);
        writer->result = new_result;
        R_PreserveObject(writer->result);
        UNPROTECT(1);
        writer->result_size = writer->result_size * 2 + 1;
    }

    writer->x = REAL(VECTOR_ELT(writer->result, 0)) + writer->feat_id;
    writer->y = REAL(VECTOR_ELT(writer->result, 1)) + writer->feat_id;
    writer->z = REAL(VECTOR_ELT(writer->result, 2)) + writer->feat_id;
    writer->m = REAL(VECTOR_ELT(writer->result, 3)) + writer->feat_id;

    *writer->x = NA_REAL;
    *writer->y = NA_REAL;
    *writer->z = NA_REAL;
    *writer->m = NA_REAL;

    writer->feat_id++;
}

int xy_writer_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    xy_writer_t* data = (xy_writer_t*) handler_data;

    if (data->result != R_NilValue) {
        Rf_error("Destination vector was already allocated"); // # nocov
    }

    if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        data->result = PROTECT(xy_writer_alloc_result(XY_SIZE_DEFAULT));
        data->result_size = XY_SIZE_DEFAULT;
    } else {
        data->result = PROTECT(xy_writer_alloc_result(meta->size));
        data->result_size = meta->size;
    }

    R_PreserveObject(data->result);
    UNPROTECT(1);

    data->feat_id = 0;

    return WK_CONTINUE;
}

int xy_writer_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    xy_writer_t* data = (xy_writer_t*) handler_data;
    data->has_coord = 0;
    xy_writer_append_empty(data);
    return WK_CONTINUE;
}

int xy_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    xy_writer_t* data = (xy_writer_t*) handler_data;

    // EMPTY and any set of features that (could) contain a single point work with this
    // handler! (error otherwise)
    if (meta->size != 0 &&
        meta->geometry_type != WK_POINT &&
        meta->geometry_type != WK_MULTIPOINT &&
        meta->geometry_type != WK_GEOMETRYCOLLECTION) {
        Rf_error(
            "[%d] Can't convert geometry with type '%d' to coordinate",
            data->feat_id + 1,
            meta->geometry_type
        );
    }

    // keep track of zm flags to possibly trim output
    data->flags |= meta->flags;

    return WK_CONTINUE;
}

int xy_writer_coord(const wk_meta_t* meta, const wk_coord_t coord, uint32_t coord_id, void* handler_data) {
    xy_writer_t* data = (xy_writer_t*) handler_data;

    if (data->has_coord) {
        Rf_error("[%d] Feature contains more than one coordinate.", data->feat_id + 1);
    } else {
        data->has_coord = 1;
    }

    *(data->x) = coord.v[0];
    *(data->y) = coord.v[1];

    if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        *(data->z) = coord.v[2];
        *(data->m) = coord.v[3];
    } else if(meta->flags & WK_FLAG_HAS_Z) {
        *(data->z) = coord.v[2];
    } else if(meta->flags & WK_FLAG_HAS_M) {
        *(data->m) = coord.v[2];
    }

    return WK_CONTINUE;
}

SEXP xy_writer_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    xy_writer_t* data = (xy_writer_t*) handler_data;

    R_xlen_t final_size = data->feat_id;
    if (final_size != data->result_size) {
        SEXP new_result = PROTECT(xy_writer_realloc_result(data->result, final_size));
        R_ReleaseObject(data->result);
        data->result = new_result;
        R_PreserveObject(data->result);
        UNPROTECT(1);
    }

    if ((data->flags & WK_FLAG_HAS_Z) && (data->flags & WK_FLAG_HAS_M)) {
        SEXP xy_class = PROTECT(Rf_allocVector(STRSXP, 5));
        SET_STRING_ELT(xy_class, 0, Rf_mkChar("wk_xyzm"));
        SET_STRING_ELT(xy_class, 1, Rf_mkChar("wk_xyz"));
        SET_STRING_ELT(xy_class, 2, Rf_mkChar("wk_xym"));
        SET_STRING_ELT(xy_class, 3, Rf_mkChar("wk_xy"));
        SET_STRING_ELT(xy_class, 4, Rf_mkChar("wk_rcrd"));

        Rf_setAttrib(data->result, R_ClassSymbol, xy_class);
        UNPROTECT(1);
        return data->result;

    } else if(data->flags & WK_FLAG_HAS_Z) {
        const char* xyz_names[] = {"x", "y", "z", ""};
        SEXP xyz = PROTECT(Rf_mkNamed(VECSXP, xyz_names));
        for (int i = 0; i < 3; i++) {
            SET_VECTOR_ELT(xyz, i, VECTOR_ELT(data->result, i));
        }

        SEXP xy_class = PROTECT(Rf_allocVector(STRSXP, 3));
        SET_STRING_ELT(xy_class, 0, Rf_mkChar("wk_xyz"));
        SET_STRING_ELT(xy_class, 1, Rf_mkChar("wk_xy"));
        SET_STRING_ELT(xy_class, 2, Rf_mkChar("wk_rcrd"));

        Rf_setAttrib(xyz, R_ClassSymbol, xy_class);
        UNPROTECT(2);
        return xyz;
        
    } else if(data->flags & WK_FLAG_HAS_M) {
        const char* xym_names[] = {"x", "y", "m", ""};
        SEXP xym = PROTECT(Rf_mkNamed(VECSXP, xym_names));
        SET_VECTOR_ELT(xym, 0, VECTOR_ELT(data->result, 0));
        SET_VECTOR_ELT(xym, 1, VECTOR_ELT(data->result, 1));
        SET_VECTOR_ELT(xym, 2, VECTOR_ELT(data->result, 3));

        SEXP xy_class = PROTECT(Rf_allocVector(STRSXP, 3));
        SET_STRING_ELT(xy_class, 0, Rf_mkChar("wk_xym"));
        SET_STRING_ELT(xy_class, 1, Rf_mkChar("wk_xy"));
        SET_STRING_ELT(xy_class, 2, Rf_mkChar("wk_rcrd"));

        Rf_setAttrib(xym, R_ClassSymbol, xy_class);
        UNPROTECT(2);
        return xym;
    } else {
        const char* xy_names[] = {"x", "y", ""};
        SEXP xy = PROTECT(Rf_mkNamed(VECSXP, xy_names));
        for (int i = 0; i < 2; i++) {
            SET_VECTOR_ELT(xy, i, VECTOR_ELT(data->result, i));
        }

        SEXP xy_class = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(xy_class, 0, Rf_mkChar("wk_xy"));
        SET_STRING_ELT(xy_class, 1, Rf_mkChar("wk_rcrd"));

        Rf_setAttrib(xy, R_ClassSymbol, xy_class);
        UNPROTECT(2);
        return xy;
    }
}

void xy_writer_deinitialize(void* handler_data) {
    xy_writer_t* data = (xy_writer_t*) handler_data;
    if (data->result != R_NilValue) {
        R_ReleaseObject(data->result);
        data->result = R_NilValue;
    }
}

void xy_writer_finalize(void* handler_data) {
    xy_writer_t* data = (xy_writer_t*) handler_data;
    if (data != NULL) {
        free(data);
    }
}

SEXP wk_c_xy_writer_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &xy_writer_vector_start;
    handler->feature_start = &xy_writer_feature_start;
    handler->geometry_start = &xy_writer_geometry_start;
    handler->coord = &xy_writer_coord;
    handler->vector_end = &xy_writer_vector_end;
    handler->deinitialize = &xy_writer_deinitialize;
    handler->finalizer = &xy_writer_finalize;

    xy_writer_t* data = (xy_writer_t*) malloc(sizeof(xy_writer_t));
    if (data == NULL) {
        wk_handler_destroy(handler); // # nocov
        Rf_error("Failed to alloc handler data"); // # nocov
    }

    data->feat_id = 0;
    data->has_coord = 0;
    data->result = R_NilValue;
    data->flags = 0;

    handler->handler_data = data;

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}
