
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>
#include <memory.h>

typedef struct {
    SEXP result;
    R_xlen_t feat_id;
} meta_handler_t;

int meta_handler_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;

    if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        Rf_error("Can't handle vector of unknown size");
    }

    if (data->result != R_NilValue) {
        Rf_error("Destination vector was already allocated"); // # nocov
    }

    const char* names[] = {"geometry_type", "size", "has_z", "has_m", "srid", "precision", ""};
    data->result = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(data->result, 0, Rf_allocVector(INTSXP, meta->size));
    SET_VECTOR_ELT(data->result, 1, Rf_allocVector(INTSXP, meta->size));
    SET_VECTOR_ELT(data->result, 2, Rf_allocVector(LGLSXP, meta->size));
    SET_VECTOR_ELT(data->result, 3, Rf_allocVector(LGLSXP, meta->size));
    SET_VECTOR_ELT(data->result, 4, Rf_allocVector(INTSXP, meta->size));
    SET_VECTOR_ELT(data->result, 5, Rf_allocVector(REALSXP, meta->size));

    R_PreserveObject(data->result);
    UNPROTECT(1);

    return WK_CONTINUE;
}

int meta_handler_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;
    data->feat_id = feat_id;
    return WK_CONTINUE;
}

int meta_handler_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;

    INTEGER(VECTOR_ELT(data->result, 0))[data->feat_id] = meta->geometry_type;
    if (meta->size == WK_SIZE_UNKNOWN) {
        INTEGER(VECTOR_ELT(data->result, 1))[data->feat_id] = NA_INTEGER;
    } else {
        INTEGER(VECTOR_ELT(data->result, 1))[data->feat_id] = meta->size;
    }
    LOGICAL(VECTOR_ELT(data->result, 2))[data->feat_id] = (meta->flags & WK_FLAG_HAS_Z) != 0;
    LOGICAL(VECTOR_ELT(data->result, 3))[data->feat_id] = (meta->flags & WK_FLAG_HAS_M) != 0;
    if (meta->srid == WK_SRID_NONE) {
        INTEGER(VECTOR_ELT(data->result, 4))[data->feat_id] = NA_INTEGER;
    } else {
        INTEGER(VECTOR_ELT(data->result, 4))[data->feat_id] = meta->srid;
    }
    REAL(VECTOR_ELT(data->result, 5))[data->feat_id] = meta->precision;

    return WK_ABORT_FEATURE;
}

int meta_handler_null_feature(void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;

    INTEGER(VECTOR_ELT(data->result, 0))[data->feat_id] = NA_INTEGER;
    INTEGER(VECTOR_ELT(data->result, 1))[data->feat_id] = NA_INTEGER;
    LOGICAL(VECTOR_ELT(data->result, 2))[data->feat_id] = NA_LOGICAL;
    LOGICAL(VECTOR_ELT(data->result, 3))[data->feat_id] = NA_LOGICAL;
    INTEGER(VECTOR_ELT(data->result, 4))[data->feat_id] = NA_INTEGER;
    REAL(VECTOR_ELT(data->result, 5))[data->feat_id] = NA_REAL;

    return WK_ABORT_FEATURE;
}

SEXP meta_handler_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;
    return data->result;
}

void meta_handler_deinitialize(void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;
    if (data->result != R_NilValue) {
        R_ReleaseObject(data->result);
        data->result = R_NilValue;
    }
}

void meta_handler_finalize(void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;
    if (data != NULL) {
        free(data);
    }
}

SEXP wk_c_meta_handler_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &meta_handler_vector_start;
    handler->feature_start = &meta_handler_feature_start;
    handler->null_feature = &meta_handler_null_feature;
    handler->geometry_start = &meta_handler_geometry_start;
    handler->vector_end = &meta_handler_vector_end;
    handler->deinitialize = &meta_handler_deinitialize;
    handler->finalizer = &meta_handler_finalize;

    meta_handler_t* data = (meta_handler_t*) malloc(sizeof(meta_handler_t));
    if (data == NULL) {
        wk_handler_destroy(handler); // # nocov
        Rf_error("Failed to alloc handler data"); // # nocov
    }
    data->feat_id = 0;
    data->result = R_NilValue;
    handler->handler_data = data;

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}


int vector_meta_handler_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    return WK_ABORT;
}

SEXP vector_meta_handler_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    const char* names[] = {"geometry_type", "size", "has_z", "has_m", ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));

    SET_VECTOR_ELT(result, 0, Rf_ScalarInteger(meta->geometry_type));
    SET_VECTOR_ELT(result, 1, Rf_ScalarReal(meta->size));
    if (meta->flags & WK_FLAG_DIMS_UNKNOWN) {
        SET_VECTOR_ELT(result, 2, Rf_ScalarLogical(NA_LOGICAL));
        SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(NA_LOGICAL));
    } else {
        SET_VECTOR_ELT(result, 2, Rf_ScalarLogical((meta->flags & WK_FLAG_HAS_Z) != 0));
        SET_VECTOR_ELT(result, 3, Rf_ScalarLogical((meta->flags & WK_FLAG_HAS_M) != 0));
    }

    UNPROTECT(1);
    return result;
}

SEXP wk_c_vector_meta_handler_new() {
    wk_handler_t* handler = wk_handler_create();
    handler->vector_start = &vector_meta_handler_vector_start;
    handler->vector_end = &vector_meta_handler_vector_end;

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}
