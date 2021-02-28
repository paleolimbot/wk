
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>
#include <memory.h>

#define COUNT_HANDLER_SIZE_DEFAULT 128

typedef struct {
    SEXP result;
    R_xlen_t feat_id;
    int n_geom;
    int n_ring;
    R_xlen_t n_coord;
} count_handler_t;

SEXP count_handler_alloc_result(R_xlen_t size) {
    const char* names[] = {"n_geom", "n_ring", "n_coord", ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(result, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(result, 1, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(result, 2, Rf_allocVector(REALSXP, size));

    UNPROTECT(1);
    return result;
}

SEXP count_handler_realloc_result(SEXP result, R_xlen_t new_size) {
    SEXP new_result = PROTECT(count_handler_alloc_result(new_size));

    R_xlen_t size_cpy;
    if (Rf_xlength(result) < new_size) {
        size_cpy = Rf_xlength(result); // reduce size
    } else {
        size_cpy = new_size; // increase size
    }

    memcpy(INTEGER(VECTOR_ELT(new_result, 0)), INTEGER(VECTOR_ELT(result, 0)), sizeof(int) * size_cpy);
    memcpy(INTEGER(VECTOR_ELT(new_result, 1)), INTEGER(VECTOR_ELT(result, 1)), sizeof(int) * size_cpy);
    memcpy(REAL(VECTOR_ELT(new_result, 2)), REAL(VECTOR_ELT(result, 2)), sizeof(int) * size_cpy);

    UNPROTECT(1);
    return result;
}

int count_handler_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;

    if (data->result != R_NilValue) {
        Rf_error("Destination vector was already allocated"); // # nocov
    }

    if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        data->result = PROTECT(count_handler_alloc_result(COUNT_HANDLER_SIZE_DEFAULT));
    } else {
        data->result = PROTECT(count_handler_alloc_result(meta->size));
    }
    R_PreserveObject(data->result);
    UNPROTECT(1);

    return WK_CONTINUE;
}

int count_handler_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;
    data->feat_id = feat_id;
    data->n_coord = 0;
    data->n_geom = 0;
    data->n_ring = 0;
    return WK_CONTINUE;
}

int count_handler_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;
    data->n_geom++;
    return WK_CONTINUE;
}

int count_handler_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;
    data->n_ring++;
    return WK_CONTINUE;
}

int count_handler_coord(const wk_meta_t* meta, const wk_coord_t coord, uint32_t coord_id, void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;
    data->n_coord++;
    return WK_CONTINUE;
}

int count_handler_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;

    if (data->feat_id >= Rf_xlength(data->result)) {
        SEXP new_result = PROTECT(count_handler_realloc_result(data->result, Rf_xlength(data->result) * 2 + 1));
        if (data->result != R_NilValue) {
            R_ReleaseObject(data->result);
        }
        data->result = new_result;
        R_PreserveObject(data->result);
        UNPROTECT(1);
    }

    INTEGER(VECTOR_ELT(data->result, 0))[data->feat_id] = data->n_geom;
    INTEGER(VECTOR_ELT(data->result, 1))[data->feat_id] = data->n_ring;
    REAL(VECTOR_ELT(data->result, 2))[data->feat_id] = data->n_coord;

    return WK_CONTINUE;
}

SEXP count_handler_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;

    R_xlen_t final_size = data->feat_id + 1;
    if (Rf_xlength(data->result) > final_size) {
        SEXP new_result = PROTECT(count_handler_realloc_result(data->result, final_size));
        if (data->result != R_NilValue) {
            R_ReleaseObject(data->result);
        }
        data->result = new_result;
        R_PreserveObject(data->result);
        UNPROTECT(1);
    }

    return data->result;
}

void count_handler_deinitialize(void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;
    if (data->result != R_NilValue) {
        R_ReleaseObject(data->result);
        data->result = R_NilValue;
    }
}

void count_handler_finalize(void* handler_data) {
    count_handler_t* data = (count_handler_t*) handler_data;
    if (data != NULL) {
        free(data);
    }
}

SEXP wk_c_count_handler_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &count_handler_vector_start;
    handler->feature_start = &count_handler_feature_start;
    handler->geometry_start = &count_handler_geometry_start;
    handler->ring_start = &count_handler_ring_start;
    handler->coord = &count_handler_coord;
    handler->feature_end = &count_handler_feature_end;
    handler->vector_end = &count_handler_vector_end;
    handler->deinitialize = &count_handler_deinitialize;
    handler->finalizer = &count_handler_finalize;

    count_handler_t* data = (count_handler_t*) malloc(sizeof(count_handler_t));
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
