
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>

typedef struct {
    SEXP problems;
    R_xlen_t feat_id;
} wk_problems_handler_t;

int wk_problems_handler_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    wk_problems_handler_t* data = (wk_problems_handler_t*) handler_data;

    if (data->problems != R_NilValue) {
        Rf_error("Destination vector was already allocated"); // # nocov
    }

    R_xlen_t n_features;
    if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        n_features = 1024;
    } else {
        n_features = meta->size;
    }

    data->problems = PROTECT(Rf_allocVector(STRSXP, n_features));
    R_PreserveObject(data->problems);
    UNPROTECT(1);

    data->feat_id = 0;

    return WK_CONTINUE;
}

int wk_problems_handler_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    wk_problems_handler_t* data = (wk_problems_handler_t*) handler_data;
    if (feat_id >= Rf_xlength(data->problems)) {
        SEXP new_result = PROTECT(Rf_allocVector(STRSXP, Rf_xlength(data->problems) * 2 + 1));
        for (R_xlen_t i = 0; i < Rf_xlength(data->problems); i++) {
            SET_STRING_ELT(new_result, i, STRING_ELT(data->problems, i));
        }
        R_ReleaseObject(data->problems);
        data->problems = new_result;
        R_PreserveObject(data->problems);
        UNPROTECT(1);
    }

    SET_STRING_ELT(data->problems, data->feat_id, NA_STRING);
    data->feat_id++;
    return WK_CONTINUE;
}

int wk_problems_handler_error(const char* message, void* handler_data) {
    wk_problems_handler_t* data = (wk_problems_handler_t*) handler_data;
    SET_STRING_ELT(data->problems, data->feat_id - 1, Rf_mkCharCE(message, CE_UTF8));
    return WK_ABORT_FEATURE;
}

SEXP wk_problems_handler_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    wk_problems_handler_t* data = (wk_problems_handler_t*) handler_data;

    if (data->feat_id != Rf_xlength(data->problems)) {
        SEXP new_result = PROTECT(Rf_allocVector(STRSXP, data->feat_id));
        for (R_xlen_t i = 0; i < Rf_xlength(new_result); i++) {
            SET_STRING_ELT(new_result, i, STRING_ELT(data->problems, i));
        }
        R_ReleaseObject(data->problems);
        data->problems = R_NilValue;
        UNPROTECT(1);
        return new_result;
    } else {
        return data->problems;
    }
}

void wk_problems_handler_deinitialize(void* handler_data) {
    wk_problems_handler_t* data = (wk_problems_handler_t*) handler_data;
    if (data->problems != R_NilValue) {
        R_ReleaseObject(data->problems);
        data->problems = R_NilValue;
    }
}

void wk_problems_handler_finalize(void* handler_data) {
    wk_problems_handler_t* data = (wk_problems_handler_t*) handler_data;
    if (data !=  NULL) {
        free(data);
    }
}

SEXP wk_c_problems_handler_new(void) {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &wk_problems_handler_vector_start;
    handler->vector_end = &wk_problems_handler_vector_end;
    handler->feature_start = &wk_problems_handler_feature_start;
    handler->error = &wk_problems_handler_error;
    handler->deinitialize = &wk_problems_handler_deinitialize;
    handler->finalizer = &wk_problems_handler_finalize;

    wk_problems_handler_t* data = (wk_problems_handler_t*) malloc(sizeof(wk_problems_handler_t));
    if (data == NULL) {
        wk_handler_destroy(handler); // # nocov
        Rf_error("Failed to alloc handler data"); // # nocov
    }

    data->feat_id = 0;
    data->problems = R_NilValue;

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    handler->handler_data = data;

    return xptr;
}
