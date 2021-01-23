#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>

typedef struct {
    R_xlen_t feat_id;
    R_xlen_t vector_size;
    SEXP container;
    int* geometry_type;
    int* has_z;
    int* has_m;
    int* srid;
    int* size;
} meta_handler_t;

SEXP wk_meta_handler_alloc_container(R_xlen_t size) {
    const char* names[] = {"geometry_type", "has_z", "has_m", "srid", "size", ""};
    SEXP container = PROTECT(Rf_mkNamed(VECSXP, names));
    SEXP geometry_type = PROTECT(Rf_allocVector(INTSXP, size));
    SEXP has_z = PROTECT(Rf_allocVector(LGLSXP, size));
    SEXP has_m = PROTECT(Rf_allocVector(LGLSXP, size));
    SEXP srid = PROTECT(Rf_allocVector(INTSXP, size));
    SEXP size_col = PROTECT(Rf_allocVector(INTSXP, size));

    SET_VECTOR_ELT(container, 0, geometry_type);
    SET_VECTOR_ELT(container, 1, has_z);
    SET_VECTOR_ELT(container, 2, has_m);
    SET_VECTOR_ELT(container, 3, srid);
    SET_VECTOR_ELT(container, 4, size_col);
    
    UNPROTECT(5);

    // make output a data.frame
    Rf_setAttrib(container, R_ClassSymbol, Rf_mkString("data.frame"));

    // set row.names
    SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = size;
    Rf_setAttrib(container, Rf_install("row.names"), row_names);
    UNPROTECT(1);

    UNPROTECT(1);
    return container;
}

int wk_meta_handler_vector_start(const wk_vector_meta_t* vector_meta, void* handler_data) {
    meta_handler_t* meta_handler = (meta_handler_t*) handler_data;
    if (vector_meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        Rf_error("Can't extract meta from a vector of unknown size");
    }

    meta_handler->vector_size = vector_meta->size;
    meta_handler->feat_id = 0;

    meta_handler->container = PROTECT(wk_meta_handler_alloc_container(vector_meta->size));

    meta_handler->geometry_type = INTEGER(VECTOR_ELT(meta_handler->container, 0));
    meta_handler->has_z = LOGICAL(VECTOR_ELT(meta_handler->container, 1));
    meta_handler->has_m = LOGICAL(VECTOR_ELT(meta_handler->container, 2));
    meta_handler->srid = INTEGER(VECTOR_ELT(meta_handler->container, 3));
    meta_handler->size = INTEGER(VECTOR_ELT(meta_handler->container, 4));
    
    R_PreserveObject(meta_handler->container);
    UNPROTECT(1);
    return WK_CONTINUE;
}

int wk_meta_handler_feature_start(const wk_vector_meta_t* vector_meta, R_xlen_t feat_id, void* handler_data) {
    meta_handler_t* meta_handler = (meta_handler_t*) handler_data;
    meta_handler->feat_id = feat_id;
    return WK_CONTINUE;
}

int wk_meta_handler_null_feature(const wk_vector_meta_t* vector_meta, R_xlen_t feat_id, void* handler_data) {
    meta_handler_t* meta_handler = (meta_handler_t*) handler_data;
    if (meta_handler->feat_id >= meta_handler->vector_size) {
        Rf_error(
            "Can't set vector meta with index %d for vector of size %d", 
            meta_handler->feat_id, 
            meta_handler->vector_size
        );
    }

    meta_handler->geometry_type[meta_handler->feat_id] = NA_INTEGER;
    meta_handler->has_z[meta_handler->feat_id] = NA_LOGICAL;
    meta_handler->has_m[meta_handler->feat_id] = NA_LOGICAL;
    meta_handler->srid[meta_handler->feat_id] = NA_INTEGER;
    meta_handler->size[meta_handler->feat_id] = NA_INTEGER;

    return WK_ABORT_FEATURE;
}

int wk_meta_handler_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    meta_handler_t* meta_handler = (meta_handler_t*) handler_data;
    if (meta_handler->feat_id >= meta_handler->vector_size) {
        Rf_error(
            "Can't set vector meta with index %d for vector of size %d", 
            meta_handler->feat_id, 
            meta_handler->vector_size
        );
    }

    meta_handler->geometry_type[meta_handler->feat_id] = meta->geometry_type;

    meta_handler->has_z[meta_handler->feat_id] = (meta->flags & WK_FLAG_HAS_Z) != 0;
    meta_handler->has_m[meta_handler->feat_id] = (meta->flags & WK_FLAG_HAS_M) != 0;

    if (meta->srid == WK_SRID_NONE) {
        meta_handler->srid[meta_handler->feat_id] = NA_INTEGER;
    } else {
        meta_handler->srid[meta_handler->feat_id] = meta->srid;
    }

    if (meta->size == WK_SIZE_UNKNOWN) {
        meta_handler->size[meta_handler->feat_id] = NA_INTEGER;
    } else {
        meta_handler->size[meta_handler->feat_id] = meta->size;
    }
    
    return WK_ABORT_FEATURE;
}

SEXP wk_meta_handler_vector_end(const wk_vector_meta_t* vector_meta, void* handler_data) {
    meta_handler_t* meta_handler = (meta_handler_t*) handler_data;
    return meta_handler->container;
}

void wk_meta_handler_vector_finally(void* handler_data) {
    meta_handler_t* meta_handler = (meta_handler_t*) handler_data;
    if (meta_handler->container != R_NilValue) {
        R_ReleaseObject(meta_handler->container);
        meta_handler->container = R_NilValue;
    }
}

void wk_meta_handler_finalize(void* handler_data) {
    meta_handler_t* meta_handler = (meta_handler_t*) handler_data;
    if (meta_handler != NULL) {
        free(meta_handler);
    }
}

int wk_vector_meta_handler_vector_start(const wk_vector_meta_t* vector_meta, void* handler_data) {
    meta_handler_t* meta_handler = (meta_handler_t*) handler_data;
    if (vector_meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        Rf_error("Can't extract meta from a vector of unknown size");
    }

    meta_handler->vector_size = vector_meta->size;
    meta_handler->feat_id = 0;

    meta_handler->container = PROTECT(wk_meta_handler_alloc_container(1));

    INTEGER(VECTOR_ELT(meta_handler->container, 0))[0] = vector_meta->geometry_type;

    int has_z, has_m;
    if (vector_meta->flags & WK_FLAG_DIMS_UNKNOWN) {
        has_z = NA_LOGICAL;
        has_m = NA_LOGICAL;
    } else {
        has_z = (vector_meta->flags & WK_FLAG_HAS_Z) != 0;
        has_m = (vector_meta->flags & WK_FLAG_HAS_M) != 0;
    }
    
    LOGICAL(VECTOR_ELT(meta_handler->container, 1))[0] = has_z;
    LOGICAL(VECTOR_ELT(meta_handler->container, 2))[0] = has_m;
    INTEGER(VECTOR_ELT(meta_handler->container, 3))[0] = NA_INTEGER;

    if (vector_meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        INTEGER(VECTOR_ELT(meta_handler->container, 4))[0] = NA_INTEGER;
    } else {
        INTEGER(VECTOR_ELT(meta_handler->container, 4))[0] = vector_meta->size;
    }
    
    R_PreserveObject(meta_handler->container);
    UNPROTECT(1);
    return WK_ABORT;
}

SEXP wk_c_meta_handler_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &wk_meta_handler_vector_start;
    handler->feature_start = &wk_meta_handler_feature_start;
    handler->null_feature = &wk_meta_handler_null_feature;
    handler->geometry_start = &wk_meta_handler_geometry_start;
    handler->vector_end = &wk_meta_handler_vector_end;
    handler->vector_finally = &wk_meta_handler_vector_finally;
    handler->finalizer = &wk_meta_handler_finalize;

    meta_handler_t* meta_handler = (meta_handler_t*) malloc(sizeof(meta_handler_t));
    meta_handler->container = R_NilValue;
    handler->handler_data = meta_handler;

    return wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
}

SEXP wk_c_vector_meta_handler_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &wk_vector_meta_handler_vector_start;
    handler->vector_end = &wk_meta_handler_vector_end;
    handler->vector_finally = &wk_meta_handler_vector_finally;
    handler->finalizer = &wk_meta_handler_finalize;

    meta_handler_t* meta_handler = (meta_handler_t*) malloc(sizeof(meta_handler_t));
    meta_handler->container = R_NilValue;
    handler->handler_data = meta_handler;

    return wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
}
