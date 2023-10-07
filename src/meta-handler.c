
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>
#include <memory.h>

typedef struct {
    SEXP result;
    R_xlen_t result_size;
    R_xlen_t feat_id;
    int is_root;
    int coord_size;
} meta_handler_t;

SEXP meta_handler_alloc_result(R_xlen_t size) {
    const char* names[] = {"geometry_type", "size", "has_z", "has_m", "srid", "precision", "is_empty", ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(result, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(result, 1, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(result, 2, Rf_allocVector(LGLSXP, size));
    SET_VECTOR_ELT(result, 3, Rf_allocVector(LGLSXP, size));
    SET_VECTOR_ELT(result, 4, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(result, 5, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(result, 6, Rf_allocVector(LGLSXP, size));

    UNPROTECT(1);
    return result;
}

SEXP meta_handler_realloc_result(SEXP result, R_xlen_t new_size) {
    SEXP new_result = PROTECT(meta_handler_alloc_result(new_size));

    R_xlen_t size_cpy;
    if (Rf_xlength(VECTOR_ELT(result, 0)) < new_size) {
        size_cpy = Rf_xlength(VECTOR_ELT(result, 0));
    } else {
        size_cpy = new_size;
    }

    memcpy(INTEGER(VECTOR_ELT(new_result, 0)), INTEGER(VECTOR_ELT(result, 0)), sizeof(int) * size_cpy);
    memcpy(INTEGER(VECTOR_ELT(new_result, 1)), INTEGER(VECTOR_ELT(result, 1)), sizeof(int) * size_cpy);
    memcpy(LOGICAL(VECTOR_ELT(new_result, 2)), LOGICAL(VECTOR_ELT(result, 2)), sizeof(int) * size_cpy);
    memcpy(LOGICAL(VECTOR_ELT(new_result, 3)), LOGICAL(VECTOR_ELT(result, 3)), sizeof(int) * size_cpy);
    memcpy(INTEGER(VECTOR_ELT(new_result, 4)), INTEGER(VECTOR_ELT(result, 4)), sizeof(int) * size_cpy);
    memcpy(REAL(VECTOR_ELT(new_result, 5)), REAL(VECTOR_ELT(result, 5)), sizeof(double) * size_cpy);
    memcpy(LOGICAL(VECTOR_ELT(new_result, 6)), LOGICAL(VECTOR_ELT(result, 6)), sizeof(int) * size_cpy);

    UNPROTECT(1);
    return new_result;
}

static inline void meta_handler_result_append(meta_handler_t* data, int geometry_type, int size,
                                              int has_z, int has_m, int srid, double precision,
                                              int is_empty) {
    if (data->feat_id >= data->result_size) {
        SEXP new_result = PROTECT(meta_handler_realloc_result(data->result, data->feat_id * 2 + 1));
        R_ReleaseObject(data->result);
        data->result = new_result;
        R_PreserveObject(data->result);
        UNPROTECT(1);
        data->result_size = data->feat_id * 2 + 1;
    }

    INTEGER(VECTOR_ELT(data->result, 0))[data->feat_id] = geometry_type;
    INTEGER(VECTOR_ELT(data->result, 1))[data->feat_id] = size;
    LOGICAL(VECTOR_ELT(data->result, 2))[data->feat_id] = has_z;
    LOGICAL(VECTOR_ELT(data->result, 3))[data->feat_id] = has_m;
    INTEGER(VECTOR_ELT(data->result, 4))[data->feat_id] = srid;
    REAL(VECTOR_ELT(data->result, 5))[data->feat_id] = precision;
    LOGICAL(VECTOR_ELT(data->result, 6))[data->feat_id] = is_empty;
    data->feat_id++;
}

int meta_handler_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;

    if (data->result != R_NilValue) {
        Rf_error("Destination vector was already allocated"); // # nocov
    }

    if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        data->result = PROTECT(meta_handler_alloc_result(1024));
        data->result_size = 1024;
    } else {
        data->result = PROTECT(meta_handler_alloc_result(meta->size));
        data->result_size = meta->size;
    }

    R_PreserveObject(data->result);
    UNPROTECT(1);

    return WK_CONTINUE;
}

int meta_handler_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;
    data->feat_id = feat_id;
    data->is_root = 1;
    return WK_CONTINUE;
}

int meta_handler_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;
    if (!data->is_root) {
        return WK_CONTINUE;
    }

    data->is_root = 0;

    int result_size;
    if (meta->size == WK_SIZE_UNKNOWN) {
        result_size = NA_INTEGER;
    } else {
        result_size = meta->size;
    }

    int result_srid;
    if (meta->srid == WK_SRID_NONE) {
        result_srid = NA_INTEGER;
    } else {
        result_srid = meta->srid;
    }

    int has_z = (meta->flags & WK_FLAG_HAS_Z) != 0;
    int has_m = (meta->flags & WK_FLAG_HAS_M) != 0;
    data->coord_size = 2 + has_z + has_m;

    meta_handler_result_append(
        data,
        meta->geometry_type,
        result_size,
        has_z,
        has_m,
        result_srid,
        meta->precision,
        // Empty unless proven otherwise
        1
    );

    return WK_CONTINUE;
}

int meta_handler_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id,
                       void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;

    for (int i = 0; i < data->coord_size; i++) {
        if (!ISNA(coord[i]) && !ISNAN(coord[i])) {
            // Not empty!
            LOGICAL(VECTOR_ELT(data->result, 6))[data->feat_id - 1] = 0;
            return WK_ABORT_FEATURE;
        }
    }

    return WK_CONTINUE;
}

int meta_handler_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    return WK_ABORT_FEATURE;
}

int meta_handler_null_feature(void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;
    meta_handler_result_append(
        data,
        NA_INTEGER,
        NA_INTEGER,
        NA_LOGICAL,
        NA_LOGICAL,
        NA_INTEGER,
        NA_REAL,
        NA_LOGICAL
    );

    return WK_ABORT_FEATURE;
}

SEXP meta_handler_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    meta_handler_t* data = (meta_handler_t*) handler_data;

    if (data->result_size != data->feat_id) {
        SEXP new_result = PROTECT(meta_handler_realloc_result(data->result, data->feat_id));
        R_ReleaseObject(data->result);
        data->result = R_NilValue;
        UNPROTECT(1);
        return new_result;
    } else {
        return data->result;
    }
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

SEXP wk_c_meta_handler_new(void) {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &meta_handler_vector_start;
    handler->feature_start = &meta_handler_feature_start;
    handler->null_feature = &meta_handler_null_feature;
    handler->geometry_start = &meta_handler_geometry_start;
    handler->coord = &meta_handler_coord;
    handler->geometry_end = &meta_handler_geometry_end;
    handler->vector_end = &meta_handler_vector_end;
    handler->deinitialize = &meta_handler_deinitialize;
    handler->finalizer = &meta_handler_finalize;

    meta_handler_t* data = (meta_handler_t*) malloc(sizeof(meta_handler_t));
    if (data == NULL) {
        wk_handler_destroy(handler); // # nocov
        Rf_error("Failed to alloc handler data"); // # nocov
    }
    data->feat_id = 0;
    data->is_root = 1;
    data->coord_size = 2;
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
    if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        SET_VECTOR_ELT(result, 1, Rf_ScalarReal(NA_REAL));
    } else {
        SET_VECTOR_ELT(result, 1, Rf_ScalarReal(meta->size));
    }

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

SEXP wk_c_vector_meta_handler_new(void) {
    wk_handler_t* handler = wk_handler_create();
    handler->vector_start = &vector_meta_handler_vector_start;
    handler->vector_end = &vector_meta_handler_vector_end;

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}
