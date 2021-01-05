
#include "wk-v1.h"
#include <stdlib.h>
#include <memory.h>
#include <Rinternals.h>

typedef struct {
    SEXP result;
    double* result_ptr[4];
    R_xlen_t feat_id;
    int has_coord;
} xyzm_writer_data_t;

int xyzm_writer_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    xyzm_writer_data_t* data = (xyzm_writer_data_t*) handler_data;

    const char* names[] = {"x", "y", "z", "m", ""};
    data->result = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(data->result, 0, Rf_allocVector(REALSXP, meta->size));
    SET_VECTOR_ELT(data->result, 1, Rf_allocVector(REALSXP, meta->size));
    SET_VECTOR_ELT(data->result, 2, Rf_allocVector(REALSXP, meta->size));
    SET_VECTOR_ELT(data->result, 3, Rf_allocVector(REALSXP, meta->size));
    
    data->result_ptr[0] = REAL(VECTOR_ELT(data->result, 0));
    data->result_ptr[1] = REAL(VECTOR_ELT(data->result, 1));
    data->result_ptr[2] = REAL(VECTOR_ELT(data->result, 2));
    data->result_ptr[3] = REAL(VECTOR_ELT(data->result, 3));

    R_PreserveObject(data->result);
    UNPROTECT(1);

    return WK_CONTINUE;
}

int xyzm_writer_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    xyzm_writer_data_t* data = (xyzm_writer_data_t*) handler_data;
    data->feat_id = feat_id;
    data->has_coord = 0;

    data->result_ptr[0][data->feat_id] = NA_REAL;
    data->result_ptr[1][data->feat_id] = NA_REAL;
    data->result_ptr[2][data->feat_id] = NA_REAL;
    data->result_ptr[3][data->feat_id] = NA_REAL;

    return WK_CONTINUE;
}

int xyzm_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    // EMPTY and any set of features that (could) contain a single point work with this
    // handler! (error otherwise)
    if (meta->size != 0 &&
        meta->geometry_type != WK_POINT &&
        meta->geometry_type != WK_MULTIPOINT &&
        meta->geometry_type != WK_GEOMETRYCOLLECTION) {
        xyzm_writer_data_t* data = (xyzm_writer_data_t*) handler_data;
        Rf_error(
            "[%d] Can't convert geometry with type '%d' to coordinate",
            data->feat_id + 1,
            meta->geometry_type
        );
    }

    return WK_CONTINUE;
}

int xyzm_writer_coord(const wk_meta_t* meta, const wk_coord_t coord, uint32_t coord_id, void* handler_data) {
    xyzm_writer_data_t* data = (xyzm_writer_data_t*) handler_data;

    if (data->has_coord) {
        Rf_error("[%d] Feature contains more than one coordinate.", data->feat_id + 1);
    } else {
        data->has_coord = 1;
    }

    data->result_ptr[0][data->feat_id] = coord.v[0];
    data->result_ptr[1][data->feat_id] = coord.v[1];

    if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        data->result_ptr[2][data->feat_id] = NA_REAL;
        data->result_ptr[3][data->feat_id] = NA_REAL;
    } else if(meta->flags & WK_FLAG_HAS_Z) {
        data->result_ptr[2][data->feat_id] = coord.v[2];   
    } else if(meta->flags & WK_FLAG_HAS_M) {
        data->result_ptr[3][data->feat_id] = coord.v[2];
    }
    
    return WK_CONTINUE;
}

SEXP xyzm_writer_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    xyzm_writer_data_t* data = (xyzm_writer_data_t*) handler_data;
    return data->result;
}

void xyzm_writer_vector_finally(void* handler_data) {
    xyzm_writer_data_t* data = (xyzm_writer_data_t*) handler_data;
    if (data->result != R_NilValue) {
        R_ReleaseObject(data->result);
        data->result = R_NilValue;
    }
}

void xyzm_writer_finalize(void* handler_data) {
    xyzm_writer_data_t* data = (xyzm_writer_data_t*) handler_data;
    if (data != NULL) {
        free(data);
    }
}

SEXP wk_c_xyzm_writer_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &xyzm_writer_vector_start;
    handler->feature_start = &xyzm_writer_feature_start;
    handler->geometry_start = &xyzm_writer_geometry_start;
    handler->coord = &xyzm_writer_coord;
    handler->vector_end = &xyzm_writer_vector_end;
    handler->vector_finally = &xyzm_writer_vector_finally;
    handler->finalizer = &xyzm_writer_finalize;
    
    xyzm_writer_data_t* data = (xyzm_writer_data_t*) malloc(sizeof(xyzm_writer_data_t));
    data->feat_id = 0;
    data->has_coord = 0;
    data->result = R_NilValue;
    memset(data->result_ptr, 0, 4);

    handler->handler_data = data;

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}
