
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include "wk-v1.h"

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b)) 

typedef struct {
    // used for bbox and envelope handlers
    double xmin, ymin, xmax, ymax;
    int use_geom_meta_bbox;

    // used for the envelope handler
    SEXP result;
    double* result_ptr[4];
    R_xlen_t result_size;
    R_xlen_t feat_id;
} wk_bbox_handler_data_t;

static inline SEXP wk_bbox_handler_alloc_result(R_xlen_t size) {
    const char* names[] = {"xmin", "ymin", "xmax", "ymax", ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(result, 0, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(result, 1, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(result, 2, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(result, 3, Rf_allocVector(REALSXP, size));

    UNPROTECT(1);
    return result;
}

static inline SEXP wk_bbox_handler_realloc_result(SEXP result, R_xlen_t new_size) {
    SEXP new_result = PROTECT(wk_bbox_handler_alloc_result(new_size));

    R_xlen_t size_cpy;
    if (Rf_xlength(VECTOR_ELT(result, 0)) < new_size) {
        size_cpy = Rf_xlength(VECTOR_ELT(result, 0));
    } else {
        size_cpy = new_size;
    }

    for (int i = 0; i < 4; i ++) {
        memcpy(
            REAL(VECTOR_ELT(new_result, i)), 
            REAL(VECTOR_ELT(result, i)), 
            sizeof(double) * size_cpy
        );
    }

    UNPROTECT(1);
    return new_result;
}

static inline void wk_bbox_handler_append(wk_bbox_handler_data_t* writer,
                                          double xmin, double ymin, double xmax, double ymax) {
    if (writer->feat_id >= writer->result_size) {
        SEXP new_result = PROTECT(wk_bbox_handler_realloc_result(writer->result, writer->result_size * 2 + 1));
        R_ReleaseObject(writer->result);
        writer->result = new_result;
        R_PreserveObject(writer->result);
        UNPROTECT(1);
        writer->result_size = writer->result_size * 2 + 1;
        for (int i = 0; i < 4; i++) {
            writer->result_ptr[i] = REAL(VECTOR_ELT(writer->result, i));
        }
    }

    writer->result_ptr[0][writer->feat_id] = xmin;
    writer->result_ptr[1][writer->feat_id] = ymin;
    writer->result_ptr[2][writer->feat_id] = xmax;
    writer->result_ptr[3][writer->feat_id] = ymax;

    writer->feat_id++;
}

int wk_bbox_handler_vector_start(const wk_vector_meta_t* vector_meta, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;

    if (vector_meta->flags & WK_FLAG_HAS_BOUNDS) {
        data->xmin = vector_meta->bounds_min[0];
        data->ymin = vector_meta->bounds_min[1];
        data->xmax = vector_meta->bounds_max[0];
        data->ymax = vector_meta->bounds_max[1];
        return WK_ABORT;
    } else {
        return WK_CONTINUE;
    }
}

int wk_bbox_handler_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;
    data->use_geom_meta_bbox = 1;
    return WK_CONTINUE;
}

int wk_bbox_handler_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;

    if (data->use_geom_meta_bbox && (meta->flags & WK_FLAG_HAS_BOUNDS)) {
        data->xmin = MIN(meta->bounds_min[0], data->xmin);
        data->ymin = MIN(meta->bounds_min[1], data->ymin);
        data->xmax = MAX(meta->bounds_max[0], data->xmax);
        data->ymax = MAX(meta->bounds_max[1], data->ymax);
        return WK_ABORT_FEATURE;
    } else {
        data->use_geom_meta_bbox = 0;
        return WK_CONTINUE;
    }
}

int wk_bbox_handler_coord(const wk_meta_t* meta, const double* coord, 
                          uint32_t coord_id, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;
    data->xmin = MIN(coord[0], data->xmin);
    data->ymin = MIN(coord[1], data->ymin);
    data->xmax = MAX(coord[0], data->xmax);
    data->ymax = MAX(coord[1], data->ymax);
    return WK_CONTINUE;
}

SEXP wk_bbox_handler_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;

    const char* names[] = {"xmin", "ymin", "xmax", "ymax", ""};
    SEXP output = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(output, 0, Rf_ScalarReal(data->xmin));
    SET_VECTOR_ELT(output, 1, Rf_ScalarReal(data->ymin));
    SET_VECTOR_ELT(output, 2, Rf_ScalarReal(data->xmax));
    SET_VECTOR_ELT(output, 3, Rf_ScalarReal(data->ymax));

    SEXP rct_class = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(rct_class, 0, Rf_mkChar("wk_rct"));
    SET_STRING_ELT(rct_class, 1, Rf_mkChar("wk_rcrd"));
    Rf_setAttrib(output, R_ClassSymbol, rct_class);
    UNPROTECT(1);

    UNPROTECT(1);
    return output;
}

void wk_bbox_handler_finalize(void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;
    free(data);
}

SEXP wk_c_bbox_handler_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &wk_bbox_handler_vector_start;
    handler->feature_start = &wk_bbox_handler_feature_start;
    handler->geometry_start = &wk_bbox_handler_geometry_start;
    handler->coord = &wk_bbox_handler_coord;
    handler->vector_end = &wk_bbox_handler_vector_end;
    handler->finalizer = &wk_bbox_handler_finalize;

    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) malloc(sizeof(wk_bbox_handler_data_t));
    if (data == NULL) {
        wk_handler_destroy(handler); // # nocov
        Rf_error("Failed to alloc handler data"); // # nocov
    }

    data->xmin = R_PosInf;
    data->ymin = R_PosInf;
    data->xmax = R_NegInf;
    data->ymax = R_NegInf;

    data->result_size = 0;
    data->feat_id = 0;
    data->use_geom_meta_bbox = 1;
    data->result = R_NilValue;
    for (int i = 0; i < 4; i++) {
        data->result_ptr[i] = NULL;
    }

    handler->handler_data = data;

    return wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
}


int wk_envelope_handler_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;

    if (data->result != R_NilValue) {
        Rf_error("Destination vector was already allocated"); // # nocov
    }

    if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        data->result = PROTECT(wk_bbox_handler_alloc_result(1024));
        data->result_size = 1024;
    } else {
        data->result = PROTECT(wk_bbox_handler_alloc_result(meta->size));
        data->result_size = meta->size;
    }

    R_PreserveObject(data->result);
    UNPROTECT(1);

    for (int i = 0; i < 4; i++) {
        data->result_ptr[i] = REAL(VECTOR_ELT(data->result, i));
    }

    data->feat_id = 0;

    return WK_CONTINUE;
}

int wk_envelope_handler_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;
    data->xmin = R_PosInf;
    data->ymin = R_PosInf;
    data->xmax = R_NegInf;
    data->ymax = R_NegInf;
    data->use_geom_meta_bbox = 1;
    return WK_CONTINUE;
}

int wk_envelope_handler_feature_null(void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;
    data->xmin = NA_REAL;
    data->ymin = NA_REAL;
    data->xmax = NA_REAL;
    data->ymax = NA_REAL;
    return WK_CONTINUE;
}

int wk_envelope_handler_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;

    if (data->use_geom_meta_bbox && (meta->flags & WK_FLAG_HAS_BOUNDS)) {
        data->xmin = MIN(meta->bounds_min[0], data->xmin);
        data->ymin = MIN(meta->bounds_min[1], data->ymin);
        data->xmax = MAX(meta->bounds_max[0], data->xmax);
        data->ymax = MAX(meta->bounds_max[1], data->ymax);
        wk_bbox_handler_append(data, data->xmin, data->ymin, data->xmax, data->ymax);
        return WK_ABORT_FEATURE;
    } else {
        data->use_geom_meta_bbox = 0;
        return WK_CONTINUE;
    }
}

int wk_envelope_handler_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;
    wk_bbox_handler_append(data, data->xmin, data->ymin, data->xmax, data->ymax);
    return WK_CONTINUE;
}

SEXP wk_envelope_handler_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;

    R_xlen_t final_size = data->feat_id;
    if (final_size != data->result_size) {
        SEXP new_result = PROTECT(wk_bbox_handler_realloc_result(data->result, final_size));
        R_ReleaseObject(data->result);
        data->result = new_result;
        R_PreserveObject(data->result);
        UNPROTECT(1);
    }

    SEXP rct_class = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(rct_class, 0, Rf_mkChar("wk_rct"));
    SET_STRING_ELT(rct_class, 1, Rf_mkChar("wk_rcrd"));
    Rf_setAttrib(data->result, R_ClassSymbol, rct_class);
    UNPROTECT(1);

    return data->result;
}

void wk_envelope_handler_deinitialize(void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;
    if (data->result != R_NilValue) {
        R_ReleaseObject(data->result);
        data->result = R_NilValue;
    }
}

SEXP wk_c_envelope_handler_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->vector_start = &wk_envelope_handler_vector_start;
    handler->feature_start = &wk_envelope_handler_feature_start;
    handler->null_feature = &wk_envelope_handler_feature_null;
    handler->geometry_start = &wk_envelope_handler_geometry_start;
    handler->coord = &wk_bbox_handler_coord;
    handler->feature_end = &wk_envelope_handler_feature_end;
    handler->vector_end = &wk_envelope_handler_vector_end;
    handler->finalizer = &wk_bbox_handler_finalize;
    handler->deinitialize = &wk_envelope_handler_deinitialize;

    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) malloc(sizeof(wk_bbox_handler_data_t));
    if (data == NULL) {
        wk_handler_destroy(handler); // # nocov
        Rf_error("Failed to alloc handler data"); // # nocov
    }

    data->xmin = R_PosInf;
    data->ymin = R_PosInf;
    data->xmax = R_NegInf;
    data->ymax = R_NegInf;

    data->result_size = 0;
    data->feat_id = 0;
    data->use_geom_meta_bbox = 1;
    data->result = R_NilValue;
    for (int i = 0; i < 4; i++) {
        data->result_ptr[i] = NULL;
    }

    handler->handler_data = data;

    return wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
}
