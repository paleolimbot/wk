
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include "wk-v1.h"

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b)) 

typedef struct {
    double xmin, ymin, xmax, ymax;
} wk_bbox_handler_data_t;

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

int wk_bbox_handler_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;

    if (meta->flags & WK_FLAG_HAS_BOUNDS) {
        data->xmin = MIN(meta->bounds_min[0], data->xmin);
        data->ymin = MIN(meta->bounds_min[1], data->ymin);
        data->xmax = MAX(meta->bounds_max[0], data->xmax);
        data->ymax = MAX(meta->bounds_max[1], data->ymax);
        return WK_ABORT_FEATURE;
    } else {
        return WK_CONTINUE;
    }
}

int wk_bbox_handler_coord(const wk_meta_t* meta, const wk_coord_t coord, 
                          uint32_t coord_id, void* handler_data) {
    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) handler_data;
    data->xmin = MIN(coord.v[0], data->xmin);
    data->ymin = MIN(coord.v[1], data->ymin);
    data->xmax = MAX(coord.v[0], data->xmax);
    data->ymax = MAX(coord.v[1], data->ymax);
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
    handler->geometry_start = &wk_bbox_handler_geometry_start;
    handler->coord = &wk_bbox_handler_coord;
    handler->vector_end = &wk_bbox_handler_vector_end;

    wk_bbox_handler_data_t* data = (wk_bbox_handler_data_t*) malloc(sizeof(wk_bbox_handler_data_t));
    data->xmin = R_PosInf;
    data->ymin = R_PosInf;
    data->xmax = R_NegInf;
    data->ymax = R_NegInf;
    handler->handler_data = data;

    return wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
}
