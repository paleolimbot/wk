
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>

#define SFC_GEOMETRY_TYPE_NOT_YET_DEFINED -1
#define SFC_MAX_RECURSION_DEPTH 32
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b)) 

typedef struct {
    SEXP sfc;
    SEXP geom[SFC_MAX_RECURSION_DEPTH];
    size_t recursion_level;
    // xmin, ymin, xmax, ymax
    double bbox[4];
    // zmin, zmax
    double z_range[2];
    // mmin, mmax
    double m_range[2];
    int geometry_type;
    R_xlen_t n_empty;
} sfc_writer_t;

sfc_writer_t* sfc_writer_new() {
    sfc_writer_t* writer = (sfc_writer_t*) malloc(sizeof(sfc_writer_t));
    writer->sfc = R_NilValue;
    for (int i = 0; i < SFC_MAX_RECURSION_DEPTH; i++) {
        writer->geom[i] = R_NilValue;
    }

    writer->recursion_level = 0;

    writer->bbox[0] = R_PosInf;
    writer->bbox[1] = R_PosInf;
    writer->bbox[2] = R_NegInf;
    writer->bbox[3] = R_NegInf;

    writer->z_range[0] = R_PosInf;
    writer->z_range[1] = R_NegInf;

    writer->m_range[0] = R_PosInf;
    writer->m_range[1] = R_NegInf;

    writer->geometry_type = SFC_GEOMETRY_TYPE_NOT_YET_DEFINED;

    return writer;
}

void sfc_writer_update_geometry_type_and_empty_count(sfc_writer_t* writer, wk_meta_t* meta) {
    if (meta->size == 0) {
        writer->n_empty++;
    }

    if (writer->geometry_type == SFC_GEOMETRY_TYPE_NOT_YET_DEFINED) {
        writer->geometry_type = meta->geometry_type;
    } else if (writer->geometry_type != meta->geometry_type) {
        writer->geometry_type = WK_GEOMETRY;
    }
}

void sfc_writer_update_ranges(sfc_writer_t* writer, wk_meta_t* meta, wk_coord_t coord) {
    writer->bbox[0] = MIN(writer->bbox[0], coord.v[0]);
    writer->bbox[1] = MIN(writer->bbox[1], coord.v[1]);
    writer->bbox[2] = MAX(writer->bbox[2], coord.v[0]);
    writer->bbox[3] = MAX(writer->bbox[3], coord.v[1]);

    if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        writer->z_range[0] = MIN(writer->z_range[0], coord.v[2]);
        writer->z_range[1] = MAX(writer->z_range[1], coord.v[2]);
        writer->m_range[0] = MIN(writer->m_range[0], coord.v[3]);
        writer->m_range[1] = MAX(writer->m_range[1], coord.v[3]);
    } else if (meta->flags & WK_FLAG_HAS_Z) {
        writer->z_range[0] = MIN(writer->z_range[0], coord.v[2]);
        writer->z_range[1] = MAX(writer->z_range[1], coord.v[2]);
    } else if (meta->flags & WK_FLAG_HAS_M) {
        writer->m_range[0] = MIN(writer->m_range[0], coord.v[2]);
        writer->m_range[1] = MAX(writer->m_range[1], coord.v[2]);
    }
}

void sfc_writer_finalize(void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    if (writer != NULL) {
        free(writer);
    }
}

SEXP wk_c_sfc_writer_new() {
    wk_handler_t* handler = wk_handler_create();
    handler->finalizer = &sfc_writer_finalize;

    handler->handler_data = sfc_writer_new();

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}


