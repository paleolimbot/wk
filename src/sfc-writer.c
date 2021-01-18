
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>

#define SFC_FLAGS_NOT_YET_DEFINED UINT32_MAX
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
    uint32_t flags;
    R_xlen_t n_empty;
    // sfc requires empty geometries to have their dimensions marked
    // however, most readers have no way of knowing the dimensions of
    // an empty
    R_xlen_t first_flags_id;
    R_xlen_t feat_id;
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
    writer->flags = SFC_FLAGS_NOT_YET_DEFINED;
    writer->n_empty = 0;
    writer->first_flags_id = -1;
    writer->feat_id = -1;

    return writer;
}

// this is intended to replicate NA_crs_
SEXP sfc_na_crs() {
    const char* crs_names[] = {"input", "wkt", ""};
    SEXP crs = PROTECT(Rf_mkNamed(VECSXP, crs_names));
    SEXP crs_input = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(crs_input, 0, NA_STRING);
    SET_VECTOR_ELT(crs, 0, crs_input);
    UNPROTECT(1);
    SEXP crs_wkt = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(crs_wkt, 0, NA_STRING);
    SET_VECTOR_ELT(crs, 1, crs_wkt);
    UNPROTECT(1);
    Rf_setAttrib(crs, R_ClassSymbol, Rf_mkString("crs"));
    UNPROTECT(1);
    return crs;
}

SEXP sfc_class_sfg(int geometry_type, uint32_t flags) {
    if (flags == SFC_FLAGS_NOT_YET_DEFINED) {
        flags = 0;
    }

    // in the form XY(ZM), GEOM_TYPE, sfg
    SEXP class = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(class, 2, Rf_mkChar("sfg"));

    if ((flags & WK_FLAG_HAS_Z) && (flags & WK_FLAG_HAS_M)) {
        SET_STRING_ELT(class, 0, Rf_mkChar("XYZM"));
    } else if (flags & WK_FLAG_HAS_Z) {
        SET_STRING_ELT(class, 0, Rf_mkChar("XYZ"));
    } else if (flags & WK_FLAG_HAS_M) {
        SET_STRING_ELT(class, 0, Rf_mkChar("XYM"));
    }

    switch (geometry_type) {
    case WK_POINT:
        SET_STRING_ELT(class, 1, Rf_mkChar("POINT"));
        break;
    case WK_LINESTRING:
        SET_STRING_ELT(class, 1, Rf_mkChar("LINESTRING"));
        break;
    case WK_POLYGON:
        SET_STRING_ELT(class, 1, Rf_mkChar("POLYGON"));
        break;
    case WK_MULTIPOINT:
        SET_STRING_ELT(class, 1, Rf_mkChar("MULTIPOINT"));
        break;
    case WK_MULTILINESTRING:
        SET_STRING_ELT(class, 1, Rf_mkChar("MULTILINESTRING"));
        break;
    case WK_MULTIPOLYGON:
        SET_STRING_ELT(class, 1, Rf_mkChar("MULTIPOLYGON"));
        break;
    case WK_GEOMETRYCOLLECTION:
        SET_STRING_ELT(class, 1, Rf_mkChar("GEOMETRYCOLLECTION"));
        break;
    default:
        Rf_error("Can't generate empty 'sfg' for geometry type '%d'", geometry_type);
    }

    UNPROTECT(1);
    return class;
}

SEXP sfc_alloc_sfg(int geometry_type, uint32_t size, uint32_t flags) {
    if (flags == SFC_FLAGS_NOT_YET_DEFINED) {
        flags = 0;
    }

    SEXP result = R_NilValue;

    int coord_size = 2;
    if ((flags & WK_FLAG_HAS_Z) && (flags & WK_FLAG_HAS_M)) {
        coord_size = 4;
    } else if (flags & WK_FLAG_HAS_Z) {
        coord_size = 3;
    } else if (flags & WK_FLAG_HAS_M) {
        coord_size = 3;
    }
    
    switch (geometry_type) {
    case WK_POINT:
        result = PROTECT(Rf_allocVector(REALSXP, coord_size));
        if (size == 0) {
            for (int i = 0; i < coord_size; i++) {
                REAL(result)[i] = NA_REAL;
            }
        }
        break;
    case WK_LINESTRING:
        result = PROTECT(Rf_allocMatrix(REALSXP, size, coord_size));
        break;
    case WK_POLYGON:
        result = PROTECT(Rf_allocVector(VECSXP, size));
        break;
    case WK_MULTIPOINT:
        result = PROTECT(Rf_allocMatrix(REALSXP, size, coord_size));
        break;
    case WK_MULTILINESTRING:
        result = PROTECT(Rf_allocVector(VECSXP, size));
        break;
    case WK_MULTIPOLYGON:
        result = PROTECT(Rf_allocVector(VECSXP, size));
        break;
    case WK_GEOMETRYCOLLECTION:
        result = PROTECT(Rf_allocVector(VECSXP, size));
        break;
    default:
        Rf_error("Can't generate empty 'sfg' for geometry type '%d'", geometry_type);
    }

    UNPROTECT(1);
    return result;
}

void sfc_writer_update_vector_attributes(sfc_writer_t* writer, const wk_meta_t* meta) {
    if (meta->size == 0) {
        writer->n_empty++;
    }

    if (writer->geometry_type == SFC_GEOMETRY_TYPE_NOT_YET_DEFINED) {
        writer->geometry_type = meta->geometry_type;
        writer->first_flags_id = writer->feat_id;
    } else if (writer->geometry_type != meta->geometry_type) {
        writer->geometry_type = WK_GEOMETRY;
    }

    // sfc objects must have consistent dimensions!
    if (writer->flags == SFC_FLAGS_NOT_YET_DEFINED) {
        writer->flags = meta->flags;
    } else if (writer->flags != meta->flags) {
        Rf_error("Can't convert geometries with incompatible dimensions to 'sfc'");
    }
}

void sfc_writer_update_ranges(sfc_writer_t* writer, const wk_meta_t* meta, wk_coord_t coord) {
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

int sfc_writer_vector_start(const wk_vector_meta_t* vector_meta, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    if (writer->sfc != R_NilValue) {
        Rf_error("Can't allocate destination 'sfc': destination was already allocated"); // # nocov
    }

    writer->sfc = PROTECT(Rf_allocVector(VECSXP, vector_meta->size));
    R_PreserveObject(writer->sfc);
    UNPROTECT(1);
    return WK_CONTINUE;
}

int sfc_writer_feature_start(const wk_vector_meta_t* vector_meta, R_xlen_t feat_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    writer->feat_id = feat_id;
    writer->recursion_level = 0;
    return WK_CONTINUE;
}

int sfc_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    if (writer->recursion_level == 0) {
        sfc_writer_update_vector_attributes(writer, meta);
    }

    writer->recursion_level++;
    return WK_CONTINUE;
}

int sfc_writer_coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t coord_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    sfc_writer_update_ranges(writer, meta, coord);
    return WK_CONTINUE;
}

int sfc_writer_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    writer->recursion_level--;
    return WK_CONTINUE;
}

int sfc_writer_feature_end(const wk_vector_meta_t* vector_meta, R_xlen_t feat_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 0));

    SEXP class = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(class, 0, Rf_mkChar("XY"));
    SET_STRING_ELT(class, 1, Rf_mkChar("GEOMETRYCOLLECTION"));
    SET_STRING_ELT(class, 2, Rf_mkChar("sfg"));

    Rf_setAttrib(result, R_ClassSymbol, class);
    UNPROTECT(1);

    SET_VECTOR_ELT(writer->sfc, feat_id, result);
    UNPROTECT(1);

    return WK_CONTINUE;
}

SEXP sfc_writer_vector_end(const wk_vector_meta_t* vector_meta, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;

    // attr(sfc, "precision")
    Rf_setAttrib(writer->sfc, Rf_install("precision"), Rf_ScalarReal(0));

    // attr(sfc, "bbox")
    const char* bbox_names[] = {"xmin", "ymin", "xmax", "ymax", ""};
    SEXP bbox = PROTECT(Rf_mkNamed(REALSXP, bbox_names));
    Rf_setAttrib(bbox, R_ClassSymbol, Rf_mkString("bbox"));

    // the bounding box may or may not have a crs attribute
    if (vector_meta->size == 0) {
        Rf_setAttrib(bbox, Rf_install("crs"), sfc_na_crs());
    }
    
    if (writer->n_empty == vector_meta->size) {
        writer->bbox[0] = NA_REAL;
        writer->bbox[1] = NA_REAL;
        writer->bbox[2] = NA_REAL;
        writer->bbox[3] = NA_REAL;
    }
    memcpy(REAL(bbox), writer->bbox, sizeof(double) * 4);
    Rf_setAttrib(writer->sfc, Rf_install("bbox"), bbox);
    UNPROTECT(1);

    // attr(sfc, "z_range"), attr(sfc, "m_range")
    if (writer->flags == SFC_FLAGS_NOT_YET_DEFINED) {
        writer->flags = 0;
    }

    if (writer->flags & WK_FLAG_HAS_Z) {
        const char* z_range_names[] = {"zmin", "zmax", ""};
        SEXP z_range = PROTECT(Rf_mkNamed(REALSXP, z_range_names));
        memcpy(REAL(z_range), writer->z_range, sizeof(double) * 2);
        Rf_setAttrib(writer->sfc, Rf_install("z_range"), z_range);
        UNPROTECT(1);
    }

    if (writer->flags & WK_FLAG_HAS_M) {
        const char* m_range_names[] = {"mmin", "mmax", ""};
        SEXP m_range = PROTECT(Rf_mkNamed(REALSXP, m_range_names));
        memcpy(REAL(m_range), writer->m_range, sizeof(double) * 2);
        Rf_setAttrib(writer->sfc, Rf_install("m_range"), m_range);
        UNPROTECT(1);
    }
    
    // attr(sfc, "crs")
    // this should be handled in R; however, inserting a placeholder here
    // because the print() method for sfc will error otherwise
    Rf_setAttrib(writer->sfc, Rf_install("crs"), sfc_na_crs());

    // attr(sfc, "n_empty")
    Rf_setAttrib(writer->sfc, Rf_install("n_empty"), Rf_ScalarInteger(writer->n_empty));

    // class(sfc)
    SEXP class = PROTECT(Rf_allocVector(STRSXP, 2));
    switch (writer->geometry_type) {
    case WK_POINT:
        SET_STRING_ELT(class, 0, Rf_mkChar("sfc_POINT"));
        break;
    case WK_LINESTRING:
        SET_STRING_ELT(class, 0, Rf_mkChar("sfc_LINESTRING"));
        break;
    case WK_POLYGON:
        SET_STRING_ELT(class, 0, Rf_mkChar("sfc_POLYGON"));
        break;
    case WK_MULTIPOINT:
        SET_STRING_ELT(class, 0, Rf_mkChar("sfc_MULTIPOINT"));
        break;
    case WK_MULTILINESTRING:
        SET_STRING_ELT(class, 0, Rf_mkChar("sfc_MULTILINESTRING"));
        break;
    case WK_MULTIPOLYGON:
        SET_STRING_ELT(class, 0, Rf_mkChar("sfc_MULTIPOLYGON"));
        break;
    case WK_GEOMETRYCOLLECTION:
        SET_STRING_ELT(class, 0, Rf_mkChar("sfc_GEOMETRYCOLLECTION"));
        break;
    default:
        SET_STRING_ELT(class, 0, Rf_mkChar("sfc_GEOMETRY"));
        break;
    }
    SET_STRING_ELT(class, 1, Rf_mkChar("sfc"));
    Rf_setAttrib(writer->sfc, R_ClassSymbol, class);
    UNPROTECT(1);

    // attr(sfc, "classes") (only for zero-length)
    if (vector_meta->size == 0) {
        Rf_setAttrib(writer->sfc, Rf_install("classes"), Rf_allocVector(STRSXP, 0));
    }

    return writer->sfc;
}

void sfc_writer_vector_finally(void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;

    if (writer->sfc != R_NilValue) {
        R_ReleaseObject(writer->sfc);
        writer->sfc = R_NilValue;
    }

    for (int i = 0; i < SFC_MAX_RECURSION_DEPTH; i++) {
        if (writer->geom[i] != R_NilValue) {
            R_ReleaseObject(writer->geom[i]);
            writer->geom[i] = R_NilValue;
        }
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
    handler->vector_start = &sfc_writer_vector_start;
    handler->feature_start = &sfc_writer_feature_start;
    handler->geometry_start = &sfc_writer_geometry_start;
    handler->coord = &sfc_writer_coord;
    handler->geometry_end = &sfc_writer_geometry_end;
    handler->feature_end = &sfc_writer_feature_end;
    handler->vector_end = &sfc_writer_vector_end;
    handler->vector_finally = &sfc_writer_vector_finally;

    handler->handler_data = sfc_writer_new();

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}
