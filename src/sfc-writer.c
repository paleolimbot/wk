
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>

#define SFC_FLAGS_NOT_YET_DEFINED UINT32_MAX
#define SFC_GEOMETRY_TYPE_NOT_YET_DEFINED -1
#define SFC_MAX_RECURSION_DEPTH 32
#define SFC_WRITER_GEOM_LENGTH SFC_MAX_RECURSION_DEPTH + 2
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b)) 

typedef struct {
    // output vector list()
    SEXP sfc;
    // container list() geometries
    SEXP geom[SFC_WRITER_GEOM_LENGTH];
    // keep track of recursion level
    size_t recursion_level;
    
    // the current coordinate sequence and information about
    // where we are in the coordinate sequence
    SEXP coord_seq;
    int coord_size;
    uint32_t coord_id;
    int coord_seq_rows;

    // attr(sfc, "bbox"): xmin, ymin, xmax, ymax
    double bbox[4];
    // attr(sfc, "z_range"): zmin, zmax
    double z_range[2];
    // attr(sfc, "m+range"): mmin, mmax
    double m_range[2];
    // used to tell if all items are the same type for output class
    int geometry_type;
    // used to enforce requirement that all sub geometries to have the same dimensions
    uint32_t flags;
    // attr(sfc, "n_empty")
    R_xlen_t n_empty;
    // needed to access feat_id in geometry handlers
    R_xlen_t feat_id;
} sfc_writer_t;

sfc_writer_t* sfc_writer_new() {
    sfc_writer_t* writer = (sfc_writer_t*) malloc(sizeof(sfc_writer_t));

    writer->sfc = R_NilValue;
    for (int i = 0; i < SFC_MAX_RECURSION_DEPTH; i++) {
        writer->geom[i] = R_NilValue;
    }
    writer->recursion_level = 0;

    writer->coord_seq = R_NilValue;
    writer->coord_id = -1;
    writer->coord_size = 2;
    writer->coord_seq_rows = -1;

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

void sfc_writer_maybe_add_class_to_sfg(sfc_writer_t* writer, SEXP item, const wk_meta_t* meta) {
    if (writer->recursion_level > 0 && 
        (meta->geometry_type == WK_LINESTRING || meta->geometry_type == WK_POLYGON)) {
            return;
        }

    // in the form XY(ZM), GEOM_TYPE, sfg
    SEXP class = PROTECT(Rf_allocVector(STRSXP, 3));
    SET_STRING_ELT(class, 2, Rf_mkChar("sfg"));

    if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        SET_STRING_ELT(class, 0, Rf_mkChar("XYZM"));
    } else if (meta->flags & WK_FLAG_HAS_Z) {
        SET_STRING_ELT(class, 0, Rf_mkChar("XYZ"));
    } else if (meta->flags & WK_FLAG_HAS_M) {
        SET_STRING_ELT(class, 0, Rf_mkChar("XYM"));
    } else {
        SET_STRING_ELT(class, 0, Rf_mkChar("XY"));
    }

    switch (meta->geometry_type) {
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
        Rf_error("Can't generate empty 'sfg' for geometry type '%d'", meta->geometry_type);
    }

    Rf_setAttrib(item, R_ClassSymbol, class);
    UNPROTECT(1);
}

int sfc_writer_is_nesting_geometrycollection(sfc_writer_t* writer) {
    return (writer->recursion_level > 0) &&
        Rf_inherits(writer->geom[writer->recursion_level - 1], "GEOMETRYCOLLECTION");
}

int sfc_writer_is_nesting_multipoint(sfc_writer_t* writer) {
    return Rf_inherits(writer->coord_seq, "MULTIPOINT");
}

void sfc_writer_update_vector_attributes(sfc_writer_t* writer, const wk_meta_t* meta) {
    if (meta->size == 0) {
        writer->n_empty++;
    }

    if (writer->geometry_type == SFC_GEOMETRY_TYPE_NOT_YET_DEFINED) {
        writer->geometry_type = meta->geometry_type;
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
    } else if ((writer->recursion_level < 0) || (writer->recursion_level >= SFC_MAX_RECURSION_DEPTH)) {
        Rf_error("Invalid recursion depth whilst parsing 'sfg': %d", writer->recursion_level);
    }

    if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        writer->coord_size = 4;
    } else if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        writer->coord_size = 3;
    } else {
        writer->coord_size = 2;
    }

    // ignore start of POINT nested in MULTIPOINT
    if (sfc_writer_is_nesting_multipoint(writer)) {
        return WK_CONTINUE;
    }

    // if POINT, LINESTRING, or MULTIPOINT
    // replace coordinate sequence with a fresh one
    // otherwise, create a list() container and push it to the writer->geom[] stack
    switch (meta->geometry_type) {
    case WK_POINT:
        if (writer->coord_seq != R_NilValue) R_ReleaseObject(writer->coord_seq);
        writer->coord_seq = PROTECT(Rf_allocVector(REALSXP, writer->coord_size));

        // empty point is NA, NA ...
        if (meta->size == 0) {
            for (int i = 0; i < writer->coord_size; i++) {
                REAL(writer->coord_seq)[i] = NA_REAL;
            }
        }

        sfc_writer_maybe_add_class_to_sfg(writer, writer->coord_seq, meta);
        R_PreserveObject(writer->coord_seq);
        UNPROTECT(1);
        writer->coord_id = 0;
        writer->coord_seq_rows = 1;
        break;
    case WK_LINESTRING:
    case WK_MULTIPOINT:
        if (writer->coord_seq != R_NilValue) R_ReleaseObject(writer->coord_seq);
        writer->coord_seq = PROTECT(Rf_allocMatrix(REALSXP, meta->size, writer->coord_size));
        sfc_writer_maybe_add_class_to_sfg(writer, writer->coord_seq, meta);
        R_PreserveObject(writer->coord_seq);
        UNPROTECT(1);
        writer->coord_id = 0;
        writer->coord_seq_rows = meta->size;
        break;
    case WK_POLYGON:
    case WK_MULTILINESTRING:
    case WK_MULTIPOLYGON:
    case WK_GEOMETRYCOLLECTION:
        if (writer->geom[writer->recursion_level] != R_NilValue) {
            R_ReleaseObject(writer->geom[writer->recursion_level]);
        }

        writer->geom[writer->recursion_level] = PROTECT(Rf_allocVector(VECSXP, meta->size));
        sfc_writer_maybe_add_class_to_sfg(writer, writer->geom[writer->recursion_level], meta);
        R_PreserveObject(writer->geom[writer->recursion_level]);
        UNPROTECT(1);
        break;
    default:
        Rf_error("Can't convert geometry type '%d' to sfg", meta->geometry_type); // # nocov
        break; // # nocov
    }

    writer->recursion_level++;
    return WK_CONTINUE;
}

int sfc_writer_coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t coord_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;

    sfc_writer_update_ranges(writer, meta, coord);

    // check that coord_seq isn't under-allocated
    if ((writer->coord_size * (writer->coord_id + 1)) > Rf_xlength(writer->coord_seq)) {
        Rf_error("Attempt to set out-of-bounds coordinate");
    }

    // could be faster to store current_values in writer, but REAL()
    // providess a nice check that the pointer will be valid
    double* current_values = REAL(writer->coord_seq);
    for (int i = 0; i < writer->coord_size; i++) {
        current_values[i * writer->coord_seq_rows + writer->coord_id] = coord.v[i];
    }

    writer->coord_id++;
    return WK_CONTINUE;
}

int sfc_writer_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;

    // ignore end of POINT nested in MULTIPOINT
    if ((meta->geometry_type == WK_POINT) && sfc_writer_is_nesting_multipoint(writer)) {
        return WK_CONTINUE;
    }

    writer->recursion_level--;

    SEXP geom;
    switch(meta->geometry_type) {
    case WK_POINT:
    case WK_LINESTRING:
    case WK_MULTIPOINT:
        geom = PROTECT(writer->coord_seq);
        R_ReleaseObject(writer->coord_seq);
        writer->coord_seq = R_NilValue;
        break;
    case WK_POLYGON:
    case WK_MULTILINESTRING:
    case WK_MULTIPOLYGON:
    case WK_GEOMETRYCOLLECTION:
        geom = PROTECT(writer->geom[writer->recursion_level]);
        // R_RelaseObject() is called on `geom` in finalize() or
        // when it is replaced in geometry_start()
        break;
    default:
        Rf_error("Can't convert geometry type '%d' to sfg", meta->geometry_type); // # nocov
        break; // # nocov
    }

    // if we're above a top-level geometry, this geometry needs to be added to the parent
    // otherwise, it needs to be added to sfc
    if (writer->recursion_level > 0) {
        // check that we did not under allocate the container
        SEXP container = writer->geom[writer->recursion_level - 1];
        SET_VECTOR_ELT(container, part_id, geom);
    } else {
        SET_VECTOR_ELT(writer->sfc, writer->feat_id, geom);
    }

    UNPROTECT(1);
    return WK_CONTINUE;
}

int sfc_writer_feature_end(const wk_vector_meta_t* vector_meta, R_xlen_t feat_id, void* handler_data) {
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

    for (int i = 0; i < (SFC_WRITER_GEOM_LENGTH); i++) {
        if (writer->geom[i] != R_NilValue) {
            R_ReleaseObject(writer->geom[i]);
            writer->geom[i] = R_NilValue;
        }
    }

    if (writer->coord_seq != R_NilValue) {
        R_ReleaseObject(writer->coord_seq);
        writer->coord_seq = R_NilValue;
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
