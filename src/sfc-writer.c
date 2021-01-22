
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>

#define SFC_FLAGS_NOT_YET_DEFINED UINT32_MAX
#define SFC_GEOMETRY_TYPE_NOT_YET_DEFINED -1
#define SFC_MAX_RECURSION_DEPTH 32
#define SFC_WRITER_GEOM_LENGTH SFC_MAX_RECURSION_DEPTH + 2
#define SFC_INITIAL_SIZE_IF_UNKNOWN 32
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
    // when all elements are empty, sfc holds the classes of these objects
    // so in addition to knowing the common geometry type, we need to know
    // all types that were encountered in the off chance that they are all empty
    // using a bitwise OR  with (1 << (wk geometry type))
    int all_geometry_types;
    // used to enforce requirement that all sub geometries to have the same dimensions
    uint32_t flags;
    // attr(sfc, "n_empty")
    R_xlen_t n_empty;
    // sfc views NULL as equivalent to EMPTY, but we can skip this replacement if
    // there were not any NULLs (almost 100% of the time)
    int any_null;
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
    writer->all_geometry_types = 0;
    writer->flags = SFC_FLAGS_NOT_YET_DEFINED;
    writer->n_empty = 0;
    writer->any_null = 0;
    writer->feat_id = -1;

    return writer;
}

int sfc_writer_is_nesting_geometrycollection(sfc_writer_t* writer) {
    return (writer->recursion_level > 0) &&
        Rf_inherits(writer->geom[writer->recursion_level - 1], "GEOMETRYCOLLECTION");
}

int sfc_writer_is_nesting_multipoint(sfc_writer_t* writer) {
    return Rf_inherits(writer->coord_seq, "MULTIPOINT");
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

SEXP sfc_writer_empty_sfg(int geometry_type, uint32_t flags) {
    SEXP result = R_NilValue;

    int coord_size;
    if ((flags & WK_FLAG_HAS_Z) && (flags & WK_FLAG_HAS_M)) {
        coord_size = 4;
    } else if ((flags & WK_FLAG_HAS_Z) && (flags & WK_FLAG_HAS_M)) {
        coord_size = 3;
    } else {
        coord_size = 2;
    }

    switch (geometry_type) {
    case WK_POINT:
        result = PROTECT(Rf_allocVector(REALSXP, coord_size));
        for (int i = 0; i < coord_size; i++) {
            REAL(result)[i] = NA_REAL;
        }
        break;
    case WK_LINESTRING:
        result = PROTECT(Rf_allocMatrix(REALSXP, 0, coord_size));
        break;
    case WK_POLYGON:
        result = PROTECT(Rf_allocVector(VECSXP, 0));
        break;
    case WK_MULTIPOINT:
        result = PROTECT(Rf_allocMatrix(REALSXP, 0, coord_size));
        break;
    case WK_MULTILINESTRING:
        result = PROTECT(Rf_allocVector(VECSXP, 0));
        break;
    case WK_MULTIPOLYGON:
        result = PROTECT(Rf_allocVector(VECSXP, 0));
        break;
    case WK_GEOMETRYCOLLECTION:
        result = PROTECT(Rf_allocVector(VECSXP, 0));
        break;
    default:
        Rf_error("Can't generate empty 'sfg' for geometry type '%d'", geometry_type);
    }

    UNPROTECT(1);
    return result;
}

void sfc_writer_maybe_add_class_to_sfg(sfc_writer_t* writer, SEXP item, const wk_meta_t* meta) {
    if (writer->recursion_level == 0 || sfc_writer_is_nesting_geometrycollection(writer)) {
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

    writer->all_geometry_types = writer->all_geometry_types | (1 << (meta->geometry_type - 1));

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

void sfc_writer_realloc_coord_seq(sfc_writer_t* writer, int new_size) {
    if (new_size == WK_SIZE_UNKNOWN) {
        new_size = SFC_INITIAL_SIZE_IF_UNKNOWN;
    }

    if (writer->coord_seq == R_NilValue) {
        writer->coord_seq = PROTECT(Rf_allocMatrix(REALSXP, new_size, writer->coord_size));
        writer->coord_seq_rows = new_size;
        R_PreserveObject(writer->coord_seq);
        UNPROTECT(1);
        return;
    } else if (new_size == writer->coord_seq_rows) {
        // don't need to reallocate!
        return;
    }

    if (!Rf_isMatrix(writer->coord_seq)) {
        Rf_error("Can't re-allocate a coordinate sequence that is not a matrix"); // # nocov
    }

    SEXP new_coord_seq = PROTECT(Rf_allocMatrix(REALSXP, new_size, writer->coord_size));        
    
    double* old_values = REAL(writer->coord_seq);
    double* new_values = REAL(new_coord_seq);

    int copy_size;
    if (new_size > writer->coord_seq_rows) {
        copy_size = writer->coord_seq_rows;
    } else {
        copy_size = new_size;
    }

    for (int j = 0; j < writer->coord_size; j++) {
        memcpy(
            new_values + (j * new_size),
            old_values + (j * writer->coord_seq_rows), 
            sizeof(double) * copy_size
        );
    }

    writer->coord_seq_rows = new_size;
    if (Rf_inherits(writer->coord_seq, "sfg")) {
        SEXP class = PROTECT(Rf_getAttrib(writer->coord_seq, R_ClassSymbol));
        Rf_setAttrib(new_coord_seq, R_ClassSymbol, class);
        UNPROTECT(1);
    }

    R_ReleaseObject(writer->coord_seq);
    writer->coord_seq = new_coord_seq;
    R_PreserveObject(writer->coord_seq);
    UNPROTECT(1);
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

int sfc_writer_null_feature(const wk_vector_meta_t* vector_meta, R_xlen_t feat_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    // sfc doesn't do NULLs and replaces them with GEOMETRYCOLLECTION EMPTY
    // however, as the dimensions have to align among features we asign a NULL here and fix
    // in vector_end()
    writer->any_null = 1;
    SET_VECTOR_ELT(writer->sfc, feat_id, R_NilValue);
    return WK_ABORT_FEATURE;
}

int sfc_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;

    // ignore start of POINT nested in MULTIPOINT
    if (sfc_writer_is_nesting_multipoint(writer)) {
        return WK_CONTINUE;
    }

    if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        writer->coord_size = 4;
    } else if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        writer->coord_size = 3;
    } else {
        writer->coord_size = 2;
    }

    if (writer->recursion_level == 0) {
        sfc_writer_update_vector_attributes(writer, meta);
    } else if ((writer->recursion_level < 0) || (writer->recursion_level >= SFC_MAX_RECURSION_DEPTH)) {
        Rf_error("Invalid recursion depth whilst parsing 'sfg': %d", writer->recursion_level);
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
        sfc_writer_realloc_coord_seq(writer, meta->size);
        sfc_writer_maybe_add_class_to_sfg(writer, writer->coord_seq, meta);
        writer->coord_id = 0;
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

int sfc_writer_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;

    sfc_writer_realloc_coord_seq(writer, size);
    writer->coord_id = 0;

    writer->recursion_level++;
    return WK_CONTINUE;
}

int sfc_writer_coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t coord_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;

    sfc_writer_update_ranges(writer, meta, coord);

    // realloc the coordinate sequence if necessary
    if (coord_id >= writer->coord_seq_rows) {
        sfc_writer_realloc_coord_seq(writer, coord_id * 1.5 + 1);
    }

    // could be faster to store current_values in writer, but REAL()
    // provides a nice check that the pointer will be valid
    double* current_values = REAL(writer->coord_seq);
    for (int i = 0; i < writer->coord_size; i++) {
        current_values[i * writer->coord_seq_rows + writer->coord_id] = coord.v[i];
    }

    writer->coord_id++;
    return WK_CONTINUE;
}

int sfc_writer_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;
    writer->recursion_level--;

    // the realloc here constrains writer->coord_seq to the actual number of coords
    // if the size was correct at the outset, this will do nothing
    sfc_writer_realloc_coord_seq(writer, writer->coord_id);

    SEXP geom = PROTECT(writer->coord_seq);
    R_ReleaseObject(writer->coord_seq);
    writer->coord_seq = R_NilValue;

    SET_VECTOR_ELT(writer->geom[writer->recursion_level - 1], ring_id, geom);
    UNPROTECT(1);

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
    case WK_LINESTRING:
    case WK_MULTIPOINT:
        // the realloc here constrains writer->coord_seq to the actual number of coords
        // if the size was correct at the outset, this will do nothing
        // don't do this for points, which should never be reallocated
        sfc_writer_realloc_coord_seq(writer, writer->coord_id);
        geom = PROTECT(writer->coord_seq);
        R_ReleaseObject(writer->coord_seq);
        writer->coord_seq = R_NilValue;
        break;
    case WK_POINT:
        geom = PROTECT(writer->coord_seq);
        R_ReleaseObject(writer->coord_seq);
        writer->coord_seq = R_NilValue;
        break;
    case WK_POLYGON:
    case WK_MULTILINESTRING:
    case WK_MULTIPOLYGON:
    case WK_GEOMETRYCOLLECTION:
        geom = PROTECT(writer->geom[writer->recursion_level]);
        // R_ReleaseObject() is called on `geom` in finalize() or
        // when it is replaced in geometry_start()
        break;
    default:
        Rf_error("Can't convert geometry type '%d' to sfg", meta->geometry_type); // # nocov
        break; // # nocov
    }

    // if we're above a top-level geometry, this geometry needs to be added to the parent
    // otherwise, it needs to be added to sfc
    if (writer->recursion_level > 0) {
        SET_VECTOR_ELT(writer->geom[writer->recursion_level - 1], part_id, geom);
    } else if (meta->geometry_type == WK_POINT) {
        // at the top level, we have to check if all point coordinates are NA
        // because this is 'empty' for the purposes of sfc
        double* values = REAL(geom);
        int all_na = 1;
        for (int i = 0; i < writer->coord_size; i++) {
            if (!ISNA(values[i]) && !ISNAN(values[i])) {
                all_na = 0;
                break;
            }
        }
        if (all_na) writer->n_empty++;

        SET_VECTOR_ELT(writer->sfc, writer->feat_id, geom);
    } else {
        SET_VECTOR_ELT(writer->sfc, writer->feat_id, geom);
    }

    UNPROTECT(1);
    return WK_CONTINUE;
}

SEXP sfc_writer_vector_end(const wk_vector_meta_t* vector_meta, void* handler_data) {
    sfc_writer_t* writer = (sfc_writer_t*) handler_data;

    // replace NULLs with EMPTY of an appropriate type
    if (writer->any_null) {
        wk_meta_t meta;
        if (writer->geometry_type == WK_GEOMETRY) {
            WK_META_RESET(meta, WK_GEOMETRYCOLLECTION);
            // also update the type list for attr(sfc, "classes")
            writer->all_geometry_types = writer->all_geometry_types | (1 << (WK_GEOMETRYCOLLECTION - 1));
        } else {
            WK_META_RESET(meta, writer->geometry_type);
        }
        meta.flags = writer->flags;
        meta.size = 0;
        writer->recursion_level = 0;
        SEXP empty = PROTECT(sfc_writer_empty_sfg(meta.geometry_type, meta.flags));
        sfc_writer_maybe_add_class_to_sfg(writer, empty, &meta);

        for (R_xlen_t i = 0; i < vector_meta->size; i++) {
            if (VECTOR_ELT(writer->sfc, i) == R_NilValue) {
                writer->n_empty++;
                SET_VECTOR_ELT(writer->sfc, i, empty);
            }
        }

        UNPROTECT(1);
    }

    // attr(sfc, "precision")
    // this should really be parrt of wk_meta_t!
    Rf_setAttrib(writer->sfc, Rf_install("precision"), Rf_ScalarReal(0));

    // attr(sfc, "bbox")
    const char* bbox_names[] = {"xmin", "ymin", "xmax", "ymax", ""};
    SEXP bbox = PROTECT(Rf_mkNamed(REALSXP, bbox_names));
    Rf_setAttrib(bbox, R_ClassSymbol, Rf_mkString("bbox"));

    // the bounding box may or may not have a crs attribute
    if (vector_meta->size == writer->n_empty) {
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

    // attr(sfc, "classes") (only for all empty)
    if (vector_meta->size == writer->n_empty) {
        int n_geometry_types = 0;
        for (int i = 0; i < 7; i++) {
            if (1 & (writer->all_geometry_types >> i)) n_geometry_types++;
        }

        const char* type_names[] = {
            "POINT", "LINESTRING", "POLYGON",
             "MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON", 
             "GEOMETRYCOLLECTION" 
             ""
        };

        SEXP classes = PROTECT(Rf_allocVector(STRSXP, n_geometry_types));
        int classes_index = 0;
        for (int i = 0; i < 7; i++) {
            if (1 & (writer->all_geometry_types >> i)) {
                SET_STRING_ELT(classes, classes_index, Rf_mkChar(type_names[i]));
                classes_index++;
            }
        }
        Rf_setAttrib(writer->sfc, Rf_install("classes"), classes);
        UNPROTECT(1);
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
    handler->null_feature = &sfc_writer_null_feature;
    handler->geometry_start = &sfc_writer_geometry_start;
    handler->ring_start = &sfc_writer_ring_start;
    handler->coord = &sfc_writer_coord;
    handler->ring_end = &sfc_writer_ring_end;
    handler->geometry_end = &sfc_writer_geometry_end;
    handler->vector_end = &sfc_writer_vector_end;
    handler->vector_finally = &sfc_writer_vector_finally;

    handler->handler_data = sfc_writer_new();

    SEXP xptr = wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
    return xptr;
}
