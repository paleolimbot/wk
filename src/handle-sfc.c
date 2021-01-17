
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <memory.h>

#define HANDLE_OR_RETURN(expr)                                 \
  result = expr;                                               \
  if (result != WK_CONTINUE) return result

#define HANDLE_CONTINUE_OR_BREAK(expr)                         \
  result = expr;                                               \
  if (result == WK_ABORT_FEATURE) continue; else if (result == WK_ABORT) break

int wk_sfc_read_sfg(SEXP x, wk_handler_t* handler, uint32_t part_id);
int wk_sfc_read_point(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id);
int wk_sfc_read_linestring(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id);
int wk_sfc_read_polygon(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id);

SEXP wk_c_read_sfc_impl(SEXP data, wk_handler_t* handler) {
    R_xlen_t n_features = Rf_xlength(data);

    wk_vector_meta_t vector_meta;
    WK_VECTOR_META_RESET(vector_meta, WK_GEOMETRY);
    vector_meta.size = n_features;
    vector_meta.flags |= WK_FLAG_DIMS_UNKNOWN;

    if (handler->vector_start(&vector_meta, handler->handler_data) != WK_ABORT) {
        int result;
        SEXP item;
        for (R_xlen_t i = 0; i < n_features; i++) {
            HANDLE_CONTINUE_OR_BREAK(handler->feature_start(&vector_meta, i, handler->handler_data));

            item = VECTOR_ELT(data, i);
            if (item == R_NilValue) {
                HANDLE_CONTINUE_OR_BREAK(handler->null_feature(&vector_meta, i, handler->handler_data));
            } else {
                HANDLE_CONTINUE_OR_BREAK(wk_sfc_read_sfg(item, handler, WK_PART_ID_NONE));
            }

            if (handler->feature_end(&vector_meta, i, handler->handler_data) == WK_ABORT) {
                break;
            }
        }
    }

    return handler->vector_end(&vector_meta, handler->handler_data);
}

SEXP wk_c_read_sfc(SEXP data, SEXP handler_xptr) {
    return wk_handler_run_xptr(&wk_c_read_sfc_impl, data, handler_xptr);
}

void wk_update_meta_from_sfg(SEXP x, wk_meta_t* meta) {
    if (Rf_inherits(x, "XY")) {
        // don't need to do anything here; default meta is xy
    } else if (Rf_inherits(x, "XYZ")) {
        meta->flags |= WK_FLAG_HAS_Z;
    } else if (Rf_inherits(x, "XYM")) {
        meta->flags |= WK_FLAG_HAS_M;
    } else if (Rf_inherits(x, "XYZM")) {
        meta->flags |= WK_FLAG_HAS_Z;
        meta->flags |= WK_FLAG_HAS_M;
    } else if (Rf_inherits(x, "sfg")) {
        Rf_error("Can't guess dimensions from class of 'sfg'");
    }
}

int wk_sfc_read_sfg(SEXP x, wk_handler_t* handler, uint32_t part_id) {
    wk_meta_t meta;
    WK_META_RESET(meta, WK_GEOMETRY);
    wk_update_meta_from_sfg(x, &meta);

    if (Rf_inherits(x, "POINT")) {
        return wk_sfc_read_point(x, handler, &meta, part_id);
    } else if (Rf_inherits(x, "LINESTRING")) {
        return wk_sfc_read_linestring(x, handler, &meta, part_id);
    } else if (Rf_inherits(x, "POLYGON")) {
        return wk_sfc_read_polygon(x, handler, &meta, part_id);
    }  else if (Rf_inherits(x, "sfg")) {
        Rf_error("Unsupported sfg type");
    } else {
        Rf_error("Element of sfc list must inherit from 'sfg'");
    }
}

int wk_sfc_read_point(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id) {
    int result;
    meta->geometry_type = WK_POINT;
    meta->size = 0;

    int coord_size = Rf_length(x);
    int all_na = 1;
    for (int i = 0; i < coord_size; i++) {
        if (!ISNA(REAL(x)[i])) {
            all_na = 0;
            meta->size = 1;
            break;
        }
    }
    
    HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));

    if (meta->size) {
        wk_coord_t coord;
        memcpy(coord.v, REAL(x), sizeof(double) * coord_size);
        HANDLE_OR_RETURN(handler->coord(meta, coord, 0, handler->handler_data));
    }

    HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));
    return WK_CONTINUE;
}

int wk_sfc_read_linestring(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id) {
    int result;
    meta->geometry_type = WK_LINESTRING;
    meta->size = Rf_nrows(x);
    int coord_size = Rf_ncols(x);
    
    HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));

    wk_coord_t coord;
    double* coords = REAL(x);
    for (uint32_t i = 0; i < meta->size; i++) {
        for (int j = 0; j < coord_size; j++) {
             coord.v[j] = coords[j * meta->size + i];
        }
        HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
    }

    HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));
    return WK_CONTINUE;
}

int wk_sfc_read_polygon(SEXP x, wk_handler_t* handler, wk_meta_t* meta, uint32_t part_id) {
    int result;
    meta->geometry_type = WK_POLYGON;
    meta->size = Rf_xlength(x);
    
    HANDLE_OR_RETURN(handler->geometry_start(meta, part_id, handler->handler_data));

    SEXP ring;
    for (uint32_t ring_id = 0; ring_id < meta->size; ring_id++) {
        ring = VECTOR_ELT(x, ring_id);
        uint32_t ring_size = Rf_nrows(ring);
        int coord_size = Rf_ncols(ring);
        
        HANDLE_OR_RETURN(handler->ring_start(meta, meta->size, ring_id, handler->handler_data));

        wk_coord_t coord;
        double* coords = REAL(ring);
        for (uint32_t i = 0; i < ring_size; i++) {
            for (int j = 0; j < coord_size; j++) {
                coord.v[j] = coords[j * ring_size + i];
            }
            HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
        }

        HANDLE_OR_RETURN(handler->ring_end(meta, meta->size, ring_id, handler->handler_data));
    }

    HANDLE_OR_RETURN(handler->geometry_end(meta, part_id, handler->handler_data));
    return WK_CONTINUE;
}
