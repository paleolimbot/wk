
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

#define HANDLE_CONTINUE_OR_BREAK(expr)                         \
  result = expr;                                               \
  if (result == WK_ABORT_FEATURE) continue; else if (result == WK_ABORT) break

SEXP wk_read_xy(SEXP data, wk_handler_t* handler) {
    R_xlen_t n_features = Rf_xlength(VECTOR_ELT(data, 0));
    int coord_size = Rf_length(data);
    double* data_ptr[coord_size];
    for (int j = 0; j < coord_size; j++) {
        data_ptr[j] = REAL(VECTOR_ELT(data, j));
    }

    wk_vector_meta_t vector_meta;
    WK_VECTOR_META_RESET(vector_meta, WK_POINT);
    vector_meta.size = n_features;

    if (Rf_inherits(data, "wk_xyz") || Rf_inherits(data, "wk_xyzm")) {
        vector_meta.flags |= WK_FLAG_HAS_Z;
    }

    if (Rf_inherits(data, "wk_xym") || Rf_inherits(data, "wk_xyzm")) {
        vector_meta.flags |= WK_FLAG_HAS_M;
    }

    if (handler->vector_start(&vector_meta, handler->handler_data) == WK_CONTINUE) {
        int result;
        wk_coord_t coord;
        wk_meta_t meta;
        WK_META_RESET(meta, WK_POINT);
        meta.flags = vector_meta.flags;

        for (R_xlen_t i = 0; i < n_features; i++) {
            if (((i + 1) % 1000) == 0) R_CheckUserInterrupt();
            
            HANDLE_CONTINUE_OR_BREAK(handler->feature_start(&vector_meta, i, handler->handler_data));

            int coord_empty = 1;
            for (int j = 0; j < coord_size; j++) {
                coord.v[j] = data_ptr[j][i];
                if (!ISNAN(coord.v[j])) {
                    coord_empty = 0;
                }
            }

            if (coord_empty) {
                meta.size = 0;
            } else {
                meta.size = 1;
            }

            HANDLE_CONTINUE_OR_BREAK(handler->geometry_start(&meta, WK_PART_ID_NONE, handler->handler_data));
            if (!coord_empty) {
                HANDLE_CONTINUE_OR_BREAK(handler->coord(&meta, coord, 0, handler->handler_data));
            }
            HANDLE_CONTINUE_OR_BREAK(handler->geometry_end(&meta, WK_PART_ID_NONE, handler->handler_data));

            if (handler->feature_end(&vector_meta, i, handler->handler_data) == WK_ABORT) {
                break;
            }
        }
    }

    SEXP result = PROTECT(handler->vector_end(&vector_meta, handler->handler_data));
    UNPROTECT(1);
    return result;
}

SEXP wk_c_read_xy(SEXP data, SEXP handlerXptr) {
  return wk_handler_run_xptr(&wk_read_xy, data, handlerXptr);
}
