#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"
#include <stdlib.h>

int wk_trans_affine_trans(R_xlen_t feature_id, double* xyzm_in, double* xyzm_out, void* trans_data) {
    double* t = (double*) trans_data;
    xyzm_out[0] = t[0] * xyzm_in[0] + t[2] * xyzm_in[1] + t[4];
    xyzm_out[1] = t[1] * xyzm_in[0] + t[3] * xyzm_in[1] + t[5];
    return WK_CONTINUE;
}

void wk_trans_affine_finalize(void* trans_data) {
    free(trans_data);
}

SEXP wk_c_trans_affine_new(SEXP trans_matrix) {
    if (!Rf_isMatrix(trans_matrix) || (Rf_nrows(trans_matrix) != 3) || (Rf_ncols(trans_matrix) != 3)) {
        Rf_error("`trans_matrix` must be a 3x3 matrix");
    }

    // create the wk_trans object
    wk_trans_t* trans = wk_trans_create();
    trans->trans = &wk_trans_affine_trans;
    trans->finalizer = &wk_trans_affine_finalize;

    // simplify the affine transform data to six numbers
    double* trans_matrix_ptr = REAL(trans_matrix);
    double* t = (double*) malloc(6 * sizeof(double));
    if (t == NULL) {
        Rf_error("Failed to alloc double[6]"); // # nocov
    }
    t[0] = trans_matrix_ptr[0];
    t[1] = trans_matrix_ptr[1];
    t[2] = trans_matrix_ptr[3];
    t[3] = trans_matrix_ptr[4];
    t[4] = trans_matrix_ptr[6];
    t[5] = trans_matrix_ptr[7];

    // this *is* the only data we need
    trans->trans_data = t;

    // keep the trans matrix as a tag so that we can return it in as.matrix()
    return wk_trans_create_xptr(trans, trans_matrix, R_NilValue);
}

SEXP wk_c_trans_affine_as_matrix(SEXP trans_xptr) {
    return R_ExternalPtrTag(trans_xptr);
}
