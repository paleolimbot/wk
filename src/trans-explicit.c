#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <wk-v1.h>

typedef struct {
  double* xyzm[4];
  R_xlen_t i;
  R_xlen_t n;
} wk_trans_explicit_t;

int wk_trans_explicit_trans(R_xlen_t feature_id, const double* xyzm_in, double* xyzm_out, void* trans_data) {
  wk_trans_explicit_t* data = (wk_trans_explicit_t*) trans_data;
  data->i++;
  R_xlen_t set_id = data->i % data->n;
  double set;
  for (int i = 0; i < 4; i++) {
    set = data->xyzm[i][set_id];
    if (ISNA(set)) {
      xyzm_out[i] = xyzm_in[i];
    } else {
      xyzm_out[i] = set;
    }
  }
  return WK_CONTINUE;
}

void wk_trans_explicit_finalize(void* trans_data) {
  free(trans_data);
}

SEXP wk_c_trans_explicit_new(SEXP xy, SEXP use_z, SEXP use_m) {
  if (Rf_xlength(xy) != 4 || TYPEOF(xy) != VECSXP) {
    Rf_error("`xy` must be an xyzm() object"); // # nocov
  }

  // prepare data for C struct / validate args
  int use_z_int = LOGICAL(use_z)[0];
  int use_m_int = LOGICAL(use_m)[0];
  R_xlen_t n = Rf_xlength(VECTOR_ELT(xy, 0));
  double* xyzm[4];
  for (int i = 0; i < 4; i++) {
    xyzm[i] = REAL(VECTOR_ELT(xy, i));
  }

  // create the wk_trans object
  wk_trans_t* trans = wk_trans_create();
  trans->trans = &wk_trans_explicit_trans;
  trans->finalizer = &wk_trans_explicit_finalize;

  wk_trans_explicit_t* data = (wk_trans_explicit_t*) malloc(sizeof(wk_trans_explicit_t));
  if (data == NULL) {
    free(trans); // # nocov
    Rf_error("Failed to alloc wk_trans_explicit_t"); // # nocov
  }

  trans->use_z = use_z_int;
  trans->use_m = use_m_int;
  memcpy(data->xyzm, xyzm, 4 * sizeof(void*));
  data->i = -1;
  data->n = n;

  trans->trans_data = data;

  // keep the xy as a tag because we need the pointers to stay valid
  return wk_trans_create_xptr(trans, xy, R_NilValue);
}
