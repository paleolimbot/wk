#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#define MAX(a, b) (((a) > (b)) ? (a) : (b))

SEXP wk_c_wkb_is_na(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);

  for (R_xlen_t i = 0; i < size; i++) {
    pResult[i] = VECTOR_ELT(geom, i) == R_NilValue;
  }

  UNPROTECT(1);
  return result;
}

SEXP wk_c_wkb_is_raw_or_null(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);
  int typeOf;
  for (R_xlen_t i = 0; i < size; i++) {
    typeOf = TYPEOF(VECTOR_ELT(geom, i));
    pResult[i] = (typeOf == NILSXP) || (typeOf == RAWSXP);
  }

  UNPROTECT(1);
  return result;
}

static R_xlen_t wk_max_length(const SEXP geom) {
  const R_xlen_t size = Rf_xlength(geom);
  R_xlen_t max = 0;

  for (R_xlen_t i = 0; i < size; i++) {
    max = MAX(max, Rf_xlength(VECTOR_ELT(geom, i)));
  }

  return max;
}

static void wk_bin_to_hex(char* dst, const unsigned char* src, const R_xlen_t n) {
  static const char hex[16] = "0123456789abcdef";

  for (R_xlen_t i = 0; i < n; i++) {
    const unsigned char byte = src[i];
    dst[2 * i] = hex[(byte >> 4) & 0xf];
    dst[2 * i + 1] = hex[byte & 0xf];
  }

  dst[2 * n] = '\0';
}

SEXP wk_c_wkb_to_hex(const SEXP geom) {
  const R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));

  const R_xlen_t buf_size = wk_max_length(geom) * 2 + 1;
  SEXP buf_shelter = PROTECT(Rf_allocVector(RAWSXP, buf_size));
  char* buf = (char*)RAW(buf_shelter);

  for (R_xlen_t i = 0; i < size; i++) {
    if (((i + 1) % 1000) == 0) R_CheckUserInterrupt();

    const SEXP item = VECTOR_ELT(geom, i);
    const R_xlen_t item_len = Rf_xlength(item);

    if (item == R_NilValue) {
      SET_STRING_ELT(result, i, NA_STRING);
      continue;
    }

    wk_bin_to_hex(buf, RAW(item), item_len);
    SET_STRING_ELT(result, i, Rf_mkChar(buf));
  }

  UNPROTECT(2);
  return result;
}
