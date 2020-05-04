// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cpp_coords_wkb
List cpp_coords_wkb(List wkb);
RcppExport SEXP _wk_cpp_coords_wkb(SEXP wkbSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type wkb(wkbSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_coords_wkb(wkb));
    return rcpp_result_gen;
END_RCPP
}
// cpp_coords_wkt
List cpp_coords_wkt(CharacterVector wkt);
RcppExport SEXP _wk_cpp_coords_wkt(SEXP wktSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type wkt(wktSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_coords_wkt(wkt));
    return rcpp_result_gen;
END_RCPP
}
// cpp_debug_wkb
void cpp_debug_wkb(List wkb);
RcppExport SEXP _wk_cpp_debug_wkb(SEXP wkbSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type wkb(wkbSEXP);
    cpp_debug_wkb(wkb);
    return R_NilValue;
END_RCPP
}
// cpp_debug_wkt
void cpp_debug_wkt(CharacterVector input);
RcppExport SEXP _wk_cpp_debug_wkt(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type input(inputSEXP);
    cpp_debug_wkt(input);
    return R_NilValue;
END_RCPP
}
// cpp_debug_wkt_streamer
void cpp_debug_wkt_streamer(CharacterVector input);
RcppExport SEXP _wk_cpp_debug_wkt_streamer(SEXP inputSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type input(inputSEXP);
    cpp_debug_wkt_streamer(input);
    return R_NilValue;
END_RCPP
}
// cpp_meta_wkb
List cpp_meta_wkb(List wkb, bool recursive);
RcppExport SEXP _wk_cpp_meta_wkb(SEXP wkbSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type wkb(wkbSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_meta_wkb(wkb, recursive));
    return rcpp_result_gen;
END_RCPP
}
// cpp_meta_wkt
List cpp_meta_wkt(CharacterVector wkt, bool recursive);
RcppExport SEXP _wk_cpp_meta_wkt(SEXP wktSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type wkt(wktSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_meta_wkt(wkt, recursive));
    return rcpp_result_gen;
END_RCPP
}
// cpp_meta_wkt_streamer
List cpp_meta_wkt_streamer(CharacterVector wkt, bool recursive);
RcppExport SEXP _wk_cpp_meta_wkt_streamer(SEXP wktSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type wkt(wktSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_meta_wkt_streamer(wkt, recursive));
    return rcpp_result_gen;
END_RCPP
}
// cpp_problems_wkb
Rcpp::CharacterVector cpp_problems_wkb(Rcpp::List wkb);
RcppExport SEXP _wk_cpp_problems_wkb(SEXP wkbSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type wkb(wkbSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_problems_wkb(wkb));
    return rcpp_result_gen;
END_RCPP
}
// cpp_problems_wkt
Rcpp::CharacterVector cpp_problems_wkt(CharacterVector wkt);
RcppExport SEXP _wk_cpp_problems_wkt(SEXP wktSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type wkt(wktSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_problems_wkt(wkt));
    return rcpp_result_gen;
END_RCPP
}
// cpp_translate_wkb_wkt
Rcpp::CharacterVector cpp_translate_wkb_wkt(Rcpp::List wkb, int includeZ, int includeM, int includeSRID, int precision, bool trim);
RcppExport SEXP _wk_cpp_translate_wkb_wkt(SEXP wkbSEXP, SEXP includeZSEXP, SEXP includeMSEXP, SEXP includeSRIDSEXP, SEXP precisionSEXP, SEXP trimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type wkb(wkbSEXP);
    Rcpp::traits::input_parameter< int >::type includeZ(includeZSEXP);
    Rcpp::traits::input_parameter< int >::type includeM(includeMSEXP);
    Rcpp::traits::input_parameter< int >::type includeSRID(includeSRIDSEXP);
    Rcpp::traits::input_parameter< int >::type precision(precisionSEXP);
    Rcpp::traits::input_parameter< bool >::type trim(trimSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_translate_wkb_wkt(wkb, includeZ, includeM, includeSRID, precision, trim));
    return rcpp_result_gen;
END_RCPP
}
// cpp_translate_wkb_wkb
Rcpp::List cpp_translate_wkb_wkb(Rcpp::List wkb, int includeZ, int includeM, int includeSRID, int endian, int bufferSize);
RcppExport SEXP _wk_cpp_translate_wkb_wkb(SEXP wkbSEXP, SEXP includeZSEXP, SEXP includeMSEXP, SEXP includeSRIDSEXP, SEXP endianSEXP, SEXP bufferSizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type wkb(wkbSEXP);
    Rcpp::traits::input_parameter< int >::type includeZ(includeZSEXP);
    Rcpp::traits::input_parameter< int >::type includeM(includeMSEXP);
    Rcpp::traits::input_parameter< int >::type includeSRID(includeSRIDSEXP);
    Rcpp::traits::input_parameter< int >::type endian(endianSEXP);
    Rcpp::traits::input_parameter< int >::type bufferSize(bufferSizeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_translate_wkb_wkb(wkb, includeZ, includeM, includeSRID, endian, bufferSize));
    return rcpp_result_gen;
END_RCPP
}
// cpp_translate_wkt_wkt
CharacterVector cpp_translate_wkt_wkt(CharacterVector wkt, int includeZ, int includeM, int includeSRID, int precision, bool trim);
RcppExport SEXP _wk_cpp_translate_wkt_wkt(SEXP wktSEXP, SEXP includeZSEXP, SEXP includeMSEXP, SEXP includeSRIDSEXP, SEXP precisionSEXP, SEXP trimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type wkt(wktSEXP);
    Rcpp::traits::input_parameter< int >::type includeZ(includeZSEXP);
    Rcpp::traits::input_parameter< int >::type includeM(includeMSEXP);
    Rcpp::traits::input_parameter< int >::type includeSRID(includeSRIDSEXP);
    Rcpp::traits::input_parameter< int >::type precision(precisionSEXP);
    Rcpp::traits::input_parameter< bool >::type trim(trimSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_translate_wkt_wkt(wkt, includeZ, includeM, includeSRID, precision, trim));
    return rcpp_result_gen;
END_RCPP
}
// cpp_translate_wkt_wkb
Rcpp::List cpp_translate_wkt_wkb(CharacterVector wkt, int includeZ, int includeM, int includeSRID, int endian, int bufferSize);
RcppExport SEXP _wk_cpp_translate_wkt_wkb(SEXP wktSEXP, SEXP includeZSEXP, SEXP includeMSEXP, SEXP includeSRIDSEXP, SEXP endianSEXP, SEXP bufferSizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type wkt(wktSEXP);
    Rcpp::traits::input_parameter< int >::type includeZ(includeZSEXP);
    Rcpp::traits::input_parameter< int >::type includeM(includeMSEXP);
    Rcpp::traits::input_parameter< int >::type includeSRID(includeSRIDSEXP);
    Rcpp::traits::input_parameter< int >::type endian(endianSEXP);
    Rcpp::traits::input_parameter< int >::type bufferSize(bufferSizeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_translate_wkt_wkb(wkt, includeZ, includeM, includeSRID, endian, bufferSize));
    return rcpp_result_gen;
END_RCPP
}
// cpp_translate_wkt_wksexp
Rcpp::List cpp_translate_wkt_wksexp(CharacterVector wkt, int includeZ, int includeM, int includeSRID);
RcppExport SEXP _wk_cpp_translate_wkt_wksexp(SEXP wktSEXP, SEXP includeZSEXP, SEXP includeMSEXP, SEXP includeSRIDSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type wkt(wktSEXP);
    Rcpp::traits::input_parameter< int >::type includeZ(includeZSEXP);
    Rcpp::traits::input_parameter< int >::type includeM(includeMSEXP);
    Rcpp::traits::input_parameter< int >::type includeSRID(includeSRIDSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_translate_wkt_wksexp(wkt, includeZ, includeM, includeSRID));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_wk_cpp_coords_wkb", (DL_FUNC) &_wk_cpp_coords_wkb, 1},
    {"_wk_cpp_coords_wkt", (DL_FUNC) &_wk_cpp_coords_wkt, 1},
    {"_wk_cpp_debug_wkb", (DL_FUNC) &_wk_cpp_debug_wkb, 1},
    {"_wk_cpp_debug_wkt", (DL_FUNC) &_wk_cpp_debug_wkt, 1},
    {"_wk_cpp_debug_wkt_streamer", (DL_FUNC) &_wk_cpp_debug_wkt_streamer, 1},
    {"_wk_cpp_meta_wkb", (DL_FUNC) &_wk_cpp_meta_wkb, 2},
    {"_wk_cpp_meta_wkt", (DL_FUNC) &_wk_cpp_meta_wkt, 2},
    {"_wk_cpp_meta_wkt_streamer", (DL_FUNC) &_wk_cpp_meta_wkt_streamer, 2},
    {"_wk_cpp_problems_wkb", (DL_FUNC) &_wk_cpp_problems_wkb, 1},
    {"_wk_cpp_problems_wkt", (DL_FUNC) &_wk_cpp_problems_wkt, 1},
    {"_wk_cpp_translate_wkb_wkt", (DL_FUNC) &_wk_cpp_translate_wkb_wkt, 6},
    {"_wk_cpp_translate_wkb_wkb", (DL_FUNC) &_wk_cpp_translate_wkb_wkb, 6},
    {"_wk_cpp_translate_wkt_wkt", (DL_FUNC) &_wk_cpp_translate_wkt_wkt, 6},
    {"_wk_cpp_translate_wkt_wkb", (DL_FUNC) &_wk_cpp_translate_wkt_wkb, 6},
    {"_wk_cpp_translate_wkt_wksexp", (DL_FUNC) &_wk_cpp_translate_wkt_wksexp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_wk(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
