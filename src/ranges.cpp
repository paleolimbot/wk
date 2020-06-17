
#include "wk/geometry-handler.hpp"
#include "wk/wkb-reader.hpp"
#include "wk/wkt-streamer.hpp"

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
#include "wk/rcpp-sexp-reader.hpp"
using namespace Rcpp;

// I'm sure there's a more compact way to do this
double min_reg(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA || x2NA) {
    return NA_REAL;
  } else {
    return std::min(x1i, x2i);
  }
}

double max_reg(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA || x2NA) {
    return NA_REAL;
  } else {
    return std::max(x1i, x2i);
  }
}

double min_na_rm(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA && x2NA) {
    return R_PosInf;
  } else if (x1NA) {
    return x2i;
  } else if (x2NA) {
    return x1i;
  } else {
    return std::min(x1i, x2i);
  }
}

double max_na_rm(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i);
  bool x2NA = NumericVector::is_na(x2i);
  if (x1NA && x2NA) {
    return R_NegInf;
  } else if (x1NA) {
    return x2i;
  } else if (x2NA) {
    return x1i;
  } else {
    return std::max(x1i, x2i);
  }
}

double min_finite(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i) || x1i == R_NegInf || x1i == R_PosInf;
  bool x2NA = NumericVector::is_na(x2i) || x2i == R_NegInf || x2i == R_PosInf;
  if (x1NA && x2NA) {
    return R_PosInf;
  } else if (x1NA) {
    return x2i;
  } else if (x2NA) {
    return x1i;
  } else {
    return std::min(x1i, x2i);
  }
}

double max_finite(double x1i, double x2i) {
  bool x1NA = NumericVector::is_na(x1i) || x1i == R_NegInf || x1i == R_PosInf;
  bool x2NA = NumericVector::is_na(x2i) || x2i == R_NegInf || x2i == R_PosInf;
  if (x1NA && x2NA) {
    return R_NegInf;
  } else if (x1NA) {
    return x2i;
  } else if (x2NA) {
    return x1i;
  } else {
    return std::max(x1i, x2i);
  }
}

class WKRangeCalculator: public WKGeometryHandler {
public:
  double xmin;
  double ymin;
  double zmin;
  double mmin;
  double xmax;
  double ymax;
  double zmax;
  double mmax;

  WKRangeCalculator(bool naRm, bool onlyFinite): naRm(naRm), onlyFinite(onlyFinite) {
    this->reset();
  }

  void reset() {
    this->xmin = R_PosInf;
    this->ymin = R_PosInf;
    this->zmin = R_PosInf;
    this->mmin = R_PosInf;
    this->xmax = R_NegInf;
    this->ymax = R_NegInf;
    this->zmax = R_NegInf;
    this->mmax = R_NegInf;
  }

  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    if (this->onlyFinite) {
      this->xmin = min_finite(this->xmin, coord.x);
      this->ymin = min_finite(this->ymin, coord.y);
      if (coord.hasZ) this->zmin = min_finite(this->zmin, coord.z);
      if (coord.hasM) this->mmin = min_finite(this->mmin, coord.m);

      this->xmax = max_finite(this->xmax, coord.x);
      this->ymax = max_finite(this->ymax, coord.y);
      if (coord.hasZ) this->zmax = max_finite(this->zmax, coord.z);
      if (coord.hasM) this->mmax = max_finite(this->zmin, coord.m);

    } else if (this->naRm) {
      this->xmin = min_na_rm(this->xmin, coord.x);
      this->ymin = min_na_rm(this->ymin, coord.y);
      if (coord.hasZ) this->zmin = min_na_rm(this->zmin, coord.z);
      if (coord.hasM) this->mmin = min_na_rm(this->mmin, coord.m);

      this->xmax = max_na_rm(this->xmax, coord.x);
      this->ymax = max_na_rm(this->ymax, coord.y);
      if (coord.hasZ) this->zmax = max_na_rm(this->zmax, coord.z);
      if (coord.hasM) this->mmax = max_na_rm(this->zmin, coord.m);

    } else {
      this->xmin = min_reg(this->xmin, coord.x);
      this->ymin = min_reg(this->ymin, coord.y);
      if (coord.hasZ) this->zmin = min_reg(this->zmin, coord.z);
      if (coord.hasM) this->mmin = min_reg(this->mmin, coord.m);

      this->xmax = max_reg(this->xmax, coord.x);
      this->ymax = max_reg(this->ymax, coord.y);
      if (coord.hasZ) this->zmax = max_reg(this->zmax, coord.z);
      if (coord.hasM) this->mmax = max_reg(this->zmin, coord.m);
    }
  }

private:
  bool naRm;
  bool onlyFinite;
};

class WKFeatureRangeCalculator: public WKRangeCalculator {
public:
  NumericVector vxmin;
  NumericVector vymin;
  NumericVector vzmin;
  NumericVector vmmin;
  NumericVector vxmax;
  NumericVector vymax;
  NumericVector vzmax;
  NumericVector vmmax;

  WKFeatureRangeCalculator(size_t size, bool naRm, bool onlyFinite):
    WKRangeCalculator(naRm, onlyFinite),
    vxmin(size),
    vymin(size),
    vzmin(size),
    vmmin(size),
    vxmax(size),
    vymax(size),
    vzmax(size),
    vmmax(size) {}

  void nextFeatureStart(size_t featureId) {
    this->reset();
  }

  void nextFeatureEnd(size_t featureId) {
    this->vxmin[featureId] = this->xmin;
    this->vymin[featureId] = this->ymin;
    this->vzmin[featureId] = this->zmin;
    this->vmmin[featureId] = this->mmin;

    this->vxmax[featureId] = this->xmax;
    this->vymax[featureId] = this->ymax;
    this->vzmax[featureId] = this->zmax;
    this->vmmax[featureId] = this->mmin;
  }
};

List cpp_ranges_base(WKReader& reader, bool naRm, bool onlyFinite) {
  WKRangeCalculator ranges(naRm, onlyFinite);
  reader.setHandler(&ranges);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return List::create(
    _["xmin"] = ranges.xmin,
    _["ymin"] = ranges.ymin,
    _["zmin"] = ranges.zmin,
    _["mmin"] = ranges.mmin,

    _["xmax"] = ranges.xmax,
    _["ymax"] = ranges.ymax,
    _["zmax"] = ranges.zmax,
    _["mmax"] = ranges.mmax
  );
}

// [[Rcpp::export]]
List cpp_ranges_wkb(List wkb, bool naRm, bool onlyFinite) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_ranges_base(reader, naRm, onlyFinite);
}

// [[Rcpp::export]]
List cpp_ranges_wkt(CharacterVector wkt, bool naRm, bool onlyFinite) {
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return cpp_ranges_base(reader, naRm, onlyFinite);
}

// [[Rcpp::export]]
List cpp_ranges_wksxp(List wksxp, bool naRm, bool onlyFinite) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return cpp_ranges_base(reader, naRm, onlyFinite);
}

List cpp_feature_ranges_base(WKReader& reader, bool naRm, bool onlyFinite) {
  WKFeatureRangeCalculator ranges(reader.nFeatures(), naRm, onlyFinite);
  reader.setHandler(&ranges);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return List::create(
    _["xmin"] = ranges.vxmin,
    _["ymin"] = ranges.vymin,
    _["zmin"] = ranges.vzmin,
    _["mmin"] = ranges.vmmin,

    _["xmax"] = ranges.vxmax,
    _["ymax"] = ranges.vymax,
    _["zmax"] = ranges.vzmax,
    _["mmax"] = ranges.vmmax
  );
}

// [[Rcpp::export]]
List cpp_feature_ranges_wkb(List wkb, bool naRm, bool onlyFinite) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_feature_ranges_base(reader, naRm, onlyFinite);
}

// [[Rcpp::export]]
List cpp_feature_ranges_wkt(CharacterVector wkt, bool naRm, bool onlyFinite) {
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return cpp_feature_ranges_base(reader, naRm, onlyFinite);
}

// [[Rcpp::export]]
List cpp_feature_ranges_wksxp(List wksxp, bool naRm, bool onlyFinite) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return cpp_feature_ranges_base(reader, naRm, onlyFinite);
}
