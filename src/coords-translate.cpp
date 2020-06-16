
#include <cmath>
#include "wk/io.h"
#include "wk/coord.h"
#include "wk/reader.h"
#include "wk/geometry-meta.h"

#include <Rcpp.h>
#include "wk/rcpp-translate.h"
using namespace Rcpp;


class WKRcppPointCoordProvider: public WKProvider {
public:

  WKRcppPointCoordProvider(Rcpp::NumericVector x, Rcpp::NumericVector y,
                           Rcpp::NumericVector z, Rcpp::NumericVector m):
    x(x), y(y), z(z), m(m), index(-1) {}

  const WKCoord coord() {
    double xi = x[index];
    double yi = y[index];
    double zi = z[index];
    double mi = m[index];

    if (std::isnan(zi) && std::isnan(mi)) {
      return WKCoord::xy(xi, yi);
    } else if (std::isnan(mi)) {
      return WKCoord::xyz(xi, yi, zi);
    } else if (std::isnan(zi)) {
      return WKCoord::xym(xi, yi, mi);
    } else {
      return WKCoord::xyzm(xi, yi, zi, mi);
    }
  }

  bool coordEmpty() {
    return std::isnan(x[index]) &&
      std::isnan(y[index]) &&
      std::isnan(z[index]) &&
      std::isnan(m[index]);
  }

  virtual bool seekNextFeature() {
    this->index++;

    if (this->index >= this->nFeatures()) {
      return false;
    } else {
      return true;
    }
  }

  virtual size_t nFeatures() {
    return this->x.size();
  }

  bool featureIsNull() {
    return false;
  }

  void reset() {
    this->index = -1;
  }

protected:
  Rcpp::NumericVector x;
  Rcpp::NumericVector y;
  Rcpp::NumericVector z;
  Rcpp::NumericVector m;
  R_xlen_t index;
};

class WKRcppPointCoordReader: public WKReader {
public:
  WKRcppPointCoordReader(WKRcppPointCoordProvider& provider):
    WKReader(provider), provider(provider) {}

  void readFeature(size_t featureId) {
    this->handler->nextFeatureStart(featureId);

    if (this->provider.coordEmpty()) {
      WKGeometryMeta meta(WKGeometryType::Point, 0);
      this->handler->nextGeometryStart(meta, PART_ID_NONE);
      this->handler->nextGeometryStart(meta, PART_ID_NONE);
    } else {
      WKCoord coord = this->provider.coord();
      WKGeometryMeta meta(WKGeometryType::Point, 1);
      meta.hasZ = coord.hasZ;
      meta.hasM = coord.hasM;
      this->handler->nextGeometryStart(meta, PART_ID_NONE);
      this->handler->nextCoordinate(meta, coord, 0);
      this->handler->nextGeometryStart(meta, PART_ID_NONE);
    }

    this->handler->nextFeatureEnd(featureId);
  }

protected:
  WKRcppPointCoordProvider& provider;
};

// [[Rcpp::export]]
CharacterVector cpp_coords_point_translate_wkt(NumericVector x, NumericVector y,
                                               NumericVector z, NumericVector m,
                                               int precision, bool trim) {
  WKRcppPointCoordProvider provider(x, y, z, m);
  WKRcppPointCoordReader reader(provider);
  return wk::translate_wkt(reader, precision, trim);
}
