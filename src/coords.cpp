
#include "wk/geometry-handler.h"
#include "wk/wkb-reader.h"
#include "wk/wkt-streamer.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
#include "wk/sexp-reader.h"
using namespace Rcpp;

class WKCoordinateCounter: public WKGeometryHandler {
public:
  size_t nCoordinates;
  WKCoordinateCounter(): nCoordinates(0) {}
  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->nCoordinates++;
  }
};

class WKCoordinateAssembler: public WKGeometryHandler {
public:
  IntegerVector featureId;
  IntegerVector partId;
  IntegerVector ringId;
  NumericVector x;
  NumericVector y;
  NumericVector z;
  NumericVector m;

  WKCoordinateAssembler(size_t nCoordinates):
    featureId(nCoordinates),
    partId(nCoordinates),
    ringId(nCoordinates),
    x(nCoordinates),
    y(nCoordinates),
    z(nCoordinates),
    m(nCoordinates),
    i(0),
    lastFeatureId(0),
    lastPartId(0),
    lastRingId(0) {}

  List assembleCoordinates()  {
    return List::create(
      _["feature_id"] = this->featureId,
      _["part_id"] = this->partId,
      _["ring_id"] = this->ringId,
      _["x"] = this->x,
      _["y"] = this->y,
      _["z"] = this->z,
      _["m"] = this->m
    );
  }

protected:
  R_xlen_t i;
  int lastFeatureId;
  int lastPartId;
  int lastRingId;

  void nextFeatureStart(size_t featureId) {
    this->lastFeatureId = featureId + 1;
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->lastPartId  = this->lastPartId + 1;
  }

  void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->lastRingId = this->lastRingId + 1;
  }

  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    R_xlen_t i = this->i;
    this->featureId[i] = this->lastFeatureId;
    this->partId[i] = this->lastPartId;
    this->ringId[i] = this->lastRingId;

    this->x[i] = coord.x;
    this->y[i] = coord.y;
    if (coord.hasZ) {
      this->z[i] = coord.z;
    } else {
      this->z[i] = NA_REAL;
    }

    if (coord.hasM) {
      this->m[i] = coord.m;
    } else {
      this->m[i] = NA_REAL;
    }

    this->i++;
  }
};

List cpp_coords_base(WKReader& reader) {
  WKCoordinateCounter counter;
  reader.setHandler(&counter);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  reader.reset();

  WKCoordinateAssembler assembler(counter.nCoordinates);
  reader.setHandler(&assembler);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return assembler.assembleCoordinates();
}

// [[Rcpp::export]]
List cpp_coords_wkb(List wkb) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_coords_base(reader);
}

// [[Rcpp::export]]
List cpp_coords_wkt(CharacterVector wkt) {
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return cpp_coords_base(reader);
}

// [[Rcpp::export]]
List cpp_coords_wksxp(List wksxp) {
  WKSEXPProvider provider(wksxp);
  WKSEXPReader reader(provider);
  return cpp_coords_base(reader);
}
