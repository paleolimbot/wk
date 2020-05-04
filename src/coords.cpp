
#include <Rcpp.h>

#include "wk/io-rcpp.h"
#include "wk/geometry-handler.h"
#include "wk/wkb-reader.h"
#include "wk/wkt-streamer.h"

using namespace Rcpp;

class WKCoordinateCounter: public WKGeometryHandler {
public:
  size_t nCoordinates;
  WKCoordinateCounter(size_t size): nCoordinates(0) {}
  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->nCoordinates++;
  }
};

class WKCoordinateAssembler: public WKGeometryHandler {
public:
  IntegerVector featureId;
  IntegerVector nestId;
  IntegerVector partId;
  IntegerVector ringId;
  IntegerVector coordId;
  NumericVector x;
  NumericVector y;
  NumericVector z;
  NumericVector m;

  WKCoordinateAssembler(size_t nCoordinates):
    featureId(nCoordinates),
    nestId(nCoordinates),
    partId(nCoordinates),
    ringId(nCoordinates),
    coordId(nCoordinates),
    x(nCoordinates),
    y(nCoordinates),
    z(nCoordinates),
    m(nCoordinates),
    i(0) {}

protected:
  R_xlen_t i;
  std::vector<int> partIdStack;
  size_t lastFeatureId;
  int lastRingId;
  int lastNestId;

  int currentPartId() {
    if (partIdStack.size() > 0) {
      return this->partIdStack[this->partIdStack.size() - 1];
    } else {
      return NA_INTEGER;
    }
  }

  virtual void nextFeatureStart(size_t featureId) {
    this->lastNestId = 0;
    this->lastFeatureId = featureId + 1;
  }

  virtual void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    if (partId == WKReader::PART_ID_NONE) {
      this->partIdStack.push_back(NA_INTEGER);
    } else {
      this->partIdStack.push_back(partId + 1);
    }

    this->lastRingId = NA_INTEGER;

    if (meta.geometryType == WKGeometryType::GeometryCollection) {
      this->lastNestId++;
    }
  }

  virtual void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    if (meta.geometryType == WKGeometryType::GeometryCollection) {
      this->lastNestId--;
    }
    this->partIdStack.pop_back();
  }

  virtual void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->lastRingId = ringId + 1;
  }

  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    R_xlen_t i = this->i;
    this->featureId[i] = this->lastFeatureId;
    this->nestId[i] = this->lastNestId;
    this->partId[i] = this->currentPartId();
    this->ringId[i] = this->lastRingId;
    this->coordId[i] = coordId + 1;
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

// [[Rcpp::export]]
List cpp_coords_wkb(List wkb) {

  // first, count coordinates
  WKCoordinateCounter counter(wkb.size());
  WKRawVectorListProvider provider(wkb);
  WKBReader readerCounter(provider, counter);
  while (readerCounter.hasNextFeature()) {
    readerCounter.iterateFeature();
  }

  // then, assemble
  WKCoordinateAssembler assembler(counter.nCoordinates);
  provider = WKRawVectorListProvider(wkb);
  WKBReader readerAssembler(provider, assembler);
  while (readerAssembler.hasNextFeature()) {
    readerAssembler.iterateFeature();
  }

  return List::create(
    _["feature_id"] = assembler.featureId,
    _["nest_id"] = assembler.nestId,
    _["part_id"] = assembler.partId,
    _["ring_id"] = assembler.ringId,
    _["coord_id"] = assembler.coordId,
    _["x"] = assembler.x,
    _["y"] = assembler.y,
    _["z"] = assembler.z,
    _["m"] = assembler.m
  );
}
