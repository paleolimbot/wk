
#include "wk/geometry-handler.hpp"
#include "wk/wkb-reader.hpp"
#include "wk/wkt-streamer.hpp"

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
#include "wk/rcpp-sexp-reader.hpp"
using namespace Rcpp;

class WKCoordinateCounter: public WKGeometryHandler {
public:
  size_t nCoordinates;
  bool sepNA;
  bool firstGeom;

  WKCoordinateCounter(bool sepNA): nCoordinates(0), sepNA(sepNA), firstGeom(true) {}
  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    bool isSimple = meta.geometryType == WKGeometryType::Point ||
      meta.geometryType == WKGeometryType::LineString ||
      meta.geometryType == WKGeometryType::Polygon;

    this->nCoordinates += this->sepNA &&
      !this->firstGeom &&
      (meta.size > 0) &&
      isSimple;

    if ((meta.size > 0) && isSimple) {
      this->firstGeom = false;
    }
  }

  void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->nCoordinates += this->sepNA && ringId > 0;
  }

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

  WKCoordinateAssembler(size_t nCoordinates, bool sepNA):
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
    lastRingId(0),
    sepNA(sepNA),
    firstGeom(true) {}

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
  bool sepNA;
  bool firstGeom;

  void nextFeatureStart(size_t featureId) {
    this->lastFeatureId = featureId + 1;
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->lastPartId  = this->lastPartId + 1;

    bool isSimple = meta.geometryType == WKGeometryType::Point ||
      meta.geometryType == WKGeometryType::LineString ||
      meta.geometryType == WKGeometryType::Polygon;

    if (this->sepNA && !this->firstGeom && (meta.size > 0) && isSimple) {
      this->writeNASep();
    }

    if ((meta.size > 0) && isSimple) {
      this->firstGeom = false;
    }
  }

  void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->lastRingId = this->lastRingId + 1;

    if (this->sepNA && ringId > 0) {
      this->writeNASep();
    }
  }

  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
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

  void writeNASep() {
    this->featureId[i] = NA_INTEGER;
    this->partId[i] = NA_INTEGER;
    this->ringId[i] = NA_INTEGER;
    this->x[i] = NA_REAL;
    this->y[i] = NA_REAL;
    this->z[i] = NA_REAL;
    this->m[i] = NA_REAL;
    this->i++;
  }
};

List cpp_coords_base(WKReader& reader, bool sepNA) {
  WKCoordinateCounter counter(sepNA);
  reader.setHandler(&counter);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  reader.reset();

  WKCoordinateAssembler assembler(counter.nCoordinates, sepNA);
  reader.setHandler(&assembler);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return assembler.assembleCoordinates();
}

// [[Rcpp::export]]
List cpp_coords_wkb(List wkb, bool sepNA) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_coords_base(reader, sepNA);
}

// [[Rcpp::export]]
List cpp_coords_wkt(CharacterVector wkt, bool sepNA) {
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return cpp_coords_base(reader, sepNA);
}

// [[Rcpp::export]]
List cpp_coords_wksxp(List wksxp, bool sepNA) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return cpp_coords_base(reader, sepNA);
}
