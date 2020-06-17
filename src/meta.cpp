
#include "wk/geometry-handler.hpp"
#include "wk/wkb-reader.hpp"
#include "wk/wkt-reader.hpp"
#include "wk/wkt-streamer.hpp"

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
#include "wk/rcpp-sexp-reader.hpp"
using namespace Rcpp;

class WKMetaFoundException: public WKParseException {
public:
  static const int CODE_META_FOUND = 294873;
  WKMetaFoundException(): WKParseException(CODE_META_FOUND) {}
};

class WKMetaCounter: public WKGeometryHandler {
public:
  size_t nMeta;
  WKMetaCounter(): nMeta(0) {}

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->nMeta++;
  }

  void nextNull(size_t featureId) {
    this->nMeta++;
  }
};



class WKMetaAssembler: public WKGeometryHandler {
public:
  IntegerVector featureId;
  IntegerVector partId;
  IntegerVector typeId;
  IntegerVector size;
  IntegerVector srid;
  LogicalVector hasZ;
  LogicalVector hasM;
  IntegerVector nCoordinates;

  WKMetaAssembler(bool recursive, size_t nMeta):
    featureId(nMeta),
    partId(nMeta),
    typeId(nMeta),
    size(nMeta),
    srid(nMeta),
    hasZ(nMeta),
    hasM(nMeta),
    nCoordinates(nMeta),
    i(0),
    lastPartId(0),
    nCoords(0),
    recursive(recursive) {}

  List assembleMeta() {
    this->syncNCoords();
    return List::create(
      _["feature_id"] = this->featureId,
      _["part_id"] = this->partId,
      _["type_id"] = this->typeId,
      _["size"] = this->size,
      _["srid"] = this->srid,
      _["has_z"] = this->hasZ,
      _["has_m"] = this->hasM,
      _["n_coords"] = this->nCoordinates
    );
  }

  void nextFeatureStart(size_t featureId) {
    this->lastFeatureId = featureId + 1;
  }

  void nextNull(size_t featureId) {
    this->syncNCoords();
    this->featureId[i] = this->lastFeatureId;
    this->partId[i] = NA_INTEGER;
    this->typeId[i] = NA_INTEGER;
    this->size[i] = NA_INTEGER;
    this->srid[i] = NA_INTEGER;
    this->hasZ[i] = NA_LOGICAL;
    this->hasM[i] = NA_LOGICAL;

    this->nCoords = NA_INTEGER;

    this->i++;
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->syncNCoords();
    this->lastPartId = this->lastPartId + 1;

    this->featureId[i] = this->lastFeatureId;
    this->partId[i] = this->lastPartId;

    this->typeId[i] = meta.geometryType;

    if (meta.hasSize) {
      this->size[i] = meta.size;
    } else {
      this->size[i] = NA_INTEGER;
    }

    if (meta.hasSRID) {
      this->srid[i] = meta.srid;
    } else {
      this->srid[i] = NA_INTEGER;
    }

    this->hasZ[i] = meta.hasZ;
    this->hasM[i] = meta.hasM;

    this->i++;

    if (!recursive) {
      if (meta.hasSize &&
           (meta.geometryType == WKGeometryType::Point ||
            meta.geometryType == WKGeometryType::LineString)) {
        this->nCoords = meta.size;
      } else {
        this->nCoords = NA_INTEGER;
      }

      throw WKMetaFoundException();
    }
  }

  // this won't get called if recursive = false, but will if it is true
  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->nCoords++;
  }

  // throwing exceptions results in recursive searches being much faster
  // for small geometries
  bool nextError(WKParseException& error, size_t featureId) {
    if (error.code() == WKMetaFoundException::CODE_META_FOUND) {
      return true;
    } else {
      return false;
    }
  }

  void syncNCoords() {
    if (this->i > 0) {
      this->nCoordinates[i - 1] = nCoords;
      this->nCoords = 0;
    }
  }

protected:
  R_xlen_t i;
  int lastFeatureId;
  int lastPartId;
  int nCoords;
  bool recursive;
};

List cpp_meta_base(WKReader& reader, bool recursive) {
  size_t nMeta;

  if (recursive) {
    WKMetaCounter counter;
    reader.setHandler(&counter);
    while (reader.hasNextFeature()) {
      checkUserInterrupt();
      reader.iterateFeature();
    }

    nMeta = counter.nMeta;
    reader.reset();
  } else {
    nMeta = reader.nFeatures();
  }

  WKMetaAssembler assembler(recursive, nMeta);
  reader.setHandler(&assembler);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return assembler.assembleMeta();
}


// [[Rcpp::export]]
List cpp_meta_wkb(List wkb, bool recursive) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_meta_base(reader, recursive);
}

// [[Rcpp::export]]
List cpp_meta_wkt(CharacterVector wkt, bool recursive) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return cpp_meta_base(reader, recursive);
}

// [[Rcpp::export]]
List cpp_meta_wkt_streamer(CharacterVector wkt, bool recursive) {
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return cpp_meta_base(reader, recursive);
}

// [[Rcpp::export]]
List cpp_meta_wksxp(List wksxp, bool recursive) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return cpp_meta_base(reader, recursive);
}
