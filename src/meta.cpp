
#include <Rcpp.h>

#include "wk/io-rcpp.h"
#include "wk/geometry-handler.h"
#include "wk/wkb-reader.h"
#include "wk/wkt-reader.h"
#include "wk/wkt-streamer.h"

using namespace Rcpp;


class WKMetaFoundException: public WKParseException {
public:
  static const int CODE_META_FOUND = 294873;
  WKMetaFoundException(): WKParseException(CODE_META_FOUND) {}
};

class WKMetaCounter: public WKGeometryHandler {
public:
  size_t nMeta;
  WKMetaCounter(bool recursive): nMeta(0), recursive(recursive) {}

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->nMeta++;
    if (!this->recursive) {
      throw WKMetaFoundException();
    }
  }

  void nextNull(size_t featureId) {
    this->nMeta++;
  }

  bool nextError(WKParseException& error, size_t featureId) {
    if (error.code() == WKMetaFoundException::CODE_META_FOUND) {
      return true;
    } else {
      return false;
    }
  }

private:
  bool recursive;
};



class WKMetaAssembler: public WKGeometryHandler {
public:
  IntegerVector featureId;
  IntegerVector nestId;
  IntegerVector partId;
  IntegerVector typeId;
  IntegerVector size;
  IntegerVector srid;
  LogicalVector hasZ;
  LogicalVector hasM;

  WKMetaAssembler(bool recursive, size_t nMeta):
    featureId(nMeta),
    nestId(nMeta),
    partId(nMeta),
    typeId(nMeta),
    size(nMeta),
    srid(nMeta),
    hasZ(nMeta),
    hasM(nMeta),
    i(0),
    recursive(recursive) {}

  List assembleMeta() {
    return List::create(
      _["feature_id"] = this->featureId,
      _["nest_id"] = this->nestId,
      _["part_id"] = this->partId,
      _["type_id"] = this->typeId,
      _["size"] = this->size,
      _["srid"] = this->srid,
      _["has_z"] = this->hasZ,
      _["has_m"] = this->hasM
    );
  }

  void nextFeatureStart(size_t featureId) {
    this->lastNestId = 0;
    this->lastFeatureId = featureId + 1;
  }

  void nextNull(size_t featureId) {
    this->featureId[i] = this->lastFeatureId;
    this->nestId[i]  = NA_INTEGER;
    this->partId[i] = NA_INTEGER;
    this->typeId[i] = NA_INTEGER;
    this->size[i] = NA_INTEGER;
    this->srid[i] = NA_INTEGER;
    this->hasZ[i] = NA_LOGICAL;
    this->hasM[i] = NA_LOGICAL;

    this->i++;
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->featureId[i] = this->lastFeatureId;
    this->nestId[i]  = this->lastNestId;

    if (partId == WKReader::PART_ID_NONE) {
      this->partId[i] = NA_INTEGER;
    } else {
      this->partId[i] = partId + 1;
    }

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
      throw WKMetaFoundException();
    }

    if (meta.geometryType == WKGeometryType::GeometryCollection) {
      this->lastNestId++;
    }
  }

  void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    if (meta.geometryType == WKGeometryType::GeometryCollection) {
      this->lastNestId--;
    }
  }

  bool nextError(WKParseException& error, size_t featureId) {
    if (error.code() == WKMetaFoundException::CODE_META_FOUND) {
      return true;
    } else {
      return false;
    }
  }

protected:
  R_xlen_t i;
  size_t lastFeatureId;
  int lastNestId;
  bool recursive;
};


// [[Rcpp::export]]
List cpp_meta_wkb(List wkb, bool recursive) {

  // first, count
  WKMetaCounter counter(recursive);
  WKRawVectorListProvider provider(wkb);
  WKBReader readerCounter(provider, counter);
  while (readerCounter.hasNextFeature()) {
    readerCounter.iterateFeature();
  }

  // then, assemble
  WKMetaAssembler assembler(recursive, counter.nMeta);
  provider = WKRawVectorListProvider(wkb);
  WKBReader readerAssembler(provider, assembler);
  while (readerAssembler.hasNextFeature()) {
    readerAssembler.iterateFeature();
  }

  return assembler.assembleMeta();
}

// [[Rcpp::export]]
List cpp_meta_wkt(CharacterVector wkt, bool recursive) {

  // first, count
  WKMetaCounter counter(recursive);
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer readerCounter(provider, counter);
  while (readerCounter.hasNextFeature()) {
    readerCounter.iterateFeature();
  }

  // then, assemble
  WKMetaAssembler assembler(recursive, counter.nMeta);
  provider = WKCharacterVectorProvider(wkt);
  WKTReader readerAssembler(provider, assembler);
  while (readerAssembler.hasNextFeature()) {
    readerAssembler.iterateFeature();
  }

  return assembler.assembleMeta();
}

// [[Rcpp::export]]
List cpp_meta_wkt_streamer(CharacterVector wkt, bool recursive) {

  // first, count coordinates
  WKMetaCounter counter(recursive);
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer readerCounter(provider, counter);
  while (readerCounter.hasNextFeature()) {
    readerCounter.iterateFeature();
  }

  // then, assemble
  WKMetaAssembler assembler(recursive, counter.nMeta);
  provider = WKCharacterVectorProvider(wkt);
  WKTStreamer readerAssembler(provider, assembler);
  while (readerAssembler.hasNextFeature()) {
    readerAssembler.iterateFeature();
  }

  return assembler.assembleMeta();
}
