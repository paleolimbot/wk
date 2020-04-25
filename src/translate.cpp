#include <Rcpp.h>
#include "wkheaders/wkb-iterator.h"
#include "wkheaders/io-rcpp.h"
#include <iostream>
#include <sstream>
using namespace Rcpp;



class WKTTranslateIterator: WKBIterator {
public:

  WKTTranslateIterator(BinaryReader* reader): WKBIterator(reader) {

  }

  bool hasNextFeature() {
    return WKBIterator::hasNextFeature();
  }

  std::string translateFeature() {
    this->out = std::stringstream();
    this->iterate();
    return this->out.str();
  }

  // wait until the SRID to print the geometry type
  // if there is one
  void nextGeometryType(GeometryType geometryType, uint32_t partId) {
    if (!geometryType.hasSRID) {
      this->writeGeometrySep(geometryType, partId, 0);
    }
  }

  void nextSRID(GeometryType geometryType, uint32_t partId, uint32_t srid) {
    this->writeGeometrySep(geometryType, partId, srid);
  }

  void nextGeometry(GeometryType geometryType, uint32_t partId, uint32_t size) {
    if (size > 0) {
      this->out << "(";
    } else {
      this->out << "EMPTY";
    }

    WKBIterator::nextGeometry(geometryType, partId, size);

    if (size > 0) {
      this->out << ")";
    }
  }

  void nextLinearRing(GeometryType geometryType, uint32_t ringId, uint32_t size) {
    this->writeRingSep(ringId);
    this->out << "(";
    WKBIterator::nextLinearRing(geometryType, ringId, size);
    this->out << ")";
  }

  void nextCoordinate(const WKCoord coord, uint32_t coordId) {
    this->writeCoordSep(coordId);
    this->out << coord[0];
    for (size_t i=1; i < coord.size(); i++) {
      this->out << " " << coord[i];
    }
  }

  void writeGeometrySep(const GeometryType geometryType, uint32_t partId, uint32_t srid) {
    bool iterCollection = iteratingCollection();
    bool iterMulti = iteratingMulti();

    if ((iterCollection || iterMulti) && partId > 0) {
      this->out << ", ";
    }
    
    if(iterMulti) {
      return;
    }
    
    if(!iterCollection && geometryType.hasSRID) {
      this->out << "SRID=" << srid << ";";
    }
    
    this->out << geometryType.wktType() << " ";
  }

  void writeRingSep(uint32_t ringId) {
    if (ringId > 0) {
      this->out << ", ";
    }
  }

  void writeCoordSep(uint32_t coordId) {
    if (coordId > 0) {
      this->out << ", ";
    }
  }

  bool iteratingMulti() {
    size_t stackSize = this->recursionLevel();
    if (stackSize <= 1) {
      return false;
    }

    const GeometryType nester = this->lastGeometryType(-2);
    return nester.simpleGeometryType == SimpleGeometryType::MultiPoint ||
      nester.simpleGeometryType == SimpleGeometryType::MultiLineString ||
      nester.simpleGeometryType == SimpleGeometryType::MultiPolygon;
  }

  bool iteratingCollection() {
    size_t stackSize = this->recursionLevel();
    if (stackSize <= 1) {
      return false;
    }

    const GeometryType nester = this->lastGeometryType(-2);
    return nester.simpleGeometryType == SimpleGeometryType::GeometryCollection;
  }

private:
  std::stringstream out;
};

// [[Rcpp::export]]
CharacterVector cpp_translate_wkb_wkt(List x) {
  CharacterVector out(x.size());
  WKTTranslateIterator iter(new WKRawVectorListReader(x));

  R_xlen_t i = 0;
  while (iter.hasNextFeature()) {
    out[i] = iter.translateFeature();
    i++;
  }

  return out;
}
