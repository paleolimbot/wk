#include <Rcpp.h>
#include "wkheaders/wkb-iterator.h"
#include "wkheaders/io-utils.h"
#include <iostream>
using namespace Rcpp;

class RawVectorBinaryReader: public BinaryReader {
public:
  bool hasNext;
  RawVector data;
  R_xlen_t offset;

  RawVectorBinaryReader(RawVector data) {
    this->data = data;
    this->offset = 0;
    this->hasNext = true;
  }

protected:
  unsigned char readCharRaw() {
    return readBinary<unsigned char>();
  }

  double readDoubleRaw() {
    return readBinary<double>();
  }

  uint32_t readUint32Raw() {
    return readBinary<uint32_t>();
  }

  bool seekNextFeature() {
    if (this->hasNext) {
      this->hasNext = false;
    }

    return this->hasNext;
  }

private:
  template<typename T>
  T readBinary() {
    // Rcout << "Reading " << sizeof(T) << " starting at " << this->offset << "\n";
    if ((this->offset + sizeof(T)) > this->data.size()) {
      stop("Reached end of RawVector input");
    }

    T dst;
    memcpy(&dst, &(this->data[this->offset]), sizeof(T));
    this->offset += sizeof(T);
    return dst;
  }
};

class WKTTranslateIterator: public WKBIterator {
public:

  WKTTranslateIterator(BinaryReader* reader, std::ostream& out): WKBIterator(reader), out(out) {

  }

  void nextFeature() {
    WKBIterator::nextFeature();
    this->out << "\n";
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

  void nextCoordinate(Coordinate coord, uint32_t coordId) {
    this->writeCoordSep(coordId);
    this->out << coord[0];
    for (size_t i=1; i < coord.size(); i++) {
      this->out << " " << coord[i];
    }
  }

  void writeGeometrySep(GeometryType geometryType, uint32_t partId, uint32_t srid) {
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

    GeometryType nester = this->lastGeometryType(-2);
    return nester.simpleGeometryType == SimpleGeometryType::MultiPoint ||
      nester.simpleGeometryType == SimpleGeometryType::MultiLineString ||
      nester.simpleGeometryType == SimpleGeometryType::MultiPolygon;
  }

  bool iteratingCollection() {
    size_t stackSize = this->recursionLevel();
    if (stackSize <= 1) {
      return false;
    }

    GeometryType nester = this->lastGeometryType(-2);
    return nester.simpleGeometryType == SimpleGeometryType::GeometryCollection;
  }

private:
  std::ostream& out;
};

// [[Rcpp::export]]
void test_basic_reader(RawVector data) {
  WKTTranslateIterator iter(new RawVectorBinaryReader(data), Rcout);
  iter.nextFeature();
}
