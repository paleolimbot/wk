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
  void nextGeometryType(GeometryType geometryType) {
    if (!geometryType.hasSRID) {
      this->writeGeometrySep(geometryType, 0);
    }
  }

  void nextSRID(GeometryType geometryType, uint32_t srid) {
    this->writeGeometrySep(geometryType, srid);
  }

  void nextEmpty(GeometryType geometryType) {
    this->out << "EMPTY";
  }

  void nextGeometry(GeometryType geometryType, uint32_t size) {
    this->out << "(";
    WKBIterator::nextGeometry(geometryType, size);
    this->out << ")";
  }

  void nextMultiGeometry(GeometryType geometryType, uint32_t size) {
    this->out << "(";
    WKBIterator::nextMultiGeometry(geometryType, size);
    this->out << ")";
  }

  void nextCollection(GeometryType geometryType, uint32_t size) {
    this->out << "(";
    WKBIterator::nextCollection(geometryType, size);
    this->out << ")";
  }

  void nextLinearRing(GeometryType geometryType, uint32_t size) {
    this->writeRingSep();
    this->out << "(";
    WKBIterator::nextLinearRing(geometryType, size);
    this->out << ")";
  }

  void nextXY(double x, double y) {
    this->writeCoordSep();
    this->out << x << " " << y;
  }

  void nextXYZ(double x, double y, double z) {
    this->writeCoordSep();
    this->out << x << " " << y << " " << z;
  }

  void nextXYM(double x, double y, double m) {
    this->writeCoordSep();
    this->out << x << " " << y << " " << m;
  }

  void nextXYZM(double x, double y, double z, double m) {
    this->writeCoordSep();
    this->out << x << " " << y << " " << z << " " << m;
  }

  void writeGeometrySep(GeometryType geometryType, uint32_t srid) {
    bool iterCollection = iteratingCollection();
    bool iterMulti = iteratingMulti();

    if ((iterCollection || iterMulti) && this->partId > 0) {
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

  bool iteratingMulti() {
    size_t stackSize = this->stack.size();
    if (this->stack.size() <= 1) {
      return false;
    }

    GeometryType nester = this->stack[stackSize - 2];
    return nester.simpleGeometryType == SimpleGeometryType::MultiPoint ||
      nester.simpleGeometryType == SimpleGeometryType::MultiLineString ||
      nester.simpleGeometryType == SimpleGeometryType::MultiPolygon;
  }

  bool iteratingCollection() {
    size_t stackSize = this->stack.size();
    if (this->stack.size() <= 1) {
      return false;
    }

    GeometryType nester = this->stack[stackSize - 2];
    return nester.simpleGeometryType == SimpleGeometryType::GeometryCollection;
  }

  void writeRingSep() {
    if (this->ringId > 0) {
      this->out << ", ";
    }
  }

  void writeCoordSep() {
    if (this->coordId > 0 && this->geometryType.simpleGeometryType != SimpleGeometryType::Point) {
      this->out << ", ";
    }
  }

private:
  std::ostream& out;
};



// [[Rcpp::export]]
void test_basic_reader(RawVector data) {
  WKTTranslateIterator iter(new RawVectorBinaryReader(data), Rcout);
  iter.nextFeature();
}
