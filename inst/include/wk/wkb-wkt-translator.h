
#ifndef WK_TRANSLATOR_H
#define WK_TRANSLATOR_H

#include <iostream>
#include "wk/wkb-reader.h"

class WKBWKTTranslator: WKBReader {
public:

  WKBWKTTranslator(BinaryReader& reader, std::ostream& stream): WKBReader(reader), out(stream) {}

  // expose these as the public interface
  bool hasNextFeature() {
    return WKBReader::hasNextFeature();
  }

  virtual void iterateFeature() {
    WKBReader::iterateFeature();
  }

  // output stream manipulations I'd rather not remember
  // how to do again
  void ensureClassicLocale() {
    this->out.imbue(std::locale::classic());
  }

  void setRoundingPrecision(int precision) {
    this->out.precision(precision);
  }

  void setTrim(bool trim) {
    if (trim) {
      this->out.unsetf(out.fixed);
    } else {
      this->out.setf(out.fixed);
    }
  }

protected:
  std::ostream& out;
  
  // default null handling returns ""
  virtual void nextNull(size_t featureId) {

  }

  // theoretically, somebody might want to change this behaviour
  virtual void nextFeature(size_t featureId) {
    WKBReader::nextFeature(featureId);
  }

private:
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

    WKBReader::nextGeometry(geometryType, partId, size);

    if (size > 0) {
      this->out << ")";
    }
  }

  void nextLinearRing(GeometryType geometryType, uint32_t ringId, uint32_t size) {
    this->writeRingSep(ringId);
    this->out << "(";
    WKBReader::nextLinearRing(geometryType, ringId, size);
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
};

#endif