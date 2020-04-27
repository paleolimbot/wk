
#ifndef WK_TRANSLATOR_H
#define WK_TRANSLATOR_H

#include <iostream>
#include "wk/translator.h"
#include "wk/wkb-reader.h"

class WKBWKTTranslator: WKBReader, public WKTranslator {
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
  WKGeometryType newGeometryType;

  // wait until the SRID to print the geometry type if there is one
  void nextGeometryType(const WKGeometryType geometryType, uint32_t partId) {
    this->newGeometryType = this->getNewGeometryType(geometryType);
    if (!geometryType.hasSRID) {
      this->writeGeometrySep(this->newGeometryType, partId, 0);
    }
  }

  void nextSRID(const WKGeometryType geometryType, uint32_t partId, uint32_t srid) {
    this->writeGeometrySep(this->newGeometryType, partId, srid);
  }

  void nextGeometry(const WKGeometryType geometryType, uint32_t partId, uint32_t size) {
    this->writeGeometryOpen(size);
    WKBReader::nextGeometry(geometryType, partId, size);
    this->writeGeometryClose(size);
  }

  void nextLinearRing(const WKGeometryType geometryType, uint32_t ringId, uint32_t size) {
    this->writeRingSep(ringId);
    this->out << "(";
    WKBReader::nextLinearRing(geometryType, ringId, size);
    this->out << ")";
  }

  void nextCoordinate(const WKCoord coord, uint32_t coordId) {
    this->writeCoordSep(coordId);
    this->out << coord.x << " " << coord.y;
    if (this->newGeometryType.hasZ && coord.hasZ) {
      this->out << " " << coord.z;
    }
    if (this->newGeometryType.hasM && coord.hasM) {
      this->out << " " << coord.m;
    }
  }

  void writeGeometryOpen(uint32_t size) {
    if (size == 0) {
      this->out << "EMPTY";
    } else {
      this->out << "(";
    }
  }

  void writeGeometryClose(uint32_t size) {
    if (size > 0) {
      this->out << ")";
    }
  }

  void writeGeometrySep(const WKGeometryType geometryType, uint32_t partId, uint32_t srid) {
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

    const WKGeometryType nester = this->lastGeometryType(-2);
    return nester.simpleGeometryType == SimpleGeometryType::MultiPoint ||
      nester.simpleGeometryType == SimpleGeometryType::MultiLineString ||
      nester.simpleGeometryType == SimpleGeometryType::MultiPolygon;
  }

  bool iteratingCollection() {
    size_t stackSize = this->recursionLevel();
    if (stackSize <= 1) {
      return false;
    }

    const WKGeometryType nester = this->lastGeometryType(-2);
    return nester.simpleGeometryType == SimpleGeometryType::GeometryCollection;
  }
};

#endif
