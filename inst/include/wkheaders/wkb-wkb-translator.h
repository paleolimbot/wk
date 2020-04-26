
#ifndef WKHEADERS_WKB_WKB_TRANSLATOR
#define WKHEADERS_WKB_WKB_TRANSLATOR

#include <memory>
#include "wkheaders/wkb-writer.h"
#include "wkheaders/wkb-iterator.h"

class WKBWKBTranslator: WKBIterator {
  WKBWKBTranslator(BinaryReader* reader, WKBWriter* writer): WKBIterator(reader) {
    this->writer = std::unique_ptr<WKBWriter>(writer);
  }

  void setEndian(unsigned char endian) {
    this->writer->setEndian(endian);
  }

  void setIncludeSRID(int includeSRID) {
    this->includeSRID = includeSRID;
  }

  void setIncludeZ(int includeZ) {
    this->includeZ = includeZ;
  }

  void setIncludeM(int includeM) {
    this->includeM = includeM;
  }

  void nextGeometry(const GeometryType geometryType, uint32_t partId, uint32_t size) {
    this->writer->writeEndian();

    uint32_t ewkbType = geometryType.ewkbType;
    this->writer->writeUint32(ewkbType);

    if (geometryType.simpleGeometryType != SimpleGeometryType::Point) {
      this->writer->writeUint32(size);
    }

    WKBIterator::nextGeometry(geometryType, partId, size);
  }

  void nextLinearRing(const GeometryType geometryType, uint32_t ringId, uint32_t size) {
    this->writer->writeUint32(size);
    WKBIterator::nextLinearRing(geometryType, ringId, size);
  }

  void nextCoordinate(const WKCoord coord, uint32_t coordId) {
    this->writer->writeCoord(coord);
  }

private:
  std::unique_ptr<WKBWriter> writer;
  int includeSRID;
  int includeZ;
  int includeM;
};

#endif
