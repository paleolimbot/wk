
#ifndef WK_WKB_READER_H
#define WK_WKB_READER_H

#include "wk/geometry-type.h"
#include "wk/io-utils.h"
#include "wk/wk-coord.h"

class WKBReader {

public:
  const static uint32_t PART_ID_INVALID = UINT32_MAX;
  const static uint32_t RING_ID_INVALID = UINT32_MAX;
  const static uint32_t COORD_ID_INVALID = UINT32_MAX;
  const static uint32_t SRID_INVALID = UINT32_MAX;
  const static unsigned char ENDIAN_INVALID = 0xff;

  WKBReader(BinaryReader& reader): reader(reader) {
    this->swapEndian = false;
    this->featureId = 0;
    this->partId = PART_ID_INVALID;
    this->ringId = RING_ID_INVALID;
    this->coordId = COORD_ID_INVALID;
    this->srid = SRID_INVALID;
    this->endian = ENDIAN_INVALID;
    this->stack = std::vector<GeometryType>();
  }

  bool hasNextFeature() {
    return this->reader.seekNextFeature();
  }

  void iterateFeature() {
    this->readFeature();
  }

protected:

  virtual void nextFeature(size_t featureId) {
    if (this->reader.featureIsNull()) {
      this->nextNull(featureId);
    } else {
      this->readGeometry(PART_ID_INVALID);
    }
  }

  virtual void nextNull(size_t featureId) {

  }

  virtual void nextGeometry(const GeometryType geometryType, uint32_t partId, uint32_t size) {
    switch (geometryType.simpleGeometryType) {
    case SimpleGeometryType::Point:
      this->nextPoint(geometryType);
      break;
    case SimpleGeometryType::LineString:
      this->nextLinestring(geometryType, size);
      break;
    case SimpleGeometryType::Polygon:
      this->nextPolygon(geometryType, size);
      break;
    case SimpleGeometryType::MultiPoint:
    case SimpleGeometryType::MultiLineString:
    case SimpleGeometryType::MultiPolygon:
    case SimpleGeometryType::GeometryCollection:
      this->nextCollection(this->geometryType, size);
      break;
    default:
      throw std::runtime_error(
          Formatter() <<
            "Unrecognized geometry type in WKBReader::readGeometry(): " <<
            geometryType.simpleGeometryType
      );
    }
  }

  virtual void nextPoint(const GeometryType geometryType) {
    this->readPoint(geometryType, 0);
  }

  virtual void nextLinestring(const GeometryType geometryType, uint32_t size) {
    this->readLineString(geometryType, size);
  }

  virtual void nextPolygon(const GeometryType geometryType, uint32_t size) {
    this->readPolygon(geometryType, size);
  }

  virtual void nextLinearRing(const GeometryType geometryType, uint32_t ringId, uint32_t size) {
    this->readLineString(geometryType, size);
  }

  virtual void nextCollection(const GeometryType geometryType, uint32_t size) {
    this->readMultiGeometry(geometryType, size);
  }

  virtual void nextCoordinate(const WKCoord coord, uint32_t coordId) {

  }

  virtual void nextEndian(unsigned char endian, uint32_t partId) {

  }

  virtual void nextGeometryType(const GeometryType geometryType, uint32_t partId) {

  }

  virtual void nextSRID(const GeometryType geometryType, uint32_t partId, uint32_t srid) {

  }

  // accessors (may need more, these are sufficient for WKT translator)
  const GeometryType lastGeometryType(int level) {
    if (level >= 0) {
      return this->stack[level];
    } else {
      return this->stack[this->stack.size() + level];
    }
  }

  const GeometryType lastGeometryType() {
    return lastGeometryType(-1);
  }

  size_t recursionLevel() {
    return this->stack.size();
  }

  // endian swapping is hard to replicate...these might be useful
  // for subclasses that implement an extension of WKB
  unsigned char readChar() {
    return this->readCharRaw();
  }

  double readDouble() {
    if (this->swapEndian) {
      return IOUtils::swapEndian<double>(this->readDoubleRaw());
    } else
      return this->readDoubleRaw();
  }

  double readUint32() {
    if (this->swapEndian) {
      return IOUtils::swapEndian<uint32_t>(this->readUint32Raw());
    } else
      return this->readUint32Raw();
  }

private:
  BinaryReader& reader;

  size_t featureId;
  uint32_t partId;
  uint32_t ringId;
  uint32_t coordId;

  std::vector<GeometryType> stack;

  bool swapEndian;
  unsigned char endian;
  GeometryType geometryType;
  uint32_t srid;
  double x;
  double y;
  double z;
  double m;

  void readFeature() {
    this->partId = PART_ID_INVALID;
    this->ringId = RING_ID_INVALID;
    this->coordId = COORD_ID_INVALID;
    this->endian = ENDIAN_INVALID;
    this->srid = SRID_INVALID;
    this->stack.clear();

    this->nextFeature(this->featureId);
    this->featureId += 1;
  }

  void readGeometry(uint32_t partId) {
    this->endian = this->readChar();
    this->swapEndian = ((int)endian != (int)IOUtils::nativeEndian());
    this->nextEndian(this->endian, partId);

    const GeometryType geometryType = GeometryType(this->readUint32());
    this->stack.push_back(geometryType);
    this->nextGeometryType(geometryType, partId);

    if (geometryType.hasSRID) {
      this->srid = this->readUint32();
      this->nextSRID(geometryType, partId, this->srid);
    }

    if (geometryType.simpleGeometryType == SimpleGeometryType::Point) {
      this->nextGeometry(geometryType, partId, 1);
    } else {
      this->nextGeometry(geometryType, partId, this->readUint32());
    }

    this->stack.pop_back();
  }

  void readPoint(GeometryType geometryType, uint32_t coordId) {
    this->x = this->readDouble();
    this->y = this->readDouble();

    if (geometryType.hasZ && geometryType.hasM) {
      this->z = this->readDouble();
      this->m = this->readDouble();
      this->nextCoordinate(WKCoord::xyzm(x, y, z, m), coordId);

    } else if (geometryType.hasZ) {
      this->z = this->readDouble();
      this->nextCoordinate(WKCoord::xyz(x, y, z), coordId);

    } else if (geometryType.hasM) {
      this->m = this->readDouble();
      this->nextCoordinate(WKCoord::xym(x, y, m), coordId);

    } else {
      this->nextCoordinate(WKCoord::xy(x, y), coordId);
    }
  }

  void readLineString(GeometryType geometryType, uint32_t size) {
    for (uint32_t i=0; i < size; i++) {
      this->coordId = i;
      this->readPoint(geometryType, i);
    }
  }

  void readPolygon(GeometryType geometryType, uint32_t size) {
    uint32_t ringSize;
    for (uint32_t i=0; i<size; i++) {
      this->ringId = i;
      ringSize = this->readUint32();
      this->nextLinearRing(geometryType, i, ringSize);
    }
  }

  void readMultiGeometry(GeometryType geometryType, uint32_t size) {
    for (uint32_t i=0; i < size; i++) {
      this->partId = i;
      this->readGeometry(i);
    }
  }

  unsigned char readCharRaw() {
    return this->reader.readCharRaw();
  }

  double readDoubleRaw() {
    return this->reader.readDoubleRaw();
  }

  uint32_t readUint32Raw() {
    return this->reader.readUint32Raw();
  }

  bool seekNextFeature() {
    return this->reader.seekNextFeature();
  }
};

#endif
