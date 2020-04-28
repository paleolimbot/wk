
#ifndef WK_WKB_READER_H
#define WK_WKB_READER_H

#include "wk/geometry-meta.h"
#include "wk/io-utils.h"
#include "wk/coord.h"

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
    this->stack = std::vector<WKGeometryMeta>();
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

  virtual void nextGeometry(WKGeometryMeta meta, uint32_t partId) {
    switch (meta.geometryType) {
    case WKGeometryType::Point:
      this->nextPoint(meta);
      break;
    case WKGeometryType::LineString:
      this->nextLinestring(meta);
      break;
    case WKGeometryType::Polygon:
      this->nextPolygon(meta);
      break;
    case WKGeometryType::MultiPoint:
    case WKGeometryType::MultiLineString:
    case WKGeometryType::MultiPolygon:
    case WKGeometryType::GeometryCollection:
      this->nextCollection(meta);
      break;
    default:
      throw std::runtime_error(
          Formatter() <<
            "Unrecognized geometry type in WKBReader::readGeometry(): " <<
            meta.geometryType
      );
    }
  }

  virtual void nextPoint(WKGeometryMeta meta) {
    this->readPoint(meta, 0);
  }

  virtual void nextLinestring(WKGeometryMeta meta) {
    this->readLineString(meta);
  }

  virtual void nextPolygon(WKGeometryMeta meta) {
    this->readPolygon(meta);
  }

  virtual void nextLinearRing(WKGeometryMeta meta, uint32_t ringId, uint32_t size) {
    this->readLinearRing(meta, size);
  }

  virtual void nextCollection(WKGeometryMeta meta) {
    this->readMultiGeometry(meta);
  }

  virtual void nextCoordinate(const WKCoord coord, uint32_t coordId) {

  }

  virtual void nextEndian(unsigned char endian, uint32_t partId) {

  }

  virtual void nextGeometryType(WKGeometryMeta meta, uint32_t partId) {

  }

  virtual void nextSRID(WKGeometryMeta meta, uint32_t partId, uint32_t srid) {

  }

  // accessors (may need more, these are sufficient for WKT translator)
  const WKGeometryMeta lastGeometryType(int level) {
    if (level >= 0) {
      return this->stack[level];
    } else {
      return this->stack[this->stack.size() + level];
    }
  }

  const WKGeometryMeta lastGeometryType() {
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

  std::vector<WKGeometryMeta> stack;

  bool swapEndian;
  unsigned char endian;
  WKGeometryMeta meta;
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

    WKGeometryMeta meta = WKGeometryMeta(this->readUint32());
    this->stack.push_back(meta);
    this->nextGeometryType(meta, partId);

    if (meta.hasSRID) {
      meta.srid = this->readUint32();
      this->srid = meta.srid;
      this->nextSRID(meta, partId, this->srid);
    }

    if (meta.geometryType == WKGeometryType::Point) {
      meta.hasSize = true;
      meta.size = 1;
      this->nextGeometry(meta, partId);
    } else {
      meta.hasSize = true;
      meta.size = this->readUint32();
      this->nextGeometry(meta, partId);
    }
    this->stack.pop_back();
  }

  void readPoint(WKGeometryMeta meta, uint32_t coordId) {
    this->x = this->readDouble();
    this->y = this->readDouble();

    if (meta.hasZ && meta.hasM) {
      this->z = this->readDouble();
      this->m = this->readDouble();
      this->nextCoordinate(WKCoord::xyzm(x, y, z, m), coordId);

    } else if (meta.hasZ) {
      this->z = this->readDouble();
      this->nextCoordinate(WKCoord::xyz(x, y, z), coordId);

    } else if (meta.hasM) {
      this->m = this->readDouble();
      this->nextCoordinate(WKCoord::xym(x, y, m), coordId);

    } else {
      this->nextCoordinate(WKCoord::xy(x, y), coordId);
    }
  }

  void readLineString(WKGeometryMeta meta) {
    for (uint32_t i=0; i < meta.size; i++) {
      this->coordId = i;
      this->readPoint(meta, i);
    }
  }

  void readLinearRing(WKGeometryMeta meta, uint32_t size) {
    for (uint32_t i=0; i < size; i++) {
      this->coordId = i;
      this->readPoint(meta, i);
    }
  }

  void readPolygon(WKGeometryMeta meta) {
    uint32_t ringSize;
    for (uint32_t i=0; i < meta.size; i++) {
      this->ringId = i;
      ringSize = this->readUint32();
      this->nextLinearRing(meta, i, ringSize);
    }
  }

  void readMultiGeometry(WKGeometryMeta meta) {
    for (uint32_t i=0; i < meta.size; i++) {
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
