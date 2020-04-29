
#ifndef WK_WKB_READER_H
#define WK_WKB_READER_H

#include "wk/parse-exception.h"
#include "wk/geometry-meta.h"
#include "wk/io-utils.h"
#include "wk/geometry-handler.h"
#include "wk/coord.h"

class WKBReader {

public:
  const static uint32_t SIZE_UNKNOWN = UINT32_MAX;
  const static uint32_t PART_ID_INVALID = UINT32_MAX;
  const static uint32_t RING_ID_INVALID = UINT32_MAX;
  const static uint32_t COORD_ID_INVALID = UINT32_MAX;
  const static uint32_t SRID_INVALID = UINT32_MAX;
  const static unsigned char ENDIAN_INVALID = 0xff;

  WKBReader(BinaryReader& reader, WKGeometryHandler& handler): reader(reader), handler(handler) {
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
    this->partId = PART_ID_INVALID;
    this->ringId = RING_ID_INVALID;
    this->coordId = COORD_ID_INVALID;
    this->endian = ENDIAN_INVALID;
    this->srid = SRID_INVALID;
    this->stack.clear();

    try {
      this->readFeature(this->featureId);
    } catch (WKParseException& error) {
      if (!handler.nextError(error, this->featureId)) {
        throw error;
      }
    }

    this->featureId += 1;
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

protected:
  BinaryReader& reader;
  WKGeometryHandler& handler;

  size_t featureId;
  uint32_t partId;
  uint32_t ringId;
  uint32_t coordId;

  std::vector<WKGeometryMeta> stack;

  unsigned char endian;
  WKGeometryMeta meta;
  uint32_t srid;
  double x;
  double y;
  double z;
  double m;

  virtual void readFeature(size_t featureId) {
    this->handler.nextFeatureStart(featureId);

    if (this->reader.featureIsNull()) {
      this->handler.nextNull(featureId);
    } else {
      this->readGeometry(PART_ID_INVALID);
    }

    this->handler.nextFeatureEnd(featureId);
  }

  void readGeometry(uint32_t partId) {
    this->endian = this->readChar();
    this->swapEndian = ((int)endian != (int)IOUtils::nativeEndian());

    WKGeometryMeta meta = WKGeometryMeta(this->readUint32());

    if (meta.hasSRID) {
      meta.srid = this->readUint32();
      this->srid = meta.srid;
    }

    if (meta.geometryType == WKGeometryType::Point) {
      meta.hasSize = true;
      meta.size = 1;
    } else {
      meta.hasSize = true;
      meta.size = this->readUint32();
    }

    this->stack.push_back(meta);
    this->handler.nextGeometryStart(meta, partId);

    switch (meta.geometryType) {
    case WKGeometryType::Point:
      this->readCoordinate(meta, 0);
      break;
    case WKGeometryType::LineString:
      this->readLineString(meta);
      break;
    case WKGeometryType::Polygon:
      this->readPolygon(meta);
      break;
    case WKGeometryType::MultiPoint:
    case WKGeometryType::MultiLineString:
    case WKGeometryType::MultiPolygon:
    case WKGeometryType::GeometryCollection:
      this->readCollection(meta);
      break;
    default:
      throw WKParseException(
          Formatter() <<
            "Unrecognized geometry type in WKBReader::readGeometry(): " <<
              meta.geometryType
      );
    }

    this->handler.nextGeometryEnd(meta, partId);
    this->stack.pop_back();
  }

  void readLineString(WKGeometryMeta meta) {
    for (uint32_t i=0; i < meta.size; i++) {
      this->coordId = i;
      this->readCoordinate(meta, i);
    }
  }

  void readLinearRing(WKGeometryMeta meta, uint32_t ringId, uint32_t size) {
    this->handler.nextLinearRingStart(meta, ringId, size);
    for (uint32_t i=0; i < size; i++) {
      this->coordId = i;
      this->readCoordinate(meta, i);
    }
    this->handler.nextLinearRingEnd(meta, ringId, size);
  }

  void readPolygon(WKGeometryMeta meta) {
    uint32_t ringSize;
    for (uint32_t i=0; i < meta.size; i++) {
      this->ringId = i;
      ringSize = this->readUint32();
      this->readLinearRing(meta, i, ringSize);
    }
  }

  void readCollection(WKGeometryMeta meta) {
    for (uint32_t i=0; i < meta.size; i++) {
      this->partId = i;
      this->readGeometry(i);
    }
  }

  void readCoordinate(WKGeometryMeta meta, uint32_t coordId) {
    this->x = this->readDouble();
    this->y = this->readDouble();

    if (meta.hasZ && meta.hasM) {
      this->z = this->readDouble();
      this->m = this->readDouble();
      this->handler.nextCoordinate(meta, WKCoord::xyzm(x, y, z, m), coordId);

    } else if (meta.hasZ) {
      this->z = this->readDouble();
      this->handler.nextCoordinate(meta, WKCoord::xyz(x, y, z), coordId);

    } else if (meta.hasM) {
      this->m = this->readDouble();
      this->handler.nextCoordinate(meta, WKCoord::xym(x, y, m), coordId);

    } else {
      this->handler.nextCoordinate(meta, WKCoord::xy(x, y), coordId);
    }
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

private:
  bool swapEndian;

  double readUint32() {
    if (this->swapEndian) {
      return IOUtils::swapEndian<uint32_t>(this->readUint32Raw());
    } else
      return this->readUint32Raw();
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
