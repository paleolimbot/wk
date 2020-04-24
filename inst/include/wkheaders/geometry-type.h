
#ifndef WKHEADERS_GEOMETRY_TYPE_H
#define WKHEADERS_GEOMETRY_TYPE_H


#include <cstdint>
#include <string>
#include "exception.h"

// https://github.com/postgis/postgis/blob/2.1.0/doc/ZMSgeoms.txt

enum GeometryType {
  Invalid = 0,
  Point = 1,
  LineString = 2,
  Polygon = 3,
  MultiPoint = 4,
  MultiLineString = 5,
  MultiPolygon = 6,
  GeometryCollection = 7
};

class EWKBGeometryType {
public:
  uint32_t geometryType;
  bool hasZ;
  bool hasM;
  bool hasSRID;

  EWKBGeometryType() {
    this->geometryType = GeometryType::Invalid;
    this->hasZ = false;
    this->hasM = false;
    this->hasSRID = false;
  }

  EWKBGeometryType(uint32_t geometryType, bool hasZ, bool hasM, bool hasSRID) {
    this->geometryType = geometryType;
    this->hasZ = hasZ;
    this->hasM = hasM;
    this->hasSRID = hasSRID;
  }

  uint32_t wkbType() {
    uint32_t out = this->geometryType;
    if (this->hasZ) out += 0x80000000;
    if (this->hasM) out += 0x40000000;
    if (this->hasSRID) out += 0x20000000;
    return out;
  }

  static EWKBGeometryType get(GeometryType geometryType) {
    return EWKBGeometryType(geometryType, false, false, false);
  }

  static EWKBGeometryType getZ(GeometryType geometryType) {
    return EWKBGeometryType(geometryType, true, false, false);
  }

  static EWKBGeometryType getZM(GeometryType geometryType) {
    return EWKBGeometryType(geometryType, true, true, false);
  }

  static EWKBGeometryType getZMS(GeometryType geometryType) {
    return EWKBGeometryType(geometryType, true, true, true);
  }

  static EWKBGeometryType getZS(GeometryType geometryType) {
    return EWKBGeometryType(geometryType, true, false, true);
  }

  static EWKBGeometryType getM(GeometryType geometryType) {
    return EWKBGeometryType(geometryType, false, true, false);
  }

  static EWKBGeometryType getMS(GeometryType geometryType) {
    return EWKBGeometryType(geometryType, false, true, true);
  }

  static EWKBGeometryType getS(GeometryType geometryType) {
    return EWKBGeometryType(geometryType, false, false, true);
  }

  static EWKBGeometryType get(uint32_t wkbType) {
    switch (wkbType) {
    // basic geometries
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
      return EWKBGeometryType(wkbType, false, false, false);

    // Z
    case 0x80000001:
    case 0x80000002:
    case 0x80000003:
    case 0x80000004:
    case 0x80000005:
    case 0x80000006:
    case 0x80000007:
      return EWKBGeometryType(wkbType - 0x80000000, true, false, false);

    // | 0x40000000 M
    case 0x40000001:
    case 0x40000002:
    case 0x40000003:
    case 0x40000004:
    case 0x40000005:
    case 0x40000006:
    case 0x40000007:
      return EWKBGeometryType(wkbType - 0x40000000, false, true, false);

    // | 0x40000000 | 0x80000000 ZM
    case 0xC0000001:
    case 0xC0000002:
    case 0xC0000003:
    case 0xC0000004:
    case 0xC0000005:
    case 0xC0000006:
    case 0xC0000007:
      return EWKBGeometryType(wkbType - 0xC0000000, true, true, false);

    // | 0x20000000 S
    case 0x20000001:
    case 0x20000002:
    case 0x20000003:
    case 0x20000004:
    case 0x20000005:
    case 0x20000006:
    case 0x20000007:
      return EWKBGeometryType(wkbType - 0x20000000, false, false, true);

    // | 0x20000000 | 0x80000000 ZS
    case 0xA0000001:
    case 0xA0000002:
    case 0xA0000003:
    case 0xA0000004:
    case 0xA0000005:
    case 0xA0000006:
    case 0xA0000007:
      return EWKBGeometryType(wkbType - 0xA0000000, true, false, true);

    // | 0x20000000 | 0x40000000 MS
    case 0x60000001:
    case 0x60000002:
    case 0x60000003:
    case 0x60000004:
    case 0x60000005:
    case 0x60000006:
    case 0x60000007:
      return EWKBGeometryType(wkbType - 0x60000000, false, true, true);

    // | 0x20000000 | 0x40000000 | 0x80000000 ZMS
    case 0xE0000001:
    case 0xE0000002:
    case 0xE0000003:
    case 0xE0000004:
    case 0xE0000005:
    case 0xE0000006:
    case 0xE0000007:
      return EWKBGeometryType(wkbType - 0xE0000000, true, true, true);

    default:
      throw std::runtime_error(Formatter() << "Unrecognized EWKB geometry type: " << wkbType);
    }
  }
};

#endif
