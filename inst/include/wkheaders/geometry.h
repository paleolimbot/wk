
#ifndef WKHEADERS_GEOMETRY_H
#define WKHEADERS_GEOMETRY_H

#include <cstdint>
#include <vector>

enum WKGeometryType {
  Point = 1,
  Linestring = 2,
  Polygon = 3,
  MultiPoint = 4,
  MultiLinestring = 5,
  MultiPolygon = 6,
  GeometryCollection = 7
};

enum WKBByteOrder {
  BigEndian = 0,
  LitleEndian = 1
};

template <int nOrdinates>
class Coord {
public:
  double ordinates[nOrdinates];
};

template <int nOrdinates>
class LinearRing {
public:
  std::vector<Coord<nOrdinates>> points;

  LinearRing(size_t numPoints) {
    this->points = std::vector<Coord<nOrdinates>>(numPoints);
  }
};

class WKGeometry {
public:
  virtual WKGeometryType getGeometryType() = 0;
};

template <int nOrdinates>
class WKNDPoint: public WKGeometry {
public:
  Coord<nOrdinates> coord;

  WKGeometryType getGeometryType() {
    return WKGeometryType::Point;
  }
};

template <int nOrdinates>
class WKNDLinestring: public WKGeometry {
public:
  std::vector<Coord<nOrdinates>> points;

  WKNDLinestring(size_t nPoints) {
    this->points = std::vector<Coord<nOrdinates>>(nPoints);
  }

  WKGeometryType getGeometryType() {
    return WKGeometryType::Linestring;
  }
};

template <int nOrdinates>
class WKNDPolygon: public WKGeometry {
public:
  std::vector<LinearRing<nOrdinates>> rings;

  WKNDPolygon(size_t nRings) {
    this->rings = std::vector<LinearRing<nOrdinates>>(nRings);
  }

  WKGeometryType getGeometryType() {
    return WKGeometryType::Polygon;
  }
};


class WKPoint: public WKNDPoint<2> {
public:
  WKPoint(Coord<2> coord) {
    this->coord = coord;
  }

  virtual uint32_t getWKBType() {
    return WKGeometryType::Point;
  }
};

class WKLinestring: public WKNDLinestring<2> {
  virtual uint32_t getWKBType() {
    return WKGeometryType::Linestring;
  }
};

class WKPolygon: public WKNDPolygon<2> {
  virtual uint32_t getWKBType() {
    return WKGeometryType::Polygon;
  }
};

#endif
