
#ifndef WKHEADERS_GEOMETRY_H
#define WKHEADERS_GEOMETRY_H

#include <cstdint>
#include <vector>
#include "geometry-type.h"

template <int nOrdinates>
class Coord {
public:
  int nDimensions = nOrdinates;
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
  virtual GeometryType geometryType() = 0;
  virtual EWKBGeometryType ewkbGeometryType() = 0;
  virtual ~WKGeometry() {}
};

template <int nOrdinates>
class WKNDPoint: public WKGeometry {
public:
  Coord<nOrdinates> coord;

  GeometryType geometryType() {
    return GeometryType::Point;
  }
};

template <int nOrdinates>
class WKNDLinestring: public WKGeometry {
public:
  std::vector<Coord<nOrdinates>> points;

  WKNDLinestring(size_t nPoints) {
    this->points = std::vector<Coord<nOrdinates>>(nPoints);
  }

  GeometryType geometryType() {
    return GeometryType::Linestring;
  }
};

template <int nOrdinates>
class WKNDPolygon: public WKGeometry {
public:
  std::vector<LinearRing<nOrdinates>> rings;

  WKNDPolygon(size_t nRings) {
    this->rings = std::vector<LinearRing<nOrdinates>>(nRings);
  }

  GeometryType geometryType() {
    return GeometryType::Polygon;
  }
};


class WKPoint: public WKNDPoint<2> {
public:
  WKPoint(Coord<2> coord) {
    this->coord = coord;
  }

  EWKBGeometryType ewkbGeometryType() {
    return EWKBGeometryType::get(this->getGeometryType);
  }

};

class WKPointZ: public WKNDPoint<3> {
public:
  WKPointZ(Coord<3> coord) {
    this->coord = coord;
  }

  EWKBGeometryType ewkbGeometryType() {
    return EWKBGeometryType::getZ(this->getGeometryType);
  }
};

#endif
