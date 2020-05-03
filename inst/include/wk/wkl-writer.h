
#ifndef WKL_WRITER_H
#define WKL_WRITER_H

#include "wk/writer.h"
#include "wk/io-rcpp.h"


class WKLWriter: public WKWriter {
public:
  WKLWriter(WKListExporter& exporter): WKWriter(exporter), feature(R_NilValue), exporter(exporter) {

  }

protected:
  // I'm sure there's a way to do this without as much copying
  std::vector<Rcpp::List> stack;
  SEXP feature;
  Rcpp::NumericMatrix currentCoordinates;
  WKListExporter& exporter;

  void nextFeatureStart(size_t featureId) {
    this->stack.clear();
    feature = R_NilValue;
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    // make sure meta has a valid size
    if (!meta.hasSize || meta.size == WKGeometryMeta::SIZE_UNKNOWN) {
      throw std::runtime_error("Can't write WKL wihout a valid meta.size");
    }

    // make a new geometry type based on the creation options
    this->newMeta = this->getNewMeta(meta);
    bool nestingMulti = false;

    switch (meta.geometryType) {

    case WKGeometryType::Point:
    case WKGeometryType::LineString:
      this->initCoords(meta, meta.size);
      if (!this->isNestingMulti()) {
        this->currentCoordinates.attr("class") = this->metaAsClass(this->newMeta);
      }
      if (meta.hasSRID && this->stack.size() > 0) {
        this->currentCoordinates.attr("srid") = meta.srid;
      }
      break;
    case WKGeometryType::Polygon:
    case WKGeometryType::MultiPoint:
    case WKGeometryType::MultiLineString:
    case WKGeometryType::MultiPolygon:
    case WKGeometryType::GeometryCollection:
      nestingMulti = this->isNestingMulti();
      this->stack.push_back(Rcpp::List(this->newMeta.size));
      if (this->newMeta.geometryType != WKGeometryType::Polygon || !nestingMulti) {
        this->stack[this->stack.size() - 1].attr("class") = this->metaAsClass(this->newMeta);
      }
      if (meta.hasSRID && this->stack.size() > 1) {
        this->stack[this->stack.size() - 1] = meta.srid;
      }
      break;

    default:
      throw WKParseException(
          Formatter() <<
            "Unrecognized geometry type: " <<
              meta.geometryType
      );
    }
  }

  void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->initCoords(meta, size);
  }

  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    for (int i = 0; i < coord.size(); i++) {
      this->currentCoordinates(coordId, i) = coord[i];
    }
  }

  void nextLinearRingEnd(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->stack[this->stack.size() - 1][ringId] = this->currentCoordinates;
  }

  void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    SEXP geometry;

    switch (meta.geometryType) {

    case WKGeometryType::Point:
    case WKGeometryType::LineString:
      geometry = this->currentCoordinates;
      break;
    case WKGeometryType::Polygon:
    case WKGeometryType::MultiPoint:
    case WKGeometryType::MultiLineString:
    case WKGeometryType::MultiPolygon:
    case WKGeometryType::GeometryCollection:
      geometry = this->stack[this->stack.size() - 1];
      this->stack.pop_back();
      break;

    default:
      throw WKParseException(
          Formatter() <<
            "Unrecognized geometry type: " <<
              meta.geometryType
      );
    }

    if (this->stack.size() > 0) {
      this->stack[this->stack.size() - 1][partId] = geometry;
    } else {
      this->feature = geometry;
    }
  }

  void nextFeatureEnd(size_t featureId) {
    this->exporter.setFeature(this->feature);
  }

  std::string metaAsClass(const WKGeometryMeta& meta) {
    return Formatter() << "wk_wkl_" << meta.wktType();
  }

  void initCoords(const WKGeometryMeta& meta, uint32_t size) {
    int coordSize = 2 + meta.hasZ + meta.hasM;
    currentCoordinates = Rcpp::NumericMatrix(size, coordSize);
    if (meta.hasZ && meta.hasM) {
      Rcpp::colnames(currentCoordinates) = Rcpp::CharacterVector::create("x", "y", "z", "m");
    } else if (meta.hasZ) {
      Rcpp::colnames(currentCoordinates) = Rcpp::CharacterVector::create("x", "y", "z");
    } else if (meta.hasM) {
      Rcpp::colnames(currentCoordinates) = Rcpp::CharacterVector::create("x", "y", "m");
    } else {
      Rcpp::colnames(currentCoordinates) = Rcpp::CharacterVector::create("x", "y");
    }
  }

  bool isNestingMulti() {
    if (stack.size() > 0) {
        Rcpp::List nestingGeometry = this->stack[this->stack.size() - 1];
        if (nestingGeometry.hasAttribute("class")) {
        std::string nestingClass = Rcpp::as<std::string>(this->stack[this->stack.size() - 1].attr("class"));
        return nestingClass == "wk_wkl_MULTIPOINT" ||
          nestingClass == "wk_wkl_MULTILINESTRING" ||
          nestingClass == "wk_wkl_MULTIPOLYGON";
      } else {
        return false;
      }
    } else {
      return false;
    }
  }

  bool isNestingCollection() {
    if (stack.size() > 0) {
      Rcpp::List nestingGeometry = this->stack[this->stack.size() - 1];
      if (nestingGeometry.hasAttribute("class")) {
        std::string nestingClass = Rcpp::as<std::string>(nestingGeometry.attr("class"));
        return nestingClass == "wk_wkl_GEOMETRYCOLLECTION";
      } else {
        return false;
      }
    } else {
      return false;
    }
  }
};

#endif
