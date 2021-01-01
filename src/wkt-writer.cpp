
#include "cpp11.hpp"
#include "wk-v1.hpp"
#include <iostream>
#include <sstream>
#include <vector>

class WKTWriterHandler: public WKVoidHandler {
public:
  cpp11::writable::strings result;
  std::stringstream out;
  wk_meta_t parentMeta;
  std::vector<const wk_meta_t*> stack;

  WKTWriterHandler(int precision = 16, bool trim = true) {
    this->out.imbue(std::locale::classic());
    this->out.precision(precision);
    if (trim) {
      this->out.unsetf(out.fixed);
    } else {
      this->out.setf(out.fixed);
    }
  }

  bool isNestingCollection() {
    return this->stack.size() > 0 && 
      (this->stack[this->stack.size() - 1]->geometryType == WK_GEOMETRYCOLLECTION);
  }

  char vectorStart(const wk_meta_t* meta) {
    result = cpp11::writable::strings(meta->size);
    return WK_CONTINUE;
  }

  char featureStart(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    out.str("");
    this->stack.clear();
    return WK_CONTINUE;
  }

  char nullFeature(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    result[feat_id] = NA_STRING;
    return WK_ABORT_FEATURE;
  }

  char geometryStart(const wk_meta_t* meta, uint32_t nParts, uint32_t partId) {
    if ((partId != 0) && (this->stack.size() > 0)) {
      out << ", ";
    }

    if ((meta->srid != WK_SRID_NONE) && (this->stack.size() == 0)) {
      out << "SRID=" << meta->srid << ";";
    }

    if ((this->stack.size() == 0) || this->isNestingCollection()) {
        switch (meta->geometryType) {
        case WK_POINT:
            out << "POINT ";
            break;
        case WK_LINESTRING:
            out << "LINESTRING ";
            break;
        case WK_POLYGON:
            out << "POLYGON ";
            break;
        case WK_MULTIPOINT:
            out << "MULTIPOINT ";
            break;
        case WK_MULTILINESTRING:
            out << "MULTILINESTRING ";
            break;
        case WK_MULTIPOLYGON:
            out << "MULTIPOLYGON ";
            break;
        case WK_GEOMETRYCOLLECTION:
            out << "GEOMETRYCOLLECTION ";
            break;
        
        default:
            std::stringstream err;
            err << "Can't write geometry type '" << meta->geometryType << "' as WKT";
            throw WKHandlerException(err.str().c_str());
        }

        if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
            out << "ZM ";
        } else if (meta->flags & WK_FLAG_HAS_Z) {
            out << "Z ";
        } else if (meta->flags & WK_FLAG_HAS_M) {
            out << "M ";
        }
    }

    if (meta->size == 0) {
      out << "EMPTY";
    } else {
      out << "(";
    }

    this->stack.push_back(meta);
    return WK_CONTINUE;
  }

  char ringStart(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId) {
    out << "(";
    return WK_CONTINUE;
  }

  char coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t nCoords, uint32_t coordId) {
    if (coordId > 0) {
      out << ", ";
    }

    out << coord.v[0] << " " << coord.v[1];
    if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        out << " " << coord.v[2] << " " << coord.v[3];
    } else if ((meta->flags & WK_FLAG_HAS_Z) || (meta->flags & WK_FLAG_HAS_M)) {
        out << " " << coord.v[2];
    }

    return WK_CONTINUE;
  }

  char ringEnd(const wk_meta_t* meta, uint32_t size, uint32_t nRings, uint32_t ringId) {
    out << ")";
    return WK_CONTINUE;
  }

  char geometryEnd(const wk_meta_t* meta, uint32_t nParts, uint32_t partId) {
    this->stack.pop_back();

    if (meta->size != 0) {
      out << ")";
    }

    return WK_CONTINUE;
  }

  char featureEnd(const wk_meta_t* meta, R_xlen_t n_features, R_xlen_t feat_id) {
    result[feat_id] = this->out.str();
    return WK_CONTINUE;
  }

  SEXP vectorEnd(const wk_meta_t* meta) {
    return this->result;
  }

  void nextFeatureStart(size_t feat_id) {
    this->stack.clear();
  }
};

[[cpp11::register]]
cpp11::sexp wk_cpp_wkt_writer(int precision = 16, bool trim = true) {
  return WKHandlerFactory<WKTWriterHandler>::create_xptr(new WKTWriterHandler(precision, trim));
}
