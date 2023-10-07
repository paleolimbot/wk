
#include <iostream>
#include <sstream>
#include <vector>
#include "internal/wk-v1-handler.hpp"

class WKTWriterHandler : public WKVoidHandler {
 public:
  SEXP result;
  std::stringstream out;
  std::string current_item;
  std::vector<wk_meta_t> stack;
  R_xlen_t feat_id;

  WKTWriterHandler(int precision, bool trim) {
    this->result = R_NilValue;
    this->out.imbue(std::locale::classic());
    this->out.precision(precision);
    if (trim) {
      this->out.unsetf(out.fixed);
    } else {
      this->out.setf(out.fixed);
    }
  }

  void resultInit(R_xlen_t size) {
    SEXP new_result = PROTECT(Rf_allocVector(STRSXP, size));
    if (this->result != R_NilValue) {
      R_ReleaseObject(this->result);
    }
    this->result = new_result;
    R_PreserveObject(this->result);
    UNPROTECT(1);
  }

  void resultEnsureSize() {
    R_xlen_t current_size = Rf_xlength(this->result);
    if (this->feat_id >= current_size) {
      SEXP new_result = PROTECT(Rf_allocVector(STRSXP, current_size * 2 + 1));
      for (R_xlen_t i = 0; i < current_size; i++) {
        SET_STRING_ELT(new_result, i, STRING_ELT(this->result, i));
      }
      if (this->result != R_NilValue) {
        R_ReleaseObject(this->result);
      }
      this->result = new_result;
      R_PreserveObject(this->result);
      UNPROTECT(1);
    }
  }

  void resultFinalize() {
    R_xlen_t current_size = Rf_xlength(this->result);
    if (this->feat_id != current_size) {
      SEXP new_result = PROTECT(Rf_allocVector(STRSXP, this->feat_id));
      for (R_xlen_t i = 0; i < this->feat_id; i++) {
        SET_STRING_ELT(new_result, i, STRING_ELT(this->result, i));
      }
      if (this->result != R_NilValue) {
        R_ReleaseObject(this->result);
      }
      this->result = new_result;
      R_PreserveObject(this->result);
      UNPROTECT(1);
    }
  }

  void resultAppend(const std::string& item) {
    this->resultEnsureSize();
    SET_STRING_ELT(this->result, this->feat_id, Rf_mkCharLen(item.data(), item.size()));
    this->feat_id++;
  }

  void resultAppendNull() {
    this->resultEnsureSize();
    SET_STRING_ELT(this->result, this->feat_id, NA_STRING);
    this->feat_id++;
  }

  bool isNestingCollection() {
    return !this->stack.empty() &&
           (this->stack.back().geometry_type == WK_GEOMETRYCOLLECTION);
  }

  int vector_start(const wk_vector_meta_t* meta) {
    this->feat_id = 0;
    if (meta->size != WK_VECTOR_SIZE_UNKNOWN) {
      this->resultInit(meta->size);
    } else {
      this->resultInit(1024);
    }
    return WK_CONTINUE;
  }

  virtual int feature_start(const wk_vector_meta_t* meta, R_xlen_t feature_id) {
    out.str("");
    this->stack.clear();
    return WK_CONTINUE;
  }

  virtual int null_feature() {
    this->resultAppendNull();
    return WK_ABORT_FEATURE;
  }

  int geometry_start(const wk_meta_t* meta, uint32_t part_id) {
    if ((part_id != 0) && !this->stack.empty()) {
      out << ", ";
    }

    if ((meta->srid != WK_SRID_NONE) && this->stack.empty()) {
      out << "SRID=" << meta->srid << ";";
    }

    if (this->stack.empty() || this->isNestingCollection()) {
      switch (meta->geometry_type) {
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
          err << "Can't write geometry type '" << meta->geometry_type << "' as WKT";
          return this->error(err.str().c_str());
      }

      if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
        out << "ZM ";
      } else if ((meta->flags & WK_FLAG_HAS_Z)) {
        out << "Z ";
      } else if ((meta->flags & WK_FLAG_HAS_M)) {
        out << "M ";
      }
    }

    if (meta->size == 0) {
      out << "EMPTY";
    } else {
      out << "(";
    }

    this->stack.push_back(*meta);
    return WK_CONTINUE;
  }

  int ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    if (ring_id > 0) {
      out << ", ";
    }

    out << "(";
    return WK_CONTINUE;
  }

  virtual int coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id) {
    if (coord_id > 0) {
      out << ", ";
    }

    out << coord[0] << " " << coord[1];
    if ((meta->flags & WK_FLAG_HAS_Z) && (meta->flags & WK_FLAG_HAS_M)) {
      out << " " << coord[2] << " " << coord[3];
    } else if ((meta->flags & WK_FLAG_HAS_Z) || (meta->flags & WK_FLAG_HAS_M)) {
      out << " " << coord[2];
    }

    return WK_CONTINUE;
  }

  int ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    out << ")";
    return WK_CONTINUE;
  }

  int geometry_end(const wk_meta_t* meta, uint32_t part_id) {
    this->stack.pop_back();

    if (meta->size != 0) {
      out << ")";
    }

    return WK_CONTINUE;
  }

  int feature_end(const wk_vector_meta_t* meta, R_xlen_t feature_id) {
    current_item = this->out.str();
    this->resultAppend(current_item);
    return WK_CONTINUE;
  }

  virtual SEXP vector_end(const wk_vector_meta_t* meta) {
    if (this->result != R_NilValue) {
      this->resultFinalize();
      SEXP cls = PROTECT(Rf_allocVector(STRSXP, 2));
      SET_STRING_ELT(cls, 0, Rf_mkChar("wk_wkt"));
      SET_STRING_ELT(cls, 1, Rf_mkChar("wk_vctr"));
      Rf_setAttrib(this->result, R_ClassSymbol, cls);
      UNPROTECT(1);
    }

    return this->result;
  }

  void deinitialize() {
    if (this->result != R_NilValue) {
      R_ReleaseObject(this->result);
      this->result = R_NilValue;
    }
  }
};

class WKTFormatHandler : public WKTWriterHandler {
 public:
  WKTFormatHandler(int precision, bool trim, int max_coords)
      : WKTWriterHandler(precision, trim), current_coords(0), max_coords(max_coords) {}

  int feature_start(const wk_vector_meta_t* meta, R_xlen_t feature_id) {
    this->current_coords = 0;
    return WKTWriterHandler::feature_start(meta, feature_id);
  }

  int null_feature() {
    this->out << "<null feature>";
    return WK_CONTINUE;
  }

  int coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id) {
    WKTWriterHandler::coord(meta, coord, coord_id);
    if (++this->current_coords >= this->max_coords) {
      this->out << "...";
      this->current_item = this->out.str();
      this->resultAppend(this->current_item);
      return WK_ABORT_FEATURE;
    } else {
      return WK_CONTINUE;
    }
  }

  int error(const char* message) {
    this->out << "!!! " << message;
    this->current_item = this->out.str();
    this->resultAppend(this->current_item);
    return WK_ABORT_FEATURE;
  }

  SEXP vector_end(const wk_vector_meta_t* meta) {
    if (this->result != R_NilValue) {
      this->resultFinalize();
    }

    return this->result;
  }

 private:
  int current_coords;
  int max_coords;
};

extern "C" SEXP wk_c_wkt_writer(SEXP precision_sexp, SEXP trim_sexp) {
  int precision = INTEGER(precision_sexp)[0];
  int trim = LOGICAL(trim_sexp)[0];
  return WKHandlerFactory<WKTWriterHandler>::create_xptr(
      new WKTWriterHandler(precision, trim));
}

extern "C" SEXP wk_c_wkt_formatter(SEXP precision_sexp, SEXP trim_sexp,
                                   SEXP max_coords_sexp) {
  int precision = INTEGER(precision_sexp)[0];
  int trim = LOGICAL(trim_sexp)[0];
  int max_coords = INTEGER(max_coords_sexp)[0];
  return WKHandlerFactory<WKTFormatHandler>::create_xptr(
      new WKTFormatHandler(precision, trim, max_coords));
}
