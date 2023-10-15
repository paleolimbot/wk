#include <vector>
#include "internal/wk-v1-handler.hpp"

#define HANDLE_OR_RETURN(expr) \
  result = expr;               \
  if (result != WK_CONTINUE) return result

enum class Direction { Clockwise = -1, CounterClockwise = 1 };

static int meta_coord_dim(uint32_t flags) {
  bool has_z = (flags & WK_FLAG_HAS_Z) != 0;
  bool has_m = (flags & WK_FLAG_HAS_M) != 0;
  return 2 + has_z + has_m;
}

class OrientFilter : public WKVoidHandler {
 public:
  OrientFilter(wk_handler_t* next, Direction direction)
      : next(next), direction(direction) {}

  void initialize(int* dirty) {
    WKVoidHandler::initialize(dirty);
    next->initialize(&next->dirty, next->handler_data);
  }

  int vector_start(const wk_vector_meta_t* meta) {
    coords.reserve(256);

    return next->vector_start(meta, next->handler_data);
  }

  int feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id) {
    is_polygon_ring = false;
    return next->feature_start(meta, feat_id, next->handler_data);
  }

  int null_feature() { return next->null_feature(next->handler_data); }

  int geometry_start(const wk_meta_t* meta, uint32_t part_id) {
    n_dim = meta_coord_dim(meta->flags);

    return next->geometry_start(meta, part_id, next->handler_data);
  }

  int ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    is_polygon_ring = true;
    coords.clear();

    return next->ring_start(meta, size, ring_id, next->handler_data);
  }

  int coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id) {
    if (!is_polygon_ring) {
      return next->coord(meta, coord, coord_id, next->handler_data);
    }

    // defer handler coord until ring_end
    coords.insert(coords.end(), coord, coord + n_dim);
    return WK_CONTINUE;
  }

  int ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    is_polygon_ring = false;
    int result;
    size_t n_coords = coords.size() / n_dim;

    if (need_reorder(ring_id == 0)) {
      // reverse coords
      for (uint32_t i = 0, j = n_coords - 1; i < n_coords; i++, j--) {
        HANDLE_OR_RETURN(next->coord(meta, &coords[j * n_dim], i, next->handler_data));
      }
    } else {
      // original order
      for (uint32_t i = 0; i < n_coords; i++) {
        HANDLE_OR_RETURN(next->coord(meta, &coords[i * n_dim], i, next->handler_data));
      }
    }

    return next->ring_end(meta, size, ring_id, next->handler_data);
  }

  int geometry_end(const wk_meta_t* meta, uint32_t part_id) {
    return next->geometry_end(meta, part_id, next->handler_data);
  }

  int feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id) {
    return next->feature_end(meta, feat_id, next->handler_data);
  }

  SEXP vector_end(const wk_vector_meta_t* meta) {
    return next->vector_end(meta, next->handler_data);
  }

  void deinitialize() { return next->deinitialize(next->handler_data); }

 private:
  wk_handler_t* next;
  const Direction direction;
  bool is_polygon_ring;
  std::vector<double> coords;
  uint32_t n_dim;

  double signed_area() const {
    if (coords.size() < n_dim * 3) {
      return 0.0;
    }

    double area = 0.0;
    double x0 = coords[0];

    // shoelace formula
    // https://en.wikipedia.org/wiki/Shoelace_formula
    // x0 fixed at origin reduces overflow, removes wrap-around index logic
    for (uint32_t i = 1 * n_dim; i < coords.size() - n_dim; i += n_dim) {
      double x = coords[i] - x0;
      double ym = coords[i - n_dim + 1];
      double yp = coords[i + n_dim + 1];

      area += x * (yp - ym);
    }

    return area / 2.0;
  }

  bool need_reorder(bool is_outer_ring) const {
    double area = signed_area();
    bool ring_is_ccw = ((area > 0) == is_outer_ring);
    bool orient_ccw = direction == Direction::CounterClockwise;

    return area != 0 && ring_is_ccw != orient_ccw;
  }
};

extern "C" SEXP wk_c_orient_filter_new(SEXP handler_xptr, SEXP direction) {
  Direction direction_enum = static_cast<Direction>(INTEGER(direction)[0]);
  wk_handler_t* next = static_cast<wk_handler_t*>(R_ExternalPtrAddr(handler_xptr));

  return WKHandlerFactory<OrientFilter>::create_xptr(
      new OrientFilter(next, direction_enum), handler_xptr);
}
