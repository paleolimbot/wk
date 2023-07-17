#include "internal/wk-v1-handler.hpp"
#include <vector>

enum class Direction {
  Clockwise = -1,
  CounterClockwise = 1
};

class OrientFilter: public WKVoidHandler {
public:
  OrientFilter(wk_handler_t* next, Direction direction): next(next), direction(direction) {}

  virtual void initialize(int* dirty) {
    next->initialize(&next->dirty, next->handler_data);
  }

  virtual int vector_start(const wk_vector_meta_t* meta) {
    return next->vector_start(meta, next->handler_data);
  }

  virtual int feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id) {
    return next->feature_start(meta, feat_id, next->handler_data);
  }

  virtual int null_feature() {
    return next->null_feature(next->handler_data);
  }

  virtual int geometry_start(const wk_meta_t* meta, uint32_t part_id) {
    return next->geometry_start(meta, part_id, next->handler_data);
  }

  virtual int ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    return next->ring_start(meta, size, ring_id, next->handler_data);
  }

  virtual int coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id) {
    return next->coord(meta, coord, coord_id, next->handler_data);
  }

  virtual int ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id) {
    return next->ring_end(meta, size, ring_id, next->handler_data);
  }

  virtual int geometry_end(const wk_meta_t* meta, uint32_t part_id) {
    return next->geometry_end(meta, part_id, next->handler_data);
  }

  virtual int feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id) {
    return next->feature_end(meta, feat_id, next->handler_data);
  }

  virtual SEXP vector_end(const wk_vector_meta_t* meta) {
    return next->vector_end(meta, next->handler_data);
  }

  virtual void deinitialize() {
    return next->deinitialize(next->handler_data);
  }

private:
  wk_handler_t* next;
  const Direction direction;
};

extern "C" SEXP wk_c_orient_filter_new(SEXP handler_xptr, SEXP direction) {
  Direction direction_enum = static_cast<Direction>(INTEGER(direction)[0]);
  wk_handler_t* next = static_cast<wk_handler_t*>(R_ExternalPtrAddr(handler_xptr));

  return WKHandlerFactory<OrientFilter>::create_xptr(new OrientFilter(next, direction_enum));
}
