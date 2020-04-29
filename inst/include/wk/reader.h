
#ifndef WK_READER_H
#define WK_READER_H

#include "wk/geometry-meta.h"
#include "wk/geometry-handler.h"

class WKReader  {
public:
  const static uint32_t PART_ID_NONE = UINT32_MAX;
  const static uint32_t RING_ID_NONE = UINT32_MAX;
  const static uint32_t COORD_ID_NONE = UINT32_MAX;

  WKReader(WKGeometryHandler& handler): handler(handler) {}

  // stack accessors (may need more, these are sufficient for WKT translator)
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
  std::vector<WKGeometryMeta> stack;
  WKGeometryHandler& handler;
};


#endif
