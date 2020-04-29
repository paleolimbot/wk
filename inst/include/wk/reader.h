
#ifndef WK_READER_H
#define WK_READER_H

#include "wk/geometry-meta.h"

class WKReader  {
public:
  const static uint32_t SIZE_UNKNOWN = UINT32_MAX;
  const static uint32_t PART_ID_INVALID = UINT32_MAX;
  const static uint32_t RING_ID_INVALID = UINT32_MAX;
  const static uint32_t COORD_ID_INVALID = UINT32_MAX;

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



};


#endif
