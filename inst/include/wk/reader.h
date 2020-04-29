
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

protected:

  WKGeometryHandler& handler;
};


#endif
