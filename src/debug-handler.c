
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

typedef struct {
  int level;
  wk_handler_t* next;
} debug_handler_t;

// this is not a pretty solution to the vector_meta*/meta* issue
void wk_handler_debug_print_vector_meta(const wk_vector_meta_t* meta) {
  switch (meta->geometry_type) {
  case WK_POINT:
    Rprintf("POINT");
    break;
  case WK_LINESTRING:
    Rprintf("LINESTRING");
    break;
  case WK_POLYGON:
    Rprintf("POLYGON");
    break;
  case WK_MULTIPOINT:
    Rprintf("MULTIPOINT");
    break;
  case WK_MULTILINESTRING:
    Rprintf("MULTILINESTRING");
    break;
  case WK_MULTIPOLYGON:
    Rprintf("MULTIPOLYGON");
    break;
  case WK_GEOMETRYCOLLECTION:
    Rprintf("GEOMETRYCOLLECTION");
    break;
  default:
    Rprintf("<Unknown type / %d>", meta->geometry_type);
    break;
  }

  if ((meta->flags & WK_FLAG_HAS_Z) || (meta->flags & WK_FLAG_HAS_M) ||
      (meta->flags & WK_FLAG_HAS_BOUNDS)) {
    Rprintf(" ");
  }
  if (meta->flags & WK_FLAG_HAS_Z) Rprintf("Z");
  if (meta->flags & WK_FLAG_HAS_M) Rprintf("M");
  if (meta->flags & WK_FLAG_HAS_BOUNDS) Rprintf("B");

  if (meta->size != WK_SIZE_UNKNOWN) {
    if (meta->size == 0) {
      Rprintf("[EMPTY]");
    } else {
      Rprintf("[%d]", meta->size);
    }
  }

  Rprintf(" <%p>", (void*) meta);
}

void wk_handler_debug_print_meta(const wk_meta_t* meta) {
  switch (meta->geometry_type) {
  case WK_POINT:
    Rprintf("POINT");
    break;
  case WK_LINESTRING:
    Rprintf("LINESTRING");
    break;
  case WK_POLYGON:
    Rprintf("POLYGON");
    break;
  case WK_MULTIPOINT:
    Rprintf("MULTIPOINT");
    break;
  case WK_MULTILINESTRING:
    Rprintf("MULTILINESTRING");
    break;
  case WK_MULTIPOLYGON:
    Rprintf("MULTIPOLYGON");
    break;
  case WK_GEOMETRYCOLLECTION:
    Rprintf("GEOMETRYCOLLECTION");
    break;
  default:
    Rprintf("<Unknown type / %d>", meta->geometry_type);
    break;
  }

  if ((meta->flags & WK_FLAG_HAS_Z) || (meta->flags & WK_FLAG_HAS_M) ||
      (meta->srid != WK_SRID_NONE) || (meta->flags & WK_FLAG_HAS_BOUNDS)) {
    Rprintf(" ");
  }
  if (meta->flags & WK_FLAG_HAS_Z) Rprintf("Z");
  if (meta->flags & WK_FLAG_HAS_M) Rprintf("M");
  if (meta->srid != WK_SRID_NONE) Rprintf("S");
  if (meta->flags & WK_FLAG_HAS_BOUNDS) Rprintf("B");

  if (meta->size != WK_SIZE_UNKNOWN) {
    if (meta->size == 0) {
      Rprintf("[EMPTY]");
    } else {
      Rprintf("[%d]", meta->size);
    }
  }

  Rprintf(" <%p>", (void*) meta);
}

void wk_handler_debug_print_indent(debug_handler_t* debug_handler) {
  for (int i = 0; i < debug_handler->level; i++) {
    Rprintf("  ");
  }
}

void wk_handler_debug_reset(debug_handler_t* debug_handler, int value) {
  debug_handler->level = value;
}

void wk_handler_debug_indent(debug_handler_t* debug_handler) {
  debug_handler->level++;
}

void wk_handler_debug_dedent(debug_handler_t* debug_handler) {
  debug_handler->level--;
}

void wk_handler_debug_print_result(int result) {
  switch (result) {
  case WK_CONTINUE:
    Rprintf(" => WK_CONTINUE\n");
    break;
  case WK_ABORT_FEATURE:
    Rprintf(" => WK_ABORT_FEATURE\n");
    break;
  case WK_ABORT:
    Rprintf(" => WK_ABORT\n");
    break;
  default:
    Rprintf(" => [uknown %d]\n", result);
    break;
  }
}

void wk_handler_debug_initialize(int* dirty, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;
  wk_handler_debug_reset(debug_handler, 0);
  Rprintf("initialize (dirty = %d ", *dirty);
  debug_handler->next->initialize(dirty, debug_handler->next->handler_data);
  Rprintf(" -> %d)\n", *dirty);
}

int wk_handler_debug_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_print_indent(debug_handler);
  Rprintf("vector_start: ");
  wk_handler_debug_print_vector_meta(meta);
  wk_handler_debug_indent(debug_handler);
  int result = debug_handler->next->vector_start(meta, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);
  return result;
}

SEXP wk_handler_debug_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_dedent(debug_handler);
  wk_handler_debug_print_indent(debug_handler);
  Rprintf("vector_end: <%p>\n", meta);
  return debug_handler->next->vector_end(meta, debug_handler->next->handler_data);;
}

int wk_handler_debug_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_print_indent(debug_handler);
  Rprintf("feature_start (%d): <%p> ", feat_id + 1, meta);
  int result = debug_handler->next->feature_start(meta, feat_id, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);

  wk_handler_debug_indent(debug_handler);
  return result;
}

int wk_handler_debug_feature_null(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_print_indent(debug_handler);
  Rprintf("null_feature (%d) <%p> ", feat_id + 1, meta);
  int result = debug_handler->next->null_feature(meta, feat_id, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);
  return result;
}

int wk_handler_debug_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_dedent(debug_handler);
  wk_handler_debug_print_indent(debug_handler);
  Rprintf("feature_end (%d): <%p> ", feat_id + 1, meta);
  int result = debug_handler->next->feature_end(meta, feat_id, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);
  return result;
}

int wk_handler_debug_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_print_indent(debug_handler);
  if (part_id == WK_PART_ID_NONE) {
    Rprintf("geometry_start (<none>): ", part_id + 1);
  } else {
    Rprintf("geometry_start (%d): ", part_id + 1);
  }

  wk_handler_debug_print_meta(meta);

  int result = debug_handler->next->geometry_start(meta, part_id, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);

  wk_handler_debug_indent(handler_data);
  return result;
}

int wk_handler_debug_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_dedent(debug_handler);
  wk_handler_debug_print_indent(debug_handler);
  if (part_id == WK_PART_ID_NONE) {
    Rprintf("geometry_end (<none>) ", part_id + 1);
  } else {
    Rprintf("geometry_end (%d) ", part_id + 1);
  }

  int result = debug_handler->next->geometry_end(meta, part_id, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);
  return result;
}

int wk_handler_debug_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_print_indent(debug_handler);
  if (size != WK_SIZE_UNKNOWN) {
    Rprintf("ring_start[%d] (%d): <%p> ", size, ring_id + 1, meta);
  } else {
    Rprintf("ring_start (%d): <%p> ", ring_id + 1, meta);
  }
  wk_handler_debug_indent(debug_handler);

  int result = debug_handler->next->ring_start(meta, size, ring_id, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);
  return result;
}

int wk_handler_debug_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_dedent(debug_handler);
  wk_handler_debug_print_indent(debug_handler);
  if (size != WK_SIZE_UNKNOWN) {
    Rprintf("ring_end[%d] (%d): <%p> ", size, ring_id + 1, meta);
  } else {
    Rprintf("ring_end (%d): <%p> ", ring_id + 1, meta);
  }

  int result = debug_handler->next->ring_end(meta, size, ring_id, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);
  return result;
}

int wk_handler_debug_coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t coord_id, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_print_indent(debug_handler);
  Rprintf("coord (%d): <%p> (%f %f", coord_id + 1, meta, coord.v[0], coord.v[1]);
  if (meta->flags & WK_FLAG_HAS_Z || meta->flags & WK_FLAG_HAS_M) Rprintf(" %f", coord.v[2]);
  if (meta->flags & WK_FLAG_HAS_Z && meta->flags & WK_FLAG_HAS_M) Rprintf(" %f", coord.v[3]);
  Rprintf(") ");

  int result = debug_handler->next->coord(meta, coord, coord_id, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);
  return result;
}

int wk_handler_debug_error(R_xlen_t feat_id, int code, const char* message, void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;

  wk_handler_debug_print_indent(debug_handler);
  Rprintf("error [i=%d](%d): %s", feat_id, code, message);
  int result = debug_handler->next->error(feat_id, code, message, debug_handler->next->handler_data);
  wk_handler_debug_print_result(result);

  if (result == WK_ABORT_FEATURE) {
    wk_handler_debug_reset(debug_handler, 1);
  } else if (result == WK_ABORT) {
    wk_handler_debug_reset(debug_handler, 0);
  }

  return result;
}

void wk_handler_debug_deinitialize(void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;
  Rprintf("deinitialize");
  debug_handler->next->deinitialize(debug_handler->next->handler_data);
  Rprintf("\n");
}

void wk_handler_debug_finalize(void* handler_data) {
  debug_handler_t* debug_handler = (debug_handler_t*) handler_data;
  if (debug_handler != NULL) {
    if (debug_handler->next != NULL) {
      debug_handler->next->finalizer(debug_handler->next->handler_data);
    }
    free(debug_handler);
  }
}

SEXP wk_c_handler_debug_new(SEXP handler_xptr) {
  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_handler_debug_initialize;
  handler->vector_start = &wk_handler_debug_vector_start;
  handler->vector_end = &wk_handler_debug_vector_end;

  handler->feature_start = &wk_handler_debug_feature_start;
  handler->null_feature = &wk_handler_debug_feature_null;
  handler->feature_end = &wk_handler_debug_feature_end;

  handler->geometry_start = &wk_handler_debug_geometry_start;
  handler->geometry_end = &wk_handler_debug_geometry_end;

  handler->ring_start = &wk_handler_debug_ring_start;
  handler->ring_end = &wk_handler_debug_ring_end;

  handler->coord = &wk_handler_debug_coord;

  handler->error = &wk_handler_debug_error;

  handler->deinitialize = &wk_handler_debug_deinitialize;
  handler->finalizer = &wk_handler_debug_finalize;

  debug_handler_t* debug_handler = (debug_handler_t*) malloc(sizeof(debug_handler_t));
  debug_handler->level = 0;

  debug_handler->next = R_ExternalPtrAddr(handler_xptr);
  if (debug_handler->next->api_version != 1) {
    Rf_error("Can't run a wk_handler with api_version '%d'", debug_handler->next->api_version);
  }

  handler->handler_data = debug_handler;

  return wk_handler_create_xptr(handler, handler_xptr, R_NilValue);
}
