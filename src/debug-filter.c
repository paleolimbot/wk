
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "wk-v1.h"

typedef struct {
  int level;
  wk_handler_t* next;
} debug_filter_t;

// this is not a pretty solution to the vector_meta*/meta* issue
void wk_debug_filter_print_vector_meta(const wk_vector_meta_t* meta) {
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

void wk_debug_filter_print_meta(const wk_meta_t* meta) {
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
      (meta->srid != WK_SRID_NONE) || (meta->flags & WK_FLAG_HAS_BOUNDS) ||
      (meta->precision != WK_PRECISION_NONE)) {
    Rprintf(" ");
  }
  if (meta->flags & WK_FLAG_HAS_Z) Rprintf("Z");
  if (meta->flags & WK_FLAG_HAS_M) Rprintf("M");
  if (meta->srid != WK_SRID_NONE) Rprintf("S");
  if (meta->flags & WK_FLAG_HAS_BOUNDS) Rprintf("B");
  if (meta->precision != WK_PRECISION_NONE) Rprintf("P");

  if (meta->size != WK_SIZE_UNKNOWN) {
    if (meta->size == 0) {
      Rprintf("[EMPTY]");
    } else {
      Rprintf("[%d]", meta->size);
    }
  }

  Rprintf(" <%p>", (void*) meta);
}

void wk_debug_filter_print_indent(debug_filter_t* debug_filter) {
  for (int i = 0; i < debug_filter->level; i++) {
    Rprintf("  ");
  }
}

void wk_debug_filter_reset(debug_filter_t* debug_filter, int value) {
  debug_filter->level = value;
}

void wk_debug_filter_indent(debug_filter_t* debug_filter) {
  debug_filter->level++;
}

void wk_debug_filter_dedent(debug_filter_t* debug_filter) {
  debug_filter->level--;
}

void wk_debug_filter_print_result(int result) {
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

void wk_debug_filter_initialize(int* dirty, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;
  *dirty = 1;
  wk_debug_filter_reset(debug_filter, 0);
  Rprintf("initialize (dirty = %d ", debug_filter->next->dirty);
  debug_filter->next->initialize(&debug_filter->next->dirty, debug_filter->next->handler_data);
  Rprintf(" -> %d)\n", *dirty);
}

int wk_debug_filter_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_print_indent(debug_filter);
  Rprintf("vector_start: ");
  wk_debug_filter_print_vector_meta(meta);
  wk_debug_filter_indent(debug_filter);
  int result = debug_filter->next->vector_start(meta, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);
  return result;
}

SEXP wk_debug_filter_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_dedent(debug_filter);
  // indenting here is more confusing than helpful
  Rprintf("vector_end: <%p>\n", meta);
  return debug_filter->next->vector_end(meta, debug_filter->next->handler_data);;
}

int wk_debug_filter_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_print_indent(debug_filter);
  Rprintf("feature_start (%d): <%p> ", feat_id + 1, meta);
  int result = debug_filter->next->feature_start(meta, feat_id, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);

  wk_debug_filter_indent(debug_filter);
  return result;
}

int wk_debug_filter_feature_null(void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_print_indent(debug_filter);
  Rprintf("null_feature ");
  int result = debug_filter->next->null_feature(debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);
  return result;
}

int wk_debug_filter_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_dedent(debug_filter);
  wk_debug_filter_print_indent(debug_filter);
  Rprintf("feature_end (%d): <%p> ", feat_id + 1, meta);
  int result = debug_filter->next->feature_end(meta, feat_id, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);
  return result;
}

int wk_debug_filter_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_print_indent(debug_filter);
  if (part_id == WK_PART_ID_NONE) {
    Rprintf("geometry_start (<none>): ", part_id + 1);
  } else {
    Rprintf("geometry_start (%d): ", part_id + 1);
  }

  wk_debug_filter_print_meta(meta);

  int result = debug_filter->next->geometry_start(meta, part_id, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);

  wk_debug_filter_indent(handler_data);
  return result;
}

int wk_debug_filter_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_dedent(debug_filter);
  wk_debug_filter_print_indent(debug_filter);
  if (part_id == WK_PART_ID_NONE) {
    Rprintf("geometry_end (<none>) ", part_id + 1);
  } else {
    Rprintf("geometry_end (%d) ", part_id + 1);
  }

  int result = debug_filter->next->geometry_end(meta, part_id, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);
  return result;
}

int wk_debug_filter_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_print_indent(debug_filter);
  if (size != WK_SIZE_UNKNOWN) {
    Rprintf("ring_start[%d] (%d): <%p> ", size, ring_id + 1, meta);
  } else {
    Rprintf("ring_start (%d): <%p> ", ring_id + 1, meta);
  }
  wk_debug_filter_indent(debug_filter);

  int result = debug_filter->next->ring_start(meta, size, ring_id, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);
  return result;
}

int wk_debug_filter_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_dedent(debug_filter);
  wk_debug_filter_print_indent(debug_filter);
  if (size != WK_SIZE_UNKNOWN) {
    Rprintf("ring_end[%d] (%d): <%p> ", size, ring_id + 1, meta);
  } else {
    Rprintf("ring_end (%d): <%p> ", ring_id + 1, meta);
  }

  int result = debug_filter->next->ring_end(meta, size, ring_id, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);
  return result;
}

int wk_debug_filter_coord(const wk_meta_t* meta, const wk_coord_t coord, uint32_t coord_id, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_print_indent(debug_filter);
  Rprintf("coord (%d): <%p> (%f %f", coord_id + 1, meta, coord.v[0], coord.v[1]);
  if (meta->flags & WK_FLAG_HAS_Z || meta->flags & WK_FLAG_HAS_M) Rprintf(" %f", coord.v[2]);
  if (meta->flags & WK_FLAG_HAS_Z && meta->flags & WK_FLAG_HAS_M) Rprintf(" %f", coord.v[3]);
  Rprintf(") ");

  int result = debug_filter->next->coord(meta, coord, coord_id, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);
  return result;
}

int wk_debug_filter_error(const char* message, void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;

  wk_debug_filter_print_indent(debug_filter);
  Rprintf("error: %s", message);
  int result = debug_filter->next->error(message, debug_filter->next->handler_data);
  wk_debug_filter_print_result(result);

  if (result == WK_ABORT_FEATURE) {
    wk_debug_filter_reset(debug_filter, 1);
  } else if (result == WK_ABORT) {
    wk_debug_filter_reset(debug_filter, 0);
  }

  return result;
}

void wk_debug_filter_deinitialize(void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;
  Rprintf("deinitialize");
  debug_filter->next->deinitialize(debug_filter->next->handler_data);
  Rprintf("\n");
}

void wk_debug_filter_finalize(void* handler_data) {
  debug_filter_t* debug_filter = (debug_filter_t*) handler_data;
  if (debug_filter != NULL) {
    // finalizer for debug_filter->next is run by the externalptr finalizer
    // and should not be called here
    free(debug_filter);
  }
}

SEXP wk_c_debug_filter_new(SEXP handler_xptr) {
  wk_handler_t* handler = wk_handler_create();

  handler->initialize = &wk_debug_filter_initialize;
  handler->vector_start = &wk_debug_filter_vector_start;
  handler->vector_end = &wk_debug_filter_vector_end;

  handler->feature_start = &wk_debug_filter_feature_start;
  handler->null_feature = &wk_debug_filter_feature_null;
  handler->feature_end = &wk_debug_filter_feature_end;

  handler->geometry_start = &wk_debug_filter_geometry_start;
  handler->geometry_end = &wk_debug_filter_geometry_end;

  handler->ring_start = &wk_debug_filter_ring_start;
  handler->ring_end = &wk_debug_filter_ring_end;

  handler->coord = &wk_debug_filter_coord;

  handler->error = &wk_debug_filter_error;

  handler->deinitialize = &wk_debug_filter_deinitialize;
  handler->finalizer = &wk_debug_filter_finalize;

  debug_filter_t* debug_filter = (debug_filter_t*) malloc(sizeof(debug_filter_t));
  if (debug_filter == NULL) {
    wk_handler_destroy(handler); // # nocov
    Rf_error("Failed to alloc handler data"); // # nocov
  }

  debug_filter->level = 0;

  debug_filter->next = R_ExternalPtrAddr(handler_xptr);
  if (debug_filter->next->api_version != 1) {
    Rf_error("Can't run a wk_handler with api_version '%d'", debug_filter->next->api_version); // # nocov
  }

  handler->handler_data = debug_filter;

  // include the external pointer as a tag for this external pointer
  // which guarnatees that it will not be garbage collected until
  // this object is garbage collected
  return wk_handler_create_xptr(handler, handler_xptr, R_NilValue);
}
