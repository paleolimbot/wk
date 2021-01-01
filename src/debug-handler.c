
#include "wk-v1.h"
#include <Rinternals.h>

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

void wk_handler_debug_print_indent(void* handler_data) {
  int* intData = (int*) handler_data;
  for (int i = 0; i < intData[0]; i++) {
    Rprintf("  ");
  }
}

void wk_handler_debug_reset(void* handler_data) {
  int* intData = (int*) handler_data;
  intData[0] = 0;
}

void wk_handler_debug_indent(void* handler_data) {
  int* intData = (int*) handler_data;
  intData[0]++;
}

void wk_handler_debug_dedent(void* handler_data) {
  int* intData = (int*) handler_data;
  intData[0]--;
}

char wk_handler_debug_vector_start(const wk_meta_t* meta, void* handler_data) {
  wk_handler_debug_reset(handler_data);
  wk_handler_debug_print_indent(handler_data);
  Rprintf("vector_start: ");
  wk_handler_debug_print_meta(meta);
  Rprintf("\n");
  wk_handler_debug_indent(handler_data);
  return WK_CONTINUE;
}

SEXP wk_handler_debug_vector_end(const wk_meta_t* meta, void* handler_data) {
  wk_handler_debug_dedent(handler_data);
  wk_handler_debug_print_indent(handler_data);
  Rprintf("vector_end <%p>\n", meta);
  return R_NilValue;
}

char wk_handler_debug_feature_start(const wk_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  wk_handler_debug_print_indent(handler_data);
  Rprintf("feature_start (%d) <%p>\n", feat_id + 1, meta);
  wk_handler_debug_indent(handler_data);
  return WK_CONTINUE;
}

char wk_handler_debug_feature_null(const wk_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  wk_handler_debug_print_indent(handler_data);
  Rprintf("null_feature (%d) <%p>\n", feat_id + 1, meta);
  return WK_CONTINUE;
}

char wk_handler_debug_feature_end(const wk_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  wk_handler_debug_dedent(handler_data);
  wk_handler_debug_print_indent(handler_data);
  Rprintf("feature_end (%d) <%p>\n", feat_id + 1, meta);
  return WK_CONTINUE;
}

char wk_handler_debug_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  wk_handler_debug_print_indent(handler_data);
  Rprintf("geometry_start (%d): ", part_id + 1);

  wk_handler_debug_print_meta(meta);
  Rprintf("\n");
  wk_handler_debug_indent(handler_data);
  return WK_CONTINUE;
}

char wk_handler_debug_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  wk_handler_debug_dedent(handler_data);
  wk_handler_debug_print_indent(handler_data);
  Rprintf("geometry_end (%d) <%p> \n", part_id + 1, meta);

  return WK_CONTINUE;
}

char wk_handler_debug_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  wk_handler_debug_print_indent(handler_data);
  if (size != WK_SIZE_UNKNOWN) {
    Rprintf("ring_start[%d] (%d) <%p>\n", size, ring_id + 1, meta);
  } else {
    Rprintf("ring_start (%d) <%p>\n", ring_id + 1, meta);
  }
  wk_handler_debug_indent(handler_data);
  return WK_CONTINUE;
}

char wk_handler_debug_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  wk_handler_debug_dedent(handler_data);
  wk_handler_debug_print_indent(handler_data);
  if (size != WK_SIZE_UNKNOWN) {
    Rprintf("ring_end[%d] (%d) <%p>\n", size, ring_id + 1, meta);
  } else {
    Rprintf("ring_end (%d) <%p>\n", ring_id + 1, meta);
  }
  return WK_CONTINUE;
}

char wk_handler_debug_coord(const wk_meta_t* meta, wk_coord_t coord, uint32_t coord_id, void* handler_data) {
  wk_handler_debug_print_indent(handler_data);
  Rprintf("coord (%d) <%p> (%f %f", coord_id + 1, meta, coord.v[0], coord.v[1]);
  if (meta->flags & WK_FLAG_HAS_Z || meta->flags & WK_FLAG_HAS_M) Rprintf(" %f", coord.v[2]);
  if (meta->flags & WK_FLAG_HAS_Z && meta->flags & WK_FLAG_HAS_M) Rprintf(" %f", coord.v[3]);
  Rprintf(")\n");
  return WK_CONTINUE;
}

char wk_handler_debug_error(R_xlen_t feat_id, int code, const char* message, void* handler_data) {
  wk_handler_debug_print_indent(handler_data);
  Rprintf("error [i=%d](%d): %s\n", feat_id, code, message);
  return WK_ABORT;
}

void wk_handler_debug_vector_finally(void* handler_data) {
  Rprintf("vector_finally\n");
}

SEXP wk_c_handler_debug_new() {
  wk_handler_t* handler = wk_handler_create();

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

  handler->vector_finally = &wk_handler_debug_vector_finally;

  SEXP recursiveDepth = PROTECT(Rf_allocVector(INTSXP, 1));
  int* pRecursiveDepth = INTEGER(recursiveDepth);
  pRecursiveDepth[0] = 0;
  handler->handler_data = pRecursiveDepth;

  SEXP xptr = wk_handler_create_xptr(handler, recursiveDepth, R_NilValue);
  UNPROTECT(1);
  return xptr;
}
