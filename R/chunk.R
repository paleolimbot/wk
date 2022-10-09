
#' Chunking strategies
#'
#' It is often impractical, inefficient, or impossible to perform
#' an operation on a vector of geometries with all the geometries loaded
#' into memory at the same time. These functions help generalize the
#' pattern of split-apply-combine to one or more handlers recycled along a
#' common length. These functions are designed for developers rather than users
#' and should be considered experimental.
#'
#' @param reduce For [wk_chunk_strategy_coordinates()] this refers to
#'   the function used with [Reduce()] to combine coordinate counts
#'   from more than one handleable.
#' @param n_chunks,chunk_size Exactly one of the number of
#'   chunks or the chunk size. For [wk_chunk_strategy_feature()]
#'   the chunk size refers to the number of features; for
#'   [wk_chunk_strategy_coordinates()] this refers to the number
#'   of coordinates as calculated from multiple handleables
#'   using `reduce`.
#'
#' @return A function that returns a `data.frame` with columns `from` and `to`
#'   when called with a `handleable` and the feature count.
#' @export
#'
#' @examples
#' feat <- c(as_wkt(xy(1:4, 1:4)), wkt("LINESTRING (1 1, 2 2)"))
#' wk_chunk_strategy_single()(list(feat), 5)
#' wk_chunk_strategy_feature(chunk_size = 2)(list(feat), 5)
#' wk_chunk_strategy_coordinates(chunk_size = 2)(list(feat), 5)
#'
wk_chunk_strategy_single <- function() {
  function(handleables, n_features) {
    new_data_frame(list(from = 1, to = n_features))
  }
}

#' @rdname wk_chunk_strategy_single
#' @export
wk_chunk_strategy_feature <- function(n_chunks = NULL, chunk_size = NULL) {
  force(n_chunks)
  force(chunk_size)

  function(handleables, n_features) {
    chunk_info <- chunk_info(n_features, n_chunks = n_chunks, chunk_size = chunk_size)

    from <- (chunk_info$chunk_size * (seq_len(chunk_info$n_chunks) - 1L)) + 1L
    to <- chunk_info$chunk_size * seq_len(chunk_info$n_chunks)
    to[chunk_info$n_chunks] <- n_features
    new_data_frame(list(from = from, to = to))
  }
}

#' @rdname wk_chunk_strategy_single
#' @export
wk_chunk_strategy_coordinates <- function(n_chunks = NULL, chunk_size = NULL, reduce = "*") {
  force(n_chunks)
  force(reduce)

  function(handleables, n_features) {
    coord_count <- lapply(handleables, function(handleable) {
      vm <- wk_vector_meta(handleable)
      if (identical(vm$geometry_type, 1L)) {
        1L
      } else {
        wk_count(handleable)$n_coord
      }
    })

    coord_count <- Reduce(reduce, coord_count)
    if (identical(coord_count, 1L)) {
      return(wk_chunk_strategy_feature(n_chunks, chunk_size)(handleables, n_features))
    }

    coord_count <- rep_len(coord_count, n_features)

    coord_count_total <- sum(coord_count)
    chunk_info <- chunk_info(coord_count_total, n_chunks, chunk_size)

    from <- rep(NA_integer_, chunk_info$n_chunks)
    to <- rep(NA_integer_, chunk_info$n_chunks)
    from[1] <- 1L

    coord_count_chunk <- (coord_count_total / chunk_info$n_chunks)
    coord_count_feat <- cumsum(coord_count)

    for (chunk_id in seq_len(chunk_info$n_chunks - 1L)) {
      next_coord_gt <- coord_count_feat >= coord_count_chunk
      if (!any(next_coord_gt)) {
        to[chunk_id] <- n_features
        break
      }

      i <- max(min(which(next_coord_gt)), from[chunk_id] + 1L)
      to[chunk_id] <- i
      from[chunk_id + 1L] <- i + 1L
      coord_count[1:i] <- 0L
      coord_count_feat <- cumsum(coord_count)
    }

    valid <- !is.na(from)
    from <- from[valid]
    to <- to[valid]

    if (is.na(to[length(to)])) {
      to[length(to)] <- n_features
    }

    new_data_frame(list(from = from, to = to))
  }
}

chunk_info <- function(n_features, n_chunks = NULL, chunk_size = NULL) {
  if (is.null(n_chunks) && is.null(chunk_size)) {
    stop("Must specify exactly one of `n_chunks` or `chunk_size`", call. = FALSE)
  } else if (is.null(n_chunks)) {
    n_chunks <- ((n_features - 1L) %/% chunk_size) + 1L
  } else if (is.null(chunk_size)) {
    if (n_features == 0) {
      n_chunks <- 0L
      chunk_size <- 1L
    } else {
      chunk_size <- ((n_features - 1L) %/% n_chunks) + 1L
    }
  } else {
    stop("Must specify exactly one of `n_chunks` or `chunk_size`", call. = FALSE)
  }

  list(n_chunks = n_chunks, chunk_size = chunk_size)
}
