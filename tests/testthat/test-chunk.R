
test_that("chunk map feature works", {
  expect_null(wk_chunk_map_feature(xy(1:5, 1:5), identity))
  expect_null(wk_chunk_map_feature(xy(), identity))

  expect_identical(
    wk_chunk_map_feature(
      wk_linestring(xy(1:10, 1:10), c(1, 1, 2, 2, 2, 3, 3, 4, 4, 4)),
      function(features) {
        coords <- wk_coords(features)
        vapply(split(coords, coords$feature_id), nrow, integer(1))
      },
      output_template = integer()
    ),
    c(2L, 3L, 2L, 3L)
  )

  # check with list(handleable) vs just handleable
  expect_identical(
    wk_chunk_map_feature(xy(1:5, 1:5), identity, output_template = xy()),
    wk_chunk_map_feature(list(xy(1:5, 1:5)), identity, output_template = xy())
  )

  # invalid inputs
  expect_error(wk_chunk_map_feature(NULL), "must be a list")
  expect_error(wk_chunk_map_feature(list(NULL)), "must be objects with a")
  expect_error(
    wk_chunk_map_feature(list(xy(1:2, 1:2), xy(1:3, 1:3)), identity),
    "must be recycleable to a common length"
  )

  expect_error(wk_chunk_map_feature(xy(1:5, 1:5), "not a function"), "is not TRUE")
  expect_error(
    wk_chunk_map_feature(xy(1:5, 1:5), identity, vector_args = "bad arg"),
    "is not TRUE"
  )
  expect_error(
    wk_chunk_map_feature(xy(1:5, 1:5), identity, input_handler_factory = "bad arg"),
    "is not TRUE"
  )
  expect_error(
    wk_chunk_map_feature(xy(1:5, 1:5), identity, strategy = "bad arg"),
    "is not TRUE"
  )
})

test_that("chunk map feature works with vectorized and non-vectorized args", {
  skip_if_not(packageVersion("base") >= "3.6")

  wk_chunk_map_feature(
    xy(1, 1),
    fun = function(x, l, y) {
      expect_identical(l, letters[1:5])
      expect_identical(y, "zippity")
    },
    vector_args = data.frame(l = letters[1:5], stringsAsFactors = FALSE),
    args = list(y = "zippity"),
    strategy = wk_chunk_strategy_single()
  )

  wk_chunk_map_feature(
    xy(1:3, 1:3),
    fun = function(x, l, y) {
      expect_identical(l, letters[1])
      expect_identical(y, "zippity")
    },
    vector_args = data.frame(l = letters[1], stringsAsFactors = FALSE),
    args = list(y = "zippity"),
    strategy = wk_chunk_strategy_single()
  )

})

test_that("chunk map feature doesn't expand handleables more than necessary", {
  wk_chunk_map_feature(
    xy(1, 1),
    fun = function(x) {
      expect_identical(x, xy(1, 1))
    },
    strategy = wk_chunk_strategy_single()
  )
})

test_that("single chunk strategy works", {
  feat <- c(as_wkt(xy(1:4, 1:4)), wkt("LINESTRING (1 1, 2 2)"))
  expect_identical(
    wk_chunk_strategy_single()(list(feat), 5),
    data.frame(from = 1, to = 5)
  )
})

test_that("chunk by feature strategy works", {
  feat <- c(as_wkt(xy(1:4, 1:4)), wkt("LINESTRING (1 1, 2 2)"))
  expect_identical(
    wk_chunk_strategy_feature(chunk_size = 2)(list(feat), 5),
    data.frame(from = c(1, 3, 5), to = c(2, 4, 5))
  )
})

test_that("chunk by coordinates strategy works", {
  n_coord <- c(1, 5, 1, 5, 1)
  xs <- unlist(lapply(n_coord, seq_len))
  ys <- unlist(lapply(n_coord, seq_len))
  id <- vctrs::vec_rep_each(seq_along(n_coord), n_coord)
  feat <- wk_linestring(xy(xs, ys), feature_id = id)

  expect_identical(
    wk_chunk_strategy_coordinates(chunk_size = 6)(list(feat), length(n_coord)),
    data.frame(from = c(1L, 3L, 5L), to = c(2L, 4L, 5L))
  )

  # for points there's a shortcut for calculating the chunks
  expect_identical(
    wk_chunk_strategy_coordinates(chunk_size = 2)(list(xy(1:6, 1:6)), 6),
    data.frame(from = c(1, 3, 5), to = c(2, 4, 6))
  )
})

test_that("chunk_info() works", {
  expect_identical(
    chunk_info(5, chunk_size = 2),
    list(n_chunks = 3, chunk_size = 2)
  )
  expect_identical(
    chunk_info(5, chunk_size = 5),
    list(n_chunks = 1, chunk_size = 5)
  )
  expect_identical(
    chunk_info(0, chunk_size = 5),
    list(n_chunks = 0, chunk_size = 5)
  )

  expect_identical(
    chunk_info(5, n_chunks = 3),
    list(n_chunks = 3, chunk_size = 2)
  )
  expect_identical(
    chunk_info(5, n_chunks = 1),
    list(n_chunks = 1, chunk_size = 5)
  )
  expect_identical(
    chunk_info(0, n_chunks = 5),
    list(n_chunks = 0L, chunk_size = 1L)
  )
  expect_error(chunk_info(1), "exactly one")
  expect_error(chunk_info(1, chunk_size = 1, n_chunks = 1), "exactly one")
})
