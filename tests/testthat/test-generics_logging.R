# Tests for the change-logging helpers in R/generics_logging.R.
# startlog()/endlog() communicate through a snapshot registry matched by an id
# attribute, with a most-recent-snapshot fallback for verbs that strip
# attributes (e.g. dplyr::summarise), so real pipelines are exercised here.

test_that("startlog tags the data frame with a snapshot id", {
  df <- startlog(data.frame(a = 1:3, b = 1:3))
  expect_type(attr(df, "startlog_id"), "character")
  suppressMessages(endlog(df))  # consume the snapshot
})

test_that("endlog reports dropped rows through a filter pipeline", {
  df <- data.frame(a = 1:10)
  expect_message(
    df |> startlog() |> dplyr::filter(a > 3) |> endlog(),
    "3/10 (30%) rows have been dropped",
    fixed = TRUE
  )
})

test_that("endlog reports added rows", {
  df <- data.frame(a = 1:10)
  expect_message(
    df |> startlog() |> dplyr::bind_rows(df) |> endlog(),
    "10 rows have been added (10 -> 20, +100%)",
    fixed = TRUE
  )
})

test_that("endlog reports dropped and added columns", {
  df <- data.frame(a = 1:5, b = 1:5, c = 1:5, d = 1:5)
  expect_message(
    df |> startlog() |> dplyr::select(a, b) |> endlog(),
    "2/4 (50%) columns have been dropped",
    fixed = TRUE
  )
  expect_message(
    df |> startlog() |> dplyr::mutate(e = 1) |> endlog(),
    "1 column has been added (4 -> 5, +25%)",
    fixed = TRUE
  )
})

test_that("endlog strips the bookkeeping attribute and stores the summary", {
  df <- data.frame(a = 1:3)
  out <- suppressMessages(df |> startlog() |> endlog())
  expect_null(attr(out, "startlog_id"))
  expect_identical(attr(out, "endlog"),
                   c(rows = 0L, columns = 0L, cells = NA_integer_))
  attr(out, "endlog") <- NULL
  expect_identical(out, df)
})

test_that("the endlog attribute records signed row/column changes and cell counts", {
  df <- data.frame(a = 1:10, b = 1:10, c = 1:10)
  out <- suppressMessages(
    df |> startlog() |> dplyr::filter(a > 3) |> dplyr::select(a, b) |> endlog()
  )
  expect_identical(attr(out, "endlog"),
                   c(rows = -3L, columns = -1L, cells = NA_integer_))
  out <- suppressMessages(
    df |> startlog() |> dplyr::bind_rows(df) |> dplyr::mutate(d = 1) |> endlog()
  )
  expect_identical(attr(out, "endlog"),
                   c(rows = 10L, columns = 1L, cells = NA_integer_))
  out <- suppressMessages(
    df |> startlog(clone = TRUE) |>
      dplyr::mutate(a = replace(a, 1:2, 99L)) |> endlog()
  )
  expect_identical(attr(out, "endlog"),
                   c(rows = 0L, columns = 0L, cells = 2L))
})

test_that("endlog reports cell-level changes when the shape is unchanged", {
  df <- data.frame(a = 1:10, b = letters[1:10])
  expect_message(
    df |> startlog(clone = TRUE) |>
      dplyr::mutate(a = replace(a, 1:2, 99L)) |> endlog(),
    "2/20 (10%) cells have been changed",
    fixed = TRUE
  )
})

test_that("endlog counts changes in missingness as cell changes", {
  df <- data.frame(a = c(1, 2, NA), b = c("x", "y", "z"))
  # one value -> NA, one NA -> value: two cell changes
  expect_message(
    df |> startlog(clone = TRUE) |>
      dplyr::mutate(a = c(NA, 2, 3)) |> endlog(),
    "2/6 (33.33%) cells have been changed",
    fixed = TRUE
  )
})

test_that("endlog reports zero cell changes on an untouched clone", {
  df <- data.frame(a = 1:3)
  expect_message(
    df |> startlog(clone = TRUE) |> endlog(),
    "0/3 (0%) cells have been changed",
    fixed = TRUE
  )
})

test_that("endlog formats large counts with a thousands separator", {
  df <- data.frame(a = seq_len(2000))
  expect_message(
    df |> startlog() |> dplyr::filter(a <= 500) |> endlog(),
    "1,500/2,000 (75%) rows have been dropped",
    fixed = TRUE
  )
  df <- data.frame(a = seq_len(1200))
  expect_message(
    df |> startlog() |> dplyr::bind_rows(df) |> endlog(),
    "1,200 rows have been added (1,200 -> 2,400, +100%)",
    fixed = TRUE
  )
  # ntot is a double from prod(); guards against scientific-notation fallback
  df <- data.frame(a = seq_len(1e5))
  expect_message(
    df |> startlog(clone = TRUE) |> dplyr::mutate(a = a + 1L) |> endlog(),
    "100,000/100,000 (100%) cells have been changed",
    fixed = TRUE
  )
})

test_that("endlog suggests clone=TRUE when the shape is unchanged", {
  df <- data.frame(a = 1:3)
  expect_message(
    df |> startlog() |> dplyr::mutate(a = a * 2) |> endlog(),
    "clone=TRUE"
  )
})

test_that("endlog reports renamed columns when the shape is unchanged", {
  df <- data.frame(a = 1:3, b = 1:3)
  msgs <- capture_messages(
    df |> startlog() |> dplyr::rename(z = a) |> endlog()
  )
  expect_true(any(grepl("Columns renamed: a -> z", msgs, fixed = TRUE)))
})

test_that("endlog falls back to the snapshot registry when attributes are stripped", {
  df <- data.frame(g = c("a", "a", "b"), x = 1:3)
  out <- df |> dplyr::group_by(g) |> dplyr::summarise(x = sum(x))
  expect_null(attr(out, "startlog_id"))  # summarise strips the tag
  expect_warning(
    expect_message(
      df |> startlog() |> dplyr::group_by(g) |> dplyr::summarise(x = sum(x)) |>
        endlog(),
      "1/3 (33.33%) row has been dropped",
      fixed = TRUE
    ),
    "stripped by an intermediate verb"
  )
})

test_that("endlog warns when startlog was never called", {
  expect_warning(endlog(data.frame(a = 1)), "startlog")
})
