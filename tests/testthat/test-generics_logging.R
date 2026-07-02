# Tests for the change-logging helpers in R/generics_logging.R.
# endlog() consumes attributes left by startlog(); dplyr verbs strip custom
# attributes, so these tests set the bookkeeping attributes directly to exercise
# endlog()'s reporting logic deterministically.

test_that("startlog records the data-frame shape", {
  df <- startlog(data.frame(a = 1:3, b = 1:3))
  expect_equal(attr(df, "old_shape"), c(3L, 2L))
  expect_null(attr(df, "old_df"))
})

test_that("startlog(clone = TRUE) stashes a copy of the data", {
  df <- startlog(data.frame(a = 1:3), clone = TRUE)
  expect_s3_class(attr(df, "old_df"), "data.frame")
})

test_that("endlog reports row changes and strips the bookkeeping attributes", {
  df <- data.frame(a = 1:5, b = 1:5)
  attr(df, "old_shape") <- c(7L, 2L)   # pretend two rows were dropped
  expect_message(endlog(df), "Rows changed by -2")
  expect_null(attr(endlog(df), "old_shape"))
})

test_that("endlog reports cell-level changes when the shape is unchanged", {
  old <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  new <- old
  new$a[1] <- 99
  attr(new, "old_shape") <- dim(old)
  attr(new, "old_df") <- old
  expect_message(endlog(new), "Data changed in 1")
})

test_that("endlog warns when startlog was never called", {
  expect_warning(endlog(data.frame(a = 1)))
})
