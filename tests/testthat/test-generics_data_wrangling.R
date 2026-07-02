# Tests for the data-wrangling helpers in R/generics_data_wrangling.R.

test_that("nan2na converts NaN to NA in numeric columns only", {
  df <- data.frame(x = c(1, NaN, 3), y = c("a", "b", "c"), stringsAsFactors = FALSE)
  res <- nan2na(df)
  expect_true(is.na(res$x[2]))
  expect_false(is.nan(res$x[2]))    # now a plain NA, not a NaN
  expect_equal(res$y, c("a", "b", "c"))
})

test_that("nan2na leaves data without NaN untouched", {
  df <- data.frame(x = c(1, 2, NA), y = 4:6)
  expect_equal(nan2na(df), df)
})
