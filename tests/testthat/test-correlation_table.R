# Tests for correlation_table() in R/correlation_table.R.

test_that("correlation_table returns a flextable", {
  df  <- mtcars[, c("mpg", "wt", "hp", "disp")]
  tbl <- correlation_table(df)
  expect_s3_class(tbl, "flextable")
})

test_that("correlation_table keeps only numeric columns", {
  df <- mtcars[, c("mpg", "wt", "hp")]
  df$grp <- factor(rep(c("a", "b"), length.out = nrow(df)))
  # the factor column must not break the correlation computation
  expect_s3_class(correlation_table(df), "flextable")
})
