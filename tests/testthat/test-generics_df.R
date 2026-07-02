# Tests for the data-frame column utilities in R/generics_df.R.

test_that("sort_columns_by_name orders columns in natural order", {
  df <- data.frame(x2 = 1, x10 = 2, x1 = 3)
  expect_equal(colnames(sort_columns_by_name(df)), c("x1", "x2", "x10"))
})

test_that("sort_columns_by_label orders columns by their variable labels", {
  df <- data.frame(a = 1, b = 2, c = 3)
  df <- labelled::set_variable_labels(df, a = "Zebra", b = "Apple", c = "Mango")
  expect_equal(colnames(sort_columns_by_label(df)), c("b", "c", "a"))
})

test_that("catcols prints the column names matching a regex", {
  expect_output(catcols(mtcars, regex = "^c", sort = TRUE), "carb")
  # sort = TRUE puts carb before cyl
  expect_output(catcols(mtcars, regex = "^c", sort = TRUE), "carb\ncyl")
})

test_that("catcols with no regex prints every column", {
  expect_output(catcols(data.frame(z = 1, a = 2), sort = TRUE), "a\nz")
})
