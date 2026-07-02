# Tests for the list utilities in R/generics_list.R.

test_that("list_rename_names maps names via the mapping (strict)", {
  l1 <- list(dv1 = "M1", dv2 = "M2")
  o  <- list(dv1 = "Label1", dv2 = "Label2")
  res <- list_rename_names(l1, o)
  expect_named(res, c("Label1", "Label2"))
  expect_equal(unname(unlist(res)), c("M1", "M2"))
})

test_that("list_rename_names keeps unmatched names when .strict = FALSE", {
  l1 <- list(dv1 = "M1", dv2 = "M2")
  o  <- list(dv2 = "Label2")
  res <- list_rename_names(l1, o, .strict = FALSE)
  expect_named(res, c("dv1", "Label2"))
})

test_that("list_rename_names errors when a name is missing and .strict = TRUE", {
  expect_error(list_rename_names(list(dv1 = "M1"), list(dv2 = "L2"), .strict = TRUE))
})

test_that("list_rename_values recodes matching values and leaves the rest", {
  res <- list_rename_values(list("a", "b", "c"), list(a = "Apple", c = "Cherry"))
  expect_equal(res, list("Apple", "b", "Cherry"))
})

test_that("sort_named_list_by_names uses natural (human) order", {
  l <- list(item10 = 1, item2 = 2, item1 = 3)
  expect_named(sort_named_list_by_names(l), c("item1", "item2", "item10"))
})

test_that("sort_named_list_by_values uses natural order of the values", {
  l <- list(a = "x10", b = "x2", c = "x1")
  expect_equal(unname(unlist(sort_named_list_by_values(l))), c("x1", "x2", "x10"))
})
