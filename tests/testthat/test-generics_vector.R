# Tests for the vector utilities in R/generics_vector.R.

test_that("as_named_list names each element after its own value", {
  res <- as_named_list(c("a", "b", "c"))
  expect_type(res, "list")
  expect_named(res, c("a", "b", "c"))
  expect_equal(res$a, "a")
})

test_that("vec_relocate moves a single element to a new position", {
  v <- c("one", "two", "three", "four", "five")
  expect_equal(vec_relocate(v, "four", 2),
               c("one", "four", "two", "three", "five"))
})

test_that("vec_relocate moves multiple elements and preserves the set", {
  v <- letters[1:5]
  res <- vec_relocate(v, c("e", "a"), c(1, 5))
  expect_length(res, 5)
  expect_setequal(res, v)
})

test_that("vec_relocate errors on absent or duplicated elements", {
  expect_error(vec_relocate(c("a", "b"), "z", 1))
  expect_error(vec_relocate(c("a", "a", "b"), "a", 1))
})

test_that("catvec prints one indexed element per line", {
  expect_output(catvec(c("x", "y")), "\\[1\\] x")
  expect_output(catvec(c("x", "y")), "\\[2\\] y")
})
