# Tests for the miscellaneous helpers in R/generics.R
# (save_backup / read_backup).

test_that("save_backup and read_backup round-trip an object", {
  dir <- withr::local_tempdir()
  obj <- list(a = 1, b = "two", c = 1:5)
  expect_output(save_backup(obj, dir, "myobj"))       # prints the save path
  expect_equal(read_backup(dir, "myobj"), obj)
})

test_that("read_backup errors when no backup matches", {
  dir <- withr::local_tempdir()
  expect_error(read_backup(dir, "does_not_exist"))
})
