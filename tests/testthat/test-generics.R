# Tests for the backup pipeline in R/generics.R
# (save_backup / read_backup).
#
# Several tests need a pre-existing backup that is OLDER than anything
# save_backup() writes now: they craft it by calling saveRDS() directly on a
# hand-built "<stem>_2020-01-01_00-00-00.rds" name. saveRDS() output is
# byte-identical to save_backup()'s for the same object, so the crafted files
# also exercise the SHA-256 comparison, without any sleeps.

.craft_backup <- function(obj, dir, stem, timestamp = "2020-01-01_00-00-00") {
  fp <- file.path(dir, paste0(stem, "_", timestamp, ".rds"))
  saveRDS(obj, fp)
  fp
}

.n_backups <- function(dir) length(list.files(dir, pattern = "\\.rds$"))

test_that("save_backup and read_backup round-trip an object", {
  dir <- withr::local_tempdir()
  obj <- list(a = 1, b = "two", c = 1:5)
  expect_output(save_backup(obj, dir, "myobj"))       # prints the save path
  expect_equal(read_backup(dir, "myobj"), obj)
})

test_that("save_backup writes a time-stamped name and returns its path invisibly", {
  dir <- withr::local_tempdir()
  res <- withVisible(save_backup(1:10, dir, "myobj"))
  expect_false(res$visible)
  expect_true(file.exists(res$value))
  expect_match(
    basename(res$value),
    "^myobj_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.rds$"
  )
})

test_that("save_backup errors when out_dir does not exist", {
  expect_error(save_backup(1, file.path(tempdir(), "no_such_dir"), "x"))
})

test_that("read_backup picks the newest backup by time stamp", {
  dir <- withr::local_tempdir()
  .craft_backup("old", dir, "obj", "2020-01-01_00-00-00")
  .craft_backup("mid", dir, "obj", "2021-06-15_12-30-00")
  .craft_backup("new", dir, "obj", "2022-02-01_09-00-00")
  expect_equal(read_backup(dir, "obj"), "new")
})

test_that("read_backup errors informatively when no backup matches", {
  dir <- withr::local_tempdir()
  expect_error(read_backup(dir, "does_not_exist"), "does_not_exist")
})

test_that("an identical re-save is refused with a warning and no new file", {
  dir <- withr::local_tempdir()
  obj <- list(a = 1:1000, b = letters)
  latest <- .craft_backup(obj, dir, "pis")
  expect_warning(
    res <- save_backup(obj, dir, "pis"),
    "Refusing to save backup"
  )
  expect_equal(.n_backups(dir), 1)          # nothing new written
  expect_equal(res, latest)                 # existing path returned
})

test_that("a changed object is saved as a new backup", {
  dir <- withr::local_tempdir()
  obj <- list(a = 1:1000)
  .craft_backup(obj, dir, "pis")
  obj$a <- 1:1001
  expect_output(fp <- save_backup(obj, dir, "pis"))
  expect_equal(.n_backups(dir), 2)
  expect_true(file.exists(fp))
  expect_equal(read_backup(dir, "pis"), obj)
})

test_that("dedup compares only against the most recent same-stem backup", {
  dir <- withr::local_tempdir()
  obj_a <- list(x = 1)
  obj_b <- list(x = 2)
  .craft_backup(obj_a, dir, "obj", "2020-01-01_00-00-00")  # older, identical
  .craft_backup(obj_b, dir, "obj", "2021-01-01_00-00-00")  # newest, different
  # obj_a matches an OLD backup but not the newest one: must save
  expect_output(save_backup(obj_a, dir, "obj"))
  expect_equal(.n_backups(dir), 3)
})

test_that("the stem match is anchored for both save and read", {
  dir <- withr::local_tempdir()
  obj <- list(a = 1:100)
  # newer backup of a stem that CONTAINS "pis": must block neither saving
  # nor reading the exact stem
  .craft_backup(obj, dir, "ai_pis", "2099-01-01_00-00-00")
  expect_output(save_backup(obj, dir, "pis"))   # identical content, other stem
  expect_equal(.n_backups(dir), 2)
  expect_equal(read_backup(dir, "pis"), obj)
  # and the suffix stem still reads its own file
  expect_equal(read_backup(dir, "ai_pis"), obj)
})

test_that("refuse_identical = FALSE always writes, without warning", {
  dir <- withr::local_tempdir()
  obj <- list(a = 1:1000)
  .craft_backup(obj, dir, "pis")
  expect_no_warning(
    expect_output(save_backup(obj, dir, "pis", refuse_identical = FALSE))
  )
  expect_equal(.n_backups(dir), 2)
})
