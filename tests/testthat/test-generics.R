# Tests for the backup pipeline in R/generics.R
# (save_backup / read_backup).
#
# Several tests need a pre-existing backup that is OLDER than anything
# save_backup() writes now: they craft it by serialising directly onto a
# hand-built "<stem>_2020-01-01_00-00-00" name, in save_backup()'s default
# format (qs2, same thread count) unless another format is requested. The
# writers' output is byte-identical to save_backup()'s for the same object
# and format, so the crafted files also exercise the SHA-256 comparison,
# without any sleeps.

.craft_backup <- function(obj, dir, stem, timestamp = "2020-01-01_00-00-00",
                          format = "qs2") {
  if (format == "qs2") {
    fp <- file.path(dir, paste0(stem, "_", timestamp, ".qs2"))
    qs2::qs_save(obj, fp, nthreads = max(1L, parallel::detectCores() - 2L))
  } else {
    fp <- file.path(dir, paste0(stem, "_", timestamp, ".rds"))
    saveRDS(obj, fp, compress = switch(
      format,
      rds_zstd = "zstd", rds_gzip = "gzip", rds_uncompressed = FALSE
    ))
  }
  fp
}

.n_backups <- function(dir) length(list.files(dir, pattern = "\\.(rds|qs2)$"))

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
    "^myobj_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.qs2$"
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

test_that("rotation keeps at most max_backups (default 5) per stem", {
  dir <- withr::local_tempdir()
  for (i in 1:5) {
    .craft_backup(list(x = i), dir, "obj", sprintf("2020-01-0%d_00-00-00", i))
  }
  expect_output(save_backup(list(x = 99), dir, "obj"))   # 6th backup
  fns <- list.files(dir, pattern = "^obj_")
  expect_length(fns, 5)
  expect_false("obj_2020-01-01_00-00-00.qs2" %in% fns)   # oldest rotated out
  expect_true("obj_2020-01-02_00-00-00.qs2" %in% fns)    # next-oldest kept
  expect_equal(read_backup(dir, "obj"), list(x = 99))    # newest is the save
})

test_that("rotation prunes pre-existing excess down to max_backups", {
  dir <- withr::local_tempdir()
  for (i in 1:4) {
    .craft_backup(list(x = i), dir, "obj", sprintf("2020-01-0%d_00-00-00", i))
  }
  expect_output(save_backup(list(x = 99), dir, "obj", max_backups = 2))
  fns <- list.files(dir, pattern = "^obj_")
  expect_length(fns, 2)
  expect_true("obj_2020-01-04_00-00-00.qs2" %in% fns)    # newest crafted kept
  expect_equal(read_backup(dir, "obj"), list(x = 99))
})

test_that("max_backups = NULL keeps every backup", {
  dir <- withr::local_tempdir()
  for (i in 1:6) {
    .craft_backup(list(x = i), dir, "obj", sprintf("2020-01-0%d_00-00-00", i))
  }
  expect_output(save_backup(list(x = 99), dir, "obj", max_backups = NULL))
  expect_length(list.files(dir, pattern = "^obj_"), 7)
})

test_that("a refused save does not rotate", {
  dir <- withr::local_tempdir()
  obj <- list(x = 99)
  for (i in 1:6) {
    .craft_backup(list(x = i), dir, "obj", sprintf("2020-01-0%d_00-00-00", i))
  }
  .craft_backup(obj, dir, "obj", "2021-01-01_00-00-00")  # newest, identical
  expect_warning(save_backup(obj, dir, "obj"), "Refusing to save backup")
  expect_length(list.files(dir, pattern = "^obj_"), 7)   # all still there
})

test_that("rotation only touches the saved stem", {
  dir <- withr::local_tempdir()
  for (i in 1:5) {
    .craft_backup(list(x = i), dir, "obj", sprintf("2020-01-0%d_00-00-00", i))
  }
  .craft_backup("other", dir, "other_stem")
  expect_output(save_backup(list(x = 99), dir, "obj"))
  expect_length(list.files(dir, pattern = "^obj_"), 5)
  expect_equal(read_backup(dir, "other_stem"), "other")  # untouched
})

test_that("invalid max_backups values error", {
  dir <- withr::local_tempdir()
  expect_error(save_backup(1, dir, "x", max_backups = 0))
  expect_error(save_backup(1, dir, "x", max_backups = -1))
  expect_error(save_backup(1, dir, "x", max_backups = c(2, 3)))
})

test_that("format controls the on-disk representation and round-trips", {
  dir <- withr::local_tempdir()
  obj <- list(a = 1:1000, b = letters)
  magic <- function(fp, n = 4) readBin(fp, "raw", n)

  expect_output(fp_z <- save_backup(obj, dir, "z", format = "rds_zstd"))
  expect_identical(magic(fp_z), as.raw(c(0x28, 0xb5, 0x2f, 0xfd)))  # zstd frame
  expect_equal(read_backup(dir, "z"), obj)

  expect_output(fp_g <- save_backup(obj, dir, "g", format = "rds_gzip"))
  expect_identical(magic(fp_g, 2), as.raw(c(0x1f, 0x8b)))           # gzip
  expect_equal(read_backup(dir, "g"), obj)

  expect_output(fp_u <- save_backup(obj, dir, "u", format = "rds_uncompressed"))
  expect_identical(magic(fp_u, 2), as.raw(c(0x58, 0x0a)))           # "X\n"
  expect_equal(read_backup(dir, "u"), obj)

  expect_error(save_backup(obj, dir, "x", format = "nope"))
})

test_that("the dedup comparison is per-format: a format switch saves anew", {
  dir <- withr::local_tempdir()
  obj <- list(a = 1:1000)
  .craft_backup(obj, dir, "pis", format = "rds_gzip")  # latest is gzip rds
  expect_no_warning(
    expect_output(save_backup(obj, dir, "pis"))        # qs2 candidate differs
  )
  expect_equal(.n_backups(dir), 2)
})

test_that("qs2 format writes .qs2, round-trips, dedups and rotates", {
  skip_if_not_installed("qs2")
  dir <- withr::local_tempdir()
  obj <- list(a = 1:1000, b = letters)

  expect_output(fp <- save_backup(obj, dir, "obj", format = "qs2"))
  expect_match(basename(fp), "\\.qs2$")
  expect_equal(read_backup(dir, "obj"), obj)

  # identical qs2 re-save is refused against the .qs2 backup
  expect_warning(
    save_backup(obj, dir, "obj", format = "qs2"),
    "Refusing to save backup"
  )
  expect_equal(.n_backups(dir), 1)

  # .rds and .qs2 backups of a stem rotate together
  for (i in 1:5) {
    .craft_backup(list(x = i), dir, "mix", sprintf("2020-01-0%d_00-00-00", i),
                  format = "rds_zstd")
  }
  expect_output(save_backup(obj, dir, "mix", format = "qs2"))
  fns <- list.files(dir, pattern = "^mix_")
  expect_length(fns, 5)
  expect_false("mix_2020-01-01_00-00-00.rds" %in% fns)  # oldest rotated out
  expect_equal(read_backup(dir, "mix"), obj)            # newest is the .qs2
})
