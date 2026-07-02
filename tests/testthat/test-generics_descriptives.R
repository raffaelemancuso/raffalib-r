# Tests for the descriptive-statistics helpers in R/generics_descriptives.R.

test_that("descquant returns evenly spaced quantiles", {
  q <- descquant(0:100, int = 0.25)
  expect_length(q, 5)
  expect_named(q, c("0%", "25%", "50%", "75%", "100%"))
  expect_equal(unname(q[["50%"]]), 50)
})

test_that("descquant ignores NA values", {
  q <- descquant(c(0:10, NA), int = 0.5)
  expect_equal(unname(q[["100%"]]), 10)
})

test_that("myfreq returns a frequency table that counts NA", {
  res <- myfreq(factor(c("a", "a", "b", NA)))
  expect_s3_class(res, "data.frame")
  expect_true("freq" %in% colnames(res))
  expect_equal(sum(res$freq), 4)   # 2 x 'a', 1 x 'b', 1 x NA
})

test_that("descstrange reports NA, NaN and Inf counts", {
  x <- c(1, NA, NaN, Inf, -Inf)
  expect_output(descstrange(x), "N. missing")
  expect_output(descstrange(x), "N. NANs")
  expect_output(descstrange(x), "N. infinity")
})

test_that("get_dropped_obs returns the rows dropped by listwise deletion", {
  df <- data.frame(y = c(1, 2, 3, 4), x = c(1, NA, 3, 4), z = 1:4)
  mod <- lm(y ~ x, df)
  dropped <- get_dropped_obs(mod, df)          # all_model_vars = TRUE (default)
  expect_equal(nrow(dropped), 1L)
  expect_setequal(colnames(dropped), c("y", "x"))
  expect_true(is.na(dropped$x))
})

test_that("get_dropped_obs(all_model_vars = FALSE) keeps only the missing model vars", {
  df <- data.frame(y  = c(1, 2, 3, 4),
                   x1 = c(1, NA, 3, 4),
                   x2 = c(1, 2, NA, 4))
  mod <- lm(y ~ x1 + x2, df)
  res <- get_dropped_obs(mod, df, all_model_vars = FALSE)
  expect_equal(nrow(res), 2L)
  expect_setequal(colnames(res), c("x1", "x2"))   # y is never missing -> excluded
})

test_that("get_dropped_obs appends other_vars to the result", {
  df <- data.frame(y = c(1, NA), x = c(1, 2), z = c(9, 9))
  mod <- lm(y ~ x, df)
  res <- get_dropped_obs(mod, df, other_vars = "z")
  expect_equal(nrow(res), 1L)
  expect_true(all(c("y", "x", "z") %in% colnames(res)))
})

test_that("na_per_group counts missing values per group", {
  df <- data.frame(g = c("a", "a", "b", "b"), v = c(1, NA, NA, NA))
  res <- na_per_group(df, v, g)
  expect_equal(nrow(res), 2L)
  expect_equal(res$na[res$g == "b"], 2L)
  expect_equal(res$p[res$g == "b"], 1)
})

test_that("dupsa passes for unique keys and errors on duplicates", {
  expect_no_error(dupsa(data.frame(id = 1:3, v = 4:6), id))
  expect_error(dupsa(data.frame(id = c(1, 1, 2), v = 1:3), id))
})

test_that("dupsa rejects a data frame that already has an 'n' column", {
  expect_error(dupsa(data.frame(n = 1:2), n))
})
