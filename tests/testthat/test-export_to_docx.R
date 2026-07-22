# Tests for the Word-export helpers in R/export_to_docx.R.
# These exercise the real officer/flextable writers and assert a non-empty
# .docx file lands on disk.

test_that("flextable2docx writes a non-empty .docx file", {
  ft  <- flextable::flextable(head(mtcars))
  out <- withr::local_tempfile(fileext = ".docx")
  flextable2docx(ft, out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})

test_that("flextable2docx forwards word_prop to the caption/page setup", {
  ft  <- flextable::flextable(head(mtcars))
  out <- withr::local_tempfile(fileext = ".docx")
  flextable2docx(ft, out, word_prop = list(caption_text = "Table 1", paper_format = "A3"))
  expect_true(file.exists(out))
})

test_that("flextable_collapse_categorical replaces dummy rows with a single Yes row", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  m2 <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
  ft  <- modelsummary::modelsummary(list(m1, m2), output = "flextable")
  ds0 <- ft$body$dataset
  terms0 <- trimws(as.character(ds0[[ft$col_keys[1]]]))

  out <- flextable_collapse_categorical(ft, c("Cylinders" = "factor(cyl)"))
  ds  <- out$body$dataset
  terms <- trimws(as.character(ds[[out$col_keys[1]]]))

  expect_false(any(startsWith(terms, "factor(cyl)")))
  # two dummy levels x (estimate + SE) rows collapsed into one row
  expect_equal(nrow(ds), nrow(ds0) - 3)
  # the collapsed row sits where the first dummy row was, with "Yes" throughout
  i <- match("Cylinders", terms)
  expect_equal(i, match(TRUE, startsWith(terms0, "factor(cyl)")))
  expect_true(all(ds[i, out$col_keys[-1]] == "Yes"))
})

test_that("flextable_collapse_categorical supports custom values and unnamed prefixes", {
  m  <- lm(mpg ~ wt + factor(cyl) + factor(gear), data = mtcars)
  ft <- modelsummary::modelsummary(m, output = "flextable")
  out <- flextable_collapse_categorical(
    ft, c("factor(cyl)", "Gear" = "factor(gear)"), value = "Included"
  )
  terms <- trimws(as.character(out$body$dataset[[out$col_keys[1]]]))
  expect_true(all(c("factor(cyl)", "Gear") %in% terms))
  vals <- out$body$dataset[match(c("factor(cyl)", "Gear"), terms), out$col_keys[-1]]
  expect_true(all(vals == "Included"))
})

test_that("flextable_collapse_categorical warns when a variable matches no rows", {
  ft <- flextable::flextable(data.frame(term = c("a", "b"), est = 1:2))
  expect_warning(flextable_collapse_categorical(ft, "nonexistent"), "No rows matching")
})

test_that("flextable2docx collapses categorical variables before export", {
  m   <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  ft  <- modelsummary::modelsummary(m, output = "flextable")
  out <- withr::local_tempfile(fileext = ".docx")
  flextable2docx(ft, out, collapse_categorical = c("Cylinders" = "factor(cyl)"))
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})

test_that("plot2docx writes a .docx file from a base plot", {
  out <- withr::local_tempfile(fileext = ".docx")
  plot2docx(officer::plot_instr(code = plot(1:10)), out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})

test_that("plot2docx errors when only one of width/height is supplied", {
  out <- withr::local_tempfile(fileext = ".docx")
  expect_error(
    plot2docx(officer::plot_instr(code = plot(1:10)), out,
              plot_width = 100, plot_height = NULL)
  )
})

test_that("ggplot2docx writes a .docx file", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  out <- withr::local_tempfile(fileext = ".docx")
  ggplot2docx(p, out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0)
})
