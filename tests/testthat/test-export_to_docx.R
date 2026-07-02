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
