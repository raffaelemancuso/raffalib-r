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

test_that("flextable_collapse_group replaces several variables with ONE row", {
  m1 <- lm(mpg ~ wt + factor(cyl) + factor(gear), data = mtcars)
  m2 <- lm(mpg ~ wt + hp + factor(cyl) + factor(gear), data = mtcars)
  ft  <- modelsummary::modelsummary(list(m1, m2), output = "flextable")
  ds0 <- ft$body$dataset
  terms0 <- trimws(as.character(ds0[[ft$col_keys[1]]]))

  out <- flextable_collapse_group(
    ft, vars = c("factor(cyl)", "factor(gear)"), label = "Controls", value = "YES"
  )
  ds    <- out$body$dataset
  terms <- trimws(as.character(ds[[out$col_keys[1]]]))

  expect_false(any(startsWith(terms, "factor(")))
  expect_equal(sum(terms == "Controls"), 1L)
  # every collapsed block is gone, replaced by a single row
  expect_lt(nrow(ds), nrow(ds0))
  expect_true(all(ds[match("Controls", terms), out$col_keys[-1]] == "YES"))
  # untouched coefficients survive
  expect_true(any(startsWith(terms, "wt")))
})

test_that("flextable_collapse_group puts the row where the group started", {
  # cyl comes before gear in the model, so collapsing cyl must land above gear
  m  <- lm(mpg ~ wt + factor(cyl) + factor(gear), data = mtcars)
  ft <- modelsummary::modelsummary(m, output = "flextable")
  terms0 <- trimws(as.character(ft$body$dataset[[ft$col_keys[1]]]))
  i_cyl0 <- match(TRUE, startsWith(terms0, "factor(cyl)"))

  out <- flextable_collapse_group(ft, vars = "factor(cyl)", label = "Controls", value = "YES")
  terms <- trimws(as.character(out$body$dataset[[out$col_keys[1]]]))

  # exactly where the first block of the group was
  expect_equal(match("Controls", terms), i_cyl0)
  # so it still precedes the coefficients that followed the group
  expect_lt(match("Controls", terms), match(TRUE, startsWith(terms, "factor(gear)")))
  expect_gt(match("Controls", terms), match(TRUE, startsWith(terms, "wt")))
})

test_that("flextable_collapse_group moves the coefficient/GOF rule, never duplicates it", {
  m  <- lm(mpg ~ wt + factor(cyl) + factor(gear), data = mtcars)
  ft <- modelsummary::modelsummary(m, output = "flextable")
  out <- flextable_collapse_group(ft, vars = "factor(cyl)", label = "A", value = "YES")
  out <- flextable_collapse_group(out, vars = "factor(gear)", label = "B", value = "YES")

  terms <- trimws(as.character(out$body$dataset[[out$col_keys[1]]]))
  bottom <- apply(out$body$styles$cells$border.width.bottom$data, 1, max)
  gof <- match("Num.Obs.", terms)

  # the rule sits on the last coefficient row (our second inserted row), not on
  # each collapsed row; the final entry is the table's own closing rule
  expect_equal(which(bottom > 0), c(gof - 1L, length(terms)))
  expect_equal(terms[gof - 1L], "B")
})

test_that("flextable_collapse_group keeps the table renderable after insertion", {
  m  <- lm(mpg ~ wt + factor(cyl) + factor(gear), data = mtcars)
  ft <- modelsummary::modelsummary(m, output = "flextable")
  out <- flextable_collapse_group(
    ft, vars = c("factor(cyl)", "factor(gear)"), label = "Controls", value = "YES"
  )
  # the parallel style/content structures must stay in step with the dataset
  n <- nrow(out$body$dataset)
  expect_equal(out$body$content$nrow, n)
  expect_equal(nrow(out$body$spans$rows), n)
  expect_equal(length(out$body$rowheights), n)
  for (grp in c("cells", "pars", "text")) {
    for (prop in names(out$body$styles[[grp]])) {
      expect_equal(out$body$styles[[grp]][[prop]]$nrow, n)
    }
  }
  tmp <- withr::local_tempfile(fileext = ".docx")
  flextable2docx(out, tmp)
  expect_gt(file.info(tmp)$size, 0)
})

test_that("flextable_collapse_group resolves variable NAMES through labels", {
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  d$gear_f <- factor(d$gear)
  d <- labelled::set_variable_labels(d, cyl_f = "Cylinders", gear_f = "Gears")
  m  <- lm(mpg ~ wt + cyl_f + gear_f, data = d)
  ft <- modelsummary::modelsummary(m, output = "flextable", coef_rename = TRUE)

  # named by model variable, not by the label the table displays
  out <- flextable_collapse_group(
    ft, vars = c("cyl_f", "gear_f"), label = "Controls", labels = d
  )
  terms <- trimws(as.character(out$body$dataset[[out$col_keys[1]]]))
  expect_false(any(startsWith(terms, "Cylinders")))
  expect_false(any(startsWith(terms, "Gears")))
  expect_equal(sum(terms == "Controls"), 1L)
})

test_that("flextable_collapse_group treats vars as name prefixes", {
  d <- mtcars
  d$ctrl_a <- factor(d$cyl)
  d$ctrl_b <- factor(d$gear)
  d <- labelled::set_variable_labels(d, ctrl_a = "First", ctrl_b = "Second")
  m  <- lm(mpg ~ wt + ctrl_a + ctrl_b, data = d)
  ft <- modelsummary::modelsummary(m, output = "flextable", coef_rename = TRUE)

  # one prefix picks up both ctrl_a and ctrl_b
  out <- flextable_collapse_group(ft, vars = "ctrl_", label = "Controls", labels = d)
  terms <- trimws(as.character(out$body$dataset[[out$col_keys[1]]]))
  expect_false(any(startsWith(terms, "First")))
  expect_false(any(startsWith(terms, "Second")))
  expect_equal(sum(terms == "Controls"), 1L)
})

test_that("flextable_collapse_group matches unlabelled terms printed with spaces", {
  # modelsummary prints an unlabelled `start_year` as "start year [2011]", which
  # matches neither the raw name nor a (dropped) label
  d <- data.frame(y = rnorm(90), start_year = sample(2010:2012, 90, TRUE))
  m  <- lm(y ~ factor(start_year), data = d)
  ft <- modelsummary::modelsummary(m, output = "flextable", coef_rename = TRUE)
  terms0 <- trimws(as.character(ft$body$dataset[[ft$col_keys[1]]]))
  expect_true(any(startsWith(terms0, "start year")))

  out <- flextable_collapse_group(ft, vars = "start_year", label = "Years FE", value = "YES")
  terms <- trimws(as.character(out$body$dataset[[out$col_keys[1]]]))
  expect_false(any(startsWith(terms, "start year")))
  expect_equal(sum(terms == "Years FE"), 1L)
})

test_that("flextable_collapse_group finds the term column past grouping columns", {
  # modelsummary `shape =` layouts put `component` first and the terms second
  ft <- flextable::flextable(data.frame(
    component = c("conditional", "", "", ""),
    term      = c("(Intercept)", "gender [male]", "", "wt"),
    m1        = c("1.0", "2.0", "(0.1)", "3.0"),
    stringsAsFactors = FALSE
  ))
  out <- flextable_collapse_group(ft, vars = "gender", label = "Controls", value = "YES")
  ds  <- out$body$dataset
  # the collapsed row sits where the gender block was, between Intercept and wt
  expect_equal(trimws(as.character(ds$term)), c("(Intercept)", "Controls", "wt"))
  # the grouping column keeps its own content, only the model column gets YES
  expect_equal(trimws(as.character(ds$component)), c("conditional", "", ""))
  expect_equal(trimws(as.character(ds$m1)), c("1.0", "YES", "3.0"))
})

test_that("flextable_drop_component removes a component's rows", {
  ft <- flextable::flextable(data.frame(
    component = c("conditional", "", "", "dispersion", "", ""),
    term      = c("(Intercept)", "", "wt", "(Intercept)", "", "Num.Obs."),
    m1        = c("1.0", "(0.1)", "2.0", "3.0", "(0.3)", "32"),
    stringsAsFactors = FALSE
  ))
  out <- flextable_drop_component(ft, "dispersion")
  ds <- out$body$dataset
  # the dispersion row and its standard-error row go; the GOF row stays
  expect_equal(trimws(as.character(ds$term)), c("(Intercept)", "", "wt", "Num.Obs."))
  expect_equal(trimws(as.character(ds$component)), c("conditional", "", "", ""))
})

test_that("flextable_drop_component keeps the coefficient/GOF rule", {
  # the rule lives on the last coefficient row, which is the dispersion block:
  # deleting it must not take the table's only separator away
  m  <- glmmTMB::glmmTMB(count ~ spp, data = glmmTMB::Salamanders, family = glmmTMB::nbinom2)
  ft <- modelsummary::modelsummary(list(m), output = "flextable",
                                   shape = component + term + statistic ~ model)
  rule_before <- max(ft$body$styles$cells$border.width.bottom$data)
  expect_gt(rule_before, 0)

  out <- flextable_drop_component(ft, "dispersion")
  terms <- trimws(as.character(out$body$dataset[[flextable_term_col(out)]]))
  bottom <- apply(out$body$styles$cells$border.width.bottom$data, 1, max)
  gof <- match("Num.Obs.", terms)

  expect_false(is.na(gof))
  # still exactly one rule above the GOF block, on the new last coefficient row
  expect_gt(bottom[gof - 1L], 0)
  expect_equal(sum(bottom[seq_len(gof - 2L)] > 0), 0)
})

test_that("flextable_drop_component warns rather than mangling the table", {
  ft <- flextable::flextable(data.frame(term = c("a", "b"), est = c("1", "2")))
  expect_warning(flextable_drop_component(ft, "dispersion"), "No .*component. column")
  ft2 <- flextable::flextable(data.frame(
    component = c("conditional", ""), term = c("(Intercept)", "wt"), m1 = c("1", "2"),
    stringsAsFactors = FALSE
  ))
  expect_warning(flextable_drop_component(ft2, "zi"), "No rows for component")
})

test_that("flextable_collapse_group warns when nothing matches", {
  ft <- flextable::flextable(data.frame(term = c("a", "b"), est = 1:2))
  expect_warning(
    flextable_collapse_group(ft, "nonexistent", label = "Controls"),
    "No rows matching"
  )
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
