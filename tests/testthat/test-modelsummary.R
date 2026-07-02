# Tests for the modelsummary helpers in R/modelsummary.R.

test_that("modelsummary_missing_variables_in_coef_map finds unmapped terms", {
  mod <- lm(mpg ~ wt + hp, mtcars)
  miss <- modelsummary_missing_variables_in_coef_map(mod, list(wt = "Weight"))
  expect_setequal(miss, c("(Intercept)", "hp"))
})

test_that("modelsummary_common_coefs_at_bottom orders shared coefs (and intercept) last", {
  m1 <- lm(mpg ~ wt + hp, mtcars)
  m2 <- lm(mpg ~ wt + disp, mtcars)
  res <- modelsummary_common_coefs_at_bottom(list(m1, m2), coef_rename = FALSE)
  # unique coefs first (hp, disp), then the shared one (wt), then the intercept
  expect_equal(res, c("hp", "disp", "wt", "(Intercept)"))
})

test_that("modelsummary_getgofmap returns a formatted gof map", {
  gm <- modelsummary_getgofmap()
  expect_type(gm, "list")
  expect_true("nobs" %in% names(gm))
  expect_equal(gm$nobs$fmt(1234), "1,234")   # integer formatting with thousands sep
})

test_that("modelsummary_name_models_list names models by their response label", {
  m  <- lm(mpg ~ wt, mtcars)
  df <- labelled::set_variable_labels(mtcars, mpg = "Miles per gallon")
  res <- modelsummary_name_models_list(list(m), df)
  expect_named(res, "Miles per gallon")
})

test_that("modelsummary_build_coef_map maps parameters to labels, falling back to names", {
  m  <- lm(mpg ~ wt + hp, mtcars)
  df <- labelled::set_variable_labels(mtcars, wt = "Weight")
  cm <- modelsummary_build_coef_map(list(m), df)
  expect_equal(cm[["wt"]], "Weight")            # labelled
  expect_equal(cm[["hp"]], "hp")                # unlabelled -> keeps its name
  expect_equal(cm[["(Intercept)"]], "(Intercept)")
})
