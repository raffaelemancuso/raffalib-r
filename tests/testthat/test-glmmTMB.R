# Tests for the glmmTMB post-fit helpers in R/glmmTMB.R
# (glmmTMB_get_optimum, glmmTMB_get_hessian_1 / _2).
# A single small mixed model with a random intercept is fit once and reused.

skip_on_cran()

mod <- glmmTMB::glmmTMB(
  count ~ mined + (1 | site),
  family = poisson,
  data   = glmmTMB::Salamanders
)

test_that("glmmTMB_get_optimum returns fixed-effect start values", {
  opt <- glmmTMB_get_optimum(mod)
  expect_named(opt, c("beta", "betazi", "betadisp"))
  # beta holds the conditional fixed effects, ready for the `start` argument
  expect_equal(unname(opt$beta), unname(glmmTMB::fixef(mod)$cond))
  expect_true("(Intercept)" %in% names(opt$beta))
})

test_that("glmmTMB_get_hessian_1 returns a square matrix over the fixed params", {
  H1 <- glmmTMB_get_hessian_1(mod)
  expect_true(is.matrix(H1))
  expect_equal(nrow(H1), ncol(H1))
  expect_equal(nrow(H1), length(with(mod$obj$env, last.par.best[-random])))
})

test_that("the two Hessian methods agree at the optimum", {
  H1 <- glmmTMB_get_hessian_1(mod)
  H2 <- glmmTMB_get_hessian_2(mod)
  expect_equal(dim(H1), dim(H2))
  # H1 (from optimHess) carries dimnames, H2 (from numDeriv) does not; compare
  # the numeric content only.
  expect_equal(H1, H2, tolerance = 1e-3, ignore_attr = TRUE)
})
