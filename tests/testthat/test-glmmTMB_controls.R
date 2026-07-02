# Tests for the glmmTMB optimizer-control constructors in
# R/glmmTMB_optimx.R, R/glmmTMB_calibrar.R and R/glmmTMB_optimh.R.
# These only build glmmTMBControl objects (no model is fitted), so they are fast.

test_that("optimx control constructors carry the requested method", {
  ctrl <- glmmTMB_control_optimx_bfgs()
  expect_type(ctrl, "list")
  expect_equal(ctrl$optArgs$method, "BFGS")
  expect_true(is.function(ctrl$optimizer))
})

test_that("optimx constructors pass the method named in their suffix", {
  expect_equal(glmmTMB_control_optimx_nvm()$optArgs$method, "nvm")
  expect_equal(glmmTMB_control_optimx_lbfgsb()$optArgs$method, "L-BFGS-B")
})

test_that("calibrar control constructors build a glmmTMB control list", {
  ctrl <- glmmTMB_control_calibrar_nelder_mead()
  expect_type(ctrl, "list")
  expect_equal(ctrl$optArgs$method, "Nelder-Mead")
  expect_true(is.function(ctrl$optimizer))
})

test_that("optimh control constructors build a glmmTMB control list", {
  ctrl <- glmmTMB_control_optimh_cmaes()
  expect_type(ctrl, "list")
  expect_equal(ctrl$optArgs$method, "CMA-ES")
  expect_true(is.function(ctrl$optimizer))
})

test_that("the private control builders require a method", {
  expect_error(glmmTMB_control_optimx(method = NULL))
  expect_error(glmmTMB_control_calibrar(method = NULL))
  expect_error(glmmTMB_control_optimh(method = NULL))
})

test_that("extra optArgs are merged with the method", {
  ctrl <- glmmTMB_control_optimx_bfgs(optArgs = list(hessian = TRUE))
  expect_equal(ctrl$optArgs$method, "BFGS")
  expect_true(ctrl$optArgs$hessian)
})
