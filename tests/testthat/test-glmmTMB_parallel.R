# Tests for fit_glmmTMB_parallel() in R/glmmTMB_parallel.R.
#
# fit_glmmTMB_parallel() serializes each spec -- and with it any raffalib
# control/optimizer object -- to workers that do not load raffalib. Optimizer
# bodies must therefore call only base functions and fully-qualified pkg::fun;
# an unqualified raffalib helper (e.g. myinfo()) dies on the worker with
# "could not find function". These tests are the regression guard for that.
#
# Most of them fit real (small) models on a cluster, so they are slower than the
# rest of the suite.

# Fast, precise guard: names the offending function without spawning a cluster.
test_that("optimizer functions reference no raffalib objects (worker-safe)", {
  skip_if_not_installed("codetools")

  optimizers <- list(
    glmmTMB_nloptr_optim       = glmmTMB_nloptr_optim,
    glmmTMB_minqa_optim        = glmmTMB_minqa_optim,
    glmmTMB_lbfgsb3c_optim     = glmmTMB_lbfgsb3c_optim,
    glmmTMB_optimx_optim       = glmmTMB_optimx_optim,
    glmmTMB_calibrar_optimizer = glmmTMB_calibrar_optimizer,
    glmmTMB_optimh_optimizer   = glmmTMB_optimh_optimizer
  )
  raffalib_objects <- ls(asNamespace("raffalib"), all.names = TRUE)

  for (nm in names(optimizers)) {
    offenders <- intersect(
      codetools::findGlobals(optimizers[[nm]], merge = TRUE),
      raffalib_objects
    )
    expect_identical(
      offenders, character(0),
      info = paste0(nm, "() calls raffalib object(s) [",
                    paste(offenders, collapse = ", "),
                    "] unqualified; this breaks fit_glmmTMB_parallel() workers")
    )
  }
})

test_that("fit_glmmTMB_parallel returns one fit per spec, named", {
  skip_on_cran()

  d <- glmmTMB::Salamanders[1:120, ]
  specs <- list(
    a = list(formula = count ~ mined + (1 | site), family = poisson),
    b = list(formula = count ~ mined + (1 | site), family = poisson)
  )
  mods <- fit_glmmTMB_parallel(specs, data = d, ncores = 2)

  expect_length(mods, 2)
  expect_named(mods, c("a", "b"))
  expect_s3_class(mods[[1]], "glmmTMB")
  # identical specs must give identical fits
  expect_equal(as.numeric(logLik(mods[[1]])), as.numeric(logLik(mods[[2]])))
})

test_that("raffalib optimizer controls survive the trip to bare workers", {
  skip_on_cran()

  d <- glmmTMB::Salamanders[1:120, ]
  specs <- list(
    a = list(formula = count ~ mined + (1 | site), family = poisson),
    b = list(formula = count ~ mined + (1 | site), family = poisson)
  )

  controls <- list(
    nloptr   = glmmTMB_control_nloptr_ln_bobyqa(),
    minqa    = glmmTMB_control_minqa_bobyqa(),
    optimx   = glmmTMB_control_optimx_nlminb(),
    lbfgsb3c = glmmTMB_control_lbfgsb3c(),
    calibrar = glmmTMB_control_calibrar_nelder_mead(),
    optimh   = glmmTMB_control_optimh_nmk()
  )

  for (nm in names(controls)) {
    mods <- fit_glmmTMB_parallel(specs, data = d, ncores = 2,
                                 control = controls[[nm]])
    expect_s3_class(mods[[1]], "glmmTMB")
    expect_true(is.finite(as.numeric(logLik(mods[[1]]))),
                info = paste(nm, "produced a non-finite logLik on the workers"))
  }
})

test_that("a package the workers cannot load is reported by name", {
  skip_on_cran()

  d <- glmmTMB::Salamanders[1:120, ]
  specs <- list(
    a = list(formula = count ~ mined + (1 | site), family = poisson),
    b = list(formula = count ~ mined + (1 | site), family = poisson)
  )
  expect_error(
    fit_glmmTMB_parallel(specs, data = d, ncores = 2,
                         packages = "definitelyNotAPackage123"),
    "definitelyNotAPackage123"
  )
})

test_that("ncores <= 1 runs serially without building a cluster", {
  d <- glmmTMB::Salamanders[1:120, ]
  specs <- list(a = list(formula = count ~ mined + (1 | site), family = poisson))
  mods <- fit_glmmTMB_parallel(specs, data = d, ncores = 1)

  expect_length(mods, 1)
  expect_s3_class(mods[[1]], "glmmTMB")
})
