# Cross-implementation validation of glmmTMB_2sls(). Where the control function
# is provably textbook 2SLS -- an identity-link Gaussian second stage fitted off
# an OLS first stage -- every number it reports has a closed-form counterpart in
# an established IV package, so these are checks of NUMERICAL IDENTITY against
# ivreg, AER, fixest, estimatr, lfe and plm, not "close enough" comparisons. The
# residual gap is glmmTMB's optimiser tolerance (~1e-6 relative), not a
# difference between estimators; tolerances are set an order of magnitude above
# it. The non-linear path (Poisson, logit) is validated against ivtools, whose
# ivglm(estmethod = "ts", ctrl = TRUE) is the same control-function estimator
# and whose stacked estimating-equation sandwich is the analytic counterpart of
# the bootstrap standard errors.
#
# Two deliberate discrepancies are asserted rather than papered over:
#   * glmmTMB fits sigma by ML (RSS/n) where lm/ivreg use RSS/(n - p). Its naive
#     SEs are therefore smaller by exactly sqrt((n - p)/n) and its Wald chi2
#     larger by exactly n/(n - p) than the OLS/F versions the other packages
#     report. See ml_df_factor() in helper-glmmTMB_2sls.R.
#   * OLS SEs from the augmented control-function regression are NOT the 2SLS
#     SEs: the augmented regression's error is the structural error purged of
#     the part the control function explains. The two differ by exactly the
#     ratio of residual standard deviations.
#
# DGPs and helpers live in helper-glmmTMB_2sls.R.

ss_formula_iv <- y ~ x + w                      # structural equation, shared below
fs_formula_iv <- x ~ z1 + z2 + w                # over-identified first stage

# coefficients keyed by bare term name, control-function residual dropped
cf_beta <- function(m) {
  b <- stats::setNames(m$coeftable$estimate, sub("^cond::", "", rownames(m$coeftable)))
  b[c("(Intercept)", "x", "w")]
}
cf_se <- function(m) {
  s <- stats::setNames(m$coeftable$std.error, sub("^cond::", "", rownames(m$coeftable)))
  s[c("(Intercept)", "x", "w")]
}


# --- coefficients -----------------------------------------------------------

test_that("over-identified CF coefficients equal ivreg, AER, fixest and estimatr", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("fixest")
  d <- sim_overid()
  m <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  b <- cf_beta(m)

  bench <- list(
    ivreg  = coef(ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d)),
    fixest = stats::setNames(
      coef(fixest::feols(y ~ w | x ~ z1 + z2, data = d))[c("(Intercept)", "fit_x", "w")],
      c("(Intercept)", "x", "w"))
  )
  # AER re-registers ivreg's S3 methods when its namespace loads; the estimates
  # are unaffected, but keep the announcement out of the test log
  if (suppressMessages(requireNamespace("AER", quietly = TRUE)))
    bench$AER <- coef(AER::ivreg(y ~ x + w | z1 + z2 + w, data = d))
  if (requireNamespace("estimatr", quietly = TRUE))
    bench$estimatr <- estimatr::iv_robust(y ~ x + w | z1 + z2 + w, data = d,
                                          se_type = "classical")$coefficients

  for (nm in names(bench)) for (k in names(b))
    expect_true(approx2(b[[k]], bench[[nm]][[k]], 1e-4),
                info = sprintf("%s coef %s: CF=%.8f %s=%.8f", nm, k, b[[k]], nm, bench[[nm]][[k]]))
})

test_that("just-identified CF equals ivreg and the Wald ratio estimator", {
  skip_if_not_installed("ivreg")
  d <- sim_overid()
  m <- glmmTMB_2sls(x ~ z1 + w, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  b_cf <- cf_beta(m)[["x"]]
  b_iv <- coef(ivreg::ivreg(y ~ x + w | z1 + w, data = d))[["x"]]
  # with one instrument 2SLS collapses to the ratio of the reduced-form to the
  # first-stage coefficient on z1, both taken net of the exogenous control
  b_wald <- coef(lm(y ~ z1 + w, d))[["z1"]] / coef(lm(x ~ z1 + w, d))[["z1"]]
  expect_true(approx2(b_cf, b_iv, 1e-4), info = sprintf("CF=%.8f ivreg=%.8f", b_cf, b_iv))
  expect_true(approx2(b_cf, b_wald, 1e-4), info = sprintf("CF=%.8f Wald=%.8f", b_cf, b_wald))
})


# --- standard errors --------------------------------------------------------

test_that("analytic SEs equal ivreg's classical 2SLS SEs", {
  skip_if_not_installed("ivreg")
  d  <- sim_overid()
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian())
  iv <- ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d)
  expect_identical(m$vcov_type, "iid")           # the default without a cluster
  expect_match(m$coeftable$se_type[1], "analytic 2SLS \\(iid\\)")
  se_iv <- coef(summary(iv))[, "Std. Error"]
  for (k in names(se_iv))
    expect_true(approx2(cf_se(m)[[k]], se_iv[[k]], 1e-4),
                info = sprintf("%s: CF=%.8f ivreg=%.8f", k, cf_se(m)[[k]], se_iv[[k]]))
  # Wald intervals come with them, on the same scale
  ci <- m$coeftable[seq_along(se_iv), c("conf.low", "conf.high")]
  expect_true(all(ci$conf.low < m$coeftable$estimate[seq_along(se_iv)]))
  expect_true(all(ci$conf.high > m$coeftable$estimate[seq_along(se_iv)]))
  expect_equal(ci$conf.high - ci$conf.low, 2 * qnorm(0.975) * unname(cf_se(m)))
})

test_that("robust analytic SEs equal estimatr's and fixest's HC0/HC1", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("fixest")
  d <- sim_overid(hetero = TRUE, seed = 19)
  for (type in c("HC0", "HC1")) {
    m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(),
                       vcov_type = type)
    se <- estimatr::iv_robust(y ~ x + w | z1 + z2 + w, data = d, se_type = type)$std.error
    for (i in seq_along(se))
      expect_true(approx2(cf_se(m)[[i]], se[[i]], 1e-4),
                  info = sprintf("%s %d: CF=%.8f estimatr=%.8f",
                                 type, i, cf_se(m)[[i]], se[[i]]))
  }
  # fixest's "hetero" vcov is HC1 for a 2SLS fit
  m1 <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(),
                     vcov_type = "HC1")
  se_fe <- fixest::se(fixest::feols(y ~ w | x ~ z1 + z2, data = d, vcov = "hetero"))
  se_fe <- stats::setNames(se_fe[c("(Intercept)", "fit_x", "w")], c("(Intercept)", "x", "w"))
  for (k in names(se_fe))
    expect_true(approx2(cf_se(m1)[[k]], se_fe[[k]], 1e-4),
                info = sprintf("%s: CF=%.8f fixest=%.8f", k, cf_se(m1)[[k]], se_fe[[k]]))
})

test_that("clustered analytic SEs equal estimatr's CR1 and fixest's", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("fixest")
  d <- sim_overid(hetero = TRUE, seed = 19)
  d$g <- factor(rep(seq_len(120), length.out = nrow(d)))
  m <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(),
                    cluster = "g")
  expect_identical(m$vcov_type, "cluster")       # auto-selected by `cluster`
  expect_match(m$coeftable$se_type[1], "cluster on g, 120 groups")
  se_es <- estimatr::iv_robust(y ~ x + w | z1 + z2 + w, data = d, clusters = g,
                               se_type = "stata")$std.error
  se_fe <- fixest::se(fixest::feols(y ~ w | x ~ z1 + z2, data = d, cluster = ~ g))
  se_fe <- stats::setNames(se_fe[c("(Intercept)", "fit_x", "w")], c("(Intercept)", "x", "w"))
  for (i in seq_along(se_fe)) {
    expect_true(approx2(cf_se(m)[[i]], se_es[[i]], 1e-4),
                info = sprintf("estimatr %d: CF=%.8f CR1=%.8f", i, cf_se(m)[[i]], se_es[[i]]))
    expect_true(approx2(cf_se(m)[[i]], se_fe[[i]], 1e-4),
                info = sprintf("fixest %d: CF=%.8f cl=%.8f", i, cf_se(m)[[i]], se_fe[[i]]))
  }
})

test_that("clustering widens the analytic SEs when clusters share a shock", {
  skip_if_not_installed("estimatr")
  # sim_overid()'s groups are arbitrary, so CR1 need not exceed the iid SE
  # there; sim_clustered() has a genuine shared cluster shock, where it must
  d <- sim_clustered()
  m_cl  <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = gaussian(),
                        cluster = "g")
  m_iid <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = gaussian(),
                        vcov_type = "iid")
  expect_gt(cf_se(m_cl)[["x"]], cf_se(m_iid)[["x"]])
  se_es <- estimatr::iv_robust(y ~ x + w | z + w, data = d, clusters = g,
                               se_type = "stata")$std.error
  for (i in seq_along(se_es))
    expect_true(approx2(cf_se(m_cl)[[i]], se_es[[i]], 1e-4),
                info = sprintf("%d: CF=%.8f CR1=%.8f", i, cf_se(m_cl)[[i]], se_es[[i]]))
})

test_that("the second stage's model-based SEs are the augmented-OLS SEs up to the ML df factor", {
  # the naive SEs are no longer what the object reports, but they are still what
  # the underlying glmmTMB fit carries -- and they are exactly the augmented
  # control-function regression's OLS SEs rescaled from RSS/(n - p) to RSS/n
  d <- sim_overid()
  m <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian())
  se_naive <- summary(m$second_stage)$coefficients$cond[, "Std. Error"]
  d$.v <- resid(lm(x ~ z1 + z2 + w, d))
  aug  <- lm(y ~ x + w + .v, d)                 # same fit, by hand, with OLS sigma
  se_aug <- coef(summary(aug))[, "Std. Error"]  # order: (Intercept), x, w, .v
  expect_equal(length(se_aug), length(se_naive))
  for (i in seq_along(se_aug))
    expect_true(approx2(se_naive[[i]], se_aug[[i]] / sqrt(ml_df_factor(m)), 1e-4),
                info = sprintf("%d: glmmTMB=%.8f augOLS=%.8f", i, se_naive[[i]], se_aug[[i]]))
})

test_that("the naive SEs the analytic ones replace were too small, by a known factor", {
  skip_if_not_installed("ivreg")
  d  <- sim_overid()
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian())
  iv <- ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d)
  d$.v <- resid(lm(x ~ z1 + z2 + w, d))
  aug  <- lm(y ~ x + w + .v, d)
  se_naive <- summary(m$second_stage)$coefficients$cond[, "Std. Error"][1:3]
  # Var_2sls = sigma_u^2 (X'P_Z X)^-1 and Var_aug = sigma_e^2 (X'P_Z X)^-1 share
  # the same bread, so undoing the ML df factor and rescaling by the residual-SD
  # ratio turns the naive SEs into ivreg's, coefficient by coefficient
  rescaled <- se_naive * sqrt(ml_df_factor(m)) * resid_sd(iv) / resid_sd(aug)
  se_iv    <- coef(summary(iv))[, "Std. Error"]
  for (i in seq_along(se_iv))
    expect_true(approx2(rescaled[[i]], se_iv[[i]], 1e-4),
                info = sprintf("%d: rescaled=%.8f ivreg=%.8f", i, rescaled[[i]], se_iv[[i]]))
  # the CF regression's error variance is the structural one net of what the
  # control function explains, so every naive SE understates its analytic
  # counterpart -- which is why the naive ones are not what the object reports
  expect_lt(resid_sd(aug), resid_sd(iv))
  expect_true(all(se_naive < cf_se(m)))
})

test_that("vcov_type is validated and reported honestly", {
  d <- sim_overid(n = 800, seed = 55)
  expect_error(glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(),
                            vcov_type = "cluster"), "needs a `cluster` column")
  expect_error(glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(),
                            vcov_type = "HC7"), "should be one of")
  # a non-linear second stage has no analytic form: say so rather than silently
  # handing back naive SEs under a robust-sounding label
  dp <- d; dp$y <- rpois(nrow(dp), exp(-0.3 + 0.3 * dp$x))
  expect_message(mp <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = dp,
                                    family = poisson(), vcov_type = "HC1"),
                 "no analytic covariance")
  expect_true(is.na(mp$vcov_type))
  expect_match(mp$coeftable$se_type[1], "NAIVE")
  # an explicit bootstrap still wins over the analytic form
  expect_message(mb <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d,
                                    family = gaussian(), vcov_type = "HC1",
                                    n_boot = 20, parallel = FALSE, seed = 2),
                 "bootstrap SEs were requested")
  expect_match(mb$coeftable$se_type[1], "cluster-bootstrap")
})

test_that("cluster-bootstrap SEs track the analytic 2SLS SEs (iid and HC1)", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("estimatr")
  d <- sim_overid(n = 2000, seed = 21)
  m <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(),
                    n_boot = 120, parallel = FALSE, seed = 5)
  se_iv <- coef(summary(ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d)))[, "Std. Error"]
  r <- cf_se(m) / se_iv
  expect_true(all(r > 0.7 & r < 1.4), info = paste(sprintf("%.3f", r), collapse = " "))

  # under heteroskedasticity the bootstrap should follow HC1, not the iid SE
  dh <- sim_overid(n = 2000, hetero = TRUE, seed = 21)
  mh <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = dh, family = gaussian(),
                     n_boot = 120, parallel = FALSE, seed = 5)
  se_hc <- estimatr::iv_robust(y ~ x + w | z1 + z2 + w, data = dh,
                               se_type = "HC1")$std.error
  rh <- cf_se(mh) / se_hc
  expect_true(all(rh > 0.7 & rh < 1.4), info = paste(sprintf("%.3f", rh), collapse = " "))
})


# --- weak-instrument test ---------------------------------------------------

test_that("weak-instrument F matches ivreg and fixest exactly", {
  skip_if_not_installed("ivreg")
  d  <- sim_overid()
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  dg <- iv_diag(ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d))["Weak instruments", ]
  wi <- m$weak_instrument
  # both sides are pure OLS here, so this is an exact identity
  expect_equal(wi$F, dg[["statistic"]], tolerance = 1e-10)
  expect_identical(as.integer(wi$df1), as.integer(dg[["df1"]]))
  expect_identical(as.integer(wi$df2), as.integer(dg[["df2"]]))
  expect_equal(wi$p.value.F, dg[["p-value"]], tolerance = 1e-10)
  # the Wald chi2 is the same test in large-sample form
  expect_equal(wi$statistic, wi$df1 * wi$F, tolerance = 1e-8)

  skip_if_not_installed("fixest")
  fs <- fixest::fitstat(fixest::feols(y ~ w | x ~ z1 + z2, data = d, vcov = "iid"), "ivwald")
  expect_equal(wi$F, fs[[1]]$stat, tolerance = 1e-8)
})

test_that("excluded instruments are auto-derived, factor instruments included", {
  skip_if_not_installed("ivreg")
  d <- sim_overid()
  # no `instruments` argument: they are the first-stage terms absent from the
  # structural equation, exactly as ivreg derives them
  auto <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  told <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(),
                       instruments = c("z1", "z2"), n_boot = 0)
  expect_identical(auto$weak_instrument$instruments, c("z1", "z2"))
  expect_equal(auto$weak_instrument$F, told$weak_instrument$F)

  # a factor instrument contributes several columns; df1 must count columns, not
  # terms, which the nested-RSS form gets right and name matching would not
  set.seed(3)
  d$zf <- factor(sample(letters[1:3], nrow(d), TRUE))
  d$xf <- d$x + 0.9 * (d$zf == "b") - 0.7 * (d$zf == "c")
  d$yf <- 1 + 1.5 * d$xf - 0.8 * d$w + rnorm(nrow(d))
  mf <- glmmTMB_2sls(xf ~ zf + w, yf ~ xf + w, data = d, family = gaussian(), n_boot = 0)
  dg <- iv_diag(ivreg::ivreg(yf ~ xf + w | zf + w, data = d))["Weak instruments", ]
  expect_identical(as.integer(mf$weak_instrument$df1), 2L)
  expect_equal(mf$weak_instrument$F, dg[["statistic"]], tolerance = 1e-10)
})

test_that("a non-linear first stage falls back to the Wald chi2 form", {
  d <- sim_overid(n = 1500, seed = 41)
  d$xb <- as.numeric(d$x > 0)                    # binary endogenous regressor
  m <- glmmTMB_2sls(xb ~ z1 + z2 + w, y ~ xb + w, data = d, family = gaussian(),
                    first_family = binomial(), n_boot = 0)
  wi <- m$weak_instrument
  expect_identical(wi$instruments, c("z1", "z2"))
  expect_null(wi$F)                              # no OLS RSS decomposition to build one
  expect_gt(wi$statistic, 0)
  expect_identical(as.integer(wi$df), 2L)
  expect_equal(generics::glance(m)$weak.inst, wi$p.value)
  expect_null(m$overid_test)                     # first stage is not a linear projection
  expect_match(paste(utils::capture.output(print(m)), collapse = "\n"),
               "Weak instruments \\(z1, z2\\): chi2\\(2\\)")
})

test_that("weak instruments are flagged as weak and strong ones are not", {
  skip_if_not_installed("ivreg")
  d_strong <- sim_overid(n = 2000, seed = 31)
  d_weak   <- d_strong
  set.seed(99)                                   # instruments with no first-stage signal
  d_weak$z1 <- rnorm(nrow(d_weak)); d_weak$z2 <- rnorm(nrow(d_weak))
  f_strong <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d_strong,
                           family = gaussian(), n_boot = 0)$weak_instrument$F
  f_weak   <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d_weak,
                           family = gaussian(), n_boot = 0)$weak_instrument$F
  expect_gt(f_strong, 100)                       # far past the Staiger-Stock rule of 10
  expect_lt(f_weak, 10)
})


# --- Wu-Hausman -------------------------------------------------------------

test_that("Wu-Hausman matches ivreg and fixest up to the ML df factor", {
  skip_if_not_installed("ivreg")
  d  <- sim_overid()
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  dg <- iv_diag(ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d))["Wu-Hausman", ]
  # ivreg's F and glmmTMB's Wald chi2 are the same test on the control-function
  # term; they differ only through the ML-vs-OLS error variance
  expect_true(approx2(m$endogeneity_test$statistic, dg[["statistic"]] * ml_df_factor(m), 1e-3),
              info = sprintf("chi2=%.6f F*n/(n-p)=%.6f",
                             m$endogeneity_test$statistic, dg[["statistic"]] * ml_df_factor(m)))
  expect_identical(as.integer(dg[["df1"]]), 1L)
  # and the F itself is the squared t on the residual in the augmented regression
  d$.v <- resid(lm(x ~ z1 + z2 + w, d))
  t_aug <- coef(summary(lm(y ~ x + w + .v, d)))[".v", "t value"]
  expect_equal(dg[["statistic"]], t_aug^2, tolerance = 1e-8)

  skip_if_not_installed("fixest")
  fs <- fixest::fitstat(fixest::feols(y ~ w | x ~ z1 + z2, data = d, vcov = "iid"), "wh")
  expect_equal(dg[["statistic"]], fs$wh$stat, tolerance = 1e-8)
})

test_that("Wu-Hausman agrees with ivreg on an exogenous regressor too", {
  skip_if_not_installed("ivreg")
  d <- sim_overid(rho = 0, seed = 77)            # x no longer shares the confounder
  m <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  dg <- iv_diag(ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d))["Wu-Hausman", ]
  expect_true(approx2(m$endogeneity_test$statistic, dg[["statistic"]] * ml_df_factor(m), 1e-3))
  expect_gt(m$endogeneity_test$p.value, 0.05)
  expect_gt(dg[["p-value"]], 0.05)
})


# --- Sargan -----------------------------------------------------------------

test_that("Sargan matches ivreg and fixest on valid instruments", {
  skip_if_not_installed("ivreg")
  d  <- sim_overid()
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  dg <- iv_diag(ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d))["Sargan", ]
  expect_true(approx2(m$overid_test$statistic, dg[["statistic"]], 1e-4),
              info = sprintf("CF=%.10f ivreg=%.10f", m$overid_test$statistic, dg[["statistic"]]))
  expect_identical(as.integer(m$overid_test$df), as.integer(dg[["df1"]]))
  expect_true(approx2(m$overid_test$p.value, dg[["p-value"]], 1e-4))
  expect_gt(m$overid_test$p.value, 0.05)         # instruments really are valid here

  skip_if_not_installed("fixest")
  fs <- fixest::fitstat(fixest::feols(y ~ w | x ~ z1 + z2, data = d, vcov = "iid"), "sargan")
  expect_true(approx2(m$overid_test$statistic, fs$sargan$stat, 1e-4))
  expect_identical(as.integer(m$overid_test$df), as.integer(fs$sargan$df))
})

test_that("Sargan rejects an instrument that violates the exclusion restriction", {
  skip_if_not_installed("ivreg")
  d  <- sim_overid(gamma = 0.5, seed = 13)       # z2 acts on y directly
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  dg <- iv_diag(ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d))["Sargan", ]
  expect_lt(m$overid_test$p.value, 0.01)
  expect_true(approx2(m$overid_test$statistic, dg[["statistic"]], 1e-4),
              info = sprintf("CF=%.6f ivreg=%.6f", m$overid_test$statistic, dg[["statistic"]]))
  # the same DGP dropping the bad instrument is exactly identified: nothing left
  # to test, and no statistic should be invented
  m1 <- glmmTMB_2sls(x ~ z1 + w, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  expect_null(m1$overid_test)
})

test_that("Sargan is NULL where the control function is not 2SLS", {
  d <- sim_overid()
  # exactly identified: no over-identifying restrictions
  expect_null(glmmTMB_2sls(x ~ z1 + w, ss_formula_iv, data = d,
                           family = gaussian(), n_boot = 0)$overid_test)
  # non-linear second stage: the second-stage residual is not the structural error
  dp <- d; dp$y <- rpois(nrow(dp), exp(-0.3 + 0.3 * dp$x))
  mp <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = dp, family = poisson(), n_boot = 0)
  expect_null(mp$overid_test)
  expect_false(is.null(mp$weak_instrument$F))    # the first stage is still OLS
  # mixed second stage: likewise not a linear projection
  d$g <- factor(rep(seq_len(150), length.out = nrow(d)))
  mm <- glmmTMB_2sls(fs_formula_iv, y ~ x + w + (1 | g), data = d,
                     family = gaussian(), n_boot = 0)
  expect_null(mm$overid_test)
})


# --- first stage, missing data, glance --------------------------------------

test_that("the stored first stage matches lm and fixest", {
  skip_if_not_installed("fixest")
  d <- sim_overid()
  m <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  expect_s3_class(m$first_stage, "lm")
  expect_equal(coef(m$first_stage), coef(lm(x ~ z1 + z2 + w, d)))
  b_fe <- coef(fixest::feols(x ~ z1 + z2 + w, data = d))
  for (k in names(b_fe))
    expect_true(approx2(coef(m$first_stage)[[k]], b_fe[[k]], 1e-8))
})

test_that("listwise deletion reproduces ivreg on the same complete cases", {
  skip_if_not_installed("ivreg")
  d <- sim_overid()
  d$w[c(3, 50, 900)] <- NA; d$y[7] <- NA
  m  <- suppressMessages(glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d,
                                      family = gaussian(), n_boot = 0))
  iv <- ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d)      # na.action = na.omit
  expect_equal(nobs(m), nobs(iv))
  expect_equal(nobs(m), nrow(d) - 4L)
  b <- cf_beta(m); b_iv <- coef(iv)
  for (k in names(b))
    expect_true(approx2(b[[k]], b_iv[[k]], 1e-4),
                info = sprintf("%s: CF=%.8f ivreg=%.8f", k, b[[k]], b_iv[[k]]))
})

test_that("glance reports the three diagnostics with ivreg's p-values", {
  skip_if_not_installed("ivreg")
  d  <- sim_overid()
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  gl <- generics::glance(m)
  dg <- iv_diag(ivreg::ivreg(y ~ x + w | z1 + z2 + w, data = d))
  expect_true(all(c("nobs", "weak.inst", "wu.haus", "sargan") %in% names(gl)))
  expect_equal(gl$weak.inst, dg["Weak instruments", "p-value"], tolerance = 1e-10)
  expect_true(approx2(gl$sargan, dg["Sargan", "p-value"], 1e-4))
  # Wu-Hausman is the chi2 p-value against ivreg's F p-value: the same test in
  # its large-sample form, so they agree in order of magnitude, not to the digit
  expect_lt(abs(log10(gl$wu.haus) - log10(dg["Wu-Hausman", "p-value"])), 1)
  # a non-linear fit has no Sargan to report
  dp <- d; dp$y <- rpois(nrow(dp), exp(-0.3 + 0.3 * dp$x))
  expect_true(is.na(generics::glance(
    glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = dp, family = poisson(),
                 n_boot = 0))$sargan))
})

test_that("print() surfaces all three diagnostics", {
  d <- sim_overid()
  m <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian(), n_boot = 0)
  out <- paste(utils::capture.output(print(m)), collapse = "\n")
  expect_match(out, "Weak instruments \\(z1, z2\\): F\\(2, ")
  expect_match(out, "Wu-Hausman")
  expect_match(out, "Overidentification \\(Sargan\\)")
})


# --- further implementations: lfe, plm --------------------------------------

test_that("linear CF equals lfe::felm, coefficients and iid SEs", {
  skip_if_not_installed("lfe")
  d  <- sim_overid()
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian())
  fe <- lfe::felm(y ~ w | 0 | (x ~ z1 + z2), data = d)
  b  <- coef(fe); se <- sqrt(diag(vcov(fe)))       # endogenous term is "`x(fit)`"
  key <- c("(Intercept)" = "(Intercept)", x = "`x(fit)`", w = "w")
  for (k in names(key)) {
    expect_true(approx2(cf_beta(m)[[k]], b[[key[[k]]]], 1e-4),
                info = sprintf("coef %s: CF=%.8f felm=%.8f", k, cf_beta(m)[[k]], b[[key[[k]]]]))
    expect_true(approx2(cf_se(m)[[k]], se[[key[[k]]]], 1e-4),
                info = sprintf("se %s: CF=%.8f felm=%.8f", k, cf_se(m)[[k]], se[[key[[k]]]]))
  }
})

test_that("linear CF equals plm's pooled IV, coefficients and SEs", {
  skip_if_not_installed("plm")
  d <- sim_overid()
  d$id <- seq_len(nrow(d)); d$t <- 1L
  m  <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian())
  # pooled-model IV estimation (Balestra-Varadharajan-Krishnakumar collapses to
  # plain 2SLS when there is no panel structure to transform away)
  pm <- plm::plm(y ~ x + w | z1 + z2 + w, data = d, model = "pooling",
                 index = c("id", "t"))
  ct <- coef(summary(pm))
  for (k in names(cf_beta(m))) {
    expect_true(approx2(cf_beta(m)[[k]], ct[k, "Estimate"], 1e-4),
                info = sprintf("coef %s: CF=%.8f plm=%.8f", k, cf_beta(m)[[k]], ct[k, "Estimate"]))
    expect_true(approx2(cf_se(m)[[k]], ct[k, "Std. Error"], 1e-4),
                info = sprintf("se %s: CF=%.8f plm=%.8f", k, cf_se(m)[[k]], ct[k, "Std. Error"]))
  }
})


# --- the non-linear path against ivtools ------------------------------------
# ivtools::ivglm(estmethod = "ts", ctrl = TRUE) is the same control-function
# estimator, parameterised on the first-stage FITTED VALUES plus the residual
# instead of the raw regressor plus the residual: substituting x = xhat + r
# shows the two models are identical, with ivtools' R coefficient equal to
# b_x + b_cf and every structural coefficient unchanged. That identity is
# asserted too. Its variance comes from stacking both stages' estimating
# equations into one sandwich, so it accounts for the generated regressor
# analytically -- the benchmark the bootstrap SEs are checked against, on the
# path where raffalib has no analytic form of its own.

test_that("linear CF equals ivtools' control function, reparameterisation included", {
  skip_if_not_installed("ivtools")
  d <- sim_overid()
  m <- glmmTMB_2sls(fs_formula_iv, ss_formula_iv, data = d, family = gaussian())
  ts <- ivtools::ivglm(estmethod = "ts",
                       fitX.LZ = glm(x ~ z1 + z2 + w, data = d),
                       fitY.LX = glm(y ~ x + w, data = d),
                       data = d, ctrl = TRUE)
  for (k in names(cf_beta(m)))
    expect_true(approx2(cf_beta(m)[[k]], ts$est[[k]], 1e-4),
                info = sprintf("coef %s: CF=%.8f ivtools=%.8f", k, cf_beta(m)[[k]], ts$est[[k]]))
  b_cf <- m$coeftable["cond::cf_resid", "estimate"]
  expect_true(approx2(cf_beta(m)[["x"]] + b_cf, ts$est[["R"]], 1e-4),
              info = sprintf("b_x+b_cf=%.8f ivtools R=%.8f",
                             cf_beta(m)[["x"]] + b_cf, ts$est[["R"]]))
})

test_that("Poisson CF equals ivtools; bootstrap SEs track its two-stage sandwich", {
  skip_if_not_installed("ivtools")
  d <- sim_count(beta = 0.4)
  m <- glmmTMB_2sls(x ~ z + w, ss_formula_iv, data = d, family = poisson(),
                    n_boot = 150, parallel = FALSE, seed = 11)
  ts <- ivtools::ivglm(estmethod = "ts",
                       fitX.LZ = glm(x ~ z + w, data = d),
                       fitY.LX = glm(y ~ x + w, family = poisson, data = d),
                       data = d, ctrl = TRUE)
  se_ts <- sqrt(diag(ts$vcov))
  for (k in names(cf_beta(m))) {
    expect_true(approx2(cf_beta(m)[[k]], ts$est[[k]], 1e-4),
                info = sprintf("coef %s: CF=%.8f ivtools=%.8f", k, cf_beta(m)[[k]], ts$est[[k]]))
    r <- cf_se(m)[[k]] / se_ts[[k]]
    expect_true(r > 0.8 && r < 1.25,
                info = sprintf("se %s: boot=%.6f sandwich=%.6f ratio=%.3f",
                               k, cf_se(m)[[k]], se_ts[[k]], r))
  }
  # and the naive SE on the endogenous regressor really is too small: the
  # sandwich propagates first-stage noise that the model-based SE ignores
  se_naive <- summary(m$second_stage)$coefficients$cond["x", "Std. Error"]
  expect_lt(se_naive, se_ts[["x"]])
})

test_that("logit CF equals ivtools (Rivers-Vuong); bootstrap SEs track its sandwich", {
  skip_if_not_installed("ivtools")
  d <- sim_bin(beta = 0.8)
  m <- glmmTMB_2sls(x ~ z + w, ss_formula_iv, data = d, family = binomial(),
                    n_boot = 150, parallel = FALSE, seed = 12)
  ts <- ivtools::ivglm(estmethod = "ts",
                       fitX.LZ = glm(x ~ z + w, data = d),
                       fitY.LX = glm(y ~ x + w, family = binomial, data = d),
                       data = d, ctrl = TRUE)
  se_ts <- sqrt(diag(ts$vcov))
  for (k in names(cf_beta(m))) {
    expect_true(approx2(cf_beta(m)[[k]], ts$est[[k]], 1e-4),
                info = sprintf("coef %s: CF=%.8f ivtools=%.8f", k, cf_beta(m)[[k]], ts$est[[k]]))
    r <- cf_se(m)[[k]] / se_ts[[k]]
    expect_true(r > 0.8 && r < 1.25,
                info = sprintf("se %s: boot=%.6f sandwich=%.6f ratio=%.3f",
                               k, cf_se(m)[[k]], se_ts[[k]], r))
  }
})
