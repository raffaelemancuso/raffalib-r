# Validation tests for glmmTMB_2sls() on toy datasets with a KNOWN data-
# generating process. Each test checks the function against an independent
# benchmark (textbook 2SLS, hand-rolled control-function GLMs, and the true
# structural coefficient). DGPs and helpers live in helper-glmmTMB_2sls.R.

test_that("Gaussian CF-2SLS equals textbook 2SLS and hand-rolled linear CF", {
  skip_if_not_installed("fixest")
  d <- sim_linear()
  m <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = gaussian(), n_boot = 0)
  b_cf <- cf_coef(m, "x")
  # (a) textbook 2SLS via fixest
  b_iv <- coef(fixest::feols(y ~ w | x ~ z, data = d))[["fit_x"]]
  expect_true(approx2(b_cf, b_iv, 1e-3), info = sprintf("CF=%.6f feols=%.6f", b_cf, b_iv))
  # (b) hand-rolled linear control function
  d$.v <- resid(lm(x ~ z + w, d))
  b_man <- coef(lm(y ~ x + w + .v, d))[["x"]]
  expect_true(approx2(b_cf, b_man, 1e-3), info = sprintf("CF=%.6f manual=%.6f", b_cf, b_man))
})

test_that("Gaussian IV recovers true beta while naive OLS is biased", {
  d <- sim_linear(n = 8000, beta = 1.5, seed = 2)
  b_iv  <- cf_coef(glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d,
                                family = gaussian(), n_boot = 0), "x")
  b_ols <- coef(lm(y ~ x + w, d))[["x"]]
  expect_lt(abs(b_iv - 1.5), 0.1)
  expect_gt(abs(b_ols - 1.5), abs(b_iv - 1.5) + 0.05)
})

test_that("Wu-Hausman flags endogeneity and stays null under exogeneity", {
  d_endo <- sim_linear(n = 6000, rho = 0.7, seed = 3)
  p_endo <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d_endo,
                         family = gaussian(), n_boot = 0)$endogeneity_test$p.value
  expect_lt(p_endo, 0.01)
  # exogenous: x no longer shares the confounder; median p over 3 seeds avoids
  # the ~5% chance a single null draw lands below 0.05.
  p_exo <- median(vapply(1:3, function(s) {
    de <- sim_linear(n = 6000, rho = 0.0, seed = 30 + s)
    glmmTMB_2sls(x ~ z + w, y ~ x + w, data = de,
                 family = gaussian(), n_boot = 0)$endogeneity_test$p.value
  }, numeric(1)))
  expect_gt(p_exo, 0.05)
})

test_that("Poisson CF equals hand-rolled CF Poisson and recovers beta", {
  d <- sim_count(beta = 0.4)
  m  <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = poisson(), n_boot = 0)
  b_cf <- cf_coef(m, "x")
  d$.v <- resid(lm(x ~ z + w, d))
  b_man   <- coef(glm(y ~ x + w + .v, family = poisson, data = d))[["x"]]
  b_naive <- coef(glm(y ~ x + w,      family = poisson, data = d))[["x"]]
  expect_true(approx2(b_cf, b_man, 3e-3), info = sprintf("CF=%.5f manual=%.5f", b_cf, b_man))
  expect_lt(abs(b_cf - 0.4), 0.08)
  expect_gt(abs(b_naive - 0.4), abs(b_cf - 0.4))
})

test_that("Logit CF equals hand-rolled CF logit (Rivers-Vuong)", {
  d <- sim_bin(beta = 0.8)
  m  <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = binomial(), n_boot = 0)
  b_cf <- cf_coef(m, "x")
  d$.v <- resid(lm(x ~ z + w, d))
  b_man <- coef(glm(y ~ x + w + .v, family = binomial, data = d))[["x"]]
  expect_true(approx2(b_cf, b_man, 3e-3), info = sprintf("CF=%.5f manual=%.5f", b_cf, b_man))
  expect_true(b_cf > 0.3 && b_cf < 1.5, info = sprintf("CF=%.3f (true=0.8)", b_cf))
})

test_that("parallel matches serial point estimate; bootstrap SE ~ analytic SE", {
  skip_if_not_installed("fixest")
  d <- sim_linear(n = 3000, seed = 6)
  m_ser <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = gaussian(),
                        n_boot = 80, parallel = FALSE, seed = 123)
  # bootstrap SE within 30% of the 2SLS analytic SE
  se_boot <- m_ser$coeftable["cond::x", "std.error"]
  se_iv   <- sqrt(diag(vcov(fixest::feols(y ~ w | x ~ z, data = d, vcov = "iid"))))[["fit_x"]]
  r <- se_boot / se_iv
  expect_true(r > 0.7 && r < 1.4, info = sprintf("boot=%.4f iv=%.4f ratio=%.2f", se_boot, se_iv, r))
  # parallel must give the identical point estimate (same seed)
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)
  m_par <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = gaussian(),
                        n_boot = 80, parallel = TRUE, seed = 123)
  expect_true(approx2(cf_coef(m_ser, "x"), cf_coef(m_par, "x"), 1e-8),
              info = sprintf("ser=%.8f par=%.8f", cf_coef(m_ser, "x"), cf_coef(m_par, "x")))
})

test_that("cluster bootstrap SE exceeds iid bootstrap SE under clustering", {
  d <- sim_clustered()
  se_iid  <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = gaussian(),
                          n_boot = 120, cluster = NULL, parallel = FALSE,
                          seed = 11)$coeftable["cond::x", "std.error"]
  se_clus <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = gaussian(),
                          n_boot = 120, cluster = "g", parallel = FALSE,
                          seed = 11)$coeftable["cond::x", "std.error"]
  expect_gt(se_clus, se_iid)
})

test_that("zero-inflation is surfaced by tidy(); glance flags log link", {
  d <- sim_zinb()
  m  <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = glmmTMB::nbinom2(),
                     ziformula = ~ x + w, n_boot = 0)
  td <- generics::tidy(m)
  expect_true(any(td$component == "conditional"))
  expect_true(any(td$component == "zero_inflated"))
  gl <- generics::glance(m)
  expect_identical(gl$link, "log")
})

test_that("tidy/glance/nobs are well-formed and weak instrument is detected", {
  d <- sim_linear(n = 2500, seed = 9)
  m <- glmmTMB_2sls(x ~ z + w, y ~ x + w, data = d, family = gaussian(),
                    instruments = "z", n_boot = 40, parallel = FALSE, seed = 9)
  td <- generics::tidy(m); gl <- generics::glance(m)
  expect_true(all(c("component", "term", "estimate", "std.error", "p.value") %in% names(td)))
  expect_false(any(td$term == m$cf_name))   # control-function residual hidden from tidy
  expect_true(all(c("nobs", "weak.inst", "wu.haus") %in% names(gl)))
  expect_equal(nobs(m), nrow(d))
  expect_lt(gl$weak.inst, 1e-6)
})
