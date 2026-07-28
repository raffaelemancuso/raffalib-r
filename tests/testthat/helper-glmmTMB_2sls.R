# Helpers + data-generating processes for the glmmTMB_2sls validation tests.
# In every DGP: z is the instrument, w an exogenous control, u an UNOBSERVED
# confounder that enters both x (the endogenous regressor) and the outcome, so
# naive regression of the outcome on x is biased while IV/CF is consistent.

# author-tuned tolerance: |a - b| <= tol * (1 + |b|)
approx2 <- function(a, b, tol = 1e-3) isTRUE(abs(a - b) <= tol * (1 + abs(b)))

# pull a fixed-effect coefficient from a fitted glmmTMB_2sls via the tidy method
cf_coef <- function(m, term, comp = "conditional") {
  td <- generics::tidy(m)
  v  <- td$estimate[td$component == comp & td$term == term]
  if (length(v) == 1L) v else NA_real_
}

sim_linear <- function(n = 4000, beta = 1.5, rho = 0.7, pi_z = 1.0, seed = 1) {
  set.seed(seed)
  z <- rnorm(n); w <- rnorm(n); u <- rnorm(n)
  x <- 0.5 + pi_z * z + 0.5 * w + rho * u + rnorm(n)
  y <- 1.0 + beta * x - 0.8 * w + rho * u + rnorm(n)
  data.frame(y, x, z, w)
}
sim_count <- function(n = 6000, beta = 0.4, rho = 0.6, seed = 4) {   # Poisson
  set.seed(seed)
  z <- rnorm(n); w <- rnorm(n); u <- rnorm(n)
  x   <- 0.6 * z + 0.3 * w + rho * u + rnorm(n)
  eta <- -0.3 + beta * x + 0.3 * w + rho * u
  data.frame(y = rpois(n, exp(eta)), x, z, w)
}
sim_bin <- function(n = 9000, beta = 0.8, rho = 0.6, seed = 5) {     # logit
  set.seed(seed)
  z <- rnorm(n); w <- rnorm(n); u <- rnorm(n)
  x   <- 0.7 * z + 0.3 * w + rho * u + rnorm(n)
  eta <- -0.2 + beta * x + 0.3 * w + rho * u
  data.frame(y = rbinom(n, 1, plogis(eta)), x, z, w)
}
sim_clustered <- function(n_g = 250, m = 15, beta = 1.0, rho = 0.6, seed = 7) {
  set.seed(seed)
  g  <- rep(seq_len(n_g), each = m); N <- n_g * m
  ag <- rnorm(n_g, sd = 1.3)[g]                 # shared cluster shock -> within-cluster corr
  z <- rnorm(N); w <- rnorm(N); u <- rnorm(N)
  x <- 0.7 * z + 0.3 * w + rho * u + rnorm(N)
  y <- 1.0 + beta * x - 0.5 * w + rho * u + ag + rnorm(N)
  data.frame(y, x, z, w, g = factor(g))
}
# Over-identified linear IV: two instruments (z1, z2) for one endogenous x, plus
# an exogenous control w. `gamma` gives z2 a DIRECT effect on the outcome, which
# violates the exclusion restriction and is precisely what a Sargan test exists
# to detect; gamma = 0 leaves both instruments valid. `hetero = TRUE` makes the
# structural error's variance depend on w, so iid and HC1 standard errors part
# company and a bootstrap should track the latter.
sim_overid <- function(n = 3000, beta = 1.5, rho = 0.7, gamma = 0,
                       hetero = FALSE, seed = 42) {
  set.seed(seed)
  z1 <- rnorm(n); z2 <- rnorm(n); w <- rnorm(n); u <- rnorm(n)
  x  <- 0.5 + 0.8 * z1 + 0.6 * z2 + 0.5 * w + rho * u + rnorm(n)
  s  <- if (hetero) exp(0.5 * w) else 1
  y  <- 1.0 + beta * x - 0.8 * w + gamma * z2 + rho * u + s * rnorm(n)
  data.frame(y, x, z1, z2, w)
}

# ivreg's diagnostics table: rows "Weak instruments", "Wu-Hausman", "Sargan",
# columns "df1", "df2", "statistic", "p-value".
iv_diag <- function(fit) summary(fit, diagnostics = TRUE)$diagnostics

# residual SD on each fit's own residual df -- the quantity that separates the
# augmented control-function regression's SEs from the 2SLS ones
resid_sd <- function(fit) sqrt(sum(stats::residuals(fit)^2) / stats::df.residual(fit))

# glmmTMB estimates sigma by ML (RSS/n) where lm/ivreg use RSS/(n - p), so every
# glmmTMB-based SE is smaller by sqrt((n - p)/n) and every Wald chi2 built from
# one is larger by n/(n - p). p counts the augmented second-stage fixed effects,
# the control-function term included.
ml_df_factor <- function(m) {
  n <- stats::nobs(m$second_stage)
  n / (n - nrow(m$coeftable))
}

sim_zinb <- function(n = 5000, beta = 0.3, rho = 0.5, seed = 8) {
  set.seed(seed)
  z <- rnorm(n); w <- rnorm(n); u <- rnorm(n)
  x  <- 0.6 * z + 0.3 * w + rho * u + rnorm(n)
  mu <- exp(-0.2 + beta * x + 0.2 * w + rho * u)
  y  <- rnbinom(n, size = 2, mu = mu)
  y[rbinom(n, 1, plogis(-0.4 + 0.5 * x)) == 1] <- 0   # extra structural zeros, x-dependent
  data.frame(y, x, z, w)
}
