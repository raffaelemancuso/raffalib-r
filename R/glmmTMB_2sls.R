#!/usr/bin/env R

# ============================================================================
# Control-function 2SLS with a glmmTMB second stage
# ----------------------------------------------------------------------------
# Estimates an instrumental-variable model whose second stage is a (possibly
# non-Gaussian, possibly mixed) glmmTMB model, via the control-function
# representation of 2SLS:
#   1. First stage: regress the endogenous regressor on the instrument(s) and
#      all exogenous controls; take the residual v_hat.
#   2. Second stage: fit the structural glmmTMB model on the ORIGINAL
#      endogenous regressor PLUS v_hat as an extra covariate.
#
# For a Gaussian second stage this reproduces textbook 2SLS exactly; for a
# count/binary second stage it is the consistent alternative to the
# "forbidden regression" (plugging first-stage fitted values into a non-linear
# model). The coefficient on v_hat is a Durbin-Wu-Hausman endogeneity test, and
# where the model is linear (so the control function IS 2SLS) an over-identified
# fit also carries a Sargan test of the over-identifying restrictions.
#
# Because v_hat is a GENERATED regressor, naive second-stage SEs are invalid.
# Where the fit is linear the control function IS 2SLS, so the exact analytic
# 2SLS covariance applies (iid, HC0/HC1 or clustered) and is used by default --
# no bootstrap needed, because the 2SLS sandwich already accounts for the
# generated regressor. Elsewhere, `n_boot > 0` returns cluster-bootstrap SEs
# that propagate first-stage uncertainty. The bootstrap is parallelised
# across replicates (respecting the
# caller's future::plan(); needs the `future.apply` package, else runs serially)
# and warm-started from the full-sample fit, so it is much faster without
# changing the estimates. `tidy`/`glance`/`nobs` methods let the result flow
# straight into modelsummary().
# ============================================================================

#' Control-function 2SLS with a glmmTMB second stage
#'
#' Estimates an instrumental-variable model whose second stage is a (possibly
#' non-Gaussian, possibly mixed) \code{glmmTMB} model via the control-function
#' representation of 2SLS. The first stage regresses the endogenous regressor on
#' the instrument(s) and exogenous controls; its residual is added as an extra
#' covariate in the structural second stage. For a Gaussian second stage this
#' reproduces textbook 2SLS; for count/binary second stages it is the consistent
#' alternative to the "forbidden regression". The coefficient on the residual is
#' a Durbin-Wu-Hausman endogeneity test.
#'
#' Because the control-function residual is a *generated* regressor, the naive
#' second-stage standard errors are invalid. Where the fit is linear throughout
#' — an identity-link Gaussian second stage with no random effects and no
#' zero-inflation, off an OLS first stage — the control function is numerically
#' identical to two-stage least squares, and the exact analytic 2SLS covariance
#' applies: the 2SLS sandwich already accounts for the generated regressor, so
#' no bootstrap is needed. Those standard errors are used automatically, in the
#' flavour chosen by `vcov_type`. Only a non-linear second stage needs
#' `n_boot > 0`. Statistics and confidence intervals are normal-based, as
#' `glmmTMB` reports them, where [ivreg::ivreg()] uses a *t* with `n - k` degrees
#' of freedom; the standard errors themselves are identical.
#'
#' A linear over-identified model also reports a Sargan test of the
#' over-identifying restrictions in `$overid_test`. It is `NULL` for exactly
#' identified models (no restrictions to test) and for non-linear second stages,
#' where the residual is not the structural error and Sargan's \eqn{n R^2} has no
#' standing. `tests/testthat/test-glmmTMB_2sls-benchmarks.R` checks the
#' coefficients, standard errors and all three diagnostics against
#' \pkg{ivreg}, \pkg{AER}, \pkg{fixest} and \pkg{estimatr}.
#'
#' @param first_stage Formula for the first stage; the LHS must be the single
#'   endogenous regressor.
#' @param second_stage Formula for the structural second stage (the
#'   control-function residual is appended automatically).
#' @param data A data frame. Rows with missing model variables are dropped
#'   listwise so both stages share identical rows.
#' @param family Family for the second-stage \code{glmmTMB} fit.
#' @param first_family Family for the first stage (default \code{gaussian()}).
#' @param instruments Optional character vector of instrument names for the
#'   weak-instrument test. When `NULL` the excluded instruments are derived as
#'   the first-stage terms absent from `second_stage`, exactly as
#'   [ivreg::ivreg()] derives them.
#' @param cf_name Name of the appended control-function residual column.
#' @param ziformula,dispformula Zero-inflation and dispersion formulas passed to
#'   \code{glmmTMB}.
#' @param n_boot Number of cluster-bootstrap replicates for valid (generated-
#'   regressor) standard errors. Only needed when the second stage is non-linear:
#'   a linear fit gets exact analytic standard errors for free, so leave this at
#'   \code{0} there.
#' @param vcov_type Flavour of the analytic covariance matrix: \code{"iid"},
#'   \code{"HC0"}, \code{"HC1"} or \code{"cluster"} (CR1, the Stata default;
#'   needs \code{cluster}). \code{"auto"}, the default, is \code{"cluster"} when
#'   \code{cluster} is supplied and \code{"iid"} otherwise. Ignored when the
#'   standard errors come from the bootstrap or when no analytic form exists.
#' @param cluster Optional column name; whole clusters are resampled in the
#'   bootstrap and form the score groups of a clustered analytic covariance.
#' @param seed Optional RNG seed for the bootstrap.
#' @param parallel Whether to parallelise the bootstrap via \code{future.apply}
#'   (respects the caller's \code{future::plan()}).
#' @param ... Extra arguments passed to \code{glmmTMB::glmmTMB}.
#' @return An object of class \code{glmmTMB_2sls} with \code{print}, \code{tidy},
#'   \code{glance} and \code{nobs} methods. Diagnostics live in
#'   \code{$weak_instrument} (first-stage Wald), \code{$endogeneity_test}
#'   (Wu-Hausman) and \code{$overid_test} (Sargan; \code{NULL} unless the model
#'   is linear and over-identified), each a list of \code{statistic}, \code{df}
#'   and \code{p.value}.
#' @export
glmmTMB_2sls <- function(first_stage,
                         second_stage,
                         data,
                         family,
                         first_family = stats::gaussian(),
                         instruments  = NULL,
                         cf_name      = "cf_resid",
                         ziformula    = ~0,
                         dispformula  = ~1,
                         n_boot       = 0L,
                         vcov_type    = c("auto", "iid", "HC0", "HC1", "cluster"),
                         cluster      = NULL,
                         seed         = NULL,
                         parallel     = TRUE,
                         ...) {

  vcov_type <- match.arg(vcov_type)
  vcov_asked <- vcov_type != "auto"
  if (vcov_type == "auto") vcov_type <- if (is.null(cluster)) "iid" else "cluster"
  if (vcov_type == "cluster" && is.null(cluster))
    stop("`vcov_type = \"cluster\"` needs a `cluster` column.")
  stopifnot(inherits(first_stage, "formula"),
            inherits(second_stage, "formula"),
            is.data.frame(data))
  if (cf_name %in% names(data))
    stop(sprintf("Column '%s' already exists in `data`; choose another `cf_name`.", cf_name))
  endog <- all.vars(first_stage[[2L]])
  if (length(endog) != 1L) stop("First-stage LHS must be a single endogenous variable.")
  if (!is.null(cluster) && !cluster %in% names(data)) stop("`cluster` not found in `data`.")

  ## listwise-complete data so both stages share identical rows ---------------
  vars <- intersect(unique(c(all.vars(first_stage), all.vars(second_stage),
                             all.vars(ziformula), all.vars(dispformula), cluster)),
                    names(data))
  keep <- stats::complete.cases(data[, vars, drop = FALSE])
  if (any(!keep)) {
    message(sprintf("glmmTMB_2sls: dropping %d of %d rows with missing model variables.",
                    sum(!keep), length(keep)))
    data <- data[keep, , drop = FALSE]
  }
  rownames(data) <- NULL

  ## append the CF residual to the second-stage RHS by string (safe for the
  ## mixed-model `( . | . )` terms that update.formula()/terms() can mangle) ---
  lhs <- deparse(second_stage[[2L]])
  rhs <- paste(deparse(second_stage[[3L]], width.cutoff = 500L), collapse = " ")
  ss_formula <- stats::as.formula(paste0(lhs, " ~ ", rhs, " + ", cf_name),
                                  env = environment(second_stage))

  dots     <- list(...)                       # extra glmmTMB args (captured for parallel)
  has_re   <- function(f) any(grepl("\\|", deparse(f)))
  fit_first <- function(df, start = NULL) {
    if (!has_re(first_stage) && identical(first_family$family, "gaussian")) {
      fs <- stats::lm(first_stage, data = df)                 # fast path (no warm start)
      list(fit = fs, resid = unname(stats::resid(fs)))
    } else {
      fs <- glmmTMB::glmmTMB(first_stage, data = df, family = first_family, start = start)
      list(fit = fs, resid = stats::residuals(fs, type = "response"))
    }
  }

  ## one full 2SLS pass (refits BOTH stages); `*_start` warm-start the refits --
  fit_once <- function(df, fs_start = NULL, ss_start = NULL) {
    fs <- fit_first(df, start = fs_start)
    df[[cf_name]] <- fs$resid
    ss <- do.call(glmmTMB::glmmTMB,
                  c(list(formula = ss_formula, data = df, family = family,
                         ziformula = ziformula, dispformula = dispformula, start = ss_start),
                    dots))
    list(first = fs$fit, second = ss)
  }

  # combined conditional + zero-inflation fixed effects, names prefixed by
  # component so the two parts stay distinct ("cond::isaiTRUE", "zi::isaiTRUE")
  get_coefs <- function(fit) {
    fx   <- glmmTMB::fixef(fit)
    cond <- fx$cond; if (length(cond)) names(cond) <- paste0("cond::", names(cond))
    zi   <- fx$zi;   if (length(zi))   names(zi)   <- paste0("zi::",   names(zi))
    c(cond, zi)
  }

  main     <- fit_once(data)
  beta_hat <- get_coefs(main$second)

  ## Wald helper (works for lm and glmmTMB) -----------------------------------
  get_bV <- function(fit) if (inherits(fit, "glmmTMB"))
    list(b = glmmTMB::fixef(fit)$cond, V = stats::vcov(fit)$cond)
  else list(b = stats::coef(fit), V = stats::vcov(fit))
  sel_idx <- function(nms, terms)
    which(nms %in% terms |
          Reduce(`|`, lapply(terms, function(t) startsWith(nms, t))))
  wald <- function(fit, terms) {
    bv  <- get_bV(fit)
    idx <- sel_idx(names(bv$b), terms)
    if (!length(idx)) return(NULL)
    b <- bv$b[idx]; V <- bv$V[idx, idx, drop = FALSE]
    stat <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA_real_)
    list(statistic = stat, df = length(idx),
         p.value = stats::pchisq(stat, length(idx), lower.tail = FALSE))
  }
  endogeneity_test <- wald(main$second, cf_name)

  ## weak-instrument test on the EXCLUDED instruments -------------------------
  ## When `instruments` is not given they are derived the way ivreg::ivreg()
  ## does: the first-stage terms that do not appear in the structural equation.
  ## For a linear first stage the statistic is reported in ivreg's form too --
  ## the F of a nested RSS comparison that drops those columns, which is exact
  ## in finite samples and expands factor instruments correctly. The Wald chi2
  ## (= df1 * F here) is kept for the general case, which is where a glmmTMB
  ## first stage lands.
  excluded <- instruments
  if (is.null(excluded)) {
    excluded <- setdiff(attr(stats::terms(first_stage), "term.labels"),
                        attr(stats::terms(second_stage), "term.labels"))
    if (!length(excluded)) excluded <- NULL
  }
  weak_instrument <- if (!is.null(excluded)) wald(main$first, excluded) else NULL
  if (!is.null(weak_instrument)) {
    weak_instrument$instruments <- excluded
    if (inherits(main$first, "lm") && !inherits(main$first, "glmmTMB")) {
      Z   <- stats::model.matrix(main$first)
      idx <- sel_idx(colnames(Z), excluded)
      if (length(idx) && length(idx) < ncol(Z)) {
        xe   <- stats::model.response(stats::model.frame(main$first))
        rss1 <- sum(stats::lm.fit(Z, xe)$residuals^2)
        rss0 <- sum(stats::lm.fit(Z[, -idx, drop = FALSE], xe)$residuals^2)
        df1  <- length(idx); df2 <- nrow(Z) - ncol(Z)
        Fv   <- ((rss0 - rss1) / df1) / (rss1 / df2)
        weak_instrument[c("F", "df1", "df2", "p.value.F")] <-
          list(Fv, df1, df2, stats::pf(Fv, df1, df2, lower.tail = FALSE))
      }
    }
  }

  ## the pieces that exist only where the control function IS textbook 2SLS ---
  ## -- an identity-link Gaussian second stage with no random effects and no
  ## zero-inflation, fitted off an OLS first stage. There the structural
  ## residual u = y - X b is recovered from the second-stage residual by adding
  ## the control-function term back, and X projected on the instruments is the
  ## 2SLS design. Both the Sargan test and the analytic standard errors below
  ## are built from these; anywhere else (non-linear link, mixed model,
  ## zero-inflation) neither has any standing and both stay NULL.
  lin <- local({
    ss  <- main$second
    fam <- ss$modelInfo$family
    fx  <- glmmTMB::fixef(ss)
    ok  <- inherits(main$first, "lm") && !inherits(main$first, "glmmTMB") &&
      identical(fam$family, "gaussian") && identical(fam$link, "identity") &&
      !has_re(ss_formula) && !has_re(ziformula) && !has_re(dispformula) &&
      !length(fx$zi) && length(fx$disp) == 1L
    if (!ok) return(NULL)
    Z  <- stats::model.matrix(main$first)          # instruments + exogenous controls
    X  <- glmmTMB::getME(ss, "X")                  # structural design + the CF column
    j  <- match(cf_name, colnames(X))
    v  <- unname(stats::resid(main$first))
    u  <- unname(stats::residuals(ss, type = "response")) + fx$cond[[cf_name]] * v
    X  <- X[, -j, drop = FALSE]                    # structural regressors only
    Xh <- stats::lm.fit(Z, X)$fitted.values        # P_Z X, the 2SLS design
    dimnames(Xh) <- dimnames(X)
    list(Z = Z, X = X, Xhat = Xh, u = u, b = fx$cond[-j])
  })

  ## Sargan test of the over-identifying restrictions -------------------------
  ## n * R^2 from regressing the structural residual on the full instrument
  ## matrix -- exactly what ivreg::ivreg() reports.
  overid_test <- if (is.null(lin)) NULL else local({
    df <- ncol(lin$Z) - ncol(lin$X)                # #instruments - #endogenous
    if (df <= 0L) return(NULL)                     # exactly identified: nothing to test
    aux  <- stats::lm.fit(lin$Z, lin$u)
    stat <- length(lin$u) * (1 - sum(aux$residuals^2) / sum((lin$u - mean(lin$u))^2))
    list(statistic = stat, df = df,
         p.value = stats::pchisq(stat, df, lower.tail = FALSE))
  })

  ## analytic 2SLS covariance -------------------------------------------------
  ## No bootstrap needed here: the generated-regressor problem is exactly what
  ## the 2SLS sandwich already accounts for, because the control function IS
  ## 2SLS in this case. The bread is (X'P_Z X)^-1 on the projected design and
  ## the meat is classical, heteroskedasticity-robust, or clustered (CR1, the
  ## small-sample correction Stata and fixest apply).
  analytic_vcov <- function(type) {
    Xh <- lin$Xhat; u <- lin$u
    n  <- nrow(Xh); k <- ncol(Xh)
    bread <- chol2inv(chol(crossprod(Xh)))
    V <- if (type == "iid") {
      bread * (sum(u^2) / (n - k))
    } else if (type == "cluster") {
      sc <- rowsum(Xh * u, data[[cluster]])        # per-cluster score sums
      G  <- nrow(sc)
      bread %*% crossprod(sc) %*% bread * (G / (G - 1)) * ((n - 1) / (n - k))
    } else {
      HC <- bread %*% crossprod(Xh * u) %*% bread
      if (type == "HC1") HC * (n / (n - k)) else HC
    }
    dimnames(V) <- list(colnames(Xh), colnames(Xh))
    V
  }

  ## cluster bootstrap for valid SEs (generated-regressor correction) ---------
  ## Parallelised across replicates (respects the caller's future::plan) and
  ## warm-started from the full-sample optimum; both only speed up the refits --
  ## the estimates and the SE definition are unchanged.
  boot_mat <- NULL
  if (n_boot > 0L) {
    # warm-start values from the full-sample optima (mirrors raffalib::glmmTMB_get_optimum)
    get_start <- function(fit) {
      fx <- glmmTMB::fixef(fit)
      s  <- list(beta = fx$cond, betazi = fx$zi, betadisp = fx$disp,
                 theta = tryCatch(glmmTMB::getME(fit, "theta"), error = function(e) NULL))
      s  <- Filter(function(z) length(z) > 0, s)
      if (length(s)) s else NULL
    }
    ss_start <- get_start(main$second)
    fs_start <- if (inherits(main$first, "glmmTMB")) get_start(main$first) else NULL

    groups <- if (!is.null(cluster)) split(seq_len(nrow(data)), data[[cluster]]) else NULL

    # one replicate -> named coefficient vector (or NULL on failure); tries a
    # warm start first, falls back to a cold start if that errors.
    one_boot <- function(i) {
      if (!is.null(groups)) {                       # resample whole clusters
        samp <- sample(names(groups), length(groups), replace = TRUE)
        df_b <- data[unlist(groups[samp], use.names = FALSE), , drop = FALSE]
        df_b[[cluster]] <- rep(make.unique(samp), lengths(groups[samp]))  # distinct levels
      } else {                                      # resample rows
        df_b <- data[sample(nrow(data), replace = TRUE), , drop = FALSE]
      }
      fit_try <- function(fss, sss) tryCatch(
        get_coefs(fit_once(df_b, fs_start = fss, ss_start = sss)$second),
        error = function(e) NULL)
      out <- fit_try(fs_start, ss_start)
      if (is.null(out)) out <- fit_try(NULL, NULL)  # cold-start fallback
      out
    }

    if (isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)) {
      res <- future.apply::future_lapply(seq_len(n_boot), one_boot,
                                         future.seed = if (is.null(seed)) TRUE else seed,
                                         future.packages = "glmmTMB")
    } else {
      if (isTRUE(parallel))
        message("glmmTMB_2sls: 'future.apply' not installed; bootstrapping serially.")
      if (!is.null(seed)) set.seed(seed)
      res <- lapply(seq_len(n_boot), one_boot)
    }

    res <- Filter(Negate(is.null), res)
    if (length(res)) {
      boot_mat <- matrix(NA_real_, length(res), length(beta_hat),
                         dimnames = list(NULL, names(beta_hat)))
      for (j in seq_along(res)) {
        nm <- intersect(names(res[[j]]), colnames(boot_mat))
        boot_mat[j, nm] <- res[[j]][nm]
      }
      boot_mat <- boot_mat[stats::complete.cases(boot_mat), , drop = FALSE]
    }
  }

  ## coefficient table --------------------------------------------------------
  ## An explicit bootstrap wins (the caller asked for it); otherwise the exact
  ## analytic 2SLS covariance is used wherever it exists, and only a model that
  ## admits neither falls back to the naive model-based SEs.
  if (!is.null(boot_mat) && nrow(boot_mat) > 1L) {
    se <- apply(boot_mat, 2L, stats::sd)
    ci <- t(apply(boot_mat, 2L, stats::quantile, probs = c(.025, .975)))
    z  <- beta_hat / se
    coeftable <- data.frame(estimate = beta_hat, std.error = se,
                            conf.low = ci[, 1L], conf.high = ci[, 2L],
                            statistic = z, p.value = 2 * stats::pnorm(-abs(z)),
                            se_type = sprintf("cluster-bootstrap (%d reps)", nrow(boot_mat)))
  } else if (!is.null(lin)) {
    b  <- lin$b
    se <- sqrt(diag(analytic_vcov(vcov_type)))
    z  <- b / se
    q  <- stats::qnorm(0.975)
    coeftable <- data.frame(
      estimate = b, std.error = se, conf.low = b - q * se, conf.high = b + q * se,
      statistic = z, p.value = 2 * stats::pnorm(-abs(z)),
      se_type = if (vcov_type == "cluster")
        sprintf("analytic 2SLS (cluster on %s, %d groups)",
                cluster, length(unique(data[[cluster]])))
      else sprintf("analytic 2SLS (%s)", vcov_type),
      row.names = paste0("cond::", names(b)))
    # The control-function term is a nuisance parameter, not a structural one,
    # and keeps its model-based SE: that SE is valid under the null of
    # exogeneity, which is exactly the null the Wu-Hausman test evaluates.
    sm <- summary(main$second)$coefficients$cond[cf_name, , drop = FALSE]
    coeftable <- rbind(coeftable, data.frame(
      estimate = sm[, 1L], std.error = sm[, 2L],
      conf.low = sm[, 1L] - q * sm[, 2L], conf.high = sm[, 1L] + q * sm[, 2L],
      statistic = sm[, 3L], p.value = sm[, 4L],
      se_type = "model-based (valid under H0 of exogeneity)",
      row.names = paste0("cond::", cf_name)))
  } else {
    sm <- summary(main$second)$coefficients
    naive_part <- function(mat, comp) {
      if (is.null(mat) || !NROW(mat)) return(NULL)
      data.frame(estimate = mat[, 1L], std.error = mat[, 2L],
                 statistic = mat[, 3L], p.value = mat[, 4L],
                 row.names = paste0(comp, "::", rownames(mat)), stringsAsFactors = FALSE)
    }
    coeftable <- rbind(naive_part(sm$cond, "cond"), naive_part(sm$zi, "zi"))
    coeftable$se_type <- "model-based (NAIVE - invalid for generated regressor)"
  }
  if (vcov_asked && !startsWith(coeftable$se_type[1L], "analytic"))
    message("glmmTMB_2sls: `vcov_type` ignored - ",
            if (!is.null(boot_mat)) "the bootstrap SEs were requested."
            else "no analytic covariance exists for a non-linear second stage.")

  structure(list(second_stage = main$second, first_stage = main$first,
                 coeftable = coeftable, endogeneity_test = endogeneity_test,
                 weak_instrument = weak_instrument, overid_test = overid_test,
                 vcov_type = if (!is.null(lin)) vcov_type else NA_character_,
                 boot = boot_mat, endogenous = endog, cf_name = cf_name),
            class = "glmmTMB_2sls")
}

# --- S3 methods -------------------------------------------------------------

#' @export
print.glmmTMB_2sls <- function(x, ...) {
  cat("Control-function 2SLS via glmmTMB\n")
  cat(sprintf("Endogenous: %s   |   CF term: %s\n\n", x$endogenous, x$cf_name))
  ct <- x$coeftable
  rownames(ct) <- sub("^cond::", "", rownames(ct))   # keep "zi::" to flag ZI rows
  show <- setdiff(names(ct), "se_type")
  print(round(ct[, show], 4L))
  cat("\nSE type:", x$coeftable$se_type[1L], "\n")
  if (!is.null(x$weak_instrument)) {
    wi <- x$weak_instrument
    if (is.null(wi$F))
      cat(sprintf("Weak instruments (%s): chi2(%d) = %.1f, p = %.3g\n",
                  paste(wi$instruments, collapse = ", "), wi$df, wi$statistic, wi$p.value))
    else
      cat(sprintf("Weak instruments (%s): F(%d, %d) = %.1f, p = %.3g\n",
                  paste(wi$instruments, collapse = ", "), wi$df1, wi$df2, wi$F, wi$p.value.F))
  }
  if (!is.null(x$endogeneity_test))
    cat(sprintf("Endogeneity (Wu-Hausman on %s): chi2(1) = %.1f, p = %.3g\n",
                x$cf_name, x$endogeneity_test$statistic, x$endogeneity_test$p.value))
  if (!is.null(x$overid_test))
    cat(sprintf("Overidentification (Sargan): chi2(%d) = %.3f, p = %.3g\n",
                x$overid_test$df, x$overid_test$statistic, x$overid_test$p.value))
  invisible(x)
}

# tidy/glance/nobs so modelsummary() can tabulate the object directly.
# The control-function residual (a nuisance term) is dropped from the table;
# its significance is reported as the Wu-Hausman gof instead.
#' @exportS3Method generics::tidy
tidy.glmmTMB_2sls <- function(x, ...) {
  ct   <- x$coeftable
  full <- rownames(ct)
  comp <- ifelse(startsWith(full, "zi::"), "zero_inflated", "conditional")
  term <- sub("^(cond|zi)::", "", full)
  keep <- term != x$cf_name                      # drop the control-function residual
  out  <- data.frame(component = comp[keep], effect = "fixed", term = term[keep],
                     estimate = ct$estimate[keep], std.error = ct$std.error[keep],
                     statistic = ct$statistic[keep], p.value = ct$p.value[keep],
                     stringsAsFactors = FALSE)
  if (!is.null(ct$conf.low)) {
    out$conf.low  <- ct$conf.low[keep]
    out$conf.high <- ct$conf.high[keep]
  }
  rownames(out) <- NULL
  out
}

#' @exportS3Method generics::glance
glance.glmmTMB_2sls <- function(x, ...) {
  link <- tryCatch(x$second_stage$modelInfo$family$link, error = function(e) NA_character_)
  if (is.null(link)) link <- NA_character_
  # weak.inst reports the first-stage F p-value where it exists (ivreg's form),
  # falling back to the Wald chi2 for a non-linear first stage
  wi <- x$weak_instrument
  data.frame(
    nobs      = tryCatch(stats::nobs(x$second_stage), error = function(e) NA_integer_),
    weak.inst = if (is.null(wi)) NA_real_ else if (!is.null(wi$F)) wi$p.value.F else wi$p.value,
    wu.haus   = if (!is.null(x$endogeneity_test)) x$endogeneity_test$p.value else NA_real_,
    sargan    = if (!is.null(x$overid_test))      x$overid_test$p.value      else NA_real_,
    link      = link,
    coef_exp  = if (!is.na(link) && link != "identity") "yes" else "no",
    stringsAsFactors = FALSE
  )
}

#' @importFrom stats nobs
#' @export
nobs.glmmTMB_2sls <- function(object, ...) stats::nobs(object$second_stage)

#' Extract model parameters from a control-function 2SLS fit
#'
#' Registers [parameters::model_parameters()] for `glmmTMB_2sls`, which puts the
#' object on the easystats extraction path rather than the `broom` one. The
#' shape of the output — columns, component/effect structure, and crucially the
#' `pretty_labels` attribute — comes from the second-stage `glmmTMB` fit, while
#' the numbers are replaced by the control-function estimates and their
#' cluster-bootstrap standard errors. The control-function residual is dropped,
#' as the `tidy()` method does; its significance is reported instead as the
#' Wu-Hausman statistic by the `glance()` method.
#'
#' The point of the method is labelling. `modelsummary`'s `coef_rename = TRUE`
#' reads the `pretty_labels` attribute, which only `parameters` produces: a
#' `tidy()` method cannot supply it, however complete, so without this method a
#' table of these objects silently falls back to raw term names such as
#' `genderM` or `publication_type_detailedConfProc`. Term names themselves stay
#' raw, so `coef_omit` and `coef_map` continue to match the model's own
#' variables rather than their labels.
#'
#' @param model A `glmmTMB_2sls` object.
#' @param ... Passed through to [parameters::model_parameters()] for the second
#'   stage.
#' @return The second stage's `parameters_model` data frame, restricted to the
#'   parameters the control function estimates and carrying its numbers.
#' @seealso [glmmTMB_2sls()]
#' @exportS3Method parameters::model_parameters
model_parameters.glmmTMB_2sls <- function(model, ...) {
  out <- parameters::model_parameters(model$second_stage, ...)
  ct  <- model$coeftable
  if (!inherits(out, "data.frame") || !"Parameter" %in% names(out)) {
    return(out)
  }

  # `parameters` omits these columns for a single-component fit, but
  # tidy.glmmTMB_2sls() always emitted them and downstream code relies on it:
  # `modelsummary(shape = component + term + statistic ~ model)` errors out with
  # "Group columns (component) were not found" if they are missing.
  if (!"Component" %in% names(out)) out[["Component"]] <- "conditional"
  if (!"Effects" %in% names(out)) out[["Effects"]] <- "fixed"

  full    <- rownames(ct)
  ct_term <- sub("^(cond|zi)::", "", full)
  ct_comp <- ifelse(startsWith(full, "zi::"), "zero_inflated", "conditional")

  out_comp <- if ("Component" %in% names(out)) {
    as.character(out$Component)
  } else {
    rep("conditional", nrow(out))
  }
  out_comp[is.na(out_comp)] <- "conditional"
  idx <- match(
    paste(out$Parameter, out_comp, sep = "\r"),
    paste(ct_term, ct_comp, sep = "\r")
  )

  # rows the control function does not estimate (dispersion, random-effect
  # variances) are dropped, matching what tidy() reports
  keep <- !is.na(idx) & out$Parameter != model$cf_name
  if (!any(keep)) return(out)

  put <- function(x, candidates, values) {
    col <- intersect(candidates, names(x))
    if (length(col)) x[[col[1]]][keep] <- values[idx[keep]]
    x
  }
  out <- put(out, "Coefficient", ct$estimate)
  out <- put(out, "SE", ct$std.error)
  out <- put(out, c("z", "t", "Statistic"), ct$statistic)
  out <- put(out, "p", ct$p.value)
  if (!is.null(ct$conf.low)) {
    out <- put(out, "CI_low", ct$conf.low)
    out <- put(out, "CI_high", ct$conf.high)
  }

  # subsetting a parameters_model drops its attributes, and `pretty_labels` is
  # the one that matters here, so put them back
  att <- attributes(out)
  res <- out[keep, , drop = FALSE]
  for (a in setdiff(names(att), c("names", "row.names", "class"))) {
    attr(res, a) <- att[[a]]
  }
  class(res) <- att[["class"]]
  rownames(res) <- NULL
  res
}
