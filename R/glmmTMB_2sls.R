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
# model). The coefficient on v_hat is a Durbin-Wu-Hausman endogeneity test.
#
# Because v_hat is a GENERATED regressor, naive second-stage SEs are invalid;
# `n_boot > 0` returns cluster-bootstrap SEs that propagate first-stage
# uncertainty. The bootstrap is parallelised across replicates (respecting the
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
#' @param first_stage Formula for the first stage; the LHS must be the single
#'   endogenous regressor.
#' @param second_stage Formula for the structural second stage (the
#'   control-function residual is appended automatically).
#' @param data A data frame. Rows with missing model variables are dropped
#'   listwise so both stages share identical rows.
#' @param family Family for the second-stage \code{glmmTMB} fit.
#' @param first_family Family for the first stage (default \code{gaussian()}).
#' @param instruments Optional character vector of instrument names, used for the
#'   weak-instrument Wald test.
#' @param cf_name Name of the appended control-function residual column.
#' @param ziformula,dispformula Zero-inflation and dispersion formulas passed to
#'   \code{glmmTMB}.
#' @param n_boot Number of cluster-bootstrap replicates for valid (generated-
#'   regressor) standard errors; \code{0} returns naive model-based SEs.
#' @param cluster Optional column name; whole clusters are resampled in the
#'   bootstrap.
#' @param seed Optional RNG seed for the bootstrap.
#' @param parallel Whether to parallelise the bootstrap via \code{future.apply}
#'   (respects the caller's \code{future::plan()}).
#' @param ... Extra arguments passed to \code{glmmTMB::glmmTMB}.
#' @return An object of class \code{glmmTMB_2sls} with \code{print}, \code{tidy},
#'   \code{glance} and \code{nobs} methods.
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
                         cluster      = NULL,
                         seed         = NULL,
                         parallel     = TRUE,
                         ...) {

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
  wald <- function(fit, terms) {
    bv  <- get_bV(fit)
    idx <- which(names(bv$b) %in% terms |
                 Reduce(`|`, lapply(terms, function(t) startsWith(names(bv$b), t))))
    if (!length(idx)) return(NULL)
    b <- bv$b[idx]; V <- bv$V[idx, idx, drop = FALSE]
    stat <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA_real_)
    list(statistic = stat, df = length(idx),
         p.value = stats::pchisq(stat, length(idx), lower.tail = FALSE))
  }
  weak_instrument  <- if (!is.null(instruments)) wald(main$first, instruments) else NULL
  endogeneity_test <- wald(main$second, cf_name)

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
  if (!is.null(boot_mat) && nrow(boot_mat) > 1L) {
    se <- apply(boot_mat, 2L, stats::sd)
    ci <- t(apply(boot_mat, 2L, stats::quantile, probs = c(.025, .975)))
    z  <- beta_hat / se
    coeftable <- data.frame(estimate = beta_hat, std.error = se,
                            conf.low = ci[, 1L], conf.high = ci[, 2L],
                            statistic = z, p.value = 2 * stats::pnorm(-abs(z)),
                            se_type = sprintf("cluster-bootstrap (%d reps)", nrow(boot_mat)))
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

  structure(list(second_stage = main$second, first_stage = main$first,
                 coeftable = coeftable, endogeneity_test = endogeneity_test,
                 weak_instrument = weak_instrument, boot = boot_mat,
                 endogenous = endog, cf_name = cf_name),
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
  if (!is.null(x$weak_instrument))
    cat(sprintf("Weak-instrument Wald: chi2(%d) = %.1f, p = %.3g\n",
                x$weak_instrument$df, x$weak_instrument$statistic, x$weak_instrument$p.value))
  if (!is.null(x$endogeneity_test))
    cat(sprintf("Endogeneity (Wu-Hausman on %s): chi2(1) = %.1f, p = %.3g\n",
                x$cf_name, x$endogeneity_test$statistic, x$endogeneity_test$p.value))
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
  data.frame(
    nobs      = tryCatch(stats::nobs(x$second_stage), error = function(e) NA_integer_),
    weak.inst = if (!is.null(x$weak_instrument))  x$weak_instrument$p.value  else NA_real_,
    wu.haus   = if (!is.null(x$endogeneity_test)) x$endogeneity_test$p.value else NA_real_,
    link      = link,
    coef_exp  = if (!is.na(link) && link != "identity") "yes" else "no",
    stringsAsFactors = FALSE
  )
}

#' @importFrom stats nobs
#' @export
nobs.glmmTMB_2sls <- function(object, ...) stats::nobs(object$second_stage)
