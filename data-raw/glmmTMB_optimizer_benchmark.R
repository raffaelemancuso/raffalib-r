#!/usr/bin/env R

# paper-5-RM: Replication code for my paper n. 5
# Copyright (C) 2025 Raffaele Mancuso

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# ============================================================================
# Benchmark every raffalib optimizer control function for glmmTMB.
# ----------------------------------------------------------------------------
# Fits the SAME negative-binomial GLMM (random intercept) with every calibrar,
# optimx and optimh control wrapper in raffalib (`glmmTMB_control_calibrar_*()`,
# `glmmTMB_control_optimx_*()`, `glmmTMB_control_optimh_*()`), plus glmmTMB's
# native optimizer as a baseline,
# and records per optimizer:
#   * Converged   -- fit$fit$convergence == 0 AND a positive-definite Hessian
#   * Time (s)    -- wall-clock of the glmmTMB() call
#   * logLik/AIC  -- to confirm each reaches the SAME optimum
#   * dLogLik     -- logLik deficit vs the best optimizer (0 = reached optimum)
#   * max|grad|   -- max abs gradient at the solution (should be ~0)
# Results are printed and written as a markdown table to `out_md`.
#
# Run in the project R (needs glmmTMB + calibrar + raffalib + callr + pkgload).
# Optimizer backend packages (optimx/minqa/dfoptim/BB/nloptr/...) are installed
# automatically if missing. Each fit runs in a separate process and is
# hard-killed if it exceeds `per_method_timeout`; full error messages for any
# method that fails are logged to both the markdown and the Excel file.
# ============================================================================

## ---- config ---------------------------------------------------------------
raffalib_path     <- "C:/data/progetti_miei/raffalib-r"
per_method_timeout <- 40        # seconds; slow optimizers are cut off at this
seed              <- 1
G                 <- 20         # random-intercept groups
n_per             <- 100        # observations per group  (N = G * n_per)
incremental       <- TRUE       # if TRUE, run ONLY controls absent from the
                                # existing .xlsx and merge; set FALSE (or delete
                                # the .xlsx) to re-benchmark every control

# Write the outputs next to THIS script (not the current working directory).
script_dir <- tryCatch(this.path::this.dir(), error = function(e) getwd())
out_md     <- file.path(script_dir, "glmmTMB_optimizer_benchmark.md")
out_xlsx   <- file.path(script_dir, "glmmTMB_optimizer_benchmark.xlsx")

# Backend packages the tested optimizers dispatch to (calibrar optim2 + optimh
# solvers + every optimx allmeth method). Install any that are not installed.
method_pkgs <- c("optimx", "lbfgsb3c", "BB", "ucminf", "minqa", "dfoptim",
                 "lbfgs", "subplex", "marqLevAlg", "nloptr", "pracma",
                 "cmaes", "DEoptim", "GenSA", "pso", "rgenoud", "soma")
missing_pkgs <- method_pkgs[!vapply(method_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  message("Installing missing optimizer backends: ", paste(missing_pkgs, collapse = ", "))
  cran <- getOption("repos")["CRAN"]
  if (is.na(cran) || !nzchar(cran) || cran == "@CRAN@")
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  utils::install.packages(missing_pkgs)
}

suppressWarnings(suppressMessages({
  library(glmmTMB)
  devtools::load_all(raffalib_path, export_all = FALSE)
}))
stopifnot(requireNamespace("calibrar",  quietly = TRUE),
          requireNamespace("openxlsx2", quietly = TRUE))

## ---- representative model: NB GLMM with a random intercept ----------------
set.seed(seed)
grp <- factor(rep(seq_len(G), each = n_per))
N   <- G * n_per
b_g <- rnorm(G, sd = 0.6)[grp]
x1  <- rnorm(N); x2 <- rnorm(N); x3 <- factor(rbinom(N, 1, 0.5))
eta <- 0.4 + 0.5 * x1 - 0.3 * x2 + 0.2 * (x3 == "1") + b_g
dat <- data.frame(y = rnbinom(N, size = 2, mu = exp(eta)), x1, x2, x3, grp)
form <- y ~ x1 + x2 + x3 + (1 | grp)

## ---- the controls under test ----------------------------------------------
# Every calibrar AND optimx control function in raffalib, plus glmmTMB's native
# optimizer (the "__native__" sentinel). Enumerated from the namespace so new
# controls are picked up automatically.
R <- asNamespace("raffalib")
ctrl_names <- c("__native__",
                sort(grep("^glmmTMB_control_calibrar_", ls(R), value = TRUE)),
                sort(grep("^glmmTMB_control_optimx_",   ls(R), value = TRUE)),
                sort(grep("^glmmTMB_control_nloptr_",   ls(R), value = TRUE)),
                sort(grep("^glmmTMB_control_minqa_",    ls(R), value = TRUE)),
                sort(grep("^glmmTMB_control_lbfgsb3c$", ls(R), value = TRUE))
                #sort(grep("^glmmTMB_control_optimh_",   ls(R), value = TRUE))
              )

## ---- incremental: load prior results, run only untested controls ----------
# The control label used as the primary key in the results table (matches
# run_one()'s `control` field): the native sentinel maps to its printed name.
ctrl_label <- function(nm) if (identical(nm, "__native__")) "glmmTMB default (nlminb)" else nm

valid_labels <- vapply(ctrl_names, ctrl_label, character(1))
prev_bench <- NULL
if (incremental && file.exists(out_xlsx)) {
  prev_bench <- as.data.frame(openxlsx2::read_xlsx(out_xlsx), stringsAsFactors = FALSE)
  cat(sprintf("Incremental mode: %d controls already benchmarked in %s\n",
              nrow(prev_bench), basename(out_xlsx)))
  # drop cached rows for controls whose constructor no longer exists (removed
  # optimizers), so the merged output only ever contains live constructors
  orphans <- setdiff(prev_bench$control, valid_labels)
  if (length(orphans)) {
    cat(sprintf("Pruning %d cached row(s) for removed control(s): %s\n",
                length(orphans), paste(orphans, collapse = ", ")))
    prev_bench <- prev_bench[prev_bench$control %in% valid_labels, , drop = FALSE]
  }
}
done_controls <- if (!is.null(prev_bench)) prev_bench$control else character(0)
todo_names <- ctrl_names[!vapply(ctrl_names, ctrl_label, character(1)) %in% done_controls]
cat(sprintf("%d control(s) to benchmark this run (of %d total; %d cached).\n",
            length(todo_names), length(ctrl_names), length(done_controls)))

## ---- worker process with a HARD per-fit timeout ---------------------------
# setTimeLimit() only triggers at R-level interrupt checks and CANNOT interrupt
# glmmTMB's compiled optimizer, so a slow fit runs right past the limit. We
# therefore run each fit in a separate R process (callr) and KILL it if it
# overruns per_method_timeout.
stopifnot(requireNamespace("callr", quietly = TRUE),
          requireNamespace("pkgload", quietly = TRUE))

new_worker <- function() {
  s <- callr::r_session$new()
  s$run(function(p) suppressWarnings(suppressMessages({
    library(glmmTMB); pkgload::load_all(p, export_all = FALSE, quiet = TRUE)
  })), args = list(raffalib_path))
  s$run(function(d, f) { assign(".dat", d, globalenv()); assign(".form", f, globalenv()); TRUE },
        args = list(dat, form))
  s
}
worker <- if (length(todo_names)) new_worker() else NULL

# Runs INSIDE the worker; env stripped to base so callr doesn't serialise the
# master globals on every call. Returns scalar fit metrics.
fit_in_worker <- function(nm) {
  ctrl <- if (identical(nm, "__native__")) glmmTMB::glmmTMBControl()
          else get(nm, envir = asNamespace("raffalib"))()
  tt <- system.time(fit <- glmmTMB::glmmTMB(get(".form", globalenv()),
                      data = get(".dat", globalenv()),
                      family = glmmTMB::nbinom2(), control = ctrl))["elapsed"]
  ll <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
  # conditional fixed-effect estimates and their standard errors (named vectors)
  est <- tryCatch(glmmTMB::fixef(fit)$cond,            error = function(e) NULL)
  se  <- tryCatch(sqrt(diag(stats::vcov(fit)$cond)),   error = function(e) NULL)
  list(time_s   = as.numeric(tt),
       logLik   = ll,
       AIC      = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
       max_grad = tryCatch(max(abs(fit$obj$gr(fit$fit$par))), error = function(e) NA_real_),
       est      = est,
       se       = se,
       pdHess   = isTRUE(fit$sdr$pdHess),
       converged = isTRUE(fit$fit$convergence == 0) && isTRUE(fit$sdr$pdHess) && is.finite(ll))
}
environment(fit_in_worker) <- baseenv()

## ---- run one optimizer -> one row -----------------------------------------
run_one <- function(fn_name) {
  is_base <- identical(fn_name, "__native__")
  family  <- if (is_base) "native"
             else if (grepl("_calibrar_",      fn_name)) "calibrar"
             else if (grepl("_optimx_",        fn_name)) "optimx"
             else if (grepl("_nloptr_",        fn_name)) "nloptr"
             else if (grepl("_minqa_",         fn_name)) "minqa"
             else if (grepl("^glmmTMB_control_lbfgsb3c$", fn_name)) "lbfgsb3c"
             else if (grepl("_optimh_",        fn_name)) "optimh" else "?"
  control <- if (is_base) "glmmTMB default (nlminb)" else fn_name
  method  <- if (is_base) "nlminb (native)"
             else if (grepl("^glmmTMB_control_lbfgsb3c$", fn_name)) "lbfgsb3c (L-BFGS-B)"
             else sub("^glmmTMB_control_(calibrar|optimx|optimh|nloptr|minqa)_", "", fn_name)
  row <- data.frame(family = family, control = control, method = method, converged = FALSE,
                    time_s = NA_real_, logLik = NA_real_, AIC = NA_real_,
                    max_grad = NA_real_, note = "", error = "", stringsAsFactors = FALSE)

  worker$call(fit_in_worker, args = list(fn_name))
  st <- worker$poll_process(per_method_timeout * 1000)   # ms
  if (!identical(st, "ready")) {                          # overran -> hard-kill & restart
    try(worker$close(), silent = TRUE)
    worker <<- new_worker()
    row$note <- sprintf("timeout (>%gs)", per_method_timeout)
  } else {
    out <- tryCatch(worker$read(), error = function(e) list(error = e))
    if (!is.null(out$error)) {                            # the fit errored in the worker
      row$note  <- "error"
      row$error <- conditionMessage(out$error)            # FULL message (logged in both outputs)
      if (!identical(worker$get_state(), "idle")) {       # session crashed -> restart
        try(worker$close(), silent = TRUE); worker <<- new_worker()
      }
    } else {
      r <- out$result
      row$time_s <- r$time_s; row$logLik <- r$logLik; row$AIC <- r$AIC
      row$max_grad <- r$max_grad; row$converged <- isTRUE(r$converged)
      coef_store[[control]] <<- list(est = r$est, se = r$se)   # keyed by unique control name
      if (!isTRUE(r$pdHess) && is.finite(r$logLik)) row$note <- "non-PD Hessian"
    }
  }
  cat(sprintf("  %-38s %-5s %s\n", control, if (row$converged) "OK" else "fail",
              if (nzchar(row$note)) row$note else sprintf("%.2fs", row$time_s)))
  row
}

cat(sprintf("\nBenchmarking %d control(s) on an NB GLMM (N=%d, %d groups), %gs timeout each...\n\n",
            length(todo_names), N, G, per_method_timeout))
coef_store <- list()                                  # control name -> list(est, se)
new_bench <- if (length(todo_names)) do.call(rbind, lapply(todo_names, run_one)) else NULL
if (!is.null(worker)) try(worker$close(), silent = TRUE)

## ---- attach the newly-fitted coefficient columns --------------------------
# The model is the same for every optimizer, so the coefficient set is shared;
# pull each new fit's estimates/SEs and attach b_<coef>/se_<coef>.
pull <- function(ct, cn, what) {
  z <- coef_store[[ct]][[what]]
  if (!is.null(z) && cn %in% names(z)) z[[cn]] else NA_real_
}
if (!is.null(new_bench)) {
  new_coef_names <- unique(unlist(lapply(coef_store, function(z) names(z$est))))
  for (cn in new_coef_names) {
    new_bench[[paste0("b_",  cn)]] <- vapply(new_bench$control, pull, numeric(1), cn = cn, what = "est")
    new_bench[[paste0("se_", cn)]] <- vapply(new_bench$control, pull, numeric(1), cn = cn, what = "se")
  }
}

## ---- merge cached + new results -------------------------------------------
# `logLik` is persisted so ΔlogLik can be recomputed exactly across merges.
# Bootstrap: an older cache may predate the logLik column; since every run
# targets the SAME likelihood, reconstruct it from the stored ΔlogLik and the
# best converged logLik of THIS run (which reaches the same optimum).
new_best_ll <- if (!is.null(new_bench))
  suppressWarnings(max(new_bench$logLik[new_bench$converged], na.rm = TRUE)) else -Inf
if (!is.null(prev_bench) && !"logLik" %in% names(prev_bench)) {
  prev_bench$logLik <- if (is.finite(new_best_ll)) prev_bench$dLogLik + new_best_ll else NA_real_
}

if (is.null(prev_bench)) {
  bench <- new_bench
} else if (is.null(new_bench)) {
  bench <- prev_bench
} else {
  all_cols <- union(names(prev_bench), names(new_bench))
  align_cols <- function(df) { for (cc in setdiff(all_cols, names(df))) df[[cc]] <- NA; df[, all_cols, drop = FALSE] }
  bench <- rbind(align_cols(prev_bench), align_cols(new_bench))
}
# read_xlsx turns empty strings into NA; restore "" so nzchar()/rendering work
for (col in c("note", "error")) if (col %in% names(bench)) bench[[col]][is.na(bench[[col]])] <- ""
bench$converged <- as.logical(bench$converged)   # xlsx may read as 0/1 or "TRUE"
bench$converged[is.na(bench$converged)] <- FALSE

## ---- post-process: logLik deficit vs best, sort ---------------------------
if ("logLik" %in% names(bench) && any(is.finite(bench$logLik[bench$converged]))) {
  best_ll <- suppressWarnings(max(bench$logLik[bench$converged], na.rm = TRUE))
  if (!is.finite(best_ll)) best_ll <- suppressWarnings(max(bench$logLik, na.rm = TRUE))
  bench$dLogLik <- bench$logLik - best_ll
}   # else: keep the cached dLogLik column as-is
bench <- bench[order(!bench$converged, bench$time_s, na.last = TRUE), ]

# coefficient columns present after the merge (prev order first), for rendering
coef_names <- sub("^b_", "", grep("^b_", names(bench), value = TRUE))

## ---- render markdown ------------------------------------------------------
num <- function(x, d) ifelse(is.na(x), "—", formatC(x, format = "f", digits = d))
sci <- function(x)    ifelse(is.na(x), "—", formatC(x, format = "g", digits = 2))
expo <- function(x)   ifelse(is.na(x), "—", formatC(x, format = "e", digits = 2))
tick <- function(b)   ifelse(isTRUE(b), "yes", "no")

md <- c(
  "# glmmTMB optimizer-control benchmark (calibrar + optimx + nloptr + minqa + lbfgsb3c)",
  "",
  sprintf("- **Model:** `%s`, family negative binomial (`nbinom2`)", deparse(form)),
  sprintf("- **Data:** simulated, N = %d, %d random-intercept groups (seed = %d)", N, G, seed),
  sprintf("- **Per-optimizer timeout:** %g s", per_method_timeout),
  sprintf("- **Platform:** %s, R %s, glmmTMB %s",
          Sys.info()[["sysname"]], getRversion(),
          as.character(utils::packageVersion("glmmTMB"))),
  sprintf("- **Run:** %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "",
  "`ΔlogLik` is the log-likelihood deficit vs the best converged optimizer (0 = reached the same optimum). `max|grad|` is the largest absolute gradient at the solution (≈0 at a true optimum). Each coefficient column shows the conditional fixed-effect estimate with its standard error in parentheses: `estimate (SE)`.",
  "",
  "**Only optimizers that can work with `glmmTMB` are included.** For nloptr, raffalib wraps only the LOCAL algorithms (`NLOPT_LN_*` derivative-free, `NLOPT_LD_*` derivative-based). NLopt's global algorithms (`NLOPT_GN_*`/`NLOPT_GD_*`) are omitted because they require finite parameter bounds, which `glmmTMB` never supplies (it optimizes on an unconstrained transformed scale), and the AUGLAG/MLSL meta-algorithms are omitted because they require a subsidiary local optimizer (`opts$local_opts`) that the plain control interface does not set — so both classes fail before starting. `optimParallel` is likewise omitted: `glmmTMB` supplies an exact analytic TMB gradient (leaving nothing to parallelise) and its compiled TMB objective does not serialise to the optimParallel workers.",
  "",
  paste0("| Family | Control function | `method` | Converged | Time (s) | ΔlogLik | max\\|grad\\| | ",
         paste(coef_names, collapse = " | "), " | Note |"),
  paste0("|---|---|---|:--:|--:|--:|--:|", paste(rep("--:", length(coef_names)), collapse = "|"), "|---|")
)
for (i in seq_len(nrow(bench))) {
  b <- bench[i, ]
  coef_cells <- vapply(coef_names, function(cn)
    sprintf("%s (%s)", num(b[[paste0("b_", cn)]], 3), num(b[[paste0("se_", cn)]], 3)), character(1))
  md <- c(md, sprintf("| %s | `%s` | %s | %s | %s | %s | %s | %s | %s |",
    b$family, b$control, b$method, tick(b$converged), num(b$time_s, 2),
    expo(b$dLogLik), sci(b$max_grad), paste(coef_cells, collapse = " | "), b$note))
}
fam_summary <- function(fam) {
  n <- sum(bench$family == fam)
  if (n == 0L) return(NULL)
  sprintf("%s %d/%d", fam, sum(bench$converged & bench$family == fam), n)
}
md <- c(md, "",
        sprintf("_%d/%d converged (%s). Fastest converged: %s._",
                sum(bench$converged), nrow(bench),
                paste(Filter(Negate(is.null),
                             lapply(c("calibrar", "optimx", "nloptr", "minqa",
                                      "lbfgsb3c", "optimh"),
                                    fam_summary)),
                      collapse = ", "),
                { ok <- bench[bench$converged, ]; if (nrow(ok)) ok$method[which.min(ok$time_s)] else "none" }))

## full error messages for any method that errored
err <- bench[nzchar(bench$error), , drop = FALSE]
if (nrow(err)) {
  md <- c(md, "", "## Errors", "")
  for (i in seq_len(nrow(err)))
    md <- c(md, sprintf("**`%s`** (%s):", err$control[i], err$method[i]),
            "", "```text", err$error[i], "```", "")
}

writeLines(md, out_md)
cat("\n", paste(md, collapse = "\n"), "\n", sep = "")
cat(sprintf("\nMarkdown table written to %s\n", normalizePath(out_md, mustWork = FALSE)))

## ---- Excel export ---------------------------------------------------------
coef_cols <- as.vector(rbind(paste0("b_", coef_names), paste0("se_", coef_names)))
# persist logLik + AIC so incremental merges can recompute dLogLik exactly
xlsx_cols <- c("family", "control", "method", "converged", "time_s",
               "logLik", "AIC", "dLogLik", "max_grad", coef_cols, "note", "error")
openxlsx2::write_xlsx(bench[, intersect(xlsx_cols, names(bench))], out_xlsx)
cat(sprintf("Excel file written to %s\n", normalizePath(out_xlsx, mustWork = FALSE)))
