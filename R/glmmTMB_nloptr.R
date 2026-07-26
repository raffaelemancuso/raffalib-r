# raffalib-r misc helper functions
# Copyright (C) 2026 Raffaele Mancuso
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# --- GLMMTMB NLOPTR OPTIMIZER --- #

#' Optimizer wrapper around nloptr::nloptr() for glmmTMB
#'
#' Adapts [nloptr::nloptr()] to the calling convention `glmmTMB` expects for the
#' `optimizer` argument of [glmmTMB::glmmTMBControl()]: it maps `par`/`fn`/`gr`
#' onto nloptr's `x0`/`eval_f`/`eval_grad_f`, forwards the nloptr `opts` (given
#' through `control`), and returns a list with `par`, `objective` and a
#' `convergence` code (0 = success). Use a `glmmTMB_control_nloptr_*()`
#' constructor rather than calling this directly.
#'
#' The gradient is passed to nloptr only for derivative-based algorithms
#' (`NLOPT_LD_*`); derivative-free ones (`NLOPT_LN_*`) ignore it.
#'
#' @section Only glmmTMB-compatible algorithms are exposed:
#' raffalib provides constructors **only for NLopt's LOCAL algorithms**
#' (`NLOPT_LN_*` derivative-free and `NLOPT_LD_*` derivative-based). NLopt's
#' other algorithm classes are deliberately omitted because they cannot optimize
#' a `glmmTMB` likelihood as `glmmTMB` calls the optimizer — see
#' [glmmTMB_control_nloptr_methods] for the full rationale.
#'
#' @param par Starting parameter vector.
#' @param fn Objective function (returns a scalar).
#' @param gr Gradient function, or `NULL`.
#' @param lower,upper Box constraints (scalar or length-`par`); default
#'   unconstrained.
#' @param control nloptr `opts` list; must contain (or defaults to) `algorithm`.
#' @param ... Absorbed for API compatibility (e.g. an unused `method`).
#' @return A list with `par`, `objective`, `convergence` (0 = converged) and
#'   `message`.
#' @export
glmmTMB_nloptr_optim <- function(par, fn, gr = NULL, lower = -Inf, upper = Inf,
                                 control = list(), ...) {
  # base print(), not myinfo(): serialized to workers by fit_glmmTMB_parallel(),
  # which does not load raffalib there (only base / pkg::fun survive the trip).
  print("nloptr optimization")
  algorithm <- if (!is.null(control$algorithm)) control$algorithm else "NLOPT_LN_BOBYQA"
  needs_grad <- grepl("_(LD|GD)_", algorithm)

  opts <- control
  opts$algorithm <- algorithm
  if (is.null(opts$maxeval)) opts$maxeval <- 10000L
  if (is.null(opts$xtol_rel)) opts$xtol_rel <- 1e-8

  n <- length(par)
  lb <- if (length(lower) == 1L) rep(lower, n) else lower
  ub <- if (length(upper) == 1L) rep(upper, n) else upper

  # glmmTMB's objective (TMB's obj$fn/obj$gr) carries `...` in its formals;
  # nloptr's .checkfunargs then insists we forward an extra argument. Wrapping
  # in a fixed-signature closure gives nloptr the clean `function(x)` it wants.
  eval_f <- function(x) fn(x)
  eval_grad_f <- if (needs_grad) function(x) gr(x) else NULL

  ret <- nloptr::nloptr(
    x0 = par,
    eval_f = eval_f,
    eval_grad_f = eval_grad_f,
    lb = lb,
    ub = ub,
    opts = opts
  )

  # nloptr status > 0 signals success: 1 SUCCESS, 2 STOPVAL, 3 FTOL, 4 XTOL
  # (5 MAXEVAL / 6 MAXTIME are stops; negatives are errors). glmmTMB wants 0.
  conv <- if (ret$status %in% 1:4) 0L else 1L
  print(paste0("nloptr status: ", ret$status, " (", ret$message, ")"))
  list(
    par = ret$solution,
    objective = ret$objective,
    convergence = conv,
    message = ret$message
  )
}

#' glmmTMB control object for an arbitrary NLopt algorithm
#'
#' The general constructor behind the `glmmTMB_control_nloptr_*()` family: it
#' builds a [glmmTMB::glmmTMBControl()] object that optimizes with
#' [glmmTMB_nloptr_optim()] using whichever NLopt algorithm you name. Prefer the
#' per-algorithm wrappers in [glmmTMB_control_nloptr_methods] for the local
#' algorithms; use this one when you need an algorithm they deliberately omit
#' (a bounded global search, or a meta-algorithm needing `local_opts`) and can
#' supply the extra `opts` it requires.
#'
#' @param algorithm NLopt algorithm name, e.g. `"NLOPT_LN_BOBYQA"`.
#' @param opts A named list of nloptr `opts` (e.g. `maxeval`, `xtol_rel`,
#'   `ftol_rel`, `local_opts`), merged over the algorithm selection and
#'   defaults.
#' @param optArgs A named list of extra arguments for the optimizer function.
#' @return A `glmmTMBControl` object for the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB_control_nloptr_methods], [glmmTMB_nloptr_optim()].
#' @examples
#' \dontrun{
#' glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   family = poisson, data = glmmTMB::Salamanders,
#'   control = glmmTMB_control_nloptr("NLOPT_LN_SBPLX", opts = list(maxeval = 5000))
#' )
#' }
#' @export
# Function to pass to the `control` argument of glmmTMB::glmmTMB
# (mirrors glmmTMB_control_calibrar / glmmTMB_control_optimx so the APIs match)
glmmTMB_control_nloptr <- function(algorithm = "NLOPT_LN_BOBYQA",
                                   opts = list(), optArgs = list()) {
  myopts <- list(algorithm = algorithm, maxeval = 10000L, xtol_rel = 1e-8)
  if (length(opts) > 0) {
    opts <- rlist::list.merge(myopts, opts)
  } else {
    opts <- myopts
  }
  opts$algorithm <- if (!is.null(opts$algorithm)) opts$algorithm else algorithm

  glmmTMB::glmmTMBControl(
    optimizer = glmmTMB_nloptr_optim,
    optArgs = optArgs,
    optCtrl = opts
  )
}

#' glmmTMB control objects using nloptr optimizers
#'
#' A convenience constructor for each NLopt **local** algorithm that can
#' optimize a `glmmTMB` likelihood: each builds a [glmmTMB::glmmTMBControl()]
#' object telling `glmmTMB` to optimize with the named algorithm instead of the
#' default `nlminb()`. Useful when the default optimizer stalls, or to
#' cross-check the optimum. Derivative-free LOCAL algorithms (`_LN_`, e.g.
#' BOBYQA) are the recommended first fallback; derivative-based LOCAL algorithms
#' (`_LD_`) exploit glmmTMB's exact analytic TMB gradient.
#'
#' @section Algorithm classes intentionally NOT provided:
#' NLopt exposes ~37 algorithms, but only the **local** ones are wrapped here.
#' The others are omitted because they cannot work the way `glmmTMB` invokes an
#' optimizer, so exposing them would only offer constructors that always fail:
#' \itemize{
#'   \item **Global algorithms** (`NLOPT_GN_*`, `NLOPT_GD_*` — DIRECT, CRS,
#'     StoGO, ISRES, ESCH, and the MLSL drivers) require **finite** lower/upper
#'     bounds on every parameter. `glmmTMB` optimizes on an unconstrained,
#'     transformed scale and passes `lower = -Inf`, `upper = Inf`, so nloptr
#'     errors before it can start.
#'   \item **AUGLAG / MLSL meta-algorithms** (`NLOPT_LN_AUGLAG`,
#'     `NLOPT_LD_AUGLAG`, `*_AUGLAG_EQ`, `NLOPT_G*_MLSL*`) are not standalone:
#'     they require a subsidiary local optimizer supplied via `opts$local_opts`,
#'     which the plain `glmmTMB` control interface does not set. Without it
#'     nloptr aborts.
#' }
#' If you genuinely need one of these (e.g. a bounded global search), call
#' [glmmTMB_control_nloptr()] directly with the appropriate `opts` (finite
#' `lb`/`ub`, or a `local_opts` list).
#'
#' Each function selects the NLopt algorithm named in its suffix:
#' \describe{
#'   \item{`glmmTMB_control_nloptr_ln_bobyqa`}{`NLOPT_LN_BOBYQA`}
#'   \item{`glmmTMB_control_nloptr_ln_cobyla`}{`NLOPT_LN_COBYLA`}
#'   \item{`glmmTMB_control_nloptr_ln_newuoa`}{`NLOPT_LN_NEWUOA`}
#'   \item{`glmmTMB_control_nloptr_ln_newuoa_bound`}{`NLOPT_LN_NEWUOA_BOUND`}
#'   \item{`glmmTMB_control_nloptr_ln_neldermead`}{`NLOPT_LN_NELDERMEAD`}
#'   \item{`glmmTMB_control_nloptr_ln_sbplx`}{`NLOPT_LN_SBPLX`}
#'   \item{`glmmTMB_control_nloptr_ln_praxis`}{`NLOPT_LN_PRAXIS`}
#'   \item{`glmmTMB_control_nloptr_ld_lbfgs`}{`NLOPT_LD_LBFGS`}
#'   \item{`glmmTMB_control_nloptr_ld_slsqp`}{`NLOPT_LD_SLSQP`}
#'   \item{`glmmTMB_control_nloptr_ld_mma`}{`NLOPT_LD_MMA`}
#'   \item{`glmmTMB_control_nloptr_ld_ccsaq`}{`NLOPT_LD_CCSAQ`}
#'   \item{`glmmTMB_control_nloptr_ld_var1`}{`NLOPT_LD_VAR1`}
#'   \item{`glmmTMB_control_nloptr_ld_var2`}{`NLOPT_LD_VAR2`}
#'   \item{`glmmTMB_control_nloptr_ld_tnewton`}{`NLOPT_LD_TNEWTON`}
#'   \item{`glmmTMB_control_nloptr_ld_tnewton_restart`}{`NLOPT_LD_TNEWTON_RESTART`}
#'   \item{`glmmTMB_control_nloptr_ld_tnewton_precond`}{`NLOPT_LD_TNEWTON_PRECOND`}
#'   \item{`glmmTMB_control_nloptr_ld_tnewton_precond_restart`}{`NLOPT_LD_TNEWTON_PRECOND_RESTART`}
#' }
#'
#' @param opts A named list of nloptr `opts` (e.g. `maxeval`, `xtol_rel`,
#'   `ftol_rel`), merged over the algorithm selection and defaults.
#' @param optArgs A named list of extra arguments for the optimizer function.
#' @return A `glmmTMBControl` object for the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB_control_minqa_methods], [glmmTMB_control_optimx_methods],
#'   [glmmTMB_control_lbfgsb3c()].
#' @name glmmTMB_control_nloptr_methods
#' @examples
#' \dontrun{
#' glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   family = poisson, data = glmmTMB::Salamanders,
#'   control = glmmTMB_control_nloptr_ln_bobyqa()
#' )
#' }
NULL

# ---- LOCAL derivative-free (NLOPT_LN_*) ----

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_bobyqa <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_BOBYQA", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_cobyla <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_COBYLA", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_newuoa <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_NEWUOA", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_newuoa_bound <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_NEWUOA_BOUND", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_neldermead <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_NELDERMEAD", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_sbplx <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_SBPLX", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_praxis <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_PRAXIS", opts = opts, optArgs = optArgs)
}

# ---- LOCAL derivative-based (NLOPT_LD_*) ----

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_lbfgs <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_LBFGS", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_slsqp <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_SLSQP", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_mma <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_MMA", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_ccsaq <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_CCSAQ", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_var1 <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_VAR1", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_var2 <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_VAR2", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_tnewton <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_TNEWTON", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_tnewton_restart <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_TNEWTON_RESTART", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_tnewton_precond <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_TNEWTON_PRECOND", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_tnewton_precond_restart <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_TNEWTON_PRECOND_RESTART", opts = opts, optArgs = optArgs)
}
