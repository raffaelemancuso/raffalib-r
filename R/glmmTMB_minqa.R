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

# --- GLMMTMB MINQA OPTIMIZER --- #

#' Optimizer wrapper around the minqa solvers for glmmTMB
#'
#' Adapts Powell's derivative-free solvers in [minqa][minqa::bobyqa]
#' (`bobyqa`, `newuoa`, `uobyqa`) to the calling convention `glmmTMB` expects
#' for the `optimizer` argument of [glmmTMB::glmmTMBControl()]: it maps
#' `par`/`fn` onto the minqa call, returns a list with `par`, `objective` and a
#' `convergence` code (0 = success, from minqa's `ierr == 0`). The `method`
#' argument (passed through `optArgs` by the constructors) selects the solver.
#' Use a `glmmTMB_control_minqa_*()` constructor rather than calling this
#' directly.
#'
#' All three solvers are **derivative-free**, so `gr` is ignored; `bobyqa`
#' honours the box constraints `lower`/`upper`, while `newuoa`/`uobyqa` are
#' unconstrained (any finite bounds are dropped with a warning by minqa).
#'
#' @param par Starting parameter vector.
#' @param fn Objective function (returns a scalar).
#' @param gr Ignored (minqa solvers are derivative-free); accepted for API
#'   compatibility.
#' @param lower,upper Box constraints for `bobyqa` (scalar or length-`par`);
#'   ignored by `newuoa`/`uobyqa`.
#' @param control A named list of minqa control parameters (e.g. `maxfun`,
#'   `rhobeg`, `rhoend`, `npt`); `iprint` defaults to 0 (silent).
#' @param method One of `"bobyqa"`, `"newuoa"`, `"uobyqa"`.
#' @param ... Absorbed for API compatibility.
#' @return A list with `par`, `objective`, `convergence` (0 = converged) and
#'   `message`.
#' @export
glmmTMB_minqa_optim <- function(par, fn, gr = NULL, lower = -Inf, upper = Inf,
                                control = list(), method = "bobyqa", ...) {
  # base print(), not myinfo(): serialized to workers by fit_glmmTMB_parallel(),
  # which does not load raffalib there (only base / pkg::fun survive the trip).
  print("minqa optimization")
  if (is.null(control$iprint)) control$iprint <- 0L
  # minqa derives rhobeg = 0.2 * max(abs(par)); glmmTMB starts every parameter
  # at 0, so that collapses to 0 and rhoend = 1e-6*rhobeg = 0 fails minqa's
  # `0 < rhoend` check. Floor the scale at 1 so the trust-region radii are
  # positive (minqa still adapts them during the search).
  if (is.null(control$rhobeg)) control$rhobeg <- min(0.95, 0.2 * max(1, max(abs(par))))
  if (is.null(control$rhoend)) control$rhoend <- 1e-6 * control$rhobeg

  # glmmTMB's objective (TMB's obj$fn) carries `...` in its formals; wrap it in
  # a fixed-signature closure so minqa calls it cleanly as fn(x).
  eval_f <- function(x) fn(x)

  n <- length(par)
  ret <- switch(method,
    bobyqa = {
      lb <- if (length(lower) == 1L) rep(lower, n) else lower
      ub <- if (length(upper) == 1L) rep(upper, n) else upper
      minqa::bobyqa(par, eval_f, lower = lb, upper = ub, control = control)
    },
    newuoa = minqa::newuoa(par, eval_f, control = control),
    uobyqa = minqa::uobyqa(par, eval_f, control = control),
    stop("Unknown minqa method: ", method)
  )

  # minqa: ierr == 0 signals a normal exit; anything else is a warning/error.
  conv <- if (isTRUE(ret$ierr == 0)) 0L else 1L
  list(
    par = ret$par,
    objective = ret$fval,
    convergence = conv,
    message = ret$msg
  )
}

# Function to pass to the `control` argument of glmmTMB::glmmTMB
# (mirrors glmmTMB_control_optimx so the minqa and optimx APIs match)
glmmTMB_control_minqa <- function(optArgs = list(), optCtrl = list(),
                                  method = NULL) {
  if (is.null(method)) {
    stop("Please specify a method for minqa optimization.")
  }
  myoptArgs <- list(method = method)
  if (length(optArgs) > 0) {
    optArgs <- rlist::list.merge(myoptArgs, optArgs)
  } else {
    optArgs <- myoptArgs
  }
  glmmTMB::glmmTMBControl(
    optimizer = glmmTMB_minqa_optim,
    optArgs = optArgs,
    optCtrl = optCtrl
  )
}

#' glmmTMB control objects using the minqa optimizers
#'
#' Convenience constructors, one per Powell derivative-free solver in
#' [minqa][minqa::bobyqa], that build a [glmmTMB::glmmTMBControl()] object
#' instructing `glmmTMB` to optimize the likelihood with that solver (via
#' [glmmTMB_minqa_optim()]) rather than the default `nlminb()`. Handy as a
#' fallback when a gradient-based optimizer stalls, or to cross-check the
#' optimum with a derivative-free method.
#'
#' Each function selects the solver named in its suffix:
#' \describe{
#'   \item{`glmmTMB_control_minqa_bobyqa`}{`bobyqa` (bound-constrained quadratic approximation)}
#'   \item{`glmmTMB_control_minqa_newuoa`}{`newuoa` (unconstrained quadratic approximation)}
#'   \item{`glmmTMB_control_minqa_uobyqa`}{`uobyqa` (unconstrained quadratic approximation, full quadratic model)}
#' }
#'
#' @param optArgs A named list of extra arguments for [glmmTMB_minqa_optim()],
#'   merged with (and overriding) the method selection.
#' @param optCtrl A named list of minqa control parameters passed as the
#'   `optCtrl` argument of [glmmTMB::glmmTMBControl()].
#' @return A `glmmTMBControl` object for the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB_control_lbfgsb3c()], [glmmTMB_control_nloptr_methods],
#'   [glmmTMB_control_optimx_methods].
#' @name glmmTMB_control_minqa_methods
#' @examples
#' \dontrun{
#' glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   family = poisson, data = glmmTMB::Salamanders,
#'   control = glmmTMB_control_minqa_bobyqa()
#' )
#' }
NULL

#' @rdname glmmTMB_control_minqa_methods
#' @export
glmmTMB_control_minqa_bobyqa <- function(optArgs = list(), optCtrl = list()) {
  glmmTMB_control_minqa(optArgs = optArgs, optCtrl = optCtrl, method = "bobyqa")
}

#' @rdname glmmTMB_control_minqa_methods
#' @export
glmmTMB_control_minqa_newuoa <- function(optArgs = list(), optCtrl = list()) {
  glmmTMB_control_minqa(optArgs = optArgs, optCtrl = optCtrl, method = "newuoa")
}

#' @rdname glmmTMB_control_minqa_methods
#' @export
glmmTMB_control_minqa_uobyqa <- function(optArgs = list(), optCtrl = list()) {
  glmmTMB_control_minqa(optArgs = optArgs, optCtrl = optCtrl, method = "uobyqa")
}
