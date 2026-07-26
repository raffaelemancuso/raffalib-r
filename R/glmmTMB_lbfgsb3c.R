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

# --- GLMMTMB LBFGSB3C OPTIMIZER --- #

#' Optimizer wrapper around lbfgsb3c::lbfgsb3c() for glmmTMB
#'
#' Adapts [lbfgsb3c::lbfgsb3c()] — the 2011 reference implementation of the
#' limited-memory box-constrained BFGS (L-BFGS-B) with a C interface — to the
#' calling convention `glmmTMB` expects for the `optimizer` argument of
#' [glmmTMB::glmmTMBControl()]. The return is already `optim()`-compatible, so
#' this only renames `value` to `objective` and normalises `convergence`
#' (0 = success). Use [glmmTMB_control_lbfgsb3c()] rather than calling this
#' directly.
#'
#' L-BFGS-B is gradient-based; `glmmTMB` supplies an exact analytic gradient
#' from TMB's autodiff, which is passed straight through (`gr`). This makes it a
#' natural drop-in alternative to the default `nlminb()` for a stalling fit.
#'
#' @param par Starting parameter vector.
#' @param fn Objective function (returns a scalar).
#' @param gr Gradient function, or `NULL` (then lbfgsb3c uses a numerical
#'   gradient).
#' @param lower,upper Box constraints (scalar or length-`par`); default
#'   unconstrained.
#' @param control A named list of [lbfgsb3c::lbfgsb3c()] control parameters
#'   (e.g. `maxit`, `factr`, `pgtol`); `trace` defaults to 0 (silent).
#' @param ... Absorbed for API compatibility.
#' @return The [lbfgsb3c::lbfgsb3c()] list with `value` renamed to `objective`
#'   and `convergence` normalised to 0 (converged) / 1.
#' @export
glmmTMB_lbfgsb3c_optim <- function(par, fn, gr = NULL, lower = -Inf,
                                   upper = Inf, control = list(), ...) {
  # base print(), not myinfo(): serialized to workers by fit_glmmTMB_parallel(),
  # which does not load raffalib there (only base / pkg::fun survive the trip).
  print("lbfgsb3c optimization")
  if (is.null(control$trace)) control$trace <- 0L

  # glmmTMB's objective/gradient (TMB's obj$fn/obj$gr) carry `...` in their
  # formals; wrap them in fixed-signature closures for a clean call.
  eval_f <- function(x) fn(x)
  eval_g <- if (!is.null(gr)) function(x) gr(x) else NULL

  n <- length(par)
  lb <- if (length(lower) == 1L) rep(lower, n) else lower
  ub <- if (length(upper) == 1L) rep(upper, n) else upper

  ret <- lbfgsb3c::lbfgsb3c(
    par = par, fn = eval_f, gr = eval_g,
    lower = lb, upper = ub, control = control
  )

  # lbfgsb3c returns an optim()-style list: `value` + `convergence` (0 = ok).
  mask <- names(ret) == "value"
  names(ret)[mask] <- "objective"
  ret$convergence <- if (isTRUE(ret$convergence == 0)) 0L else 1L
  ret
}

#' glmmTMB control object using the lbfgsb3c L-BFGS-B optimizer
#'
#' Builds a [glmmTMB::glmmTMBControl()] instructing `glmmTMB` to optimize the
#' likelihood with [lbfgsb3c::lbfgsb3c()] (via [glmmTMB_lbfgsb3c_optim()])
#' instead of the default `nlminb()`. This calls the `lbfgsb3c` package
#' directly, distinct from routing L-BFGS-B through
#' [glmmTMB_control_optimx_lbfgsb3c()] (which dispatches via `optimx`).
#'
#' @param optArgs A named list of extra arguments for [glmmTMB_lbfgsb3c_optim()].
#' @param optCtrl A named list of [lbfgsb3c::lbfgsb3c()] control parameters
#'   passed as the `optCtrl` argument of [glmmTMB::glmmTMBControl()].
#' @return A `glmmTMBControl` object for the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB_control_minqa_methods], [glmmTMB_control_nloptr_methods],
#'   [glmmTMB_control_optimx_methods].
#' @examples
#' \dontrun{
#' glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   family = poisson, data = glmmTMB::Salamanders,
#'   control = glmmTMB_control_lbfgsb3c()
#' )
#' }
#' @export
glmmTMB_control_lbfgsb3c <- function(optArgs = list(), optCtrl = list()) {
  glmmTMB::glmmTMBControl(
    optimizer = glmmTMB_lbfgsb3c_optim,
    optArgs = optArgs,
    optCtrl = optCtrl
  )
}
