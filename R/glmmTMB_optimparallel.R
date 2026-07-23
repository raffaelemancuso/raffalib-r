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

# --- GLMMTMB OPTIMPARALLEL OPTIMIZER --- #

#' Optimizer wrapper around optimParallel::optimParallel() for glmmTMB
#'
#' Adapts [optimParallel::optimParallel()] (a parallel L-BFGS-B) to the
#' `optimizer` calling convention of [glmmTMB::glmmTMBControl()], renaming the
#' returned `value` element to `objective`. Use
#' [glmmTMB_control_optimparallel()] rather than calling this directly.
#'
#' @section Caveat — this rarely speeds up glmmTMB:
#' `optimParallel` parallelises the *numerical* gradient, evaluating the
#' objective at perturbed parameters across the cluster. But `glmmTMB` supplies
#' its optimizer an exact **analytic** gradient from TMB's autodiff, which is
#' cheap (one AD pass), so there is little for `optimParallel` to parallelise;
#' forcing a numerical gradient would be *slower*, not faster. Moreover the
#' objective is backed by a compiled TMB DLL that does not always serialise to
#' worker processes. To actually speed up a model *loop*, prefer
#' [fit_glmmTMB_parallel()] (parallel across independent fits). This wrapper is
#' provided for completeness and benchmarking.
#'
#' @param par Starting parameter vector.
#' @param fn Objective function.
#' @param gr Gradient function, or `NULL`.
#' @param lower,upper Box constraints (scalar or length-`par`).
#' @param control Passed as `optimParallel`'s `control` list; the special entry
#'   `ncores` sets the cluster size and is stripped before the call.
#' @param ... Absorbed for API compatibility.
#' @return The [optimParallel::optimParallel()] list with `value` renamed to
#'   `objective`.
#' @export
glmmTMB_optimparallel_optim <- function(par, fn, gr = NULL, lower = -Inf,
                                        upper = Inf, control = list(), ...) {
  print("optimParallel optimization")
  ncores <- if (!is.null(control$ncores)) control$ncores else max(1L, parallel::detectCores() - 1L)

  cl <- parallel::makeCluster(ncores)
  optimParallel::setDefaultCluster(cl)
  on.exit({
    optimParallel::setDefaultCluster(NULL)
    parallel::stopCluster(cl)
  }, add = TRUE)

  n <- length(par)
  lb <- if (length(lower) == 1L) rep(lower, n) else lower
  ub <- if (length(upper) == 1L) rep(upper, n) else upper

  ctrl <- control
  ctrl$ncores <- NULL  # not an optim() control entry

  ret <- optimParallel::optimParallel(
    par = par, fn = fn, gr = gr,
    lower = lb, upper = ub,
    control = ctrl,
    parallel = list(loginfo = FALSE, forward = FALSE)
  )

  # glmmTMB expects `objective` (optim returns `value`); it reads `convergence`
  mask <- names(ret) == "value"
  names(ret)[mask] <- "objective"
  ret
}

#' glmmTMB control object using the parallel L-BFGS-B optimParallel
#'
#' Builds a [glmmTMB::glmmTMBControl()] instructing `glmmTMB` to optimise with
#' [optimParallel::optimParallel()] instead of `nlminb()`. See
#' [glmmTMB_optimparallel_optim()] for the important caveat that this seldom
#' speeds up `glmmTMB` (its gradient is already analytic) — [fit_glmmTMB_parallel()]
#' is the recommended way to parallelise a model loop.
#'
#' @param ncores Cluster size. Default `parallel::detectCores() - 1`.
#' @param optCtrl A named list merged into the `optim()` `control` (e.g.
#'   `maxit`, `factr`, `pgtol`); `ncores` is honoured here too.
#' @param optArgs A named list of extra arguments for the optimizer function.
#' @return A `glmmTMBControl` object for the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [fit_glmmTMB_parallel()], [glmmTMB_control_nloptr_methods].
#' @examples
#' \dontrun{
#' glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   family = poisson, data = glmmTMB::Salamanders,
#'   control = glmmTMB_control_optimparallel()
#' )
#' }
#' @export
glmmTMB_control_optimparallel <- function(ncores = NULL, optCtrl = list(),
                                          optArgs = list()) {
  if (is.null(ncores)) ncores <- max(1L, parallel::detectCores() - 1L)
  myctrl <- list(ncores = ncores)
  if (length(optCtrl) > 0) {
    optCtrl <- rlist::list.merge(myctrl, optCtrl)
  } else {
    optCtrl <- myctrl
  }
  glmmTMB::glmmTMBControl(
    optimizer = glmmTMB_optimparallel_optim,
    optArgs = optArgs,
    optCtrl = optCtrl
  )
}
