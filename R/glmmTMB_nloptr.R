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
#' (`NLOPT_LD_*` / `NLOPT_GD_*`); derivative-free ones (`NLOPT_LN_*` /
#' `NLOPT_GN_*`) ignore it. Global (`_GN_`/`_GD_`) algorithms require FINITE
#' box constraints, which `glmmTMB` does not supply (its transformed parameters
#' are unconstrained), and the AUGLAG / MLSL meta-algorithms require a
#' subsidiary local optimizer via `opts$local_opts`; these therefore need extra
#' setup and are included for completeness. Derivative-free LOCAL algorithms
#' (e.g. BOBYQA) are the recommended fallback for a stalling default optimizer.
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
  cat(paste0("nloptr status: ", ret$status, " (", ret$message, ")
"))
  list(
    par = ret$solution,
    objective = ret$objective,
    convergence = conv,
    message = ret$message
  )
}

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
#' A convenience constructor for every NLopt algorithm exposed by
#' [nloptr::nloptr()]: each builds a [glmmTMB::glmmTMBControl()] object telling
#' `glmmTMB` to optimize the likelihood with the named algorithm instead of the
#' default `nlminb()`. Useful when the default optimizer stalls, or to
#' cross-check the optimum. Derivative-free LOCAL algorithms (`_LN_`, e.g.
#' BOBYQA) are the recommended first fallback. See [glmmTMB_nloptr_optim()] for
#' the caveats on the global (`_GN_`/`_GD_`) and meta (AUGLAG/MLSL) algorithms.
#'
#' Each function selects the NLopt algorithm named in its suffix:
#' \describe{
#'   \item{`glmmTMB_control_nloptr_gn_direct`}{`NLOPT_GN_DIRECT`}
#'   \item{`glmmTMB_control_nloptr_gn_direct_l`}{`NLOPT_GN_DIRECT_L`}
#'   \item{`glmmTMB_control_nloptr_gn_direct_l_rand`}{`NLOPT_GN_DIRECT_L_RAND`}
#'   \item{`glmmTMB_control_nloptr_gn_direct_noscal`}{`NLOPT_GN_DIRECT_NOSCAL`}
#'   \item{`glmmTMB_control_nloptr_gn_direct_l_noscal`}{`NLOPT_GN_DIRECT_L_NOSCAL`}
#'   \item{`glmmTMB_control_nloptr_gn_direct_l_rand_noscal`}{`NLOPT_GN_DIRECT_L_RAND_NOSCAL`}
#'   \item{`glmmTMB_control_nloptr_gn_orig_direct`}{`NLOPT_GN_ORIG_DIRECT`}
#'   \item{`glmmTMB_control_nloptr_gn_orig_direct_l`}{`NLOPT_GN_ORIG_DIRECT_L`}
#'   \item{`glmmTMB_control_nloptr_gd_stogo`}{`NLOPT_GD_STOGO`}
#'   \item{`glmmTMB_control_nloptr_gd_stogo_rand`}{`NLOPT_GD_STOGO_RAND`}
#'   \item{`glmmTMB_control_nloptr_ld_slsqp`}{`NLOPT_LD_SLSQP`}
#'   \item{`glmmTMB_control_nloptr_ld_lbfgs`}{`NLOPT_LD_LBFGS`}
#'   \item{`glmmTMB_control_nloptr_ln_praxis`}{`NLOPT_LN_PRAXIS`}
#'   \item{`glmmTMB_control_nloptr_ld_var1`}{`NLOPT_LD_VAR1`}
#'   \item{`glmmTMB_control_nloptr_ld_var2`}{`NLOPT_LD_VAR2`}
#'   \item{`glmmTMB_control_nloptr_ld_tnewton`}{`NLOPT_LD_TNEWTON`}
#'   \item{`glmmTMB_control_nloptr_ld_tnewton_restart`}{`NLOPT_LD_TNEWTON_RESTART`}
#'   \item{`glmmTMB_control_nloptr_ld_tnewton_precond`}{`NLOPT_LD_TNEWTON_PRECOND`}
#'   \item{`glmmTMB_control_nloptr_ld_tnewton_precond_restart`}{`NLOPT_LD_TNEWTON_PRECOND_RESTART`}
#'   \item{`glmmTMB_control_nloptr_gn_crs2_lm`}{`NLOPT_GN_CRS2_LM`}
#'   \item{`glmmTMB_control_nloptr_gn_mlsl`}{`NLOPT_GN_MLSL`}
#'   \item{`glmmTMB_control_nloptr_gd_mlsl`}{`NLOPT_GD_MLSL`}
#'   \item{`glmmTMB_control_nloptr_gn_mlsl_lds`}{`NLOPT_GN_MLSL_LDS`}
#'   \item{`glmmTMB_control_nloptr_gd_mlsl_lds`}{`NLOPT_GD_MLSL_LDS`}
#'   \item{`glmmTMB_control_nloptr_ld_mma`}{`NLOPT_LD_MMA`}
#'   \item{`glmmTMB_control_nloptr_ld_ccsaq`}{`NLOPT_LD_CCSAQ`}
#'   \item{`glmmTMB_control_nloptr_ln_cobyla`}{`NLOPT_LN_COBYLA`}
#'   \item{`glmmTMB_control_nloptr_ln_newuoa`}{`NLOPT_LN_NEWUOA`}
#'   \item{`glmmTMB_control_nloptr_ln_newuoa_bound`}{`NLOPT_LN_NEWUOA_BOUND`}
#'   \item{`glmmTMB_control_nloptr_ln_neldermead`}{`NLOPT_LN_NELDERMEAD`}
#'   \item{`glmmTMB_control_nloptr_ln_sbplx`}{`NLOPT_LN_SBPLX`}
#'   \item{`glmmTMB_control_nloptr_ln_auglag`}{`NLOPT_LN_AUGLAG`}
#'   \item{`glmmTMB_control_nloptr_ld_auglag`}{`NLOPT_LD_AUGLAG`}
#'   \item{`glmmTMB_control_nloptr_ln_auglag_eq`}{`NLOPT_LN_AUGLAG_EQ`}
#'   \item{`glmmTMB_control_nloptr_ld_auglag_eq`}{`NLOPT_LD_AUGLAG_EQ`}
#'   \item{`glmmTMB_control_nloptr_ln_bobyqa`}{`NLOPT_LN_BOBYQA`}
#'   \item{`glmmTMB_control_nloptr_gn_isres`}{`NLOPT_GN_ISRES`}
#' }
#'
#' @param opts A named list of nloptr `opts` (e.g. `maxeval`, `xtol_rel`,
#'   `ftol_rel`, or `local_opts` for AUGLAG/MLSL), merged over the algorithm
#'   selection and defaults.
#' @param optArgs A named list of extra arguments for the optimizer function.
#' @return A `glmmTMBControl` object for the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB_control_calibrar_methods], [glmmTMB_control_optimparallel()].
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

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_direct <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_DIRECT", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_direct_l <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_DIRECT_L", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_direct_l_rand <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_DIRECT_L_RAND", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_direct_noscal <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_DIRECT_NOSCAL", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_direct_l_noscal <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_DIRECT_L_NOSCAL", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_direct_l_rand_noscal <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_DIRECT_L_RAND_NOSCAL", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_orig_direct <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_ORIG_DIRECT", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_orig_direct_l <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_ORIG_DIRECT_L", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gd_stogo <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GD_STOGO", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gd_stogo_rand <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GD_STOGO_RAND", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_slsqp <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_SLSQP", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_lbfgs <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_LBFGS", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_praxis <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_PRAXIS", opts = opts, optArgs = optArgs)
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

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_crs2_lm <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_CRS2_LM", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_mlsl <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_MLSL", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gd_mlsl <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GD_MLSL", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_mlsl_lds <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_MLSL_LDS", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gd_mlsl_lds <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GD_MLSL_LDS", opts = opts, optArgs = optArgs)
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
glmmTMB_control_nloptr_ln_auglag <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_AUGLAG", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_auglag <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_AUGLAG", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_auglag_eq <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_AUGLAG_EQ", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ld_auglag_eq <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LD_AUGLAG_EQ", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_ln_bobyqa <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_LN_BOBYQA", opts = opts, optArgs = optArgs)
}

#' @rdname glmmTMB_control_nloptr_methods
#' @export
glmmTMB_control_nloptr_gn_isres <- function(opts = list(), optArgs = list()) {
  glmmTMB_control_nloptr("NLOPT_GN_ISRES", opts = opts, optArgs = optArgs)
}
