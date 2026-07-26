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

# --- OPTIMH (calibrar heuristic / global optimisers) --- #
# Mirrors glmmTMB_calibrar.R (which wraps calibrar::optim2) so the optimh,
# optim2 and optimx control APIs are identical: a generic builder plus thin
# per-method `function(optArgs, optCtrl)` wrappers.

# Function to pass to the `optimizer` argument of glmmTMB::glmmTMBControl
glmmTMB_optimh_optimizer <- function(par, fn, gr = NULL, ..., control = list()) {
  myinfo("optimh optimization")
  # No worker cluster is built here — see the note in glmmTMB_calibrar.R:
  # optimh() delegates to the same calibrar:::.optim2(), so `parallel` reaches
  # only the numerical-gradient branch that glmmTMB's analytic TMB gradient
  # bypasses. The old makeCluster() spawned idle workers and leaked them when
  # optimh() threw.
  ret <- calibrar::optimh(
    par = par,
    fn = fn,
    gr = gr,
    ...,
    control = control,
    parallel = TRUE
  )
  # convergence: An integer code. 0 indicates successful completion.
  convergence = ret$convergence == 0
  myinfo("optimh convergence:", convergence)
  myinfo("optimh message:", ret$message)
  # glmmTMB automatically handles output from optim() if we rename the value component to objective
  mask <- names(ret) == "value"
  names(ret)[mask] <- "objective"
  return(ret)
}

# Function to pass to the `control` argument of glmmTMB::glmmTMB
glmmTMB_control_optimh <- function(optArgs=list(), optCtrl=list(), method = NULL) {

  if (is.null(method)) {
    stop("Please specify a method for optimh optimization.")
  }

  ncores <- parallel::detectCores() - 1

  # Build optArgs
  myoptArgs = list(method = method)
  if(length(optArgs) > 0) {
    optArgs = rlist::list.merge(myoptArgs, optArgs)
  } else {
    optArgs = myoptArgs
  }

  # optCtrl is passed through untouched: it used to carry an `ncores` default
  # that existed only to size the optimizer's worker cluster (now removed), and
  # calibrar has no `ncores` control of its own.

  # Build glmmTMBControl
  res <- glmmTMB::glmmTMBControl(
    parallel = list(n=ncores, autopar=TRUE),
    optimizer = glmmTMB_optimh_optimizer,
    optArgs = optArgs,
    optCtrl = optCtrl
  )
  return(res)
}

#' glmmTMB control objects using calibrar's global/heuristic optimizers
#'
#' A family of convenience constructors, one per optimization method, that build
#' a [glmmTMB::glmmTMBControl()] object instructing `glmmTMB` to optimize the
#' likelihood with the corresponding global or heuristic method from
#' [calibrar::optimh()] rather than the default `nlminb()`. These derivative-free
#' / population-based methods are slower but more robust to multi-modal or badly
#' scaled likelihoods. The likelihood is evaluated with `glmmTMB`'s own parallel
#' support (`parallel::detectCores() - 1` threads, `autopar = TRUE`); the
#' optimizer itself runs single-threaded.
#'
#' Each function selects the method named in its suffix:
#' \describe{
#'   \item{`glmmTMB_control_optimh_ahres`}{`"AHR-ES"`}
#'   \item{`glmmTMB_control_optimh_nelder_mead`}{`"Nelder-Mead"`}
#'   \item{`glmmTMB_control_optimh_sann`}{`"SANN"` (simulated annealing)}
#'   \item{`glmmTMB_control_optimh_hjn`}{`"hjn"` (Hooke-Jeeves)}
#'   \item{`glmmTMB_control_optimh_bobyqa`}{`"bobyqa"`}
#'   \item{`glmmTMB_control_optimh_cmaes`}{`"CMA-ES"`}
#'   \item{`glmmTMB_control_optimh_gensa`}{`"genSA"` (generalised simulated annealing)}
#'   \item{`glmmTMB_control_optimh_de`}{`"DE"` (differential evolution)}
#'   \item{`glmmTMB_control_optimh_soma`}{`"soma"`}
#'   \item{`glmmTMB_control_optimh_genoud`}{`"genoud"` (genetic optimisation)}
#'   \item{`glmmTMB_control_optimh_pso`}{`"PSO"` (particle swarm)}
#'   \item{`glmmTMB_control_optimh_hybridpso`}{`"hybridPSO"`}
#'   \item{`glmmTMB_control_optimh_mads`}{`"mads"`}
#'   \item{`glmmTMB_control_optimh_hjk`}{`"hjk"`}
#'   \item{`glmmTMB_control_optimh_hjkb`}{`"hjkb"` (bounded Hooke-Jeeves)}
#'   \item{`glmmTMB_control_optimh_nmk`}{`"nmk"` (Nelder-Mead, Kelley)}
#'   \item{`glmmTMB_control_optimh_nmkb`}{`"nmkb"` (bounded `nmk`)}
#' }
#'
#' @param optArgs A named list of extra arguments for [calibrar::optimh()],
#'   merged with (and overriding) the method selection.
#' @param optCtrl A named list passed as the `control` argument of
#'   [calibrar::optimh()]; `ncores` sets the size of the parallel cluster.
#' @return A `glmmTMBControl` object to pass to the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB_control_calibrar_bfgs()] and [glmmTMB_control_optimx_nvm()]
#'   for the analogous local/`optimx` optimizer families.
#' @name glmmTMB_control_optimh_methods
#' @examples
#' \dontrun{
#' glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   family = poisson, data = glmmTMB::Salamanders,
#'   control = glmmTMB_control_optimh_bobyqa()
#' )
#' }
NULL

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_ahres <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "AHR-ES"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_nelder_mead <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "Nelder-Mead"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_sann <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "SANN"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_hjn <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "hjn"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_bobyqa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "bobyqa"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_cmaes <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "CMA-ES"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_gensa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "genSA"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_de <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "DE"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_soma <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "soma"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_genoud <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "genoud"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_pso <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "PSO"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_hybridpso <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "hybridPSO"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_mads <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "mads"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_hjk <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "hjk"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_hjkb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "hjkb"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_nmk <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "nmk"))
}

#' @rdname glmmTMB_control_optimh_methods
#' @export
glmmTMB_control_optimh_nmkb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "nmkb"))
}
