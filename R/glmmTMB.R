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

#' @export
glmmTMB_get_hessian_1 <- function(mod) {
  # See: https://github.com/glmmTMB/glmmTMB/issues/1226#issuecomment-3181703715
  # 250% faster than method 2
  bestpar <- with(mod$obj$env, last.par.best[-random])
  H1 <- with(mod$obj, optimHess(bestpar, fn, gr))
  return(H1)
}

#' @export
glmmTMB_get_hessian_2 <- function(mod) {
  # See: https://github.com/glmmTMB/glmmTMB/issues/1226#issuecomment-3181703715
  bestpar <- with(mod$obj$env, last.par.best[-random])
  H2 <- numDeriv::jacobian(mod$obj$gr, bestpar)
  return(H2)
}

#' Get current estimated coefficients from a glmmTMB model
#'
#' @return Object to be passed to the `start` argument of `glmmTMB`
#' @export
glmmTMB_get_optimum <- function(mod) {
  # Fixed effects
  starting_point <- list(
    beta = glmmTMB::fixef(mod)$cond,
    betazi = glmmTMB::fixef(mod)$zi,
    betadisp = glmmTMB::fixef(mod)$disp
  )
  # Random effects
  # ref <- glmmTMB::ranef(mod)
  # if(length(ref$cond) > 0) {
  #   starting_point$theta = ref$cond
  # }
  # if(length(ref$zi) > 0) {
  #   starting_point$thetazi = ref$zi
  # }
  # if(length(ref$disp) > 0) {
  #   starting_point$thetadisp = ref$disp
  # }
  return(starting_point)
}

# --- OPTMIX --- #

#' Use optmix's optimizers for glmmTMB
#'
#' @return Object to be passed to the `optimizer` argument of `glmmTMBControl()`
#' @export
glmmTMB_optmix_optim <- function(...) {
  print("optmix optimization")
  ret <- optimx::optimr(...)
  # glmmTMB automatically handles output from optim(), by renaming the value component to objective
  mask <- names(ret) == "value"
  names(ret)[mask] <- "objective"
  return(ret)
}

#' @export
glmmTMB_optmix_control_nvm <- glmmTMB::glmmTMBControl(
  parallel = parallel::detectCores() - 2,
  optimizer = glmmTMB_optmix_optim,
  optArgs = list(method = "nvm")
)

#' @export
glmmTMB_optmix_control_lbfgsb3c <- glmmTMB::glmmTMBControl(
  parallel = parallel::detectCores() - 2,
  optimizer = glmmTMB_optmix_optim,
  optArgs = list(method = "lbfgsb3c")
)

#' @export
glmmTMB_optmix_control_hjkb <- glmmTMB::glmmTMBControl(
  parallel = parallel::detectCores() - 2,
  optimizer = glmmTMB_optmix_optim,
  optArgs = list(method = "hjkb")
)

#' @export
glmmTMB_optmix_control_ncg <- glmmTMB::glmmTMBControl(
  parallel = parallel::detectCores() - 2,
  optimizer = glmmTMB_optmix_optim,
  optArgs = list(method = "ncg")
)

#' @export
glmmTMB_optmix_control_lbfgsb <- glmmTMB::glmmTMBControl(
  parallel = parallel::detectCores() - 2,
  optimizer = glmmTMB_optmix_optim,
  optArgs = list(method = "L-BFGS-B”")
)

# --- CALIBRAR --- #

# Function to pass to the `optimizer` argument of glmmTMB::glmmTMBControl
glmmTMB_calibrar_optim <- function(par, fn, gr = NULL, ..., control = list()) {
  print("calibrar optimization")
  if (!is.null(control$ncores)) {
    cl <- parallel::makeCluster(control$ncores)
  }
  ret <- calibrar::optim2(
    par = par,
    fn = fn,
    gr = gr,
    ...,
    control = control,
    parallel = TRUE
  )
  if (!is.null(control$ncores)) {
    parallel::stopCluster(cl) # close the parallel connections
  }
  # debug
  # convergence: An integer code. 0 indicates successful completion.
  convergence = ret$convergence == 0
  print(paste0("calibrar convergence: ", convergence))
  print(paste0("calibrar message: ", ret$message))
  # glmmTMB automatically handles output from optim(), by renaming the value component to objective
  mask <- names(ret) == "value"
  names(ret)[mask] <- "objective"
  return(ret)
}

# Function to pass to the `control` argument of glmmTMB::glmmTMB
glmmTMB_calibrar_control <- function(..., method = NULL) {
  if (is.null(method)) {
    stop("Please specify a method for calibrar optimization.")
  }
  ncores <- parallel::detectCores() - 1
  res <- glmmTMB::glmmTMBControl(
    parallel = list(n=ncores, autopar=TRUE),
    optimizer = glmmTMB_calibrar_optim,
    optArgs = list(method = method),
    optCtrl = list(ncores = ncores),
    # eigval_check = FALSE,
    # rank_check = "skip",
    # conv_check = "skip"
  )
  return(res)
}

#' @export
glmmTMB_calibrar_control_lbfgsb3 <- function(...) {
  return(glmmTMB_calibrar_control(method = "LBFGSB3"))
}

#' @export
glmmTMB_calibrar_control_ahres <- function(...) {
  return(glmmTMB_calibrar_control(method = "AHR-ES"))
}

#' @export
glmmTMB_calibrar_control_nlm <- function(...) {
  return(glmmTMB_calibrar_control(method = "NLM"))
}
