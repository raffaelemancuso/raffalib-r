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

# --- CALIBRAR --- #

# Function to pass to the `optimizer` argument of glmmTMB::glmmTMBControl
glmmTMB_calibrar_optimizer <- function(par, fn, gr = NULL, ..., control = list()) {
  print("[glmmTMB_calibrar_optimizer] calibrar optimization")
  #print("[glmmTMB_calibrar_optimizer] control:")
  #print(control)
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
  cat(paste0("calibrar convergence: ", convergence, "\n"))
  cat(paste0("calibrar message: ", ret$message, "\n"))
  # glmmTMB automatically handles output from optim() if we rename the value component to objective
  mask <- names(ret) == "value"
  names(ret)[mask] <- "objective"
  return(ret)
}

# Function to pass to the `control` argument of glmmTMB::glmmTMB
glmmTMB_control_calibrar <- function(optArgs=list(), optCtrl=list(), method = NULL) {

  if (is.null(method)) {
    stop("Please specify a method for calibrar optimization.")
  }

  ncores <- parallel::detectCores() - 1

  # Build optArgs
  myoptArgs = list(method = method)
  if(length(optArgs) > 0) {
    optArgs = rlist::list.merge(myoptArgs, optArgs)
  } else {
    optArgs = myoptArgs
  }
  #print("[glmmTMB_control_calibrar] optArgs:")
  #print(optArgs)

  # Build optCtrl
  myoptCtrl = list(ncores = ncores)
  if(length(optCtrl) > 0) {
    optCtrl = rlist::list.merge(myoptCtrl, optCtrl)
  } else {
    optCtrl = myoptCtrl
  }
  #print("[glmmTMB_control_calibrar] optCtrl:")
  #print(optCtrl)

  # Build glmmTMBControl
  res <- glmmTMB::glmmTMBControl(
    parallel = list(n=ncores, autopar=TRUE),
    optimizer = glmmTMB_calibrar_optimizer,
    optArgs = optArgs,
    optCtrl = optCtrl,
    # eigval_check = FALSE,
    # rank_check = "skip",
    # conv_check = "skip"
  )
  return(res)
}

#' glmmTMB control objects using calibrar's local optimizers
#'
#' A family of convenience constructors, one per optimization method, that build
#' a [glmmTMB::glmmTMBControl()] object instructing `glmmTMB` to optimize the
#' likelihood with the corresponding method from [calibrar::optim2()] rather than
#' the default `nlminb()`. They are useful when the default optimizer fails to
#' converge or you want to cross-check the optimum. The optimization is run in
#' parallel across the available cores (`parallel::detectCores() - 1`).
#'
#' Each function selects the method named in its suffix:
#' \describe{
#'   \item{`glmmTMB_control_calibrar_nelder_mead`}{`"Nelder-Mead"`}
#'   \item{`glmmTMB_control_calibrar_bfgs`}{`"BFGS"`}
#'   \item{`glmmTMB_control_calibrar_cg`}{`"CG"`}
#'   \item{`glmmTMB_control_calibrar_lbfgsb`}{`"L-BFGS-B"`}
#'   \item{`glmmTMB_control_calibrar_sann`}{`"SANN"`}
#'   \item{`glmmTMB_control_calibrar_brent`}{`"Brent"`}
#'   \item{`glmmTMB_control_calibrar_nlm`}{`"nlm"`}
#'   \item{`glmmTMB_control_calibrar_nlminb`}{`"nlminb"`}
#'   \item{`glmmTMB_control_calibrar_rcgmin`}{`"Rcgmin"`}
#'   \item{`glmmTMB_control_calibrar_Rvmmin`}{`"Rvmmin"`}
#'   \item{`glmmTMB_control_calibrar_hjn`}{`"hjn"`}
#'   \item{`glmmTMB_control_calibrar_spg`}{`"spg"`}
#'   \item{`glmmTMB_control_calibrar_lbfgsb3`}{`"LBFGSB3"`}
#'   \item{`glmmTMB_control_calibrar_ahres`}{`"AHR-ES"`}
#' }
#'
#' @param optArgs A named list of extra arguments for [calibrar::optim2()],
#'   merged with (and overriding) the method selection.
#' @param optCtrl A named list passed as the `control` argument of
#'   [calibrar::optim2()]; `ncores` sets the size of the parallel cluster.
#' @return A `glmmTMBControl` object to pass to the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB_control_optimh_ahres()] and [glmmTMB_control_optimx_nvm()]
#'   for the analogous global/`optimx` optimizer families.
#' @name glmmTMB_control_calibrar_methods
#' @examples
#' \dontrun{
#' glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   family = poisson, data = glmmTMB::Salamanders,
#'   control = glmmTMB_control_calibrar_bfgs()
#' )
#' }
NULL

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_nelder_mead <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "Nelder-Mead"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_bfgs <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "BFGS"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_cg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "CG"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_lbfgsb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "L-BFGS-B"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_sann <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "SANN"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_brent <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "Brent"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_nlm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "nlm"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_nlminb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "nlminb"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_rcgmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "Rcgmin"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_Rvmmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "Rvmmin"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_hjn <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "hjn"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_spg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "spg"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_lbfgsb3 <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "LBFGSB3"))
}

#' @rdname glmmTMB_control_calibrar_methods
#' @export
glmmTMB_control_calibrar_ahres <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_calibrar(optArgs=optArgs, optCtrl=optCtrl, method = "AHR-ES"))
}
