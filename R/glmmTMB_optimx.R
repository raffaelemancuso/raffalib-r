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

# --- GLMMTMB OPTIMX OPTIMIZER --- #

#' Optimizer wrapper around optimx::optimr() for glmmTMB
#'
#' Adapts [optimx::optimr()] to the calling convention `glmmTMB` expects for the
#' `optimizer` argument of [glmmTMB::glmmTMBControl()], renaming the returned
#' `value` element to `objective`. You normally do not call this directly; use
#' one of the `glmmTMB_control_optimx_*()` constructors instead.
#'
#' @param ... Arguments forwarded to [optimx::optimr()] (notably `par`, `fn`,
#'   `gr`, `method` and `control`).
#' @return The list returned by [optimx::optimr()] with its `value` element
#'   renamed to `objective`.
#' @export
glmmTMB_optimx_optim <- function(...) {
  myinfo("optimx optimization")
  ret <- optimx::optimr(...)
  # glmmTMB automatically handles output from optim(), by renaming the value component to objective
  mask <- names(ret) == "value"
  names(ret)[mask] <- "objective"
  return(ret)
}

# Function to pass to the `control` argument of glmmTMB::glmmTMB
# (mirrors glmmTMB_control_calibrar so the optimx and calibrar APIs match)
glmmTMB_control_optimx <- function(optArgs=list(), optCtrl=list(), method = NULL) {

  if (is.null(method)) {
    stop("Please specify a method for optimx optimization.")
  }

  ncores <- parallel::detectCores() - 2

  # Build optArgs
  myoptArgs = list(method = method)
  if(length(optArgs) > 0) {
    optArgs = rlist::list.merge(myoptArgs, optArgs)
  } else {
    optArgs = myoptArgs
  }

  # Build glmmTMBControl
  res <- glmmTMB::glmmTMBControl(
    parallel = ncores,
    optimizer = glmmTMB_optimx_optim,
    optArgs = optArgs,
    optCtrl = optCtrl
  )
  return(res)
}

#' glmmTMB control objects using optimx optimizers
#'
#' A family of convenience constructors, one per optimization method, that build
#' a [glmmTMB::glmmTMBControl()] object instructing `glmmTMB` to optimize the
#' likelihood with the corresponding method from [optimx::optimr()] (via
#' [glmmTMB_optimx_optim()]) rather than the default `nlminb()`. This exposes the
#' large catalogue of optimizers bundled with `optimx`, handy for cross-checking
#' convergence or rescuing a difficult fit.
#'
#' Each function selects the method named in its suffix:
#' \describe{
#'   \item{`glmmTMB_control_optimx_nvm`}{`"nvm"`}
#'   \item{`glmmTMB_control_optimx_lbfgsb3c`}{`"lbfgsb3c"`}
#'   \item{`glmmTMB_control_optimx_hjkb`}{`"hjkb"`}
#'   \item{`glmmTMB_control_optimx_ncg`}{`"ncg"`}
#'   \item{`glmmTMB_control_optimx_lbfgsb`}{`"L-BFGS-B"`}
#'   \item{`glmmTMB_control_optimx_bfgs`}{`"BFGS"`}
#'   \item{`glmmTMB_control_optimx_cg`}{`"CG"`}
#'   \item{`glmmTMB_control_optimx_neldermead`}{`"Nelder-Mead"`}
#'   \item{`glmmTMB_control_optimx_nlm`}{`"nlm"`}
#'   \item{`glmmTMB_control_optimx_nlminb`}{`"nlminb"`}
#'   \item{`glmmTMB_control_optimx_rcgmin`}{`"Rcgmin"`}
#'   \item{`glmmTMB_control_optimx_rtnmin`}{`"Rtnmin"`}
#'   \item{`glmmTMB_control_optimx_rvmmin`}{`"Rvmmin"`}
#'   \item{`glmmTMB_control_optimx_snewton`}{`"snewton"`}
#'   \item{`glmmTMB_control_optimx_snewtonm`}{`"snewtonm"`}
#'   \item{`glmmTMB_control_optimx_spg`}{`"spg"`}
#'   \item{`glmmTMB_control_optimx_ucminf`}{`"ucminf"`}
#'   \item{`glmmTMB_control_optimx_newuoa`}{`"newuoa"`}
#'   \item{`glmmTMB_control_optimx_bobyqa`}{`"bobyqa"`}
#'   \item{`glmmTMB_control_optimx_uobyqa`}{`"uobyqa"`}
#'   \item{`glmmTMB_control_optimx_nmkb`}{`"nmkb"`}
#'   \item{`glmmTMB_control_optimx_hjn`}{`"hjn"`}
#'   \item{`glmmTMB_control_optimx_lbfgs`}{`"lbfgs"`}
#'   \item{`glmmTMB_control_optimx_subplex`}{`"subplex"`}
#'   \item{`glmmTMB_control_optimx_mla`}{`"mla"`}
#'   \item{`glmmTMB_control_optimx_slsqp`}{`"slsqp"`}
#'   \item{`glmmTMB_control_optimx_tnewt`}{`"tnewt"`}
#'   \item{`glmmTMB_control_optimx_anms`}{`"anms"`}
#'   \item{`glmmTMB_control_optimx_pracmanm`}{`"pracmanm"`}
#'   \item{`glmmTMB_control_optimx_nlnm`}{`"nlnm"`}
#'   \item{`glmmTMB_control_optimx_snewtm`}{`"snewtm"`}
#' }
#'
#' @param optArgs A named list of extra arguments for [optimx::optimr()], merged
#'   with (and overriding) the method selection.
#' @param optCtrl A named list passed as the `optCtrl` argument of
#'   [glmmTMB::glmmTMBControl()].
#' @return A `glmmTMBControl` object to pass to the `control` argument of
#'   [glmmTMB::glmmTMB()].
#' @seealso [glmmTMB_control_calibrar_bfgs()] and [glmmTMB_control_optimh_ahres()]
#'   for the analogous `calibrar` optimizer families, and [glmmTMB_optimx_optim()]
#'   for the underlying optimizer adapter.
#' @name glmmTMB_control_optimx_methods
#' @examples
#' \dontrun{
#' glmmTMB::glmmTMB(
#'   count ~ mined + (1 | site),
#'   family = poisson, data = glmmTMB::Salamanders,
#'   control = glmmTMB_control_optimx_bfgs()
#' )
#' }
NULL

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_nvm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nvm"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_lbfgsb3c <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "lbfgsb3c"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_hjkb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "hjkb"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_ncg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "ncg"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_lbfgsb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "L-BFGS-B"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_bfgs <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "BFGS"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_cg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "CG"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_neldermead <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "Nelder-Mead"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_nlm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nlm"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_nlminb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nlminb"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_rcgmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "Rcgmin"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_rtnmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "Rtnmin"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_rvmmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "Rvmmin"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_snewton <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "snewton"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_snewtonm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "snewtonm"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_spg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "spg"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_ucminf <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "ucminf"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_newuoa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "newuoa"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_bobyqa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "bobyqa"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_uobyqa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "uobyqa"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_nmkb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nmkb"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_hjn <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "hjn"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_lbfgs <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "lbfgs"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_subplex <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "subplex"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_mla <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "mla"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_slsqp <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "slsqp"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_tnewt <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "tnewt"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_anms <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "anms"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_pracmanm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "pracmanm"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_nlnm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nlnm"))
}

#' @rdname glmmTMB_control_optimx_methods
#' @export
glmmTMB_control_optimx_snewtm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "snewtm"))
}
