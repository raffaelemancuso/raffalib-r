# --- GLMMTMB OPTIMX OPTIMIZER --- #

#' Use optimx's optimizers for glmmTMB
#'
#' @return Object to be passed to the `optimizer` argument of `glmmTMBControl()`
#' @export
glmmTMB_optimx_optim <- function(...) {
  print("optimx optimization")
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

#' @export
glmmTMB_control_optimx_nvm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nvm"))
}

#' @export
glmmTMB_control_optimx_lbfgsb3c <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "lbfgsb3c"))
}

#' @export
glmmTMB_control_optimx_hjkb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "hjkb"))
}

#' @export
glmmTMB_control_optimx_ncg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "ncg"))
}

#' @export
glmmTMB_control_optimx_lbfgsb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "L-BFGS-B"))
}

#' @export
glmmTMB_control_optimx_bfgs <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "BFGS"))
}

#' @export
glmmTMB_control_optimx_cg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "CG"))
}

#' @export
glmmTMB_control_optimx_neldermead <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "Nelder-Mead"))
}

#' @export
glmmTMB_control_optimx_nlm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nlm"))
}

#' @export
glmmTMB_control_optimx_nlminb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nlminb"))
}

#' @export
glmmTMB_control_optimx_rcgmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "Rcgmin"))
}

#' @export
glmmTMB_control_optimx_rtnmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "Rtnmin"))
}

#' @export
glmmTMB_control_optimx_rvmmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "Rvmmin"))
}

#' @export
glmmTMB_control_optimx_snewton <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "snewton"))
}

#' @export
glmmTMB_control_optimx_snewtonm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "snewtonm"))
}

#' @export
glmmTMB_control_optimx_spg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "spg"))
}

#' @export
glmmTMB_control_optimx_ucminf <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "ucminf"))
}

#' @export
glmmTMB_control_optimx_newuoa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "newuoa"))
}

#' @export
glmmTMB_control_optimx_bobyqa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "bobyqa"))
}

#' @export
glmmTMB_control_optimx_uobyqa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "uobyqa"))
}

#' @export
glmmTMB_control_optimx_nmkb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nmkb"))
}

#' @export
glmmTMB_control_optimx_hjn <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "hjn"))
}

#' @export
glmmTMB_control_optimx_lbfgs <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "lbfgs"))
}

#' @export
glmmTMB_control_optimx_subplex <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "subplex"))
}

#' @export
glmmTMB_control_optimx_mla <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "mla"))
}

#' @export
glmmTMB_control_optimx_slsqp <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "slsqp"))
}

#' @export
glmmTMB_control_optimx_tnewt <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "tnewt"))
}

#' @export
glmmTMB_control_optimx_anms <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "anms"))
}

#' @export
glmmTMB_control_optimx_pracmanm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "pracmanm"))
}

#' @export
glmmTMB_control_optimx_nlnm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "nlnm"))
}

#' @export
glmmTMB_control_optimx_snewtm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimx(optArgs=optArgs, optCtrl=optCtrl, method = "snewtm"))
}
