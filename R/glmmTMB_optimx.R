# --- GLMMTMB OPTMIX OPTIMIZER --- #

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
  optArgs = list(method = “L-BFGS-B”)
)