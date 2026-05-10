# --- CALIBRAR --- #

# Function to pass to the `optimizer` argument of glmmTMB::glmmTMBControl
glmmTMB_calibrar_optimizer <- function(par, fn, gr = NULL, ..., control = list()) {
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
    optimizer = glmmTMB_calibrar_optimizer,
    optArgs = list(method = method),
    optCtrl = list(ncores = ncores),
    # eigval_check = FALSE,
    # rank_check = "skip",
    # conv_check = "skip"
  )
  return(res)
}

#' @export
glmmTMB_calibrar_control_nelder_mead <- function(...) {
  return(glmmTMB_calibrar_control(method = "Nelder-Mead"))
}

#' @export
glmmTMB_calibrar_control_nelder_bfgs <- function(...) {
  return(glmmTMB_calibrar_control(method = "BFGS"))
}

#' @export
glmmTMB_calibrar_control_nelder_cg <- function(...) {
  return(glmmTMB_calibrar_control(method = "CG"))
}

#' @export
glmmTMB_calibrar_control_lbfgsb <- function(...) {
  return(glmmTMB_calibrar_control(method = "L-BFGS-B"))
}

#' @export
glmmTMB_calibrar_control_sann <- function(...) {
  return(glmmTMB_calibrar_control(method = "SANN"))
}

#' @export
glmmTMB_calibrar_control_brent <- function(...) {
  return(glmmTMB_calibrar_control(method = "Brent"))
}

#' @export
glmmTMB_calibrar_control_nlm <- function(...) {
  return(glmmTMB_calibrar_control(method = "nlm"))
}

#' @export
glmmTMB_calibrar_control_nlminb <- function(...) {
  return(glmmTMB_calibrar_control(method = "nlminb"))
}

#' @export
glmmTMB_calibrar_control_rcgmin <- function(...) {
  return(glmmTMB_calibrar_control(method = "Rcgmin"))
}

#' @export
glmmTMB_calibrar_control_Rvmmin <- function(...) {
  return(glmmTMB_calibrar_control(method = "Rvmmin"))
}

#' @export
glmmTMB_calibrar_control_hjn <- function(...) {
  return(glmmTMB_calibrar_control(method = "hjn"))
}

#' @export
glmmTMB_calibrar_control_spg <- function(...) {
  return(glmmTMB_calibrar_control(method = "spg"))
}

#' @export
glmmTMB_calibrar_control_lbfgsb3 <- function(...) {
  return(glmmTMB_calibrar_control(method = "LBFGSB3"))
}

#' @export
glmmTMB_calibrar_control_ahres <- function(...) {
  return(glmmTMB_calibrar_control(method = "AHR-ES"))
}
