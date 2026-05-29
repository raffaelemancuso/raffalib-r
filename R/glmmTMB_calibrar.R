# --- CALIBRAR --- #

# Function to pass to the `optimizer` argument of glmmTMB::glmmTMBControl
glmmTMB_calibrar_optimizer <- function(par, fn, gr = NULL, ..., control = list()) {
  print("calibrar optimization")
  if (!is.null(control$ncores)) {
    cl <- parallel::makeCluster(control$ncores)
  }
  print("[glmmTMB_calibrar_optimizer] control:")
  print(control)
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
glmmTMB_calibrar_control <- function(optArgs=list(), optCtrl=list(), method = NULL) {
  if (is.null(method)) {
    stop("Please specify a method for calibrar optimization.")
  }
  ncores <- parallel::detectCores() - 1
  
  myoptArgs = list(method = method)
  if(length(optArgs) > 0) {
    optArgs = rlist::list.merge(myoptArgs, optArgs)
  } else {
    optArgs = myoptArgs
  }
  print("optArgs:")
  print(optArgs)
  
  myoptCtrl = list(ncores = ncores)
  if(length(optCtrl) > 0) {
    optCtrl = rlist::list.merge(myoptArgs, optCtrl)
  } else {
    optCtrl = myoptCtrl
  }
  print("optCtrl:")
  print(optCtrl)
  
  
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

#' @export
glmmTMB_calibrar_control_nelder_mead <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "Nelder-Mead"))
}

#' @export
glmmTMB_calibrar_control_nelder_bfgs <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "BFGS"))
}

#' @export
glmmTMB_calibrar_control_nelder_cg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "CG"))
}

#' @export
glmmTMB_calibrar_control_lbfgsb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "L-BFGS-B"))
}

#' @export
glmmTMB_calibrar_control_sann <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "SANN"))
}

#' @export
glmmTMB_calibrar_control_brent <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "Brent"))
}

#' @export
glmmTMB_calibrar_control_nlm <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "nlm"))
}

#' @export
glmmTMB_calibrar_control_nlminb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "nlminb"))
}

#' @export
glmmTMB_calibrar_control_rcgmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "Rcgmin"))
}

#' @export
glmmTMB_calibrar_control_Rvmmin <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "Rvmmin"))
}

#' @export
glmmTMB_calibrar_control_hjn <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "hjn"))
}

#' @export
glmmTMB_calibrar_control_spg <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "spg"))
}

#' @export
glmmTMB_calibrar_control_lbfgsb3 <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "LBFGSB3"))
}

#' @export
glmmTMB_calibrar_control_ahres <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_calibrar_control(optArgs=optArgs, optCtrl=optCtrl, method = "AHR-ES"))
}
