# --- OPTIMH (calibrar heuristic / global optimisers) --- #
# Mirrors glmmTMB_calibrar.R (which wraps calibrar::optim2) so the optimh,
# optim2 and optmix control APIs are identical: a generic builder plus thin
# per-method `function(optArgs, optCtrl)` wrappers.

# Function to pass to the `optimizer` argument of glmmTMB::glmmTMBControl
glmmTMB_optimh_optimizer <- function(par, fn, gr = NULL, ..., control = list()) {
  print("[glmmTMB_optimh_optimizer] calibrar optimh optimization")
  if (!is.null(control$ncores)) {
    cl <- parallel::makeCluster(control$ncores)
  }
  ret <- calibrar::optimh(
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
  # convergence: An integer code. 0 indicates successful completion.
  convergence = ret$convergence == 0
  cat(paste0("optimh convergence: ", convergence, "\n"))
  cat(paste0("optimh message: ", ret$message, "\n"))
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

  # Build optCtrl
  myoptCtrl = list(ncores = ncores)
  if(length(optCtrl) > 0) {
    optCtrl = rlist::list.merge(myoptCtrl, optCtrl)
  } else {
    optCtrl = myoptCtrl
  }

  # Build glmmTMBControl
  res <- glmmTMB::glmmTMBControl(
    parallel = list(n=ncores, autopar=TRUE),
    optimizer = glmmTMB_optimh_optimizer,
    optArgs = optArgs,
    optCtrl = optCtrl
  )
  return(res)
}

#' @export
glmmTMB_control_optimh_ahres <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "AHR-ES"))
}

#' @export
glmmTMB_control_optimh_nelder_mead <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "Nelder-Mead"))
}

#' @export
glmmTMB_control_optimh_sann <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "SANN"))
}

#' @export
glmmTMB_control_optimh_hjn <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "hjn"))
}

#' @export
glmmTMB_control_optimh_bobyqa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "bobyqa"))
}

#' @export
glmmTMB_control_optimh_cmaes <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "CMA-ES"))
}

#' @export
glmmTMB_control_optimh_gensa <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "genSA"))
}

#' @export
glmmTMB_control_optimh_de <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "DE"))
}

#' @export
glmmTMB_control_optimh_soma <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "soma"))
}

#' @export
glmmTMB_control_optimh_genoud <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "genoud"))
}

#' @export
glmmTMB_control_optimh_pso <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "PSO"))
}

#' @export
glmmTMB_control_optimh_hybridpso <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "hybridPSO"))
}

#' @export
glmmTMB_control_optimh_mads <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "mads"))
}

#' @export
glmmTMB_control_optimh_hjk <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "hjk"))
}

#' @export
glmmTMB_control_optimh_hjkb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "hjkb"))
}

#' @export
glmmTMB_control_optimh_nmk <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "nmk"))
}

#' @export
glmmTMB_control_optimh_nmkb <- function(optArgs=list(), optCtrl=list()) {
  return(glmmTMB_control_optimh(optArgs=optArgs, optCtrl=optCtrl, method = "nmkb"))
}
