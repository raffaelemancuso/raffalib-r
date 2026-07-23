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

# --- FIT A LIST OF glmmTMB MODELS IN PARALLEL --- #

#' Fit many glmmTMB models in parallel, one per worker
#'
#' Fits an independent [glmmTMB::glmmTMB()] model for each element of `specs`,
#' distributing the fits across a cluster of worker processes. This is the
#' effective way to speed up a sequential `lapply()` model loop (e.g. one model
#' per outcome): because the fits are independent, wall-clock time drops roughly
#' by the number of cores. Unlike parallelising *inside* a single fit
#' (`optimParallel`), this needs no gradient tricks and works with `glmmTMB`'s
#' analytic TMB gradient — each worker simply loads `glmmTMB` (whose precompiled
#' TMB DLL ships with the package) and runs one full fit.
#'
#' Each spec is a named list of arguments for [glmmTMB::glmmTMB()] (e.g.
#' `formula`, `family`, `ziformula`, `control`). Arguments common to every model
#' are passed once through `...` and merged into each spec (the spec wins on
#' conflict), so the data set is written once per spec rather than repeated.
#' The whole call — including a custom `control`/optimizer object — is
#' serialized to the worker, so raffalib optimizer controls
#' (e.g. [glmmTMB_control_nloptr_ln_bobyqa()]) travel with the spec and work
#' without installing raffalib on the workers, as long as any package the
#' optimizer calls (referenced as `pkg::fun`) is installed.
#'
#' @param specs A non-empty list; each element a named list of arguments for
#'   [glmmTMB::glmmTMB()].
#' @param ... Arguments shared by every fit (e.g. `data`, `ziformula`,
#'   `control`), merged into each spec.
#' @param ncores Number of worker processes. Default
#'   `min(length(specs), parallel::detectCores() - 1)`. With `ncores <= 1` the
#'   fits run sequentially (no cluster).
#' @param packages Character vector of packages to load on each worker
#'   (`glmmTMB` is always loaded).
#' @param export Character vector of names of objects to copy from
#'   `envir` to the workers (rarely needed — the spec carries what it references).
#' @param envir Environment from which `export` names are taken.
#' @param seed Optional integer; if given, an RNG stream is set on the cluster
#'   for reproducibility.
#' @return A list of fitted `glmmTMB` models, in the order of `specs`
#'   (`names(specs)` are preserved).
#' @examples
#' \dontrun{
#' specs <- list(
#'   fwci   = list(formula = fwci ~ isai + (1 | project_id), family = gaussian()),
#'   top1   = list(formula = is_in_top_1_percent ~ isai + (1 | project_id),
#'                 family = binomial())
#' )
#' mods <- fit_glmmTMB_parallel(specs, data = paper_db, ziformula = ~0)
#' }
#' @export
fit_glmmTMB_parallel <- function(specs, ..., ncores = NULL,
                                 packages = character(0),
                                 export = character(0),
                                 envir = parent.frame(),
                                 seed = NULL) {
  stopifnot(is.list(specs), length(specs) > 0)
  shared <- list(...)

  # one self-contained fitter; captures `shared` in its environment so the
  # closure carries it to the worker
  fit_one <- function(spec) {
    do.call(glmmTMB::glmmTMB, utils::modifyList(shared, spec))
  }

  if (is.null(ncores)) {
    ncores <- max(1L, parallel::detectCores() - 1L)
  }
  ncores <- min(ncores, length(specs))

  if (ncores <= 1L) {
    return(stats::setNames(lapply(specs, fit_one), names(specs)))
  }

  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  if (!is.null(seed)) parallel::clusterSetRNGStream(cl, seed)

  # load glmmTMB (its TMB DLL ships with it) plus any extra packages on workers
  parallel::clusterCall(cl, function(pkgs) {
    for (p in pkgs) suppressPackageStartupMessages(requireNamespace(p, quietly = TRUE))
    invisible(NULL)
  }, unique(c("glmmTMB", packages)))

  if (length(export) > 0) {
    parallel::clusterExport(cl, export, envir = envir)
  }

  res <- parallel::parLapply(cl, specs, fit_one)
  stats::setNames(res, names(specs))
}
