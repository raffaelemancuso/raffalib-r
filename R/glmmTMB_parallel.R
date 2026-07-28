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
#' without loading raffalib on the workers, as long as any package the optimizer
#' calls (referenced as `pkg::fun`) is installed. `glmmTMB` and anything named
#' in `packages` are loaded on every worker before the fits start; if a worker
#' cannot load one of them the call fails with a message naming it.
#'
#' Arguments passed by name are recorded in each fit's `call` **as that name**
#' rather than as their value. `do.call()` substitutes values into the call it
#' builds, and `glmmTMB` stores that call verbatim, so the naive version puts a
#' copy of the whole data frame — and the whole family function — inside every
#' fitted object. Calling with `data = pis, family = glmmTMB::nbinom2` yields a
#' call reading `glmmTMB(formula = ..., data = pis, family = glmmTMB::nbinom2)`,
#' which is what a hand-written fit would record and is orders of magnitude
#' smaller. Whatever the call holds stays evaluable, so refitting from
#' `mod$call$family` still works. An expression that is not a plain name (or
#' `pkg::name`) is not recorded this way, since re-evaluating it later need not
#' give the same object; `data` is always referred to by name, falling back to
#' `.fit_data` when the caller passed an expression.
#'
#' @param specs A non-empty list; each element a named list of arguments for
#'   [glmmTMB::glmmTMB()]. A value here overrides the shared argument of the
#'   same name, and is recorded by value.
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
#' @param strip_formula_env If `TRUE` (default), pass the fitted models
#'   through [glmmTMB_strip_formula_env()] before returning them. The
#'   `do.call()` environment binds the full dataset (and any shared arguments
#'   recorded by name), and `glmmTMB` attaches that environment to every
#'   formula/`terms` of the fitted object — so without stripping, every model
#'   in the returned list, and every `.rds` backup of it, carries its own
#'   private copy of the data. Stripping rebinds those environments to
#'   [globalenv()], where the name recorded in `call$data` resolves in the
#'   calling session. Set to `FALSE` when the data is *not* reachable by that
#'   name from the global environment.
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
                                 seed = NULL,
                                 strip_formula_env = TRUE) {
  stopifnot(is.list(specs), length(specs) > 0)
  shared <- list(...)

  # How the caller wrote each shared argument, e.g. quote(pis) or
  # quote(glmmTMB::nbinom2), so the fitted objects can refer to them by name
  # instead of by value.
  shared_exprs <- as.list(substitute(list(...)))[-1L]
  data_expr <- shared_exprs[["data"]]
  data_name <- if (is.name(data_expr)) as.character(data_expr) else ".fit_data"
  # a bare name, or pkg::name -- anything more complex would be misleading to
  # record, since re-evaluating it later need not give the same object
  is_simple_ref <- function(e) {
    is.name(e) || (is.call(e) && length(e) == 3L && identical(e[[1L]], as.name("::")))
  }

  # one self-contained fitter; captures `shared` in its environment so the
  # closure carries it to the worker
  fit_one <- function(spec) {
    args <- utils::modifyList(shared, spec)
    # do.call() substitutes argument VALUES into the call it builds, and
    # glmmTMB stores that call verbatim. Passing the function object inlines
    # the entire glmmTMB body, and passing `data` inlines the whole data frame,
    # so every fitted model -- and every .rds backup of one -- carries a copy
    # of the data. Name the function instead of passing it, and refer to the
    # data by symbol, evaluated in an environment that binds it.
    env <- new.env(parent = asNamespace("glmmTMB"))
    if ("data" %in% names(args)) {
      assign(data_name, args[["data"]], envir = env)
      args[["data"]] <- as.name(data_name)
    }
    # Everything else the caller passed by name goes in as that name:
    # `family = glmmTMB::nbinom2` would otherwise inline the whole nbinom2
    # function body into every fit. What lands in mod$call stays evaluable, so
    # callers reading it back (e.g. to refit with the same family) still work.
    # Arguments a spec overrides keep the spec's value.
    for (nm in setdiff(names(args), c("data", names(spec)))) {
      e <- shared_exprs[[nm]]
      if (!is.null(e) && is_simple_ref(e)) {
        if (is.name(e)) assign(as.character(e), args[[nm]], envir = env)
        args[[nm]] <- e
      }
    }
    do.call("glmmTMB", args, envir = env)
  }

  if (is.null(ncores)) {
    ncores <- max(1L, parallel::detectCores() - 1L)
  }
  ncores <- min(ncores, length(specs))

  if (ncores <= 1L) {
    res <- stats::setNames(lapply(specs, fit_one), names(specs))
    if (strip_formula_env) res <- glmmTMB_strip_formula_env(res)
    return(res)
  }

  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  if (!is.null(seed)) parallel::clusterSetRNGStream(cl, seed)

  # The workers never load raffalib: the specs carry raffalib optimizer
  # functions, but their bodies call only base functions and fully-qualified
  # pkg::fun, so they run in a bare worker. Keep it that way -- an unqualified
  # call to a raffalib helper (e.g. myinfo()) would die there with "could not
  # find function".
  # load glmmTMB (its TMB DLL ships with it) plus any extra packages on workers
  loaded <- parallel::clusterCall(cl, function(pkgs) {
    for (p in pkgs) suppressPackageStartupMessages(requireNamespace(p, quietly = TRUE))
    vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  }, unique(c("glmmTMB", packages)))

  unavailable <- unique(unlist(lapply(loaded, function(x) names(x)[!x])))
  if (length(unavailable) > 0) {
    stop("the parallel workers could not load: ",
         paste(unavailable, collapse = ", "),
         ". Install the package(s) so the workers can reach them, or refit with ",
         "ncores = 1 to run serially.", call. = FALSE)
  }

  if (length(export) > 0) {
    parallel::clusterExport(cl, export, envir = envir)
  }

  res <- parallel::parLapply(cl, specs, fit_one)
  res <- stats::setNames(res, names(specs))
  # stripped here in the main session rather than on the workers: fit_one must
  # stay free of raffalib calls (see the note above clusterCall)
  if (strip_formula_env) res <- glmmTMB_strip_formula_env(res)
  res
}

#' Strip the data-laden environment from fitted glmmTMB models
#'
#' `glmmTMB` attaches the environment it was called in to every formula and
#' `terms` object of the fitted model: the formula in `$call`, the `terms`
#' attribute of `$frame`, and the formulas in `$modelInfo`. When that
#' environment binds the full dataset — as [fit_glmmTMB_parallel()]'s
#' `do.call()` environment does — every fitted model drags a private copy of
#' the data into session memory and into every `.rds` backup, dwarfing the
#' model itself (a project-level fit here is ~10 MB of model plus ~34 MB of
#' captured data; a publication-level one ~130 MB plus ~730 MB). This rebinds
#' every formula/`terms` environment found in the object to `env`.
#'
#' Downstream consumers (`modelsummary`, `marginaleffects`, `update()`,
#' `predict()`) locate the data by evaluating `call$data`, so stripping is
#' safe **as long as that symbol resolves in `env`** — which it does when the
#' model was fitted with `data` passed as a bare global name, or after a
#' `call$data` repair. Environments of *functions* (including the TMB object
#' in `$obj`) are never touched: they are functional state, not baggage.
#'
#' Already-saved backups are repaired the same way: read, strip, re-save.
#'
#' @param x A fitted `glmmTMB` model, or a (possibly named, possibly nested)
#'   list of them.
#' @param env Environment to rebind to (default [globalenv()]).
#' @return `x` with every formula/`terms` environment replaced by `env`.
#' @seealso [fit_glmmTMB_parallel()], which applies this by default.
#' @examples
#' \dontrun{
#' mods <- read_backup(thisdatadir, "mods_publvl")
#' mods <- glmmTMB_strip_formula_env(mods)
#' save_backup(mods, thisdatadir, "mods_publvl") # ~10x smaller
#' }
#' @export
glmmTMB_strip_formula_env <- function(x, env = globalenv()) {
  if (is.list(x) && !inherits(x, "glmmTMB")) {
    return(lapply(x, glmmTMB_strip_formula_env, env = env))
  }
  stopifnot(inherits(x, "glmmTMB"))
  strip_formula_envs_impl(x, env)
}

# Recursively rebind the environment of every formula/terms object found in
# lists, calls and attributes. Environments and closures are never entered:
# the TMB object's env is functional state, and function environments must
# stay intact.
strip_formula_envs_impl <- function(o, env) {
  if (is.environment(o) || is.function(o)) {
    return(o)
  }
  if (inherits(o, "formula") || inherits(o, "terms")) {
    environment(o) <- env
  } else if (is.call(o)) {
    for (i in seq_along(o)) {
      oi <- tryCatch(o[[i]], error = function(e) NULL)
      if (inherits(oi, "formula")) {
        environment(oi) <- env
        o[[i]] <- oi
      }
    }
  } else if (is.list(o)) {
    for (i in seq_along(o)) {
      oi <- .subset2(o, i)
      if (!is.null(oi)) o[[i]] <- strip_formula_envs_impl(oi, env)
    }
  }
  a <- attributes(o)
  for (an in setdiff(
    names(a),
    c("names", "row.names", "class", "dim", "dimnames", "levels", "comment")
  )) {
    ai <- a[[an]]
    if (inherits(ai, "formula") || inherits(ai, "terms") ||
          is.list(ai) || is.call(ai)) {
      attr(o, an) <- strip_formula_envs_impl(ai, env)
    }
  }
  o
}
