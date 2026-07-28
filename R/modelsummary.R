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

#' Find variables that a missing in a coefficient map 
#' (to pass to `coef_map` argument of `modelsummary()`) 
#' but are present in the model estimates
#'
#' @param mod The models
#' @param coef_map The coefficient map
#' @return A set of missing variables
#' @export
modelsummary_missing_variables_in_coef_map <- function(mod, coef_map) {
  ests_all <- modelsummary::get_estimates(mod)[["term"]]
  ests_ours <- names(coef_map)
  setdiff(ests_all, ests_ours)
}

#' Re-order coefficients so that, when printing more than one model, variables in common to all models are printed last
#'
#' @param mods A list of models
#' @param coef_rename This MUST be equal to the `coef_rename` value passed to `modelsummary`
#' @param include_reference Whether to include reference levels when renaming coefficients
#' @return A character vector to be passed to the `coef_map` argument of `modelsummary()`
#' @export
modelsummary_common_coefs_at_bottom <- function(mods, coef_rename=TRUE, include_reference=TRUE) {
  # Get variables from a single model
  modelsummary_getvars <- function(mod) {
    return(modelsummary::get_estimates(
      mod,
      coef_rename = coef_rename,
      include_reference = include_reference
    )$term)
  }
  # Get variables of all models
  mod_coefs <- lapply(mods, modelsummary_getvars)
  # Obtain common coefficients
  coefs_common <- Reduce(intersect, mod_coefs)
  # Build vector of coefficients, in their order
  coefs_order <- c()
  # Put first coefficients not in common
  for(i in seq_along(mod_coefs)) {
    uncommon <- setdiff(mod_coefs[[i]], coefs_common)
    coefs_order <- union(coefs_order, uncommon)
  }
  # Then insert coefficients in common
  coefs_order <- c(coefs_order, coefs_common)
  # Put intercept at the bottom
  if("(Intercept)" %in% coefs_order) {
    coefs_order <- coefs_order[coefs_order != "(Intercept)"]
    coefs_order <- c(coefs_order, "(Intercept)")
  }
  # Return
  return(coefs_order)
}

#' Get goodness-of-fit map for modelsummary, as a list of lists
#'
#' The entry order is significant: `modelsummary()` prints the goodness-of-fit
#' rows in the order they appear in `gof_map`. This preserves
#' [modelsummary::gof_map]'s own curated order, which groups the statistics
#' sensibly (`nobs`, then the R2 family with `r.squared` ahead of
#' `adj.r.squared`, then information criteria, then the rest). Splitting the
#' data frame with `plyr::dlply()` used to alphabetise it, which put
#' "R2 Adj." above "R2" and "Num.Obs." in the middle of the block.
#'
#' @return A list of lists to be passed to the `gof_map` argument of `modelsummary()`
#' @export
modelsummary_getgofmap <- function() {
  gof_df <- modelsummary::gof_map %>%
    dplyr::filter(!omit) %>%
    dplyr::select(-omit)
  gof_map <- stats::setNames(
    lapply(seq_len(nrow(gof_df)), function(i) as.list(gof_df[i, , drop = FALSE])),
    gof_df$raw
  )
  gof_map$nobs$fmt <- \(x) formatC(x, digits = 0, big.mark = ",", format = "d")
  gof_map$aic$fmt <- \(x) formatC(x, digits = 0, big.mark = ",", format = "f")
  gof_map$bic$fmt <- \(x) formatC(x, digits = 0, big.mark = ",", format = "f")
  return(gof_map)
}

#' Name a list of models by their response-variable labels
#'
#' Names each element of `mods_list` after the variable label of that model's
#' response, so that when the list is passed to [modelsummary::modelsummary()]
#' the column headers read as the (labelled) outcome names. The response of each
#' model is found with [insight::find_response()] and mapped to its label via
#' [list_rename_values()]; responses without a label keep their variable name.
#'
#' @param mods_list A list of fitted models.
#' @param df The data frame the models were fit on, whose columns carry variable
#'   labels.
#' @return `mods_list`, named by each model's labelled response variable.
#' @export
modelsummary_name_models_list <- function(mods_list, df) {

  mod_find_reponse <- function(mod) {
    if(class(mod)[[1]]=="comparisons") {
      return(attr(mod, "marginaleffects")@variable_names_response)
    } else {
      return(tryCatch(
        insight::find_response(mod),
        error = function(e) {
          cli::cli_abort(
            c(
              "{.fun insight::find_response} threw an error for a model of class {.cls {class(mod)[[1]]}}.",
              "x" = conditionMessage(e)
            ),
            parent = e
          )
        }
      ))
    }
  }

  names(mods_list) <- mods_list %>%
    lapply(mod_find_reponse) %>%
    unlist() %>%
    raffalib::list_rename_values(labelled::get_variable_labels(df))
  return(mods_list)
}

#' Build a `coef_rename` lookup from a data frame's variable labels
#'
#' `coef_rename = TRUE` asks [modelsummary::modelsummary()] to take labels from
#' the `parameters` package, which only reaches model classes `parameters`
#' supports. Custom model objects — a 2SLS wrapper, say — are extracted through
#' `broom` instead and come back with raw term names such as `genderM` or
#' `publication_type_detailedConfProc`, so those tables end up looking unlike
#' every other table in the same paper. This builds the lookup explicitly from
#' the labelled data instead.
#'
#' Factor levels are expanded so the result matches the style
#' `coef_rename = TRUE` produces: a `gender` column labelled `"Gender"` with
#' levels `F`/`M` yields `genderF -> "Gender [F]"` and `genderM -> "Gender [M]"`.
#' Logical columns get their `TRUE` term mapped to the bare label, which is what
#' a dummy regressor needs. Entries are returned longest-key-first so a
#' level-specific key is substituted before the bare variable name it contains.
#'
#' @param df A data frame carrying variable labels (see [labelled::var_label()]).
#' @param extra Optional named character vector merged in last, for terms the
#'   labels do not cover.
#' @return A named character vector suitable for the `coef_rename` argument of
#'   [modelsummary::modelsummary()].
#' @seealso [modelsummary_build_coef_map()], which builds a `coef_map` from
#'   fitted models rather than from the data.
#' @examples
#' \dontrun{
#' modelsummary(mods, coef_rename = modelsummary_coef_rename_from_labels(pis))
#' }
#' @export
modelsummary_coef_rename_from_labels <- function(df, extra = NULL) {
  stopifnot(is.data.frame(df))
  labs <- labelled::var_label(df)
  labs <- labs[!vapply(labs, is.null, logical(1))]
  out <- character(0)
  for (v in names(labs)) {
    lab <- as.character(labs[[v]])[1]
    if (is.na(lab) || !nzchar(lab)) next
    x <- df[[v]]
    if (is.factor(x)) {
      lv <- levels(x)
      if (length(lv)) out[paste0(v, lv)] <- paste0(lab, " [", lv, "]")
    } else if (is.logical(x)) {
      out[paste0(v, "TRUE")] <- lab
    }
    out[v] <- lab
  }
  if (!is.null(extra)) out[names(extra)] <- as.character(extra)
  # longest first: `genderM` must be replaced before the `gender` it contains
  out[order(nchar(names(out)), decreasing = TRUE)]
}

#' Build a labelled coefficient map from a list of models
#'
#' Collects the parameter names across every model in `mods_list`, drops
#' duplicates, and maps each to its variable label, producing a named list ready
#' for the `coef_map` argument of [modelsummary::modelsummary()]. Parameters are
#' extracted with [insight::find_parameters()] and relabelled via
#' [list_rename_values()]; parameters without a label keep their name.
#'
#' @param mods_list A list of fitted models.
#' @param df The data frame the models were fit on, whose columns carry variable
#'   labels.
#' @return A named list mapping each parameter to its variable label, for the
#'   `coef_map` argument of [modelsummary::modelsummary()].
#' @export
modelsummary_build_coef_map <- function(mods_list, df) {

    mod_find_parameters <- function(mod) {
    if(class(mod)[[1]]=="comparisons") {
      return(attr(mod, "marginaleffects")@variable_names_predictors)
    } else {
      return(tryCatch(
        insight::find_parameters(mod),
        error = function(e) {
          cli::cli_abort(
            c(
              "{.fun insight::find_parameters} threw an error for a model of class {.cls {class(mod)[[1]]}}.",
              "x" = conditionMessage(e)
            ),
            parent = e
          )
        }
      ))
    }
  }

  coef_map <- mods_list %>%
    lapply(mod_find_parameters) %>%
    unlist() %>%
    unique() %>%
    raffalib::as_named_list() %>%
    raffalib::list_rename_values(labelled::get_variable_labels(df))
  return(coef_map)
}
