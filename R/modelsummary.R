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
#' @return A list of lists to be passed to the `gof_map` argument of `modelsummary()`
#' @export
modelsummary_getgofmap <- function() {
  gof_map <- modelsummary::gof_map %>%
    dplyr::filter(!omit) %>%
    dplyr::select(-omit) %>%
    plyr::dlply(1, c)
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
