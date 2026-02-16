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
    coefs_order <- coefs_order %>% extract(. != "(Intercept)")
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

#' Build a named character vector mapping variable names to variable labels, including factor levels
#' 
#' @return A named list to be passed to the `coef_map` argument of `modelsummary()`
#' @export
modelsummary_build_labelled_coef_map <- function(df, debug=FALSE) {
  # Get a named character vector that maps variable names to variable labels
  #coefmap <- modelsummary::get_variable_labels_models(df)
  coefmap <- labelled::get_variable_labels(df)
  # For variables with missing labels, use variable name as label.
  # Otherwise modelsummary will not print the variable at all.
  for(n in names(coefmap)) {
    if(is.null(coefmap[[n]])) {
      coefmap[[n]] <- n
      if(debug) {
        print(paste0("Missing label for variable ", n, ", using variable name."))
      }
    }
  }
  # Convert factor variables from `nameLevel` into `label [level]`
  for (var in colnames(df)) {
    cc <- class(df[[var]])
    if(length(cc) != 1) {
      stop("ERROR: Variable ", var, " has multiple classes: ", paste(cc, collapse = ", "))
    }
    if (cc == "factor") {
      if(debug) {
        print(paste0("Converting factor variable: ", var))
      }
      for (lvl in levels(df[[var]])) {
        varinst_name <- paste0(var, lvl)
        var_label <- coefmap[[var]]
        varinst_label <- paste0(var_label, " [", lvl, "]")
        coefmap[[varinst_name]] <- varinst_label
        if(debug) {
          print(paste0(" - Added: ", varinst_name, " -> ", varinst_label))
        }
      }
    }
  }
  return(coefmap)
}