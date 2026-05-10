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

# --- DATA QUALITY --- #

#' Find observations dropped from a model
#' See: https://stackoverflow.com/questions/79759738/get-dataframe-of-observations-dropped-in-estimates/
#'
#' @export
get_dropped_obs <- function(mod, df, .keep_vars = FALSE) {
  model_vars <- all.vars(terms(mod))
  if (class(.keep_vars) == "character") {
    return(df[!complete.cases(df[model_vars]), c(.keep_vars, model_vars)])
  } else if (.keep_vars == FALSE) {
    return(df[!complete.cases(df[model_vars]), model_vars])
  } else if (.keep_vars == TRUE) {
    return(df[!complete.cases(df[model_vars]), ])
  } else {
    stop("Unknown value for .keep_vars")
  }
}

#' Find the number of missings of a variable per group
#'
#' @param var The variable to check for missing
#' @param ... The grouping variables
#' @return A table with the number and percentage of missing per group
#' @export
na_per_group <- function(df, var, ...) {
  df %>%
    group_by(...) %>%
    summarise(n = n(), na = sum(is.na({{ var }})), p = na / n)
}

#' Report duplicates by group
#'
#' @param df The dataframe
#' @param ... The grouping variables
#' @export
dups <- function(df, ...) {
  stopifnot(!("n" %in% colnames(df)))
  df %>%
    count(...) %>% 
    filter(n > 1)
}

#' Assert no duplicates on the basis of a set of grouping variables
#'
#' @param df The dataset to check for duplicates
#' @param ... The grouping variables
#' @export
dupsa <- function(df, ...) {
  stopifnot(
    df %>%
      dups(...) %>%
      nrow() ==
      0
  )
}

#' Convert NaNs to NAs
#'
#' @param df The dataset
#' @export
nan2na <- function(df) {
  # class(NaN) -> "numeric"
  df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~dplyr::na_if(., NaN)))
}