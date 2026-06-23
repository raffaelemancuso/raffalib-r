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

# --- DESCRIPTIVES --- #

#' @export
myfreq <- function(x) {
  DescTools::Freq(x, useNA = "always", ord="desc")
}

#' Get equally-space quantiles for a numeric vector
#' Used for preliminary descriptive statistics
#' 
#' @param x The numeric vector to get the quantiles of
#' @param int The interval for the quantiles (default is 0.1)
#' @export
descquant <- function(x, int=0.1) {
  quantile(x, probs = seq(0,1,int), na.rm = TRUE)
}

#' Reports missings and infinity values
#' 
#' @param d The vector to check for missings and infinity values
#' @export
descstrange <- function(d) {
  cperc <- function(x) {
    round(100*x/length(d),2)
  }
  n_miss <- sum(is.na(d))
  print(paste0("N. missing: ", n_miss, " (", cperc(n_miss), "%)"))
  n_nans <- sum(is.nan(d))
  print(paste0("N. NANs: ", n_nans, " (", cperc(n_nans), "%)"))
  n_inf <- sum(is.infinite(d))
  print(paste0("N. infinity: ", n_inf, " (", cperc(n_inf), "%)"))
}

#' Find observations dropped from a model
#' See: https://stackoverflow.com/questions/79759738/get-dataframe-of-observations-dropped-in-estimates/
#'
#' @export
get_dropped_obs <- function(mod, df, .keep_vars = FALSE) {
  model_vars <- all.vars(terms(mod))
  if (is.character(.keep_vars)) {
    return(df[!complete.cases(df[model_vars]), c(.keep_vars, model_vars)])
  } else if (isFALSE(.keep_vars)) {
    return(df[!complete.cases(df[model_vars]), model_vars])
  } else if (isTRUE(.keep_vars)) {
    return(df[!complete.cases(df[model_vars]), ])
  } else {
    stop("Unknown value for .keep_vars")
  }
}

#' Return the observations dropped from a fitted model due to missing values
#'
#' Given a fitted model and the data frame it was estimated on, returns the rows
#' that contain at least one `NA` in any of the variables the model uses (its
#' predictors and response). These are the observations that `feols()`/`lm()`
#' silently drop via listwise deletion, so the result is useful for diagnosing
#' *why* the estimation sample is smaller than the full data set.
#'
#' The model's variables are extracted with `insight::find_predictors()` and
#' `insight::find_response()`, then combined into `all_vars`. A row is returned
#' whenever any of those variables is `NA` (`if_any(all_of(all_vars), ~ is.na(.))`).
#'
#' @param mod A fitted model object supported by the `insight` package
#'   (e.g. an `feols` or `lm` fit).
#' @param df The data frame the model was estimated on. Must contain every
#'   predictor and response variable referenced by `mod`.
#' @param .keep_vars Controls which columns are kept in the returned data frame:
#'   - `FALSE` (default): keep only the model variables (`all_vars`).
#'   - `TRUE`: keep all columns of `df`.
#'   - a character vector: keep the model variables plus the named columns.
#'
#' @return A data frame of the dropped rows (those with at least one `NA` among
#'   the model variables), subset to the columns selected by `.keep_vars`.
get_dropped_obs_2 <- function(mod, df, .keep_vars=FALSE) {
  xs <- insight::find_predictors(mod) %>% unlist() %>% as.character()
  ys <- insight::find_response(mod)
  all_vars <- c(xs, ys)
  filtered <- df %>% filter(if_any(all_of(all_vars), ~ is.na(.)))
  if (.keep_vars == TRUE) {
    return(filtered)
  } else if (.keep_vars == FALSE) {
    return(filtered %>% select(all_of(all_vars)))
  } else {
    selected_vars <- c(all_vars, .keep_vars)
    return(filtered %>% select(all_of(selected_vars)))
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