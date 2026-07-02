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

#' Frequency table including missing values
#'
#' Thin wrapper around [DescTools::Freq()] that always reports `NA`s and orders
#' categories by descending frequency.
#'
#' @param x A factor or vector to tabulate.
#' @return A frequency table (a `data.frame`) as returned by [DescTools::Freq()].
#' @export
myfreq <- function(x) {
  DescTools::Freq(x, useNA = "always", ord="desc")
}

#' Equally spaced quantiles of a numeric vector
#'
#' Returns the quantiles of `x` at evenly spaced probabilities, a quick way to
#' get a feel for a variable's distribution during preliminary descriptive work.
#'
#' @param x The numeric vector to summarise.
#' @param int Spacing of the probabilities, in `(0, 1]` (default `0.1`, i.e.
#'   deciles).
#' @return A named numeric vector of quantiles, as returned by [stats::quantile()].
#' @export
descquant <- function(x, int=0.1) {
  quantile(x, probs = seq(0,1,int), na.rm = TRUE)
}

#' Report missing, NaN and infinite values
#'
#' Prints the count and percentage of `NA`, `NaN` and infinite values in a
#' vector.
#'
#' @param d The vector to check.
#' @return Invisibly `NULL`; called for the side effect of printing.
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

#' Return the observations a model dropped through listwise deletion
#'
#' Given a fitted model and the data frame it was estimated on, returns the rows
#' that contain at least one `NA` among the model's variables (those extracted by
#' [stats::terms()]). These are the observations that `lm()`/`glm()` and friends
#' silently drop, which is useful for diagnosing why the estimation sample is
#' smaller than the full data set. See
#' <https://stackoverflow.com/questions/79759738/get-dataframe-of-observations-dropped-in-estimates/>.
#'
#' @param mod A fitted model whose [stats::terms()] expose the model variables.
#' @param df The data frame the model was estimated on.
#' @param all_model_vars If `TRUE` (default), keep every model variable in the
#'   result; if `FALSE`, keep only the model variables that actually contain a
#'   missing value in the dropped rows.
#' @param other_vars Character vector of additional columns of `df` to append to
#'   the result.
#' @return A data frame of the dropped rows, subset to the selected columns.
#' @export
get_dropped_obs <- function(mod, df, all_model_vars=TRUE, other_vars=c()) {
  model_vars <- all.vars(terms(mod))
  mask <- !complete.cases(df[model_vars])
  if(all_model_vars) {
    cols <- model_vars
  } else {
    mask2 <- colSums(is.na(df[mask,model_vars]))>0
    cols_with_missing <- names(mask2[mask2])
    cols <- cols_with_missing
  }
  cols <- c(cols, other_vars)
  return(df[mask, cols])
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
#' @keywords internal
#' @noRd
get_dropped_obs_2 <- function(mod, df, .keep_vars=FALSE) {
  xs <- insight::find_predictors(mod) %>% unlist() %>% as.character()
  ys <- insight::find_response(mod)
  all_vars <- c(xs, ys)
  filtered <- df %>% filter(if_any(all_of(all_vars), ~ is.na(.)))
  if (isTRUE(.keep_vars)) {
    return(filtered)
  } else if (isFALSE(.keep_vars)) {
    return(filtered %>% select(all_of(all_vars)))
  } else {
    selected_vars <- c(all_vars, .keep_vars)
    return(filtered %>% select(all_of(selected_vars)))
  }
}

#' Count missing values of a variable per group
#'
#' @param df A data frame.
#' @param var The variable (unquoted) to count missing values of.
#' @param ... Grouping variables (unquoted), passed to [dplyr::group_by()].
#' @return A data frame with, per group, the group size `n`, the number of
#'   missing values `na`, and the proportion missing `p`.
#' @export
na_per_group <- function(df, var, ...) {
  df %>%
    group_by(...) %>%
    summarise(n = n(), na = sum(is.na({{ var }})), p = na / n)
}

#' Assert that a set of grouping variables uniquely identifies rows
#'
#' Errors (via [stopifnot()]) if `df` contains any duplicated combination of the
#' grouping variables. A convenient inline integrity check for pipelines. To
#' inspect the offending rows instead of asserting, see `janitor::get_dupes()`.
#'
#' @param df A data frame (must not already contain a column named `n`).
#' @param ... Grouping variables (unquoted) expected to be unique together,
#'   passed to [dplyr::count()].
#' @return Invisibly `NULL`; called for its side effect of asserting uniqueness.
#' @export
dupsa <- function(df, ...) {
  stopifnot(!("n" %in% colnames(df)))
  n_dup <- df %>%
    count(...) %>%
    filter(n > 1) %>%
    nrow()
  stopifnot(n_dup == 0)
}