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

# --- DATAFRAME UTILS --- #

#' Rename columns of a dataframe with their labels
#'
#' Columns that have no variable label keep their original name.
#'
#' @param df A data frame whose columns may carry variable labels.
#' @return The data frame with columns renamed to their labels (unlabelled
#'   columns keep their original name).
#' @export
rename_columns_with_labels <- function(df) {
  labels <- labelled::var_label(df)
  new_names <- vapply(
    colnames(df),
    function(n) {
      lbl <- labels[[n]]
      if (length(lbl) != 1 || is.na(lbl) || !nzchar(lbl)) n else as.character(lbl)
    },
    character(1),
    USE.NAMES = FALSE
  )
  colnames(df) <- new_names
  return(df)
}

#' Sort the columns of a data frame by their labels
#'
#' Reorders the columns of `df` by the natural (human) sort order of their
#' variable labels (as returned by [labelled::get_variable_labels()]).
#'
#' @param df A data frame whose columns carry variable labels.
#' @return `df` with its columns reordered by label.
#' @seealso [sort_columns_by_name()]
#' @export
sort_columns_by_label <- function(df) {
  labels <- as.character(labelled::get_variable_labels(df))
  ix <- gtools::mixedorder(labels)
  df <- df[, ix]
  return(df)
}

#' Sort the columns of a data frame by their names
#'
#' Reorders the columns of `df` by the natural (human) sort order of their
#' names, so that e.g. `x10` follows `x2`.
#'
#' @param df A data frame.
#' @return `df` with its columns reordered by name.
#' @seealso [sort_columns_by_label()]
#' @export
sort_columns_by_name <- function(df) {
  cols <- gtools::mixedorder(colnames(df))
  df <- df[, cols]
  return(df)
}

#' Print the columns of a dataframe matching a given regular expression in natural sorted order
#' 
#' @param df The dataset to print the columns of
#' @param regex A regular expression to filter the columns to print
#' @param sort Whether to sort the columns alphabetically
#' @return A character vector of column names matching the regular expression, printed to the console
#' 
#' @examples
#' # Print all columns containing "length" in their name, sorted alphabetically
#' catcols(mtcars, regex="length", sort=TRUE)
#' # Print all columns containing "length" in their name, in original order
#' catcols(mtcars, regex="length", sort=FALSE)
#' 
#' @importFrom stringr str_subset
#' @importFrom gtools mixedsort
#' @export
catcols <- function(df, regex=NULL, sort=TRUE) {
  cols <- colnames(df)
  if(!is.null(regex)) {
    cols <- cols %>% str_subset(regex)
  }
  if(sort) {
    cols <- cols %>% mixedsort()
  }
  cols %>% paste0(collapse="\n") %>% cat()
}