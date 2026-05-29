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
#' @export
rename_columns_with_labels <- function(df) {
  colnames(df) <- sapply(df, function(x) attr(x, "label"))
  return(df)
}

#' Sort columns of a dataframe by labels (natural sorting order)
#'
#' @export
sort_columns_by_label <- function(df) {
  labels <- as.character(labelled::get_variable_labels(df))
  ix <- gtools::mixedorder(labels)
  df <- df[, ix]
  return(df)
}

#' Sort columns of a dataframe by names (natural sorting order)
#'
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