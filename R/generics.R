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

#' Print character variables in a data frame
#' Works around https://github.com/easystats/parameters/issues/1142
#' 
#' @export
print_char_vars <- function(df) {
  for (col in colnames(df)) {
    cl <- class(df[[col]])
    if(cl == "character") {
      print(col)
    }
  }
}

#' Convert character variables to factor
#' Work around https://github.com/easystats/parameters/issues/1142
#' 
#' @export
char2factor <- function(df) {
  for (col in colnames(df)) {
    cl <- class(df[[col]])
    print(paste0(col , "-> ", cl))
    if(length(cl)==1 & cl == "character") {
      print(paste0("Converting ", col, " from character to factor"))
      df[[col]] %<>% as.factor()
    }
  }
  return(df)
}


#' Unfactor a variable if it's a factor variable, otherwise return the variable as is
#' @param var The variable to unfactor
#' @return The unfactored variable
#' @export
myunfactor <- function(var) {
  if(class(var) == "factor") {
    return(varhandle::unfactor(var))
  }
  return(var)
}

#' Find the number of missing of a variable per group
#' @param var The variable to check for missing
#' @param ... The grouping variables
#' @return A table with the number and percentage of missing per group
#' @export
na_per_group <- function(df, var, ...) {
  df %>% group_by(...) %>% 
    summarise(n=n(), na=sum(is.na({{var}})), p=na/n)
}

#' Find duplicates on the basis of a set of grouping variables
#' @param ... The grouping variables
#' @export
dups <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(n=n()) %>% 
    filter(n > 1)
}

#' Assert no duplicates on the basis of a set of grouping variables
#' @param ... The grouping variables
#' @export
dupsa <- function(df, ...) {
  stopifnot(
    df %>%
    dups(...) %>%
    nrow() == 0
  )
}

#' Print the columns of a dataset
#' @export
catcols <- function(df, regex=NULL, sort=TRUE) {
  cols <- colnames(df)
  if(!is.null(regex)) {
    cols <- cols %>% str_subset(regex)
  }
  if(sort) {
    cols <- cols %>% sort()
  }
  cols %>% paste0(collapse="\n") %>% cat()
}

#' Winsorize a variable
#'
#' @param x The vector to winsorize
#' @param q1 Quartile for lowest values
#' @param q2 Quartile for highest values
#' @param na.rm Whether to remove NAs
#'
#' @return The winsorized vector
#'
#' @export
winsorize <- function(x, q1 = 0.05, q2 = 0.95, na.rm = FALSE) {
  return(DescTools::Winsorize(
    x,
    val = quantile(x, probs = c(q1, q2), na.rm = na.rm)
  ))
}

#' Get descriptive quantiles of a numeric vector
#' @export
descquant <- function(x, int=0.1) {
  quantile(x, probs = seq(0,1,int), na.rm = TRUE)
}

descstrange <- function(d) {
  n_miss <- sum(is.na(d))
  print(paste0("N. missing: ", n_miss, " (", round(100*n_miss/length(d),2), "%)"))
  n_inf <- sum(is.infinite(d))
  print(paste0("N. infinity: ", n_inf, " (", round(100*n_inf/length(d),2), "%)"))
}