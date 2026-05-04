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

#' Compute population variance
#' 
#' @param x The numeric vector to compute the population variance of
#' @export
varpop <- function(x) {
  n <- length(x)
  var(x) * (n-1)/n
}

#' Save time-stamped backup of a result
#'
#' @export
save_backup <- function(obj, out_dir, file_name) {
  timestamp <- strftime(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  fn <- paste0(file_name, timestamp, ".rds")
  fp <- file.path(out_dir, fn)
  print(paste0("Saving to ", fp))
  saveRDS(obj, fp)
}

# --- DATA WRANGLING --- #

#' Unfactor a variable if it's a factor variable, otherwise return the variable as is
#' 
#' @param var The variable to unfactor
#' @return The unfactored variable
#' @export
myunfactor <- function(var) {
  if(class(var) == "factor") {
    return(varhandle::unfactor(var))
  }
  return(var)
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

# --- DESCRIPTIVES --- #

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
  n_miss <- sum(is.na(d))
  print(paste0("N. missing: ", n_miss, " (", round(100*n_miss/length(d),2), "%)"))
  n_inf <- sum(is.infinite(d))
  print(paste0("N. infinity: ", n_inf, " (", round(100*n_inf/length(d),2), "%)"))
}

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
  df %>% group_by(...) %>% 
    summarise(n=n(), na=sum(is.na({{var}})), p=na/n)
}

#' Find duplicates on the basis of a set of grouping variables
#' 
#' @param df The dataset to check for duplicates
#' @param ... The grouping variables
#' @export
dups <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(n=n()) %>% 
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
      nrow() == 0
  )
}

# --- DATAFRAME UTILS --- #

#' Rename variables of a dataframe with their labels
#' 
#' @export
rename_columns_with_labels <- function(df) {
  colnames(df) <- sapply(df, function(x) attr(x, "label"))
  return(df)
}

#' Sort columns of a dataframe by their labels
#'
#' @export
sort_columns_by_label <- function(df) {
  ix <- order(as.character(labelled::get_variable_labels(df)))
  df <- df[, ix]
  return(df)
}

#' Sort columns of a dataframe by their names (natural sorting order)
#'
#' @export
sort_columns_by_name <- function(df) {
  cols <- gtools::mixedorder(colnames(df))
  df <- df[, cols]
  return(df)
}

#' Print the columns of a dataframe (eventually, only the ones that match a given regular expression) in sorted order
#' 
#' @param df The dataset to print the columns of
#' @param regex A regular expression to filter the columns to print
#' @param sort Whether to sort the columns alphabetically
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

# --- WORKAROUNDS --- #

#' Print what are the character variables in a data frame
#' Used to debug issues due to https://github.com/easystats/parameters/issues/1142
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
#' Works around https://github.com/easystats/parameters/issues/1142
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

