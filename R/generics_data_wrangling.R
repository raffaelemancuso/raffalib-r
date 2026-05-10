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

# --- DATA WRANGLING --- #

#' @export
myscale <- function(var) {
  scale(var)[, 1]
}

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
