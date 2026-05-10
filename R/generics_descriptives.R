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