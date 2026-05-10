# ---------------------------------------------------------------------- #
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
# ---------------------------------------------------------------------- #

#' Compute population variance
#'
#' @param x The numeric vector to compute the population variance of
#' @export
varpop <- function(x) {
  n <- length(x)
  var(x) * (n - 1) / n
}

#' Alias for mean(x, na.rm=TRUE)
#'
#' @export
mean_na_rm <- function(x) {
  mean(x, na.rm=TRUE)
}