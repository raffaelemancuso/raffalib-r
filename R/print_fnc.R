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

#' Print decimal
#'
#' @param a The decimal to print
#' @return A string
#' @export
print_dec <- function(a) {
  return(formatC(a, format = "d", big.mark = ","))
}

#' Print float
#'
#' @param a The float to print
#' @param ndigits Number of decimal digits to show
#' @return A string
#' @export
print_float <- function(a, ndigits = 3) {
  return(formatC(
    a,
    format = "f",
    big.mark = ",",
    small.mark = ".",
    digits = ndigits
  ))
}

#' Print percentage
#'
#' @param a Size of subset
#' @param b Size of entire set
#' @return A string
#' @export
print_perc <- function(a, b) {
  msg <- print_dec(a)
  msg <- msg |> paste0("/")
  msg <- msg |> paste0(print_dec(b))
  msg <- msg |> paste0(" (")
  msg <- msg |> paste0(print_float((a / b) * 100,2))
  msg <- msg |> paste0("%)")
  return(msg)
}
