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

#' It will return a character string representing the name of the method that would be dispatched by the input generic given that generic and its arguments.
#' See: https://stackoverflow.com/a/42742370/1719931
#' 
#' @export 
#' @examples
#' findMethod(as.ts, iris)
#' findMethod(print, iris)
#' findMethod(print, Sys.time())
#' findMethod(print, 22)
#' findMethod(print, ordered(3))
#' findMethod(`[`, BOD, 1:2, "Time")
find_method <- function(generic, ...) {
  ch <- deparse(substitute(generic))
  f <- X <- function(x, ...) UseMethod("X")
  for(m in methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
  X(...)
}