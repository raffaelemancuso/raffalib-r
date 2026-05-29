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

#' @export
catvec <- function(v) {
  for(i in seq_along(v)) {
    cat(paste0("[", i, "] ", v[i], "\n"))
  }
}

#' @export
as.named.list <- function(v) {
  names(v) <- v
  as.list(v)
}

#' Relocate elements of a vector.
#' 
#' This function takes a vector and moves specified elements to new positions, 
#' while keeping the order of other elements unchanged. 
#' It can handle both single and multiple elements to move.
#' Inspired by: https://stackoverflow.com/a/62772613/1719931
#' 
#' @param vect The input vector to be arranged.
#' @param what A single element or a vector of elements to be moved. Each element
#' must be present in the input vector.
#' @param where A single position or a vector of positions indicating the new location(s)
#' for the specified element(s). If a single position is provided, all specified elements will be moved to that position. If a vector of positions is provided, it must be the same length as `what`, and each element will be moved to the corresponding position.
#' @return A new vector with the specified elements moved to their new positions.
#' @examples
#' vector <- c("one","two","three","four","five","six","seven","eight","nine","ten")
#' relocate.vect(vector,"eight", 4)
#' relocate.vect(vector,c("eight","one"), c(4,8))
#' @export
relocate.vect <- function(vect,what,where) {
  # Function to move a single element
  arrange.single <- function(vect,what,where) {
    stopifnot(length(what)==1)
    stopifnot(length(where)==1)
    idx <- which(vect==what)
    if(length(idx)==0) {
      stop(glue("Element '{what}' not found in vector"))
    }
    if(length(idx)>1) {
      stop(glue("More than one '{what}' found in vector"))
    }
    append(vect[-idx], vect[idx], where-1)
  }
  # Move a single element
  if(length(what)==1) {
    return(arrange.single(vect,what,where))
  }
  # Move multiple elements
  else {
    stopifnot(length(what)==length(where))
    for (i in seq_along(what)) {
      vect <- arrange.single(vect,what[i],where[i])
    }
    return(vect)
  }
}