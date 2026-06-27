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

#' Print a vector one element per line, with indices
#'
#' Prints each element of `v` on its own line, prefixed with its position in
#' `[i]` form. Useful for eyeballing long character vectors at the console.
#'
#' @param v A vector.
#' @return Invisibly `NULL`; called for the side effect of printing.
#' @examples
#' catvec(c("alpha", "beta", "gamma"))
#' @export
catvec <- function(v) {
  for(i in seq_along(v)) {
    cat(paste0("[", i, "] ", v[i], "\n"))
  }
}

#' Turn a vector into a self-named list
#'
#' Converts `v` into a list whose names equal its values. Convenient for
#' `purrr::map()`/`lapply()` loops where you want the results named after the
#' inputs.
#'
#' @param v A vector.
#' @return A named list, with `names()` equal to the values of `v`.
#' @examples
#' as_named_list(c("a", "b", "c"))
#' @export
as_named_list <- function(v) {
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
#' vec_relocate(vector,"eight", 4)
#' vec_relocate(vector,c("eight","one"), c(4,8))
#' @export
vec_relocate <- function(vect,what,where) {
  # Function to move a single element
  arrange.single <- function(vect,what,where) {
    stopifnot(length(what)==1)
    stopifnot(length(where)==1)
    idx <- which(vect==what)
    if(length(idx)==0) {
      cli::cli_abort("Element {.val {what}} not found in vector")
    }
    if(length(idx)>1) {
      cli::cli_abort("More than one {.val {what}} found in vector")
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

#' Recode the elements of a vector according to a mapping
#'
#' Replaces each element of `vec` that matches a name in `l` with the
#' corresponding value of `l`; unmatched elements are left unchanged.
#'
#' @param vec A vector to recode.
#' @param l A named mapping list whose names are the old values and whose
#'   values are the replacements.
#' @return `vec` with matched elements replaced by their mapping.
#' @examples
#' vec_rename(c("a", "b", "c"), list(a = "Apple", c = "Cherry"))
#' @export
vec_rename <- function(vec, l) {
  to_rename <- vec %in% names(l)
  vec[to_rename] <- l[vec[to_rename]] %>% unlist()
  return(vec)
}