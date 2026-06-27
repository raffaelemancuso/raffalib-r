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

# ---- LIST UTILS ---- #

#' Rename list elements according to a mapping list
#'
#' @param l List to rename
#' @param o Mapping list, where names are the old names and values are the new names (`old name`=`new name`)
#' @param .strict If TRUE, all names of l must be in names(o). If FALSE, the names of l that are not in names(o) will be kept.
#'
#' @return A list with the same elements as `l` but with names renamed according
#'   to `o`.
#'
#' @examples
#' l1 <- list(dv1 = "Model 1 here", dv2 = "Model 2 here")
#' l2 <- list(dv1 = "Label_for_DV1", dv2 = "Label_for_DV2")
#' l3 <- list(dv2 = "Label_for_DV2")
#' list_rename_names(l1, l2)
#' list_rename_names(l1, l3, .strict = FALSE)
#' # .strict = TRUE errors when a name of `l` is missing from `o`:
#' try(list_rename_names(l1, l3, .strict = TRUE))
#' @export
list_rename_names <- function(l, o, .strict = TRUE) {
  if (.strict) {
    check <- names(l) %in% names(o)
    stopifnot(all(check))
  }
  new_names <- o[names(l)]
  if (!.strict) {
    mask <- as.logical(lapply(new_names, is.null))
    new_names[mask] <- names(l)[mask]
  }
  names(l) <- as.character(new_names)
  return(l)
}

#' Recode the values of a list according to a mapping
#'
#' Replaces each element of `from` whose value matches a name in `to` with the
#' corresponding value of `to`. Elements with no match are left unchanged. This
#' is the value-side counterpart of [list_rename_names()], which renames the
#' names of a list.
#'
#' @param from A list (or vector) of values to recode.
#' @param to A named mapping list whose names are the old values and whose
#'   values are the replacements.
#' @return `from` with matched values replaced by their mapping.
#' @examples
#' list_rename_values(list("a", "b", "c"), list(a = "Apple", c = "Cherry"))
#' @export
list_rename_values <- function(from, to) {
  for (i in seq_along(from)) {
    var <- from[[i]]
    label <- to[[var]]
    if (!is.null(label)) {
      from[[i]] <- label
    }
  }
  return(from)
}

#' Sort a named list by its names
#'
#' Reorders the elements of a named list by the natural (human) sort order of
#' its names, so that e.g. `item10` follows `item2`.
#'
#' @param l A named list.
#' @return `l` reordered by name.
#' @export
sort_named_list_by_names <- function(l) {
  l <- l[gtools::mixedsort(names(l))]
  return(l)
}

#' Sort a named list by its values
#'
#' Reorders the elements of a named list by the natural (human) sort order of
#' its values (coerced to character).
#'
#' @param l A named list.
#' @return `l` reordered by value.
#' @export
sort_named_list_by_values <- function(l) {
  l <- l[gtools::mixedorder(as.character(l))]
  return(l)
}