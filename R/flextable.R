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

#' Given the index of a row, merge all the cells in that row,
#' center the text, make it bold, and add borders above and below the row.
#' 
#' @export
flextable_make_subheader <- function(tbl, ixs) {
  for (ix in ixs) {
    tbl <- tbl %>%
      flextable::merge_at(i = ix) %>%
      flextable::align(i = ix, align = "center") %>%
      flextable::bold(i = ix) %>%
      flextable::hline(i = ix - 1, border = flextable::fp_border_default()) %>%
      flextable::hline(i = ix, border = flextable::fp_border_default())
  }
  return(tbl)
}