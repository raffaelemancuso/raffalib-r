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

#' Formatter for gtsummary's statistic
#' Use like `modify_fmt_fun(statistic = raffalib::gtsummary_stat_fmt)`
#' 
#' @export
gtsummary_stat_fmt <- function(x, digits = 2) {
  res <- formatC(
    x,
    digits = digits,
    big.mark = ",",
    format = "f"
  ) %>%
    stringr::str_replace_all("NA", "") %>%
    stringr::str_trim()
  return(res)
}

#' Insert a column with the differences between the means across two groups in a gtsummary table
#' Use like `add_stat(fns = everything() ~ gtsummary_mean_diff, location = list(all_continuous() ~ "label", all_categorical() ~ "level")`
#' See: https://stackoverflow.com/a/79876424/1719931
#' 
#' @export
gtsummary_mean_diff <- \(data, variable, by, ...) {
  x <- data[[variable]]
  g <- data[[by]]
  lvls <- levels(g)
  
  switch(class(x)[1],
         factor = {
           prop <- prop.table(table(x, g), margin = 2)
           sprintf("%+.3f%%", (prop[, lvls[2]] - prop[, lvls[1]]) * 100)
         },
         numeric = sprintf("%+.3f", diff(tapply(x, g, mean, na.rm = TRUE))),
         logical = {
           prop <- prop.table(table(x, g), margin = 2)
           sprintf("%+.3f%%", ((prop[, lvls[2]] - prop[, lvls[1]]) * 100)["TRUE"])
         }
  )
}

