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

#' Create and save a correlation table as a Word document
#' @param df A data frame containing the data
#' @return A flextable object with the correlation table
#' @import dplyr
#' @import correlation
#' @import modelsummary
#' @import flextable
#' @import labelled
#' @importFrom flextable flextable
#' @importFrom flextable add_footer_lines
#' @importFrom modelsummary datasummary_correlation
#' @importFrom labelled var_label
#' @examples
#' \dontrun{
#' data(mtcars)
#' correlation_table(df = mtcars)
#'  }
#' @export
correlation_table <- function(df) {
  
  # Select numeric variables
  ss_df_num <- df %>%
    select(where(is.numeric))

  # Rename columns to numbers
  colnames(ss_df_num) <- paste0("(", seq(1, ncol(ss_df_num)), ")")

  # Compute correlation matrix
  corr_mod <- correlation::correlation(ss_df_num)

  # Footer line with labels meaning
  footer <- paste0(
    "(",
    seq(ncol(ss_df_num)),
    ") ",
    labelled::var_label(ss_df_num),
    collapse = "; "
  )

  # Return table
  tbl <- modelsummary::datasummary_correlation(
    corr_mod,
    stars = TRUE,
    out = "flextable"
  ) %>%
    flextable::add_footer_lines(values = footer)
  
  return(tbl)

}
