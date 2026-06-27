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

# --- CHANGE LOGGING --- #

#' Record a data frame's shape before a pipeline
#'
#' Stashes the current dimensions of `df` (and, optionally, a full copy) in
#' attributes so that a later call to [endlog()] can report how many rows,
#' columns or cells changed. Designed to bracket a `dplyr` pipeline:
#' `df |> startlog() |> ... |> endlog()`.
#'
#' @param df A data frame.
#' @param clone If `TRUE`, also store a copy of `df` so that [endlog()] can
#'   report cell-level changes when the shape is unchanged. Defaults to `FALSE`.
#' @return `df` unchanged, but carrying the bookkeeping attributes used by
#'   [endlog()].
#' @seealso [endlog()]
#' @export
startlog <- function(df, clone=FALSE) {
  attr(df, "old_shape") <- dim(df)
  if(clone) {
    attr(df, "old_df") <- df
  }
  return(df)
}

#' Report how a data frame changed since startlog()
#'
#' Consumes the attributes left by [startlog()] and emits a message describing
#' the change in number of rows and columns. If the shape is unchanged and
#' `startlog(clone = TRUE)` was used, reports instead the number and percentage
#' of cells whose value (or missingness) changed. The bookkeeping attributes are
#' removed from the returned object.
#'
#' @param df A data frame previously passed through [startlog()].
#' @return `df` with the [startlog()] bookkeeping attributes stripped.
#' @seealso [startlog()]
#' @export
endlog <- function(df) {
  old_shape <- attr(df, "old_shape")
  new_shape <- dim(df)
  if (is.null(old_shape)) {
    warning("No old shape found. Did you forget to use startlog()?")
  }
  
  if(!all(old_shape == new_shape)) {
    # Row changes
    delta_rows <- new_shape[1] - old_shape[1]
    if (delta_rows != 0) {
      pct_rows <- round((delta_rows / old_shape[1]) * 100, 2)
      message("Rows changed by ", delta_rows, " (", pct_rows, "%)")
    }
    # Column changes
    delta_cols <- new_shape[2] - old_shape[2]
    if (delta_cols != 0) {
      pct_cols <- round((delta_cols / old_shape[2]) * 100,
                        2)
      message("Columns changed by ", delta_cols, " (", pct_cols, "%)")
    }
  } else {
    old_df <- attr(df, "old_df")
    if(is.null(old_df)) {
      message("No changes in shape. No old dataframe found to compare (please use `clone=TRUE` in `startlog`).")
    } else {
      # Compare dataframes
      n_value_changes <- sum(old_df != df, na.rm = TRUE)
      n_na_changes <- sum(is.na(old_df) != is.na(df))
      n_changes <- n_value_changes + n_na_changes
      ntot <- prod(old_shape)
      pct_cols <- round((n_changes / ntot) * 100,
                        2)
      message("Data changed in ", n_changes, "/", formatC(ntot, big.mark=","), " (", pct_cols, "%) cells")
    }
  }
  attr(df, "old_shape") <- NULL
  attr(df, "old_df") <- NULL
  return(df)
}