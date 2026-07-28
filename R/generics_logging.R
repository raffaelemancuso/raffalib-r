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

# Internal store for startlog() snapshots. Snapshots are kept here (not only in
# an attribute) because some verbs rebuild the data frame and strip custom
# attributes (e.g. dplyr::summarise() and grouped pipelines); in that case
# endlog() falls back to the most recent unconsumed snapshot.
.logenv <- new.env(parent = emptyenv())
.logenv$snaps <- list()
.logenv$counter <- 0L

#' Record a data frame's state before a pipeline
#'
#' Takes a snapshot of `df` (its dimensions, column names and, optionally, a
#' full copy) so that a later call to [endlog()] can report how many rows,
#' columns or cells changed. Designed to bracket a pipeline:
#' `df |> startlog() |> ... |> endlog()`.
#'
#' The snapshot is stored in a package-internal registry and matched back by an
#' id carried in an attribute of `df`. If an intermediate verb strips the
#' attribute (e.g. `dplyr::summarise()`), [endlog()] falls back, with a
#' warning, to the most recent unconsumed snapshot, so well-bracketed pipelines
#' still report correctly. Every `startlog()` should be paired with an `endlog()`:
#' unconsumed snapshots stay in the registry and could be picked up by that
#' fallback.
#'
#' @param df A data frame.
#' @param clone If `TRUE`, also store a copy of `df` so that [endlog()] can
#'   report cell-level changes when the shape is unchanged. Defaults to `FALSE`.
#' @return `df` unchanged, tagged with the snapshot id used by [endlog()].
#' @seealso [endlog()]
#' @export
startlog <- function(df, clone=FALSE) {
  .logenv$counter <- .logenv$counter + 1L
  id <- as.character(.logenv$counter)
  .logenv$snaps[[id]] <- list(
    shape = dim(df),
    names = names(df),
    df = if (clone) df else NULL
  )
  attr(df, "startlog_id") <- id
  return(df)
}

#' Report how a data frame changed since startlog()
#'
#' Consumes the snapshot taken by [startlog()] and emits messages describing
#' the change, e.g. `30/100 (30%) rows have been dropped`. Rows and columns are
#' reported separately, for both drops and additions. If the shape is unchanged
#' and `startlog(clone = TRUE)` was used, reports instead the number and
#' percentage of cells whose value (or missingness) changed; renamed columns
#' are also listed.
#'
#' @param df A data frame previously passed through [startlog()].
#' @return `df` with the [startlog()] tag stripped and an `endlog` attribute
#'   summarizing the changes: a named integer vector with elements `rows` and
#'   `columns` (signed change in dimensions, negative when rows/columns were
#'   dropped) and `cells` (number of cells whose value or missingness changed
#'   when the shape is unchanged and `startlog(clone = TRUE)` was used,
#'   `NA` otherwise).
#' @seealso [startlog()]
#' @export
endlog <- function(df) {
  id <- attr(df, "startlog_id")
  attr(df, "startlog_id") <- NULL
  if (is.null(id) || is.null(.logenv$snaps[[id]])) {
    # Attribute stripped by an intermediate verb: fall back to the most
    # recent unconsumed snapshot.
    n <- length(.logenv$snaps)
    if (n == 0L) {
      warning("No startlog() snapshot found. Did you forget to use startlog()?")
      return(df)
    }
    id <- names(.logenv$snaps)[[n]]
    warning("The startlog() tag was stripped by an intermediate verb; ",
            "falling back to the most recent unconsumed snapshot.")
  }
  snap <- .logenv$snaps[[id]]
  .logenv$snaps[[id]] <- NULL

  report_dim_change(snap$shape[1], nrow(df), "row")
  report_dim_change(snap$shape[2], ncol(df), "column")
  n_changes <- NA_integer_

  if (identical(snap$shape, dim(df))) {
    renamed <- which(snap$names != names(df))
    if (length(renamed) > 0) {
      message("Columns renamed: ",
              paste0(snap$names[renamed], " -> ", names(df)[renamed],
                     collapse = ", "))
    }
    if (is.null(snap$df)) {
      message("No changes in shape. ",
              "Use `startlog(clone=TRUE)` to compare cell values.")
    } else {
      n_changes <- count_cell_changes(snap$df, df)
      ntot <- prod(snap$shape)
      message(fmt_n(n_changes), "/", fmt_n(ntot),
              " (", fmt_pct(n_changes, ntot), ") ",
              ngettext(n_changes, "cell has", "cells have"), " been changed")
    }
  }
  attr(df, "endlog") <- c(rows = nrow(df) - snap$shape[1],
                          columns = ncol(df) - snap$shape[2],
                          cells = n_changes)
  return(df)
}

# Message a row/column count change in the format
# "30/100 (30%) rows have been dropped" or
# "30 rows have been added (100 -> 130, +30%)". Silent when unchanged.
report_dim_change <- function(old_n, new_n, what) {
  delta <- new_n - old_n
  if (delta < 0) {
    message(fmt_n(-delta), "/", fmt_n(old_n), " (", fmt_pct(-delta, old_n), ") ",
            ngettext(-delta, paste0(what, " has"), paste0(what, "s have")),
            " been dropped")
  } else if (delta > 0) {
    message(fmt_n(delta), " ",
            ngettext(delta, paste0(what, " has"), paste0(what, "s have")),
            " been added (", fmt_n(old_n), " -> ", fmt_n(new_n),
            ", +", fmt_pct(delta, old_n), ")")
  }
}

# format = "d" so doubles (e.g. prod() of a shape) don't fall back to
# scientific notation.
fmt_n <- function(n) {
  formatC(n, format = "d", big.mark = ",")
}

fmt_pct <- function(num, den) {
  if (den == 0) return("NA%")
  paste0(round(num / den * 100, 2), "%")
}

# Count cells that differ between two equal-shaped data frames, comparing
# column-by-column (positionally). A change in missingness counts as a change;
# factors are compared on their labels; columns whose types cannot be compared
# with `!=` are compared on their character representation.
count_cell_changes <- function(old_df, new_df) {
  n_changes <- 0L
  for (j in seq_along(new_df)) {
    o <- old_df[[j]]
    n <- new_df[[j]]
    if (is.factor(o)) o <- as.character(o)
    if (is.factor(n)) n <- as.character(n)
    neq <- tryCatch(o != n,
                    error = function(e) as.character(o) != as.character(n))
    changed <- xor(is.na(o), is.na(n)) | (neq %in% TRUE)
    n_changes <- n_changes + sum(changed)
  }
  n_changes
}

# cli::col_*() concatenates with paste0(), where crayon::blue()/green() used
# paste() — so paste(...) here keeps the space-separated, cat()-like behaviour
# callers expect from myinfo("Rows:", n).

#' Print a coloured console message
#'
#' Thin [cat()] wrappers that colour their output with \pkg{cli}: `myinfo()`
#' prints in blue, for ordinary progress messages, and `myheader()` in green,
#' for the section titles of a script. Arguments are joined with [paste()], so
#' they are separated by spaces and terminated with a newline, matching what
#' callers expect from `cat()`.
#'
#' `myheader()` is meant to echo a script's section structure to the console:
#' the convention is an RStudio section header followed immediately by the
#' matching call, so the log mirrors the source.
#'
#' ```r
#' # Load data ----
#' myheader("Load data")
#' ```
#'
#' @param ... Objects to print, joined with [paste()].
#' @return `NULL`, invisibly; called for the side effect of printing.
#' @examples
#' myheader("Load data")
#' myinfo("Rows:", format(15839, big.mark = ","))
#' @export
myinfo <- function(...) {
  cat(cli::col_blue(paste(...)))
  cat("\n")
}

#' @rdname myinfo
#' @export
myheader <- function(...) {
  cat(cli::col_green(paste(...)))
  cat("\n")
}
