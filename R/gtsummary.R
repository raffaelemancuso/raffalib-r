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

#' Add significance stars to a gtsummary table
#'
#' A variant of [gtsummary::add_significance_stars()] with a custom star
#' formatter: it merges the stars into the estimate column (for regression
#' tables) or the p-value column (otherwise) and adds an explanatory footnote.
#'
#' Source: <https://stackoverflow.com/a/79930130> (posted by PBulls, retrieved
#' 2026-04-22, licensed CC BY-SA 4.0).
#'
#' @param x A `gtsummary` table.
#' @param pattern A glue string selecting which column(s) the stars are merged
#'   into; defaults to `"{estimate}{stars}"` for regression tables and
#'   `"{p.value}{stars}"` otherwise.
#' @param thresholds Named numeric vector mapping star symbols to p-value
#'   thresholds.
#' @param hide_ci Whether to hide the confidence-interval column.
#' @param hide_p Whether to hide the p-value column.
#' @param hide_se Whether to hide the standard-error column.
#' @return The `gtsummary` table `x` with a stars column merged in and a
#'   significance footnote added.
#' @export
gtsummary_add_significance_stars <- function(
  x,
  pattern = ifelse(
    inherits(x, c("tbl_regression", "tbl_uvregression")),
    "{estimate}{stars}",
    "{p.value}{stars}"
  ),
  thresholds = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "+" = 0.1),
  hide_ci = TRUE,
  hide_p = inherits(x, c("tbl_regression", "tbl_uvregression")),
  hide_se = FALSE
) {
  `%>%` <- magrittr::`%>%`
  gtsummary:::get_cli_abort_call()
  updated_call_list <- c(
    x$call_list,
    list(my_add_significance_stars = match.call())
  )

  # checking inputs ------------------------------------------------------------
  gtsummary:::check_not_missing(x)
  gtsummary:::check_class(x, "gtsummary")
  gtsummary:::check_class(thresholds, c("numeric", "integer"))
  gtsummary:::check_range(
    thresholds,
    range = c(0, 1),
    include_bounds = c(TRUE, TRUE)
  )
  gtsummary:::check_scalar_logical(hide_ci)
  gtsummary:::check_scalar_logical(hide_p)
  gtsummary:::check_scalar_logical(hide_se)
  if (!"p.value" %in% names(x$table_body)) {
    cli::cli_abort(
      "There is no p-value column in the table and significance stars cannot be placed.",
      call = gtsummary:::get_cli_abort_call()
    )
  }

  # assign default pattern and footnote placement ------------------------------
  ord <- order(thresholds, decreasing = TRUE)[!duplicated(thresholds)]
  sym <- names(thresholds)[ord]
  thr <- thresholds[ord]

  pattern_cols <- gtsummary:::.extract_glue_elements(pattern)
  if (rlang::is_empty(pattern_cols)) {
    cli::cli_abort(
      "The {.arg pattern} argument must be a string using glue syntax to select columns.",
      call = gtsummary:::get_cli_abort_call()
    )
  }
  if (!"stars" %in% pattern_cols) {
    cli::cli_inform(
      "The {.arg pattern} argument does not contain {.val {{stars}}} column, and no stars will be added."
    )
  }

  # adding footnote ------------------------------------------------------------
  p_footnote <-
    paste0(sym, "p<", thr) |>
    unlist() |>
    paste(collapse = "; ")

  x <- gtsummary::modify_footnote_header(
    x,
    footnote = p_footnote,
    columns = any_of(pattern_cols[1]),
    replace = FALSE
  )

  # adding stars column --------------------------------------------------------
  thr <- union(thr, 0L)
  sym <- union("", sym)
  expr_stars_case_when <-
    gtsummary:::map2(
      thr,
      seq_along(thr),
      ~ rlang::expr(p.value >= !!.x ~ !!sym[.y]) |>
        rlang::expr_deparse()
    ) %>%
    gtsummary:::reduce(.f = \(.x, .y) paste(.x, .y, sep = ", ")) %>%
    {
      paste0("dplyr::case_when(is.na(p.value) ~ '', ", ., ")")
    } |> # styler: off
    rlang::parse_expr()

  x <- gtsummary::modify_table_body(
    x,
    ~ .x |> dplyr::mutate(stars = !!expr_stars_case_when)
  )

  # updating hidden column status ----------------------------------------------
  cols_to_hide <- c(conf.low = hide_ci, p.value = hide_p, std.error = hide_se)
  cols_to_hide <- cols_to_hide[
    c("conf.low", "p.value", "std.error") %in% names(x$table_body)
  ]
  x <- x |>
    gtsummary::modify_table_styling(
      columns = tidyselect::all_of(names(cols_to_hide)),
      hide = cols_to_hide
    )

  # adding `cols_merge` to table styling ---------------------------------------
  x <- x |>
    gtsummary::modify_column_merge(
      rows = !is.na(.data$p.value),
      pattern = pattern
    )

  # return x -------------------------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- gtsummary:::.fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x
}

#' Format the statistic column of a gtsummary table
#'
#' Applies a fixed-decimal formatter (with thousands separators) to the
#' `statistic` column. Only meaningful after the column has been unhidden with
#' `gtsummary::modify_column_unhide("statistic")`, since `gtsummary` hides it by
#' default.
#'
#' @param table A `gtsummary` table.
#' @param digits Number of decimal places to display (default 6).
#' @return The `gtsummary` table with its `statistic` column reformatted.
#' @export
gtsummary_format_statistic_column <- function(table, digits = 6) {
  fmt_fnc <- function(x) {
    formatC(
      x,
      digits = digits,
      big.mark = ",",
      format = "f"
    ) %>%
      stringr::str_replace_all("NA", "") %>%
      stringr::str_trim()
  }
  # Use modify_fmt_fun(colname = <fmt fun>) to update a single column.
  return(gtsummary::modify_fmt_fun(table, statistic = fmt_fnc))
}

#' Compute the between-group difference for a gtsummary variable
#'
#' Worker function (in the form expected by [gtsummary::add_stat()]) that returns
#' the difference between the two groups defined by `by`: a difference in means
#' for continuous variables, and a difference in proportions (in percentage
#' points) for categorical and dichotomous variables. Assumes exactly two groups.
#' See <https://stackoverflow.com/a/79876424/1719931>.
#'
#' @param data The data frame underlying the table.
#' @param variable Name of the variable to summarise.
#' @param by Name of the two-level grouping variable.
#' @param tbl The `gtsummary` table being built, used to look up the variable
#'   type that `gtsummary` assigned.
#' @param ... Unused; kept for compatibility with the [gtsummary::add_stat()] API.
#' @return A numeric difference (group 2 minus group 1); for categorical and
#'   dichotomous variables the difference in proportions on the 0-1 scale
#'   (formatted as a 0%-100% percentage by [gtsummary_add_mean_diff()]).
#' @seealso [gtsummary_add_mean_diff()]
#' @export
gtsummary_mean_diff <- function(data, variable, by, tbl, ...) {

  x <- data[[variable]]
  g <- data[[by]]

  # Query the type that gtsummary actually assigned via tbl$table_body$var_type
  # See: https://stackoverflow.com/a/79935992/1719931
  var_type <- tbl$table_body |>
    filter(variable == !!variable) |>
    pull(var_type) |>
    first()

  switch(
    var_type,
    categorical = {
      # margin=1 -> proportions by rows (the sum of a row equals 1)
      # margin=2 -> proportions by columns (the sum of a column equals 1)
      prop <- prop.table(table(x, g), margin = 2)
      d <- prop[, 2] - prop[, 1]
      return(d)
    },
    continuous = {
      return(diff(tapply(x, g, mean, na.rm = TRUE)))
    },
    dichotomous = {
      prop <- prop.table(table(x, g), margin = 2)
      d <- prop[, 2] - prop[, 1]
      # the displayed level: gtsummary records it in var_level ("TRUE", "1",
      # "yes", ...); a hard d["TRUE"] lookup returned NA for 0/1-coded
      # variables. Fall back to the last level when var_level is absent.
      lev <- tryCatch(
        tbl$table_body |>
          filter(variable == !!variable) |>
          pull(var_level) |>
          na.omit() |>
          first(),
        error = function(e) NA_character_
      )
      if (is.null(lev) || is.na(lev) || !(lev %in% names(d))) lev <- tail(names(d), 1)
      return(d[lev])
    },
    {
      stop(paste0("ERROR: Unrecognized type ", var_type))
    }
  )
}

#' Rename an internal column of a gtsummary table
#'
#' Renames a column in the table body AND in every styling reference
#' (header, fmt_fun, ...). A bare `table_body` rename would orphan the
#' stylings keyed on the old name, and gtsummary would hide the column.
#'
#' @param tbl A `gtsummary` table.
#' @param old Current column name.
#' @param new New column name.
#' @return The table with the column renamed.
#' @export
gtsummary_rename_column <- function(tbl, old, new) {
  stopifnot(old %in% colnames(tbl$table_body))
  colnames(tbl$table_body)[colnames(tbl$table_body) == old] <- new
  tbl$table_styling <- lapply(tbl$table_styling, function(el) {
    if (is.data.frame(el)) {
      for (cc in intersect(c("column", "columns"), colnames(el))) {
        if (is.character(el[[cc]])) {
          el[[cc]][el[[cc]] == old] <- new
        }
      }
    }
    el
  })
  stopifnot(new %in% colnames(tbl$table_body))
  return(tbl)
}

#' Add a between-group difference column to a gtsummary table
#'
#' Adds a column of group differences computed by [gtsummary_mean_diff()] to a
#' two-group `gtsummary` table, with a suitable header and number format.
#' The column is named `diff_in_means`. Continuous rows show the difference
#' in means (sigfig format); categorical and dichotomous rows carry the 0-1
#' proportion difference from [gtsummary_mean_diff()] and are formatted as
#' 0%-100% percentages with 2 decimal places (e.g. `15.23%`).
#' See <https://stackoverflow.com/a/79876424/1719931>.
#'
#' @param table A two-group `gtsummary` table.
#' @return The table with an added `diff_in_means` column.
#' @seealso [gtsummary_mean_diff()], [gtsummary_rename_column()]
#' @export
gtsummary_add_mean_diff <- function(table) {

  x <- gtsummary::add_stat(
    table,
    fns = gtsummary::everything() ~ gtsummary_mean_diff,
    location = list(
      gtsummary::all_continuous() ~ "label",
      gtsummary::all_categorical() ~ "level",
      gtsummary::all_dichotomous() ~ "label"
    )
  ) %>%
    gtsummary_rename_column("add_stat_1", "diff_in_means") %>%
    gtsummary::modify_header(diff_in_means = "**Diff / Diff %**") %>%
    # continuous rows: difference in means, plain sigfig
    gtsummary::modify_fmt_fun(
      diff_in_means = gtsummary::label_style_sigfig(),
      rows = var_type == "continuous"
    ) %>%
    # categorical/dichotomous rows: 0-1 proportion differences rendered as
    # 0%-100% percentages, fixed 2 decimal places (label_style_percent is
    # not used: it switches to more decimals for values < 1%)
    gtsummary::modify_fmt_fun(
      diff_in_means = function(x) {
        ifelse(is.na(x), NA_character_, sprintf("%.2f%%", 100 * x))
      },
      rows = var_type %in% c("categorical", "dichotomous")
    )

  return(x)
}
