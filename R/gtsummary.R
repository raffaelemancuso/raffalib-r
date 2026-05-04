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

# Source - https://stackoverflow.com/a/79930130
# Posted by PBulls
# Retrieved 2026-04-22, License - CC BY-SA 4.0
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
      "The {.arg pattern} argumnet must be a string using glue syntax to select columns.",
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

  x <- gtsummary:::modify_footnote_header(
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

  x <- gtsummary:::modify_table_body(
    x,
    ~ .x |> dplyr::mutate(stars = !!expr_stars_case_when)
  )

  # updating hidden column status ----------------------------------------------
  cols_to_hide <- c(conf.low = hide_ci, p.value = hide_p, std.error = hide_se)
  cols_to_hide <- cols_to_hide[
    c("conf.low", "p.value", "std.error") %in% names(x$table_body)
  ]
  x <- x |>
    gtsummary:::modify_table_styling(
      columns = tidyselect::all_of(names(cols_to_hide)),
      hide = cols_to_hide
    )

  # adding `cols_merge` to table styling ---------------------------------------
  x <- x |>
    gtsummary:::modify_column_merge(
      rows = !is.na(.data$p.value),
      pattern = pattern
    )

  # return x -------------------------------------------------------------------
  # fill in the Ns in the header table modify_stat_* columns
  x <- gtsummary:::.fill_table_header_modify_stats(x)
  x$call_list <- updated_call_list
  x
}

#' Formatter for gtsummary's statistic column
#' This only makes sense if you call modify_column_unhide("statistic") before,
#' otherwise the statistic column is hidden by default.
#'
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
  return(modify_fmt_fun(table, statistic = fmt_fnc))
}

#' Compute a column with the differences between the means across two groups in a gtsummary table
#' See: https://stackoverflow.com/a/79876424/1719931
#'
#' @export
gtsummary_mean_diff <- function(data, variable, by, tbl, ...) {
  
  x <- data[[variable]]
  g <- data[[by]]
  vartype <- class(x)[1]
  
  # <UNCOMMENT THIS TO MAKE IT WORK>
  if(
    (vartype=="character" | vartype=="numeric" | vartype=="integer") &
    length(unique(x)) <= 10
  ) 
  {
    vartype <- "fake_factor"
  }

  switch(
    vartype,
    fake_factor = {
      prop <- table(x, g)
      d <- prop[, 2] - prop[, 1]
      return(d)
    },
    factor = {
      prop <- table(x, g)
      # margin=1 -> proportions by rows (the sum of a row equals 1)
      # margin=2 -> proportions by columns (the sum of a column equals 1)
      prop <- prop.table(prop, margin = 2)
      d <- prop[, 2] - prop[, 1]
      return(d * 100)
    },
    numeric = {
      return(diff(tapply(x, g, mean, na.rm = TRUE)))
    },
    integer = {
      return(diff(tapply(x, g, mean, na.rm = TRUE)))
    },
    logical = {
      prop <- prop.table(table(x, g), margin = 2)
      d <- prop[, 2] - prop[, 1]
      diffs <- d * 100
      return(diffs["TRUE"])
    },
    {
      stop(glue("ERROR: Unrecognized type {vartype}"))
    }
  )
}

#' Add a column with the differences between the means across two groups in a gtsummary table
#' See: https://stackoverflow.com/a/79876424/1719931
#'
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
    gtsummary::modify_header(add_stat_1 = "**Δ / Δ%**") %>% 
    gtsummary::modify_fmt_fun(add_stat_1 = label_style_sigfig())

  return(x)
}
