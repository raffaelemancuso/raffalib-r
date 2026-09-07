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

#' Between-group difference in proportions
#'
#' Computes, for each level of `x`, the difference between the two `g` groups
#' in the proportions selected by `statistic`.
#'
#' @param x The variable being summarised.
#' @param g The two-level grouping variable.
#' @param statistic One of `"col_pct"`, `"row_pct"`, `"cell_pct"`.
#' @return Named numeric vector of differences (group 2 minus group 1) on the
#'   0-1 scale, one element per level of `x`.
#' @noRd
gtsummary_prop_diff <- function(x, g, statistic) {
  # margin=1 -> proportions by rows (the sum of a row equals 1)
  # margin=2 -> proportions by columns (the sum of a column equals 1)
  # no margin -> proportions by cells (the sum of the table equals 1)
  prop <- switch(
    statistic,
    col_pct = prop.table(table(x, g), margin = 2),
    row_pct = prop.table(table(x, g), margin = 1),
    cell_pct = prop.table(table(x, g)),
    {
      stop(paste0("ERROR: Unrecognized statistic ", statistic))
    }
  )
  return(prop[, 2] - prop[, 1])
}

#' Compute the between-group difference for a gtsummary variable
#'
#' Worker function (in the form expected by [gtsummary::add_stat()]) that returns
#' the difference between the two groups defined by `by`. Which statistic is
#' differenced is chosen with `statistic`: mean or median for continuous
#' variables; column, row, or cell percentages for categorical and dichotomous
#' variables. Assumes exactly two groups.
#' See <https://stackoverflow.com/a/79876424/1719931>.
#'
#' @param data The data frame underlying the table.
#' @param variable Name of the variable to summarise.
#' @param by Name of the two-level grouping variable.
#' @param tbl The `gtsummary` table being built, used to look up the variable
#'   type that `gtsummary` assigned.
#' @param statistic Which statistic to difference. For continuous variables
#'   `"mean"` (default) or `"median"`; for categorical and dichotomous variables
#'   `"col_pct"` (default), `"row_pct"`, or `"cell_pct"`, i.e. proportions
#'   computed within each column (group), within each row (level), or over all
#'   cells. `NULL` falls back to the default for the variable type.
#' @param ... Unused; kept for compatibility with the [gtsummary::add_stat()] API.
#' @return A numeric difference (group 2 minus group 1); for categorical and
#'   dichotomous variables the difference in proportions on the 0-1 scale
#'   (formatted as a 0%-100% percentage by [gtsummary_add_mean_diff()]).
#' @seealso [gtsummary_add_mean_diff()]
#' @export
gtsummary_mean_diff <- function(data, variable, by, tbl, statistic = NULL, ...) {

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
      if (is.null(statistic)) statistic <- "col_pct"
      # unname: add_stat aligns the vector with the level rows by position
      return(unname(gtsummary_prop_diff(x, g, statistic)))
    },
    continuous = ,
    continuous2 = {
      if (is.null(statistic)) statistic <- "mean"
      fun <- switch(
        statistic,
        mean = mean,
        median = stats::median,
        {
          stop(paste0(
            "ERROR: Statistic ", statistic,
            " is not valid for continuous variable ", variable
          ))
        }
      )
      return(unname(diff(tapply(x, g, fun, na.rm = TRUE))))
    },
    dichotomous = {
      if (is.null(statistic)) statistic <- "col_pct"
      d <- gtsummary_prop_diff(x, g, statistic)
      # the displayed level: gtsummary records it in var_level ("TRUE", "1",
      # "yes", ...); a hard d["TRUE"] lookup returned NA for 0/1-coded
      # variables. Fall back to the last level when var_level is absent.
      lev <- tryCatch(
        tbl$table_body |>
          filter(variable == !!variable) |>
          pull(var_level) |>
          stats::na.omit() |>
          first(),
        error = function(e) NA_character_
      )
      if (is.null(lev) || is.na(lev) || !(lev %in% names(d))) lev <- utils::tail(names(d), 1)
      return(unname(d[lev]))
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

#' Collapse line breaks in a gtsummary table's footnotes
#'
#' [gtsummary::tbl_summary()] builds the default header footnote by gluing the
#' statistic labels into the statistic template, so a literal `\\n` used in the
#' template to stack cell contents (e.g. `"{mean}\\n({sd})"` becomes
#' `"Mean\\n(SD)"`) leaks into the footnote, which then renders across several
#' lines in the exported table. This helper replaces every line break in the
#' header and body footnotes with `replacement`, leaving the cell layout
#' untouched.
#'
#' @param tbl A `gtsummary` table.
#' @param replacement String substituted for each line break. Default `" "`.
#' @return The `gtsummary` table with single-line footnotes.
#' @export
gtsummary_collapse_footnote_newlines <- function(tbl, replacement = " ") {
  stopifnot(inherits(tbl, "gtsummary"))
  for (el in c("footnote_header", "footnote_body")) {
    if (
      !is.null(tbl$table_styling[[el]]) &&
        "footnote" %in% colnames(tbl$table_styling[[el]])
    ) {
      tbl$table_styling[[el]]$footnote <-
        stringr::str_replace_all(
          tbl$table_styling[[el]]$footnote,
          "\r?\n",
          replacement
        )
    }
  }
  return(tbl)
}

#' Extract the statistic placeholders of a glue template
#'
#' @param template A glue template string, e.g. `"{mean}\n{median}"`.
#' @return Character vector of the unique placeholder names, brace-stripped
#'   and trimmed.
#' @noRd
glue_stat_elements <- function(template) {
  m <- regmatches(template, gregexpr("\\{[^{}]*\\}", template))[[1]]
  unique(trimws(substr(m, 2, nchar(m) - 1)))
}

#' Format one between-group difference statistic
#'
#' @param x Raw numeric difference(s) from [gtsummary_mean_diff()].
#' @param statistic The statistic name (`"mean"`, ..., `"cell_pct"`).
#' @param digits `NULL` for the default formats (sigfig for mean/median,
#'   2-decimal percentage points with a trailing `%` sign for the percentage
#'   statistics), a non-negative integer for fixed decimal places, or a
#'   formatting function whose output is used verbatim (no `%` appended).
#' @return Character vector of formatted values.
#' @noRd
fmt_mean_diff <- function(x, statistic, digits) {
  if (is.function(digits)) {
    return(as.character(digits(x)))
  }
  if (statistic %in% c("mean", "median")) {
    if (is.null(digits)) {
      gtsummary::style_sigfig(x)
    } else {
      gtsummary::style_number(x, digits = digits, big.mark = ",")
    }
  } else {
    # percentage-point differences, with the % sign appended automatically
    sprintf("%.*f%%", if (is.null(digits)) 2 else digits, 100 * x)
  }
}

#' Build the formatted difference cell(s) for one gtsummary variable
#'
#' Computes every statistic referenced by the variable's glue `template`
#' through [gtsummary_mean_diff()], formats each with `fmt_mean_diff()`, and
#' interpolates the template. Cells with a missing component are `NA`.
#'
#' @inheritParams gtsummary_mean_diff
#' @param template Glue template string for this variable.
#' @param digits Digits setting for this variable (see `fmt_mean_diff()`).
#' @return Character vector, one element per displayed row of the variable.
#' @noRd
gtsummary_mean_diff_glue <- function(
  data, variable, by, tbl, template, digits, ...
) {
  stats <- glue_stat_elements(template)
  raw <- lapply(
    stats,
    function(s) gtsummary_mean_diff(data, variable, by, tbl, statistic = s, ...)
  )
  names(raw) <- stats
  fmt <- lapply(stats, function(s) fmt_mean_diff(raw[[s]], s, digits))
  names(fmt) <- stats
  out <- as.character(glue::glue_data(fmt, template, .trim = FALSE))
  # blank the cells where any component is missing
  na_mask <- Reduce(`|`, lapply(raw, is.na))
  out[na_mask] <- NA_character_
  return(out)
}

#' Add a between-group difference column to a gtsummary table
#'
#' Adds a column of group differences computed by [gtsummary_mean_diff()] to a
#' two-group `gtsummary` table, with a suitable header. Cell contents are
#' driven by `statistic`, a formula-list-selector of glue templates (as in
#' [gtsummary::tbl_summary()]): each `{placeholder}` is replaced by the
#' formatted between-group difference of that statistic, and surrounding
#' literal text (`%` signs, separators, line breaks) is kept verbatim, so one
#' cell can combine several statistics. The column is named `diff_in_means`
#' whatever the chosen statistics and holds formatted text, not numbers.
#' An explanatory footnote is attached to the column header; when the two
#' `by`-group levels can be recovered from the table, the footnote names them
#' and the direction of the difference (second level minus first), and it
#' describes the statistic(s) selected through `statistic`.
#' See <https://stackoverflow.com/a/79876424/1719931>.
#'
#' @param table A two-group `gtsummary` table.
#' @param statistic Formula-list-selector (as in [gtsummary::tbl_summary()])
#'   of glue templates choosing which difference(s) each variable displays.
#'   Available placeholders: `{mean}` and `{median}` for continuous variables;
#'   `{col_pct}`, `{row_pct}`, and `{cell_pct}` for categorical and dichotomous
#'   variables, i.e. differences in column, row, and cell percentages, in
#'   percentage points with a trailing `%` sign appended automatically.
#'   Templates can mix several placeholders with literal text, e.g.
#'   `list(gtsummary::all_continuous() ~ "{mean}\n{median}",
#'   gtsummary::all_categorical() ~ "{row_pct} / {col_pct}")`.
#'   Variables not covered by the selector fall back to their type's
#'   default (`"{mean}"` / `"{col_pct}"`).
#' @param digits Formula-list-selector (as in [gtsummary::tbl_summary()])
#'   overriding how a variable's differences are rounded. The same rounding
#'   applies to every placeholder of the variable's template, so each value is
#'   a single non-negative integer (not a vector) or a formatting function. An
#'   integer is the number of decimal places: continuous statistics switch
#'   from the default sigfig format to that many fixed decimal places;
#'   percentage statistics change the decimal places of the percentage
#'   (default 2), keeping the trailing `%` sign. A function is applied to the
#'   raw difference of each placeholder and its output is used verbatim (no
#'   `%` appended); note that percentage statistics are on the 0-1 scale.
#'   Variables not covered keep the default formats.
#'   Example: `list(gtsummary::all_continuous() ~ 1)`.
#' @param footnote Footnote for the difference column: `TRUE` (default) adds
#'   an auto-built explanation, a string is used as-is, `FALSE` adds none.
#' @return The table with an added `diff_in_means` column of formatted text.
#' @seealso [gtsummary_mean_diff()], [gtsummary_rename_column()]
#' @export
gtsummary_add_mean_diff <- function(
  table,
  statistic = list(
    gtsummary::all_continuous() ~ "{mean}",
    gtsummary::all_categorical() ~ "{col_pct}"
  ),
  digits = NULL,
  footnote = TRUE
) {

  # resolve the formula-list-selector into a named list keyed on variable, as
  # gtsummary::add_stat() does; both cards calls write the resolved list back
  # into `statistic` in this frame
  scoped <- gtsummary::scope_table_body(table$table_body)
  cards::process_formula_selectors(scoped, statistic = statistic)
  cards::fill_formula_selectors(
    scoped,
    statistic = list(
      gtsummary::all_continuous() ~ "{mean}",
      gtsummary::all_categorical() ~ "{col_pct}"
    )
  )
  cards::check_list_elements(
    x = statistic,
    predicate = function(x) rlang::is_string(x),
    error_msg = "The element values for the {.arg statistic} argument must be strings (glue templates)."
  )

  # resolve the digits formula-list-selector the same way; variables it does
  # not cover keep the default formats
  if (!is.null(digits)) {
    cards::process_formula_selectors(scoped, digits = digits)
    cards::check_list_elements(
      x = digits,
      predicate = function(x) {
        is.function(x) ||
          (is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 &&
             x == trunc(x))
      },
      error_msg = "The element values for the {.arg digits} argument must be single non-negative integers or functions."
    )
  } else {
    digits <- list()
  }

  # each template must reference only known statistics, compatible with its
  # variable's assigned type
  valid_stats <- c("mean", "median", "col_pct", "row_pct", "cell_pct")
  stats_by_var <- lapply(statistic, glue_stat_elements)
  var_types <- distinct(table$table_body, variable, var_type)
  var_types <- stats::setNames(var_types$var_type, var_types$variable)
  for (v in names(statistic)) {
    stats_v <- stats_by_var[[v]]
    if (length(stats_v) == 0) {
      cli::cli_abort(
        "The {.arg statistic} value for variable {.val {v}} contains no statistic placeholder; available placeholders: {.val {valid_stats}}."
      )
    }
    unknown <- setdiff(stats_v, valid_stats)
    if (length(unknown) > 0) {
      cli::cli_abort(
        "Unknown statistic {.val {unknown}} in the {.arg statistic} template for variable {.val {v}}; must be one of {.val {valid_stats}}."
      )
    }
    allowed <- switch(
      var_types[[v]],
      continuous = ,
      continuous2 = c("mean", "median"),
      categorical = ,
      dichotomous = c("col_pct", "row_pct", "cell_pct"),
      character(0)
    )
    bad <- setdiff(stats_v, allowed)
    if (length(allowed) > 0 && length(bad) > 0) {
      cli::cli_abort(
        "Statistic {.val {bad}} is not valid for {var_types[[v]]} variable {.val {v}}; must be one of {.val {allowed}}."
      )
    }
  }

  # the worker returns formatted text (a template can combine several
  # statistics per cell), so no modify_fmt_fun pass is needed afterwards
  x <- gtsummary::add_stat(
    table,
    fns = gtsummary::everything() ~
      function(data, variable, by, tbl, ...) {
        gtsummary_mean_diff_glue(
          data, variable, by, tbl,
          template = statistic[[variable]],
          digits = digits[[variable]],
          ...
        )
      },
    location = list(
      gtsummary::all_continuous() ~ "label",
      gtsummary::all_categorical() ~ "level",
      gtsummary::all_dichotomous() ~ "label"
    )
  ) %>%
    gtsummary_rename_column("add_stat_1", "diff_in_means") %>%
    gtsummary::modify_header(diff_in_means = "**Δ / Δ%**")

  # explanatory footnote on the difference column ------------------------------
  if (isTRUE(footnote)) {
    # name the by-group levels when they can be recovered from the table;
    # gtsummary_mean_diff() computes second level minus first
    by_levels <- tryCatch(
      {
        by <- table$inputs$by
        g <- table$inputs$data[[by]]
        if (is.factor(g)) levels(g) else sort(unique(stats::na.omit(g)))
      },
      error = function(e) NULL
    )
    direction <-
      if (!is.null(by_levels) && length(by_levels) == 2) {
        sprintf("%s minus %s", by_levels[2], by_levels[1])
      } else {
        "second minus first"
      }
    # compact description of the selected statistics: compute each one's
    # scope (generic when it covers every variable of its type, else the
    # variable list), then merge the statistics sharing a scope into one
    # segment, e.g. "means and medians for continuous variables"
    fam_vars <- list(
      continuous = names(var_types)[
        var_types %in% c("continuous", "continuous2")
      ],
      categorical = names(var_types)[
        var_types %in% c("categorical", "dichotomous")
      ]
    )
    fam_label <- c(
      continuous = "for continuous variables",
      categorical = "for categorical and dichotomous variables"
    )
    used <- intersect(valid_stats, unique(unlist(stats_by_var)))
    scopes <- vapply(
      used,
      function(s) {
        vars_s <- names(stats_by_var)[
          vapply(stats_by_var, function(st) s %in% st, logical(1))
        ]
        fam <- ifelse(s %in% c("mean", "median"), "continuous", "categorical")
        if (setequal(vars_s, fam_vars[[fam]])) {
          fam_label[[fam]]
        } else {
          paste0("for ", paste(vars_s, collapse = ", "))
        }
      },
      character(1)
    )
    join_and <- function(x) {
      if (length(x) <= 1) {
        return(as.character(x))
      }
      paste(paste(x[-length(x)], collapse = ", "), x[length(x)], sep = " and ")
    }
    stats_label <- function(ss) {
      parts <- character(0)
      cont <- intersect(c("mean", "median"), ss)
      if (length(cont) > 0) {
        parts <- c(parts, join_and(c(mean = "means", median = "medians")[cont]))
      }
      pct <- intersect(c("col_pct", "row_pct", "cell_pct"), ss)
      if (length(pct) > 0) {
        mods <- c(col_pct = "column", row_pct = "row", cell_pct = "cell")[pct]
        parts <- c(
          parts,
          paste0(join_and(unname(mods)), " percentages (in percentage points)")
        )
      }
      join_and(parts)
    }
    segments <- vapply(
      unique(scopes),
      function(sc) paste(stats_label(used[scopes == sc]), sc),
      character(1)
    )
    footnote <- sprintf(
      "Δ / Δ%%: difference between the two groups (%s): %s.",
      direction,
      paste(segments, collapse = "; ")
    )
  }
  if (is.character(footnote)) {
    x <- gtsummary::modify_footnote_header(
      x,
      footnote = footnote,
      columns = "diff_in_means",
      replace = FALSE
    )
  }

  return(x)
}
