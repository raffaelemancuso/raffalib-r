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

# --- DATA-WRANGLING REPORTS --- #

#' Build a fixed-column data-wrangling report
#'
#' Renders the house `.txt` report format written by every data-wrangling
#' script: line 1 title, line 2 timestamp (`YYYY-MM-DD HH:MM:SS`), line 3
#' blank, then one line per spec item. Numbers are right-aligned so the unit
#' digit is always in the same column (the already-formatted string is padded,
#' never the raw number, so thousand separators line up too). Items with a
#' denominator append a fixed-width percentage with the numeric part padded
#' *inside* the parentheses, so brackets, decimal points and `%` signs all sit
#' in fixed columns. Non-integer values (e.g. precision/recall metrics) print
#' with 3 decimals, right-aligned to the same column, and never take a
#' percentage.
#'
#' @param spec A list of items `list(lab = , n = , den = )`. `lab` is the row
#'   label (indent sub-items with two leading spaces inside the label); `n`
#'   the value; `den` the denominator whose percentage the row reports
#'   (`NA`/`NULL` = no percentage). Sub-items conventionally report their
#'   percentage of the parent item's count.
#' @param title Report title (line 1).
#' @return Character vector of report lines.
#' @seealso [report_save()]
#' @export
report_build <- function(spec, title) {
  fmt_val <- function(n) {
    if (n %% 1 == 0) scales::comma(n) else sprintf("%.3f", n)
  }
  nw <- max(nchar(vapply(spec, function(x) fmt_val(x$n), character(1))))
  lw <- max(nchar(vapply(spec, function(x) x$lab, character(1)))) + 2
  body <- vapply(spec, function(x) {
    s <- paste0(formatC(x$lab, width = -lw), formatC(fmt_val(x$n), width = nw))
    if (x$n %% 1 == 0 && !is.null(x$den) && !is.na(x$den)) {
      s <- paste0(s, sprintf(" (%5.1f%%)", 100 * x$n / x$den))
    }
    s
  }, character(1))
  c(title, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "", body)
}

#' Write a fixed-column data-wrangling report to file and console
#'
#' Builds the report with [report_build()], prints it to the console and
#' writes it to `fp` (conventionally `data/<stage>/<N>_report.txt`, numbered
#' after the producing script).
#'
#' The file is always written with LF line endings, on every platform.
#'
#' @inheritParams report_build
#' @param fp Output file path.
#' @return The report lines, invisibly.
#' @export
report_save <- function(spec, title, fp) {
  report <- report_build(spec, title)
  writeLines(report)
  # Write through a binary connection so the line separator is LF everywhere.
  # `writeLines(report, fp)` opens a text-mode connection, which on Windows
  # translates "\n" to "\r\n" and produces a CRLF file.
  con <- file(fp, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(report, con)
  message("Report saved into ", fp)
  invisible(report)
}
