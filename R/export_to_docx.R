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
# along with this program.  If not, see <https://www.gnu.org/licenses/>..

# paper sizes for different formats, in mm
paper_sizes <- list(
  "A0" = c(841, 1189),
  "A1" = c(594, 841),
  "A2" = c(420, 594),
  "A3" = c(297, 420),
  "A4" = c(210, 297),
  "letter" = c(215.9, 279.4)
)

# convert inches in mm
inch2mm <- function(s) {
  return(s * 25.4)
}

mm2inch <- function(s) {
  return(s / 25.4)
}

compute_max_size <- function(doc_gg) {
  word_size <- officer::docx_dim(doc_gg)
  width <- word_size$page['width'] -
    word_size$margins['left'] -
    word_size$margins['right']
  height <- word_size$page['height'] -
    word_size$margins['top'] -
    word_size$margins['bottom']
  width <- inch2mm(width)
  height <- inch2mm(height)
  width <- width * 0.9
  height <- height * 0.9
  return(list(width = width, height = height))
}

#' Initialise a Word document with a caption and page geometry
#'
#' Internal helper behind [plot2docx()], [ggplot2docx()] and [flextable2docx()].
#' Creates an `officer` Word document with the given caption and section
#' properties (paper size, orientation, margins). The named list of arguments
#' accepted here is exactly what those exported functions expose through their
#' `word_prop` argument.
#'
#' @param caption_text Caption to place above the figure/table.
#' @param caption_font_family,caption_font_size Caption font family and size (pt).
#' @param caption_bold,caption_italic Caption emphasis flags.
#' @param page_landscape Whether the page should be landscape (default `FALSE`).
#' @param paper_format Paper size key, one of the names of the internal
#'   `paper_sizes` table (e.g. `"A4"`, `"A3"`, `"letter"`).
#' @param page_margin_bottom,page_margin_top,page_margin_left,page_margin_right
#'   Page margins, in inches.
#' @return A list with the initialised `docx`, the `section_prop`, and the paper
#'   width and height in mm.
#' @keywords internal
prepare_docx <- function(
  caption_text = "",
  caption_font_family = "Aptos",
  caption_font_size = 12,
  caption_bold = TRUE,
  caption_italic = FALSE,
  page_landscape = FALSE,
  paper_format = "A4",
  page_margin_bottom = 1,
  page_margin_top = 1,
  page_margin_left = 1,
  page_margin_right = 1
) {
  # Define page size
  paper_size <- paper_sizes[[paper_format]]
  page_size <- officer::page_size(
    width = paper_size[1],
    height = paper_size[2],
    orient = ifelse(page_landscape, "landscape", "portrait"),
    unit = "mm"
  )

  # Define page margins
  # Unit of measurement of page margins is inches and apparently can't be changed
  page_margins <- officer::page_mar(
    bottom = page_margin_bottom,
    top = page_margin_top,
    right = page_margin_right,
    left = page_margin_left
  )

  # Define section properties
  #
  # A section is a grouping of blocks (ie. paragraphs and tables)
  # that have a set of properties that define pages on which the text will appear.
  # A Section properties object stores information about page composition,
  # such as page size, page orientation, borders and margins.
  section_prop <- officer::prop_section(
    page_size = page_size,
    page_margins = page_margins
  )

  # Formatting Properties - Text
  #
  # fp_text: Create an fp_text object that describes text Formatting Properties.
  # See: https://stackoverflow.com/a/62044378/1719931
  caption_formatting <- officer::fp_text(
    color = "black",
    font.size = caption_font_size,
    bold = caption_bold,
    italic = caption_italic,
    underlined = FALSE,
    strike = FALSE,
    font.family = caption_font_family,
    cs.family = NULL,
    eastasia.family = NULL,
    hansi.family = NULL,
    vertical.align = "baseline",
    shading.color = "transparent"
  )

  # Add formatted chunk of text
  #
  # Format a chunk of text with text formatting properties (bold, color, ...).
  # The function allows you to create pieces of text formatted the way you want.
  caption_ftext <- officer::ftext(
    caption_text,
    prop = caption_formatting
  )

  # Add formatted paragraph
  #
  # Create a paragraph representation by concatenating formatted text or images.
  # The result can be inserted in a Word document or a PowerPoint presentation
  # and can also be inserted in a block_list() call.
  #
  # All its arguments will be concatenated to create a paragraph
  # where chunks of text and images are associated with formatting properties.
  #
  # fpar() supports ftext(), external_img(), run_*() functions (i.e. run_autonum(), run_word_field())
  # when output is Word, and simple strings.
  caption_fpar <- officer::fpar(caption_ftext)

  # Create document
  docx <- officer::read_docx()

  # Add caption
  docx <- docx %>% officer::body_add_fpar(caption_fpar)

  # Return
  outs <- list(
    docx = docx,
    section_prop = section_prop,
    paper_width = paper_size[1],
    paper_height = paper_size[2]
  )

  return(outs)
}

#' Write a prepared Word document to disk
#'
#' Internal helper that closes the section opened by [prepare_docx()] (so Word
#' does not append a blank page) and writes the document to `outfp`.
#'
#' @param outs The list returned by [prepare_docx()], after content has been
#'   added to its `docx` element.
#' @param outfp Output file path for the `.docx` file.
#' @return The value returned by `print.rdocx()` (invisibly), called for its
#'   side effect of writing the file.
#' @keywords internal
finalize_docx <- function(outs, outfp) {
  # It's necessary to define the same default section than the one you want to end the document
  # so that Word agree to not add a page
  # See: https://stackoverflow.com/a/75451251/1719931
  doc <- outs[["docx"]] %>%
    officer::body_end_block_section(
      value = officer::block_section(outs[["section_prop"]])
    ) %>%
    officer::body_set_default_section(outs[["section_prop"]])

  # If the target is open/locked (e.g. the .docx is open in Word), do NOT abort
  # a long batch that generates many files: write to a timestamped fallback
  # next to it, warn naming both paths, and continue.
  tryCatch(
    print(doc, target = outfp),
    error = function(e) {
      alt <- sub(
        "\\.docx$",
        paste0("__locked_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx"),
        outfp
      )
      warning(sprintf(
        "Could not write '%s' (%s). Wrote '%s' instead \u2014 close the open file and re-run to overwrite.",
        outfp, conditionMessage(e), alt
      ), call. = FALSE)
      print(doc, target = alt)
    }
  )
}

#' Save a base R plot to a Word document
#'
#' Writes a base R plot to a `.docx` file with an optional caption and
#' controllable page geometry. The plot must be captured in an
#' [officer::plot_instr()] object rather than passed as a bare function or
#' expression. If `plot_width` and `plot_height` are both `NULL`, a size that
#' fills the printable area is guessed.
#'
#' @param gg The plot to save, as an [officer::plot_instr()] object wrapping the
#'   plotting code, e.g. `officer::plot_instr(code = plot(x, y))`. Passed as the
#'   `value` of [officer::body_add_plot()].
#' @param outfp Output file path for the `.docx` file.
#' @param plot_style Word paragraph style for the figure (default `"Normal"`).
#' @param plot_width,plot_height Figure width and height in `plot_unit`. Supply
#'   both or neither; if neither, a size is guessed from the printable area.
#' @param plot_unit Unit for `plot_width`/`plot_height` (default `"mm"`).
#' @param plot_res Raster resolution in dpi (default 300).
#' @param word_prop A named list of page/caption options forwarded to
#'   [prepare_docx()] (caption text, paper format, margins, ...).
#' @return Called for its side effect of writing `outfp`; returns the result of
#'   [prepare_docx()]'s finaliser invisibly.
#' @seealso [ggplot2docx()], [flextable2docx()]
#' @examples
#' \dontrun{
#' plot2docx(
#'   officer::plot_instr(code = plot(mtcars$wt, mtcars$mpg)),
#'   "scatter.docx",
#'   word_prop = list(caption_text = "Figure 1")
#' )
#' }
#' @export
plot2docx <- function(
  gg,
  outfp,
  plot_style = "Normal",
  plot_width = 152.4,
  plot_height = 127,
  plot_unit = "mm",
  plot_res = 300,
  word_prop = list()
) {
  # Check inputs
  if (
    (is.null(plot_width) & !is.null(plot_height)) |
      (!is.null(plot_width) & is.null(plot_height))
  ) {
    stop(
      "If you specify either plot_width or plot_height, you must specify both."
    )
  }

  if(inherits(gg, "tmap")) {
    stop(
      "Class tmap is unsupported of its own. Please pass officer::plot_instr(print(plt))."
    )
  }

  # Initialize Word document
  outs <- do.call(prepare_docx, word_prop)

  # Compute a reasonable size if the user didn't define a size
  if (is.null(plot_width)) {
    sizes <- compute_max_size(outs[["docx"]])
    plot_width <- sizes[["width"]]
    plot_height <- sizes[["height"]]
    print(paste0("Guessed plot size: ", plot_width, " x ", plot_height, " mm"))
  }

  # Add plot to docx
  outs[["docx"]] <- outs[["docx"]] %>%
    officer::body_add_plot(
      value = gg,
      style = plot_style,
      width = plot_width,
      height = plot_height,
      unit = plot_unit,
      res = plot_res
    )

  # Finalize and return
  return(finalize_docx(outs, outfp))
}

#' Save a ggplot2 plot to a Word document
#'
#' Writes a `ggplot` object to a `.docx` file with an optional caption and
#' controllable page geometry. If `plot_width` and `plot_height` are both `NULL`
#' (the default), a size that fills the printable area is guessed.
#'
#' @param gg A `ggplot` object (passed to [officer::body_add_gg()]).
#' @param outfp Output file path for the `.docx` file.
#' @param plot_style Word paragraph style for the figure (default `"Normal"`).
#' @param plot_width,plot_height Figure width and height in `plot_unit`. Supply
#'   both or neither; if neither, a size is guessed from the printable area.
#' @param plot_unit Unit for `plot_width`/`plot_height` (default `"mm"`).
#' @param word_prop A named list of page/caption options forwarded to
#'   [prepare_docx()] (caption text, paper format, margins, ...).
#' @return Called for its side effect of writing `outfp`; returns the result of
#'   [prepare_docx()]'s finaliser invisibly.
#' @seealso [plot2docx()], [flextable2docx()]
#' @export
ggplot2docx <- function(
  gg,
  outfp,
  plot_style = "Normal",
  plot_width = NULL,
  plot_height = NULL,
  plot_unit = "mm",
  word_prop = list()
) {
  # Check inputs
  if (
    (is.null(plot_width) & !is.null(plot_height)) |
      (!is.null(plot_width) & is.null(plot_height))
  ) {
    stop(
      "If you specify either plot_width or plot_height, you must specify both."
    )
  }

  # Initialize Word document
  outs <- do.call(prepare_docx, word_prop)

  # Guess plot size if not specified
  if (is.null(plot_width)) {
    sizes <- compute_max_size(outs[["docx"]])
    plot_width <- sizes[["width"]]
    plot_height <- sizes[["height"]]
    print(paste0("Guessed plot size: ", plot_width, " x ", plot_height, " mm"))
  }

  # Add plot to docx
  outs[["docx"]] <- outs[["docx"]] %>%
    officer::body_add_gg(
      value = gg,
      style = plot_style,
      # width and height are expressed in inches
      width = plot_width,
      height = plot_height,
      unit = plot_unit
    )

  # Finalize and return
  return(finalize_docx(outs, outfp))
}

#' Collapse several variables into ONE "controls included" row
#'
#' Regression tables (e.g. from [modelsummary::modelsummary()] with
#' `output = "flextable"`) devote one block of rows (estimate, standard error,
#' ...) to every level of every categorical variable. This helper replaces the
#' rows of *all* the variables in `vars` with a **single** row — the usual
#' "Controls: Yes" line of a regression table.
#'
#' Variables are given by their **names in the model** (e.g. `"gender"`,
#' `"piStage"`), not by whatever the table displays. When the table was built
#' with `coef_rename = TRUE` the displayed terms are variable *labels*
#' (`"Gender [male]"`, `"log(1+PubsPiQ1)"`), so pass the label lookup through
#' `labels` — either a named character vector (`name -> label`) or the labelled
#' data frame the models were fit on — and the names are resolved to the text
#' actually shown. Each element of `vars` is treated as a **prefix** over
#' variable names, so `"log_pi_5y_count"` picks up `log_pi_5y_count_Q1`,
#' `log_pi_5y_count_Q2`, ... in one go. Names that resolve to no label are
#' matched literally *and* in the spelling `modelsummary` gives unlabelled
#' terms, with underscores turned into spaces (`"start_year"` also matches
#' `"start year [2008]"`), which covers `coef_rename = FALSE` tables and
#' variables whose label was dropped en route (`forcats::fct_drop()` does that).
#'
#' The collapsed row takes the **position of the first row of the group**, so it
#' stays where the reader expects those variables to be. When the group is the
#' last block of coefficients it therefore ends up just above the
#' goodness-of-fit statistics, and the coefficient/GOF rule — which
#' `modelsummary` draws as a bottom border on the last coefficient row, and which
#' would otherwise be deleted along with that block — is redrawn on whatever row
#' ends up last. Rows whose term cell is empty
#' (standard errors, confidence intervals) are removed along with the
#' coefficient row they belong to. A column that is empty across every collapsed
#' row keeps its blank cell rather than gaining `value`, so grouping columns
#' (`component`, `effect`) stay intact and a model that did not include the
#' controls is not mislabelled as having done so.
#'
#' @param tbl A `flextable`, typically from
#'   [modelsummary::modelsummary()] with `output = "flextable"`.
#' @param vars Character vector of variable names (or name prefixes) to collapse.
#' @param label Text placed in the term column of the collapsed row.
#' @param value Text placed in every model column of the collapsed row
#'   (default `"Yes"`).
#' @param labels Optional `name -> label` lookup used to resolve `vars` to the
#'   displayed terms: a named character vector, or a data frame carrying
#'   variable labels (passed through [labelled::var_label()]).
#' @param term_col Column key holding the term text. By default it is detected
#'   automatically, which is what you want for `modelsummary` `shape =`
#'   layouts where the first column is `component` and the terms sit in the
#'   second, unnamed one.
#' @return The modified `flextable`.
#' @seealso [flextable2docx()], which writes the resulting table to Word.
#' @examples
#' \dontrun{
#' tbl %>% flextable_collapse_group(
#'   vars   = c("gender", "ethnicity_d", "piStage", "log_pi_5y_count"),
#'   label  = "PI-level controls",
#'   value  = "YES",
#'   labels = pis
#' )
#' }
#' @export
flextable_collapse_group <- function(tbl, vars, label, value = "Yes",
                                     labels = NULL, term_col = NULL) {
  stopifnot(
    inherits(tbl, "flextable"),
    is.character(vars), length(vars) > 0,
    is.character(label), length(label) == 1
  )

  # name -> displayed label lookup
  if (is.data.frame(labels)) labels <- labelled::var_label(labels)
  labels <- unlist(labels[!vapply(labels, is.null, logical(1))])

  # every variable whose NAME starts with one of `vars` contributes its label
  # (or, unlabelled, its bare name) as a text prefix to match on
  prefixes <- unique(unlist(lapply(vars, function(v) {
    hit <- if (length(labels)) labels[startsWith(names(labels), v)] else character(0)
    # An unlabelled variable is printed by `modelsummary` with its underscores
    # turned into spaces ("start_year" -> "start year [2008]"), so match that
    # spelling too. This matters for variables whose label was dropped along
    # the way -- `forcats::fct_drop()` discards it, for instance.
    c(unname(hit), v, gsub("_", " ", v))
  })))

  matches_any <- function(x) {
    Reduce(`|`, lapply(prefixes, function(p) startsWith(x, p)),
           init = logical(length(x)))
  }

  # locate the column holding the term text
  col_keys <- tbl$col_keys
  if (is.null(term_col)) {
    hits <- vapply(col_keys, function(ck) {
      sum(matches_any(trimws(as.character(tbl$body$dataset[[ck]]))))
    }, integer(1))
    if (max(hits) == 0) {
      warning("No rows matching ", paste(sQuote(vars), collapse = ", "),
              " were found; table left unchanged.")
      return(tbl)
    }
    term_col <- col_keys[which.max(hits)]
  }

  terms <- trimws(as.character(tbl$body$dataset[[term_col]]))
  blank <- is.na(terms) | terms == ""
  starts <- which(!blank & matches_any(terms))
  if (length(starts) == 0) {
    warning("No rows matching ", paste(sQuote(vars), collapse = ", "),
            " were found in column ", sQuote(term_col), "; table left unchanged.")
    return(tbl)
  }

  # a block is a matched row plus the blank-term rows trailing it
  rows <- integer(0)
  for (s in starts) {
    e <- s
    while (e < length(terms) && blank[e + 1]) e <- e + 1
    rows <- c(rows, s:e)
  }
  rows <- sort(unique(rows))

  # columns that are blank on every matched row (grouping columns such as
  # `component`/`effect`) must stay blank on the collapsed row
  blank_cols <- col_keys[vapply(col_keys, function(ck) {
    all(trimws(as.character(tbl$body$dataset[[ck]]))[rows] == "")
  }, logical(1))]

  flextable_insert_row(tbl, term_col, label, value,
                       at = rows[1], drop = rows, blank_cols = blank_cols)
}

#' Drop the rows of one model component from a regression table
#'
#' Models with several components — `glmmTMB`'s `dispersion`, a zero-inflation
#' part, and so on — get one block of rows per component, tagged in a
#' `component` column when the table was built with a `shape` that includes it.
#' This removes the block belonging to `component`, together with the
#' empty-term continuation rows (standard errors) that trail it.
#'
#' Useful before dropping the `component` column itself: with only one component
#' left the column carries no information, but a second `(Intercept)` row would
#' otherwise read as a duplicate of the first.
#'
#' @param tbl A `flextable`, typically from [modelsummary::modelsummary()] with
#'   `output = "flextable"` and a `shape` including `component`.
#' @param component Name of the component to remove, e.g. `"dispersion"`.
#' @param component_col Column key tagging the component (default `"component"`).
#' @param term_col Column key holding the term text. Detected by default as the
#'   column carrying `"(Intercept)"`.
#' @return The modified `flextable`; unchanged, with a warning, when the column
#'   or the component is not found.
#' @seealso [flextable_collapse_group()], [flextable_add_row_before_gof()]
#' @examples
#' \dontrun{
#' tbl %>% flextable_drop_component("dispersion")
#' }
#' @export
flextable_drop_component <- function(tbl, component, component_col = "component",
                                     term_col = NULL) {
  stopifnot(
    inherits(tbl, "flextable"),
    is.character(component), length(component) == 1
  )
  if (!component_col %in% tbl$col_keys) {
    warning("No ", sQuote(component_col), " column; table left unchanged.")
    return(tbl)
  }
  ds <- tbl$body$dataset
  comp <- trimws(as.character(ds[[component_col]]))
  starts <- which(comp == component)
  if (length(starts) == 0) {
    warning("No rows for component ", sQuote(component), "; table left unchanged.")
    return(tbl)
  }
  if (is.null(term_col)) term_col <- flextable_term_col(tbl)
  terms <- trimws(as.character(ds[[term_col]]))

  # a component block is its tagged row plus the untagged, blank-term rows that
  # follow it (standard errors); GOF rows carry a term, so they are never taken
  rows <- integer(0)
  for (s in starts) {
    e <- s
    while (e < length(terms) && terms[e + 1] == "" && comp[e + 1] == "") e <- e + 1
    rows <- c(rows, s:e)
  }
  rows <- sort(unique(rows))

  # The coefficient/GOF rule is drawn as a bottom border on the last coefficient
  # row, which is exactly what a trailing component block tends to be: deleting
  # it would take the table's only separator with it. Note where the GOF block
  # starts while the rule is still there, then redraw it on whatever row ends up
  # last above the GOF.
  bw <- tbl$body$styles$cells[["border.width.bottom"]]$data
  rule <- if (is.null(bw)) 0 else suppressWarnings(max(bw[rows, , drop = FALSE], na.rm = TRUE))
  gof_before <- flextable_gof_start(tbl)

  out <- flextable::delete_rows(tbl, i = rows, part = "body")

  if (is.finite(rule) && rule > 0 && !is.na(gof_before)) {
    target <- gof_before - sum(rows < gof_before) - 1L
    if (target >= 1L && target <= nrow(out$body$dataset)) {
      out <- flextable::hline(
        out, i = target, border = officer::fp_border(width = rule), part = "body"
      )
    }
  }
  return(out)
}

#' Add a labelled row just above the goodness-of-fit block
#'
#' Puts a single row at the end of the coefficients of a regression table — the
#' place for a line that describes the specification rather than a coefficient,
#' such as an exposure offset or a fixed-effect indicator. Nothing is removed;
#' for folding existing coefficient rows into one, see
#' [flextable_collapse_group()].
#'
#' @param tbl A `flextable`, typically from [modelsummary::modelsummary()] with
#'   `output = "flextable"`.
#' @param label Text placed in the term column of the new row.
#' @param value Text placed in every model column of the new row.
#' @param term_col Column key holding the term text. Detected by default as the
#'   column carrying `"(Intercept)"`, falling back to the first column.
#' @param blank_cols Column keys to leave empty on the new row. By default the
#'   grouping columns are detected as those blank on most body rows (`component`,
#'   `effect`), so the value does not spill into them.
#' @return The modified `flextable`.
#' @seealso [flextable_collapse_group()], [flextable2docx()]
#' @examples
#' \dontrun{
#' tbl %>% flextable_add_row_before_gof("Exposure offset", "log(years observed)")
#' }
#' @export
flextable_add_row_before_gof <- function(tbl, label, value, term_col = NULL,
                                         blank_cols = NULL) {
  stopifnot(
    inherits(tbl, "flextable"),
    is.character(label), length(label) == 1,
    is.character(value), length(value) == 1
  )
  col_keys <- tbl$col_keys
  if (is.null(term_col)) term_col <- flextable_term_col(tbl)
  if (is.null(blank_cols)) {
    others <- setdiff(col_keys, term_col)
    blank_cols <- others[vapply(others, function(ck) {
      mean(trimws(as.character(tbl$body$dataset[[ck]])) == "") > 0.5
    }, logical(1))]
  }
  flextable_insert_row(tbl, term_col, label, value, blank_cols = blank_cols)
}

# Column holding the term text: the one carrying "(Intercept)", else the first.
flextable_term_col <- function(tbl) {
  hits <- vapply(tbl$col_keys, function(ck) {
    sum(trimws(as.character(tbl$body$dataset[[ck]])) == "(Intercept)")
  }, integer(1))
  if (max(hits) > 0) tbl$col_keys[which.max(hits)] else tbl$col_keys[1]
}

# Insert one row, optionally dropping `drop` rows in the same pass. `at` is the
# row index (in the table's current numbering) the new row takes the place of;
# the default is the start of the GOF block, i.e. the end of the coefficients.
# Shared by flextable_collapse_group() and flextable_add_row_before_gof().
flextable_insert_row <- function(tbl, term_col, label, value,
                                 at = NULL,
                                 drop = integer(0),
                                 blank_cols = character(0)) {
  col_keys <- tbl$col_keys
  gof <- flextable_gof_start(tbl)
  n <- nrow(tbl$body$dataset)
  if (is.na(gof)) gof <- n + 1L
  if (is.null(at)) at <- gof

  # The coefficient/GOF rule is drawn as a bottom border on the last coefficient
  # row. If that row is inside the block being removed, the rule would vanish
  # with it, so note its width before the edit and restore it afterwards.
  bw <- tbl$body$styles$cells[["border.width.bottom"]]$data
  rule <- if (is.null(bw) || !length(drop)) {
    0
  } else {
    suppressWarnings(max(bw[drop, , drop = FALSE], na.rm = TRUE))
  }
  if (!is.finite(rule)) rule <- 0

  keep <- setdiff(seq_len(n), drop)         # surviving rows, in order
  pos <- sum(keep < at)                     # how many of them precede `at`
  # style template: the row the new one will sit under, so it inherits the look
  # of an ordinary coefficient row rather than a GOF row
  template <- if (pos > 0) keep[pos] else if (length(keep)) keep[1] else 1L

  # ONE index vector performs the whole edit: dropped rows are absent from it and
  # the template appears twice, which inserts the new row in the right place. No
  # rows are moved afterwards.
  idx <- append(keep, template, after = pos)
  tbl$body <- flextable_subset_body_rows(tbl$body, idx)
  new_row <- pos + 1L
  # rows above the GOF block afterwards: the survivors plus the inserted one
  last_coef <- sum(keep < gof) + 1L

  # Keep exactly one coefficient/GOF rule, on the last row above the GOF block.
  bottom <- tbl$body$styles$cells[["border.width.bottom"]]$data
  inherited <- if (is.null(bottom)) {
    0
  } else {
    suppressWarnings(max(bottom[new_row, ], na.rm = TRUE))
  }
  if (!is.finite(inherited)) inherited <- 0
  clear <- function(x, i) {
    flextable::hline(x, i = i, border = officer::fp_border(width = 0), part = "body")
  }
  # the inserted row copied the template's rule: take it off the template
  if (inherited > 0 && new_row > 1L) tbl <- clear(tbl, new_row - 1L)
  # ... and off the inserted row too, unless it is the one that should carry it
  if (inherited > 0 && new_row != last_coef) tbl <- clear(tbl, new_row)
  # the rule went out with the deleted block: draw it where it belongs now
  width <- max(rule, inherited)
  if (width > 0 && last_coef >= 1L && last_coef <= nrow(tbl$body$dataset)) {
    tbl <- flextable::hline(
      tbl, i = last_coef, border = officer::fp_border(width = width), part = "body"
    )
  }

  # fill the inserted row
  tbl <- flextable::compose(tbl, i = new_row, j = term_col,
                            value = flextable::as_paragraph(label),
                            part = "body")
  tbl$body$dataset[[term_col]][new_row] <- label
  for (ck in setdiff(col_keys, term_col)) {
    txt <- if (ck %in% blank_cols) "" else value
    tbl <- flextable::compose(tbl, i = new_row, j = ck,
                              value = flextable::as_paragraph(txt),
                              part = "body")
    tbl$body$dataset[[ck]][new_row] <- txt
  }
  return(tbl)
}

# First row of the goodness-of-fit block, or NA if there is none.
#
# `modelsummary` separates estimates from GOF statistics with a horizontal rule,
# which survives in the flextable as a top border on the first GOF row. Where
# several rules exist (grouped/panelled tables) the GOF block is the last one,
# because it always sits at the bottom of the table.
flextable_gof_start <- function(tbl) {
  cells <- tbl$body$styles$cells
  top <- cells[["border.width.top"]]$data
  if (!is.null(top) && is.matrix(top)) {
    hit <- which(apply(top, 1, function(r) any(!is.na(r) & r > 0)))
    hit <- hit[hit > 1]
    if (length(hit)) return(max(hit))
  }
  # some themes draw the same separator as a bottom border on the last estimate
  bot <- cells[["border.width.bottom"]]$data
  if (!is.null(bot) && is.matrix(bot)) {
    hit <- which(apply(bot, 1, function(r) any(!is.na(r) & r > 0)))
    hit <- hit[hit < nrow(bot)]
    if (length(hit)) return(max(hit) + 1L)
  }
  NA_integer_
}

# Reindex every parallel structure of a flextable body part with `idx`, the same
# operation flextable's own `delete_rows()` performs with a negative index. A
# repeated index duplicates that row, which is how a new row gets inserted while
# inheriting the styling of its template.
flextable_subset_body_rows <- function(part, idx) {
  reindex <- function(x) {
    if (is.null(x$data)) {
      cli::cli_abort("Unexpected {.pkg flextable} internals: no {.field data} field.")
    }
    x$data <- x$data[idx, , drop = FALSE]
    x$nrow <- length(idx)
    x
  }
  part$dataset <- part$dataset[idx, , drop = FALSE]
  rownames(part$dataset) <- NULL
  part$rowheights <- part$rowheights[idx]
  part$hrule <- part$hrule[idx]
  part$spans$rows <- part$spans$rows[idx, , drop = FALSE]
  part$spans$columns <- part$spans$columns[idx, , drop = FALSE]
  part$content <- reindex(part$content)
  for (grp in c("cells", "pars", "text")) {
    for (prop in names(part$styles[[grp]])) {
      part$styles[[grp]][[prop]] <- reindex(part$styles[[grp]][[prop]])
    }
  }
  part
}

#' Save a flextable to a Word document
#'
#' Writes a `flextable` (typically a regression or summary table produced by
#' `modelsummary`/`gtsummary`) to a `.docx` file, applying a uniform font,
#' alignment and (optionally) padding and column widths.
#'
#' @param tbl A `flextable` object.
#' @param outfp Output file path for the `.docx` file.
#' @param font_name,font_size Font family and size (pt) applied to the whole
#'   table.
#' @param align Table alignment, one of `"left"`, `"center"`, `"right"`.
#' @param padding Optional cell padding (applied to all parts) if not `NULL`.
#' @param column_width Optional column width(s) passed to [flextable::width()].
#' @param layout_autofit If `TRUE` (default) use an autofit layout, otherwise a
#'   fixed layout.
#' @param word_prop A named list of page/caption options forwarded to
#'   [prepare_docx()] (caption text, paper format, margins, ...).
#' @return Called for its side effect of writing `outfp`; returns the result of
#'   [prepare_docx()]'s finaliser invisibly.
#' @seealso [plot2docx()], [ggplot2docx()], [flextable_collapse_group()]
#' @importFrom magrittr %<>%
#' @export
flextable2docx <- function(
  tbl,
  outfp,
  font_name = "Aptos",
  font_size = 12,
  align = "left",
  padding = NULL,
  column_width = NULL,
  layout_autofit = TRUE,
  word_prop = list()
) {
  # Initialize Word document
  outs <- do.call(prepare_docx, word_prop)

  # Define table layout
  layout <- ifelse(layout_autofit, "autofit", "fixed")

  tbl %<>%
    flextable::font(fontname = font_name, part = "all") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::set_table_properties(layout = layout, align = align)

  if (!is.null(padding)) {
    tbl %<>%
      flextable::padding(
        padding = padding,
        part = "all"
      )
  }

  if (!is.null(column_width)) {
    tbl %<>% flextable::width(width = column_width)
  }

  # Add flextable to docx
  outs[["docx"]] <- outs[["docx"]] %>%
    flextable::body_add_flextable(value = tbl, split = TRUE, keepnext = FALSE)

  # Finalize and return
  return(finalize_docx(outs, outfp))
}
