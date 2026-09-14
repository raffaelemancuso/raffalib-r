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
#' @param caption_style Word paragraph style of the caption (default
#'   `"heading 1"`), a heading style so that the caption is listed in the
#'   navigation pane and in the PDF outline built from the headings. Must exist
#'   in officer's default Word template.
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
  caption_style = "heading 1",
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
  # The caption paragraph takes a heading style (`caption_style`, "heading 1"
  # by default), so it is listed in Word's navigation pane and becomes a
  # bookmark when the PDF outline is built from the headings. The run keeps
  # the explicit font above and the paragraph its zero spacing, so the look
  # does not change; only the style's outline level and keep-with-next apply.
  # The headings of officer's default template are numbered ("1.", set in the
  # style's own font): the numbering is switched off on the paragraph (numId
  # 0), which fp_par() cannot express, hence the edit of the paragraph XML.
  # An empty caption stays a plain paragraph (a heading would list a blank
  # entry).
  docx <- officer::read_docx()
  if (nzchar(caption_text)) {
    caption_fpar <- officer::fpar(
      caption_ftext, fp_p = officer::fp_par(word_style = caption_style)
    )
    caption_xml <- sub(
      "(<w:pStyle [^>]*/>)",
      "\\1<w:numPr><w:ilvl w:val=\"0\"/><w:numId w:val=\"0\"/></w:numPr>",
      officer::to_wml(caption_fpar, add_ns = TRUE)
    )
    docx <- officer::body_add_xml(docx, caption_xml)
  } else {
    docx <- officer::body_add_fpar(docx, officer::fpar(caption_ftext))
  }

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
#' `"piStage"`), not by whatever the table displays, and each name is matched
#' **literally** — `"log_pi_5y_count"` does *not* pick up
#' `log_pi_5y_count_Q1`, `log_pi_5y_count_Q2`, ...; list every variable. When
#' the table was built with `coef_rename = TRUE` the displayed terms are
#' variable *labels* (`"Gender [male]"`, `"log(1+PubsPiQ1)"`), so pass the
#' label lookup through `data` — either the labelled data frame the models
#' were fit on or a named character vector (`name -> label`) — and the names
#' are resolved to the text actually shown. A name's candidate texts are its
#' label plus the bare name and the spelling `modelsummary` gives unlabelled
#' terms, with underscores turned into spaces (`"start_year"` also matches
#' `"start year [2008]"`), which covers variables whose label was dropped en
#' route (`forcats::fct_drop()` does that). A term row belongs to a variable
#' when its displayed text **equals** one of these candidates or is a
#' candidate followed by a bracketed factor level (`"Gender"` matches
#' `"Gender [male]"` but not `"Gendered"`). Raw-term tables that concatenate
#' the level directly (`coef_rename = FALSE`: `"factor(cyl)6"`) are not
#' expanded — pass the displayed term texts themselves.
#'
#' Every element of `vars` must match at least one row: a name that resolves
#' to no displayed term is an **error**, not a silent no-op, so a misspelled
#' variable, or one absent from the models' right-hand side, cannot leave its
#' rows uncollapsed unnoticed.
#'
#' The collapsed row is **moved to the end of the coefficient block**, just
#' above the goodness-of-fit statistics, so successive calls stack their
#' "Controls: Yes" rows into one block there, in call order. Set
#' `move_before_gof = FALSE` to keep the row at the position of the first row
#' of the group instead, where the reader expects those variables to be. Either
#' way the coefficient/GOF rule — which `modelsummary` draws as a bottom border
#' on the last coefficient row, and which would otherwise be deleted along with
#' that block — is redrawn on whatever row ends up last. Rows whose term cell is empty
#' (standard errors, confidence intervals) are removed along with the
#' coefficient row they belong to. A column that is empty across every collapsed
#' row keeps its blank cell rather than gaining `value`, so grouping columns
#' (`component`, `effect`) stay intact and a model that did not include the
#' controls is not mislabelled as having done so.
#'
#' @param tbl A `flextable`, typically from
#'   [modelsummary::modelsummary()] with `output = "flextable"`.
#' @param vars Character vector of variable names to collapse, each matched
#'   literally (see Details); an element matching no row is an error.
#' @param new_label Text placed in the term column of the collapsed row.
#' @param new_value Text placed in every model column of the collapsed row
#'   (default `"Yes"`).
#' @param data Optional lookup used to resolve `vars` to the displayed terms:
#'   the labelled data frame the models were fit on (labels read via
#'   [labelled::var_label()]), or a named `name -> label` character vector.
#' @param term_col Column key holding the term text. By default it is detected
#'   automatically, which is what you want for `modelsummary` `shape =`
#'   layouts where the first column is `component` and the terms sit in the
#'   second, unnamed one.
#' @param move_before_gof Move the collapsed row to the end of the coefficient
#'   block, just above the goodness-of-fit statistics (default `TRUE`);
#'   successive calls stack their rows there in call order. With `FALSE` the
#'   row takes the position of the first row of the group.
#' @return The modified `flextable`.
#' @seealso [flextable2docx()], which writes the resulting table to Word.
#' @examples
#' \dontrun{
#' tbl %>% flextable_collapse_group(
#'   vars      = c("gender", "ethnicity_d", "piStage", "log_pi_5y_count"),
#'   new_label = "PI-level controls",
#'   new_value = "YES",
#'   data      = pis
#' )
#' }
#' @export
flextable_collapse_group <- function(tbl, vars, new_label, new_value = "Yes",
                                     data = NULL, term_col = NULL,
                                     move_before_gof = TRUE) {
  stopifnot(
    inherits(tbl, "flextable"),
    is.character(vars), length(vars) > 0,
    is.character(new_label), length(new_label) == 1
  )

  # name -> displayed label lookup
  labels <- data
  if (is.data.frame(labels)) labels <- labelled::var_label(labels)
  labels <- unlist(labels[!vapply(labels, is.null, logical(1))])

  # Each variable name resolves LITERALLY (no prefix expansion) to its
  # candidate display texts: its label (when `data` carries one), the bare
  # name, and the spelling `modelsummary` gives unlabelled terms, with
  # underscores turned into spaces ("start_year" -> "start year [2008]") --
  # which also covers variables whose label was dropped along the way
  # (`forcats::fct_drop()` discards it, for instance).
  texts_of <- function(v) {
    lab <- if (length(labels)) unname(labels[names(labels) == v]) else character(0)
    unique(c(lab, v, gsub("_", " ", v)))
  }
  # a term row belongs to `v` when its text IS one of v's candidates, or is a
  # candidate followed by a bracketed factor level ("Gender" matches
  # "Gender [male]" but not "Gendered")
  matches_var <- function(x, v) {
    Reduce(`|`, lapply(texts_of(v), function(p) {
      x == p | startsWith(x, paste0(p, " ["))
    }), init = logical(length(x)))
  }
  matches_any <- function(x) {
    Reduce(`|`, lapply(vars, function(v) matches_var(x, v)),
           init = logical(length(x)))
  }

  # locate the column holding the term text
  col_keys <- tbl$col_keys
  if (is.null(term_col)) {
    hits <- vapply(col_keys, function(ck) {
      sum(matches_any(trimws(as.character(tbl$body$dataset[[ck]]))))
    }, integer(1))
    if (max(hits) == 0) {
      stop("No rows matching ", paste(sQuote(vars), collapse = ", "),
           " were found in any column; every element of `vars` must match ",
           "at least one term row.")
    }
    term_col <- col_keys[which.max(hits)]
  }

  terms <- trimws(as.character(tbl$body$dataset[[term_col]]))
  blank <- is.na(terms) | terms == ""
  starts_of <- lapply(vars, function(v) which(!blank & matches_var(terms, v)))
  unmatched <- vars[lengths(starts_of) == 0]
  if (length(unmatched) > 0) {
    stop("No rows found for ", paste(sQuote(unmatched), collapse = ", "),
         " in column ", sQuote(term_col), "; every element of `vars` must ",
         "match at least one term row.")
  }
  starts <- sort(unique(unlist(starts_of)))

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

  # at = NULL lets flextable_insert_row default to the start of the GOF block,
  # i.e. the end of the coefficients
  flextable_insert_row(tbl, term_col, new_label, new_value,
                       at = if (move_before_gof) NULL else rows[1],
                       drop = rows, blank_cols = blank_cols)
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

#' Drop rows from a regression table by their term text
#'
#' Removes the rows whose term cell equals one of `terms` — exact matches on
#' the trimmed displayed text, e.g. `"SD (Observations)"` or the `"Intercept"`
#' label of a row collapsed by [flextable_collapse_group()] — together with
#' the empty-term continuation rows (standard errors) trailing each of them.
#'
#' As in [flextable_drop_component()], the coefficient/GOF separator rule is
#' preserved: if a dropped row carried it (a block at the bottom of the
#' coefficients), the rule is redrawn on whatever row ends up last above the
#' GOF block.
#'
#' @param tbl A `flextable`, typically from [modelsummary::modelsummary()]
#'   with `output = "flextable"`.
#' @param terms Character vector of displayed term texts to remove (exact,
#'   trimmed matches). Every element must match at least one row: a text
#'   found nowhere is an **error**, not a silent no-op, so a stale or
#'   misspelled entry cannot leave its rows standing unnoticed.
#' @param term_col Column key holding the term text. Detected by default as
#'   the column carrying `"(Intercept)"`, falling back to the first column.
#' @return The modified `flextable`.
#' @seealso [flextable_drop_component()], [flextable_collapse_group()]
#' @examples
#' \dontrun{
#' tbl %>% flextable_drop_term_rows(c("Intercept", "SD (Observations)"))
#' }
#' @export
flextable_drop_term_rows <- function(tbl, terms, term_col = NULL) {
  stopifnot(
    inherits(tbl, "flextable"),
    is.character(terms), length(terms) > 0
  )
  if (is.null(term_col)) term_col <- flextable_term_col(tbl)
  ds_terms <- trimws(as.character(tbl$body$dataset[[term_col]]))
  starts_of <- lapply(terms, function(tm) which(ds_terms == tm))
  unmatched <- terms[lengths(starts_of) == 0]
  if (length(unmatched) > 0) {
    stop("No rows found for ", paste(sQuote(unmatched), collapse = ", "),
         " in column ", sQuote(term_col), "; every element of `terms` must ",
         "match at least one row.")
  }
  starts <- sort(unique(unlist(starts_of)))
  blank <- is.na(ds_terms) | ds_terms == ""

  # a block is a matched row plus the blank-term rows trailing it
  rows <- integer(0)
  for (s in starts) {
    e <- s
    while (e < length(ds_terms) && blank[e + 1]) e <- e + 1
    rows <- c(rows, s:e)
  }
  rows <- sort(unique(rows))

  # note the separator rule and the GOF start while both are still in place,
  # then redraw the rule on the last remaining coefficient row (see
  # flextable_drop_component for the rationale)
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

#' Drop columns from a table and restore the footer span
#'
#' [flextable::delete_columns()] resets the footer's merge spans, and flextable
#' keeps a merged cell's text in every cell of the group — so after deleting a
#' column, a footer note that spanned the table (the significance legend, a
#' footnote) is suddenly repeated once per remaining column. This wraps the
#' deletion and re-merges the footer: the same span repair the sibling helpers
#' apply after their own edits. Footer only, deliberately: merging the body
#' would collapse adjacent cells that happen to share a value, and the header
#' carries no spans to restore.
#'
#' @param tbl A `flextable`.
#' @param j Column key(s) to delete, passed to [flextable::delete_columns()].
#' @return The modified `flextable`.
#' @seealso [flextable_drop_component()], [flextable_collapse_group()],
#'   [flextable2docx()]
#' @examples
#' \dontrun{
#' tbl %>% flextable_drop_columns(j = c("component", "effect"))
#' }
#' @export
flextable_drop_columns <- function(tbl, j) {
  stopifnot(inherits(tbl, "flextable"))
  tbl <- flextable::delete_columns(tbl, j = j)
  flextable::merge_h(tbl, part = "footer")
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
#' alignment, padding and page-filling column widths. Cells are anchored
#' "Align Top Left" (Word's naming) in the first column and "Align Top
#' Center" in every other column, across header, body and footer.
#'
#' @param tbl A `flextable` object.
#' @param outfp Output file path for the `.docx` file.
#' @param font_name,font_size Font family and size (pt) applied to the whole
#'   table.
#' @param table_alignment Placement of the whole table on the page, one of
#'   `"left"`, `"center"`, `"right"` (not the cell text alignment, which is
#'   fixed as described above).
#' @param padding.offset.top,padding.offset.bottom,padding.offset.left,padding.offset.right
#'   Per-side cell padding OFFSETS in pt, applied to every part (header,
#'   body, footer). These are never absolute values: each cell's current
#'   padding is shifted by the offset and floored at 0, so per-cell
#'   differences — gtsummary's category-level indentation is a 15pt label
#'   cell against flextable's 5pt default — are preserved by construction.
#'   `0` (or `NULL`) leaves a side untouched; e.g. `padding.offset.top = -2`
#'   compacts the default 5pt rows to 3pt. The defaults
#'   `padding.offset.left = -5` and `padding.offset.right = -5` cancel
#'   flextable's 5pt horizontal default (an indented 15pt cell keeps its 10pt
#'   indent): flextable writes horizontal padding into Word as a paragraph
#'   indent on every cell paragraph (the real cell margins are zeroed), which
#'   reads as an invisible "space" before each entry — backspace-deletable
#'   but never shown by formatting marks.
#' @param layout Table layout, one of:
#'   * `"fit_first_column"` (default) — the table fills the usable page width
#'     (computed from `word_prop`'s paper format, orientation and margins)
#'     with a fixed layout: the first column is sized to its body content —
#'     capped at 40% of the usable width — and the remaining width is split
#'     equally among the other columns. This stops Word's autofit from
#'     squeezing the term column of wide regression tables into multi-line
#'     wraps. A one-column table falls back to `"autofit"`.
#'   * `"autofit"` — Word recomputes the column widths from content.
#'   * `"fixed"` — fixed layout with the widths the `flextable` already
#'     carries.
#' @param footnotes Where the table's footer (the notes `gtsummary` and
#'   `modelsummary` add: the statistics legend, the test names, the
#'   significance stars) is written, one of:
#'   * `"table"` (default) — left in the table, as footer rows spanning it.
#'   * `"below"` — taken out of the table and written after it as plain
#'     paragraphs, one per footer row, in the table's font and with no space
#'     before or after (single line spacing), so they read as a compact block
#'     of notes; superscript markers, bold and italic runs are kept. The table
#'     itself then ends with its last body row.
#' @param blank_line_after_caption Whether to leave one empty line between the
#'   caption and the table (default `TRUE`; nothing is added when there is no
#'   caption). The empty line is a paragraph in the table's font with no
#'   paragraph spacing, so it is exactly one line high.
#' @param blank_line_before_notes Whether to leave one empty line between the
#'   table and its notes when these are written below the table
#'   (`footnotes = "below"`; default `TRUE`). Same empty line as above.
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
  table_alignment = "left",
  padding.offset.top = NULL,
  padding.offset.bottom = NULL,
  padding.offset.left = -5,
  padding.offset.right = -5,
  layout = c("fit_first_column", "autofit", "fixed"),
  footnotes = c("table", "below"),
  blank_line_after_caption = TRUE,
  blank_line_before_notes = TRUE,
  word_prop = list()
) {
  layout <- match.arg(layout)
  footnotes <- match.arg(footnotes)

  # Initialize Word document
  outs <- do.call(prepare_docx, word_prop)

  # One empty line in the table's font, with no paragraph spacing: the gap
  # between the caption and the table, and between the table and its notes
  blank_line <- officer::fpar(
    officer::ftext(" ", officer::fp_text(font.family = font_name, font.size = font_size)),
    fp_p = officer::fp_par(padding = 0, line_spacing = 1)
  )
  has_caption <- !is.null(word_prop[["caption_text"]]) && nzchar(word_prop[["caption_text"]])
  if (isTRUE(blank_line_after_caption) && has_caption) {
    outs[["docx"]] <- officer::body_add_fpar(outs[["docx"]], blank_line)
  }

  # Footer rows taken out of the table, to be written after it as paragraphs.
  # Each footer row is one merged cell whose content is a data frame of runs
  # (the superscript marker, then the note); collected before the footer is
  # deleted, so the widths below are computed on the table that is written.
  notes <- list()
  if (identical(footnotes, "below") && flextable::nrow_part(tbl, "footer") > 0) {
    notes <- flextable_footer_runs(tbl)
    tbl <- flextable::delete_part(tbl, part = "footer")
  }

  # Font and padding first: the column widths computed below depend on both
  tbl %<>%
    flextable::font(fontname = font_name, part = "all") %>%
    flextable::fontsize(size = font_size, part = "all")

  # Cell alignment, in Word's naming: "Align Top Left" for the first (term)
  # column, "Align Top Center" for the others. Top-anchoring keeps rows
  # reading level when cells differ in line count (multi-line terms,
  # estimate + SE cells). Footer notes are unaffected: they are merged rows
  # anchored on the left-aligned first column.
  tbl %<>%
    flextable::valign(valign = "top", part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all")
  if (length(tbl$col_keys) > 1) {
    tbl %<>% flextable::align(
      j = seq(2, length(tbl$col_keys)),
      align = "center",
      part = "all"
    )
  }

  # list names are flextable::padding()'s argument names; values are the
  # offsets from this function's padding.offset.* arguments
  pads <- list(
    padding.top = padding.offset.top, padding.bottom = padding.offset.bottom,
    padding.left = padding.offset.left, padding.right = padding.offset.right
  )
  pads <- pads[!vapply(pads, is.null, logical(1))]
  pads <- pads[vapply(pads, function(p) p != 0, logical(1))]
  if (length(pads) > 0) {
    # Padding arguments are SIGNED OFFSETS on each cell's current padding
    # (floored at 0), never absolute values: shifting every cell by the same
    # amount preserves per-cell DIFFERENCES by construction — gtsummary
    # indents category levels by enlarging the label cell's left padding
    # (15pt vs flextable's 5pt default), and that 10pt gap must survive any
    # compaction. The default padding.left/right = -5 removes flextable's
    # horizontal default entirely (5 -> 0) while a 15pt indent becomes 10pt.
    # Cells are updated in groups of equal current value (per column and
    # part), so the handful of distinct paddings costs a handful of calls.
    for (side in names(pads)) {
      off <- pads[[side]]
      for (part_nm in c("header", "body", "footer")) {
        pm <- tbl[[part_nm]]$styles$pars[[side]]$data
        if (is.null(pm) || !is.matrix(pm) || length(pm) == 0) next
        for (j in seq_len(ncol(pm))) {
          for (v in unique(pm[!is.na(pm[, j]), j])) {
            nv <- max(0, v + off)
            if (nv == v) next
            args <- list(tbl, i = which(pm[, j] == v), j = j, part = part_nm)
            args[[side]] <- nv
            tbl <- do.call(flextable::padding, args)
          }
        }
      }
    }
  }

  # Size the first column to its content (capped) and split the remaining
  # usable page width equally among the other columns. Needs the fixed
  # layout: under autofit Word recomputes the widths itself and squeezes the
  # term column.
  column_width <- NULL
  if (identical(layout, "fit_first_column")) {
    if (length(tbl$col_keys) > 1) {
      wp <- function(nm, def) if (is.null(word_prop[[nm]])) def else word_prop[[nm]]
      paper_w_mm <- if (isTRUE(word_prop$page_landscape)) {
        outs[["paper_height"]]
      } else {
        outs[["paper_width"]]
      }
      usable <- paper_w_mm / 25.4 -
        wp("page_margin_left", 1) - wp("page_margin_right", 1)
      # Word lays text out a little wider than gdtools measures it: a term
      # measured at exactly the column width wraps its last character onto a
      # second line ("log(KohesioUnitCost" / ")"), hence the 5% + 0.05 in slack
      w1 <- 1.05 * flextable::dim_pretty(tbl, part = "body")$widths[1] + 0.05
      w1 <- min(w1, 0.4 * usable)
      k <- length(tbl$col_keys) - 1L
      column_width <- c(w1, rep((usable - w1) / k, k))
      layout <- "fixed"
    } else {
      # a one-column table has nothing to distribute
      layout <- "autofit"
    }
  }

  tbl %<>% flextable::set_table_properties(layout = layout, align = table_alignment)

  if (!is.null(column_width)) {
    tbl %<>% flextable::width(width = column_width)
  }

  # Add flextable to docx
  outs[["docx"]] <- outs[["docx"]] %>%
    flextable::body_add_flextable(value = tbl, split = TRUE, keepnext = FALSE)

  # The notes, one paragraph each, in the table's font, with no paragraph
  # spacing and single line spacing so they form a compact block, after one
  # empty line
  if (isTRUE(blank_line_before_notes) && length(notes) > 0) {
    outs[["docx"]] <- officer::body_add_fpar(outs[["docx"]], blank_line)
  }
  for (runs in notes) {
    chunks <- lapply(seq_len(nrow(runs)), function(k) {
      officer::ftext(
        runs$txt[k],
        officer::fp_text(
          font.family = font_name,
          font.size = font_size,
          bold = isTRUE(runs$bold[k]),
          italic = isTRUE(runs$italic[k]),
          vertical.align = if (identical(runs$vertical.align[k], "superscript")) {
            "superscript"
          } else if (identical(runs$vertical.align[k], "subscript")) {
            "subscript"
          } else {
            "baseline"
          }
        )
      )
    })
    note_par <- do.call(
      officer::fpar,
      c(chunks, list(fp_p = officer::fp_par(text.align = "left", padding = 0, line_spacing = 1)))
    )
    outs[["docx"]] <- officer::body_add_fpar(outs[["docx"]], note_par)
  }

  # Finalize and return
  return(finalize_docx(outs, outfp))
}

#' The runs of each footer row of a flextable
#'
#' Internal helper of [flextable2docx()]: for every row of the footer part,
#' the formatted runs of its cells (a footer note is normally one cell merged
#' across the table, so the first column carries it) as one data frame with
#' the columns `txt`, `bold`, `italic` and `vertical.align`, empty runs
#' dropped. Rows whose every run is empty are skipped.
#'
#' @param tbl A `flextable` with a footer part.
#' @return A list of data frames, one per non-empty footer row, in order.
#' @keywords internal
flextable_footer_runs <- function(tbl) {
  content <- tbl$footer$content$data
  cols <- c("txt", "bold", "italic", "vertical.align")
  out <- list()
  for (i in seq_len(nrow(content))) {
    runs <- do.call(rbind, lapply(seq_len(ncol(content)), function(j) {
      cell <- content[i, j][[1]]
      if (is.null(cell) || nrow(cell) == 0) return(NULL)
      for (nm in setdiff(cols, names(cell))) cell[[nm]] <- NA
      cell[, cols, drop = FALSE]
    }))
    if (is.null(runs)) next
    runs <- runs[!is.na(runs$txt) & nzchar(runs$txt), , drop = FALSE]
    if (nrow(runs) > 0) out[[length(out) + 1]] <- runs
  }
  out
}

#' How many equal-width columns fit on a page next to the first column
#'
#' Companion of the default `fit_first_column` layout of [flextable2docx()],
#' which sizes the first (term) column to its content and splits the remaining
#' usable page width equally among the other columns. This helper measures the
#' natural width of every column of `tbl` at the export font
#' ([flextable::dim_pretty()], with the horizontal padding removed as
#' [flextable2docx()] does by default) and returns the largest number of
#' non-first columns whose widest member still fits its equal share of the
#' page, i.e. how many model columns a regression table can carry per page
#' without wrapping its cells. Use it to split a wide `modelsummary` table
#' into as few pages (files) as possible: build one table holding every model,
#' ask how many fit, and export the models in batches of that size.
#'
#' @inheritParams flextable2docx
#' @param tbl A `flextable` holding every candidate column.
#' @return An integer, at least 1.
#' @examples
#' \dontrun{
#' tbl_all <- modelsummary::modelsummary(mods, output = "flextable")
#' per_page <- flextable_columns_per_page(tbl_all, font_size = 10,
#'                                        word_prop = list(page_landscape = TRUE))
#' batches <- split(seq_along(mods), ceiling(seq_along(mods) / per_page))
#' }
#' @export
flextable_columns_per_page <- function(
  tbl,
  font_name = "Aptos",
  font_size = 12,
  word_prop = list()
) {
  stopifnot(inherits(tbl, "flextable"), length(tbl$col_keys) > 1)
  tbl %<>%
    flextable::font(fontname = font_name, part = "all") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::padding(padding.left = 0, padding.right = 0, part = "all")

  wp <- function(nm, def) if (is.null(word_prop[[nm]])) def else word_prop[[nm]]
  paper <- paper_sizes[[wp("paper_format", "A4")]]
  paper_w_mm <- if (isTRUE(word_prop$page_landscape)) paper[2] else paper[1]
  usable <- paper_w_mm / 25.4 -
    wp("page_margin_left", 1) - wp("page_margin_right", 1)

  widths <- flextable::dim_pretty(tbl, part = "all")$widths
  w1 <- min(flextable::dim_pretty(tbl, part = "body")$widths[1], 0.4 * usable)
  max(1L, as.integer(floor((usable - w1) / max(widths[-1]))))
}
