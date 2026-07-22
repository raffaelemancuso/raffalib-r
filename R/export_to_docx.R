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
        "Could not write '%s' (%s). Wrote '%s' instead — close the open file and re-run to overwrite.",
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

  if(class(gg)=="tmap") {
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

#' Collapse a categorical variable's rows into a single "controlled for" row
#'
#' Regression tables (e.g. from [modelsummary::modelsummary()] with
#' `output = "flextable"`) devote one block of rows (estimate, standard error,
#' ...) to every level of a categorical variable. This helper replaces all the
#' blocks belonging to a categorical variable with a single row holding the
#' variable's label in the term column and `value` (default `"Yes"`) in every
#' other column — the usual way of indicating that a set of controls or fixed
#' effects was included. The collapsed row takes the position of the variable's
#' first block; rows whose term cell is empty (e.g. standard-error rows) are
#' dropped together with the coefficient row they follow.
#'
#' @param tbl A `flextable` whose first column holds the term names.
#' @param vars Character vector of variable-name prefixes: every body row whose
#'   term starts with one of these (literal match, no regex), plus its
#'   empty-term continuation rows, is collapsed. If an element is named, the
#'   name is used as the displayed label of the collapsed row; otherwise the
#'   prefix itself is displayed.
#' @param value Text put in every non-term column of the collapsed row
#'   (default `"Yes"`).
#' @return The modified `flextable`.
#' @seealso [flextable2docx()], which exposes this via its
#'   `collapse_categorical` argument.
#' @examples
#' \dontrun{
#' mod <- lm(mpg ~ wt + factor(cyl), data = mtcars)
#' tbl <- modelsummary::modelsummary(mod, output = "flextable")
#' flextable_collapse_categorical(tbl, c("Cylinders FE" = "factor(cyl)"))
#' }
#' @export
flextable_collapse_categorical <- function(tbl, vars, value = "Yes") {
  stopifnot(inherits(tbl, "flextable"))
  labels <- names(vars)
  if (is.null(labels)) labels <- unname(vars)
  labels[labels == ""] <- vars[labels == ""]
  for (k in seq_along(vars)) {
    tbl <- flextable_collapse_one(tbl, vars[[k]], labels[[k]], value)
  }
  return(tbl)
}

# Collapse the rows of one categorical variable in a flextable body.
# A "block" is a row whose term matches `var` plus the empty-term rows that
# follow it (standard errors, confidence intervals, ...). The first block's
# first row is rewritten to `label` / `value`; every other matched row is
# deleted. Both the displayed content (via compose) and the underlying
# dataset are updated, so later operations on the flextable stay consistent.
flextable_collapse_one <- function(tbl, var, label, value) {
  col_keys <- tbl$col_keys
  terms <- trimws(as.character(tbl$body$dataset[[col_keys[1]]]))
  blank <- is.na(terms) | terms == ""
  starts <- which(!blank & startsWith(terms, var))
  if (length(starts) == 0) {
    warning("No rows matching categorical variable '", var, "' were found.")
    return(tbl)
  }
  rows <- integer(0)
  for (s in starts) {
    e <- s
    while (e < length(terms) && blank[e + 1]) e <- e + 1
    rows <- c(rows, s:e)
  }
  first <- rows[1]
  tbl <- flextable::compose(tbl, i = first, j = col_keys[1],
                            value = flextable::as_paragraph(label),
                            part = "body")
  tbl$body$dataset[[col_keys[1]]][first] <- label
  for (ck in col_keys[-1]) {
    tbl <- flextable::compose(tbl, i = first, j = ck,
                              value = flextable::as_paragraph(value),
                              part = "body")
    tbl$body$dataset[[ck]][first] <- value
  }
  drop <- setdiff(rows, first)
  if (length(drop) > 0) {
    tbl <- flextable::delete_rows(tbl, i = drop, part = "body")
  }
  return(tbl)
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
#' @param collapse_categorical Optional character vector of variable-name
#'   prefixes forwarded to [flextable_collapse_categorical()]: the coefficient
#'   rows of those categorical variables are replaced by a single row with
#'   `"Yes"` in every column before export. Name the elements to control the
#'   displayed labels.
#' @param word_prop A named list of page/caption options forwarded to
#'   [prepare_docx()] (caption text, paper format, margins, ...).
#' @return Called for its side effect of writing `outfp`; returns the result of
#'   [prepare_docx()]'s finaliser invisibly.
#' @seealso [plot2docx()], [ggplot2docx()], [flextable_collapse_categorical()]
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
  collapse_categorical = NULL,
  word_prop = list()
) {
  # Initialize Word document
  outs <- do.call(prepare_docx, word_prop)

  # Collapse categorical-control rows into single "Yes" rows
  if (!is.null(collapse_categorical)) {
    tbl <- flextable_collapse_categorical(tbl, collapse_categorical)
  }

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
