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
  "A4" = c(210, 297),
  "letter" = c(215.9, 279.4)
)

inch2mm <- function(s) {
  return(s*25.4)
}

compute_max_size <- function(doc_gg) {
  word_size <- officer::docx_dim(doc_gg)
  width <- word_size$page['width'] - word_size$margins['left'] - word_size$margins['right']
  height <- word_size$page['height'] - word_size$margins['top'] - word_size$margins['bottom']
  width <- inch2mm(width)
  height <- inch2mm(height)
  width <- width * 0.9
  height <- height * 0.9
  return(list(width=width, height=height))
}

#' Define Word document properties
#'
#' @export
word_prop <- function(
  caption_text = "",
  caption_font_family = "Aptos",
  caption_font_size = 12,
  caption_bold = TRUE,
  caption_italic = FALSE,
  page_landscape = FALSE,
  paper_format = "A4"
) {
  return(list(
    caption_text = caption_text,
    caption_font_family = caption_font_family,
    caption_font_size = caption_font_size,
    caption_bold = caption_bold,
    caption_italic = caption_italic,
    page_landscape = page_landscape,
    paper_format = paper_format
  ))
}


#' Prepare docx for export
prepare_docx <- function(myword_prop) {
  
  # Define section properties
  #
  # A section is a grouping of blocks (ie. paragraphs and tables)
  # that have a set of properties that define pages on which the text will appear.
  # A Section properties object stores information about page composition, such as page size, page orientation, borders and margins.
  paper_format <- myword_prop[["paper_format"]]
  paper_size <- paper_sizes[[paper_format]]
  page_size <- officer::page_size(
    width = paper_size[1],
    height = paper_size[2],
    orient = ifelse(myword_prop[["page_landscape"]], "landscape", "portrait"),
    unit = "mm"
  )
  # Unit of measurement of page margins is inches and apparently can't be changed
  page_margins <- officer::page_mar(bottom = 1, top = 1, right = 1, left = 1)
  section_prop <- officer::prop_section(
    page_size = page_size,
    page_margins = page_margins
  )

  # Define caption formatting
  #
  # fp_text: Create an fp_text object that describes text Formatting Properties.
  # See: https://stackoverflow.com/a/62044378/1719931
  caption_formatting <- officer::fp_text(
    color = "black",
    font.size = myword_prop[["caption_font_size"]],
    bold = myword_prop[["caption_bold"]],
    italic = myword_prop[["caption_italic"]],
    underlined = FALSE,
    strike = FALSE,
    font.family = myword_prop[["caption_font_family"]],
    cs.family = NULL,
    eastasia.family = NULL,
    hansi.family = NULL,
    vertical.align = "baseline",
    shading.color = "transparent"
  )

  # Create caption paragraph
  caption_fpar <- officer::fpar(officer::ftext(
    myword_prop[["caption_text"]],
    prop = caption_formatting
  ))

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

#' Save docx file
finalize_docx <- function(outs, outfp) {
  # It's necessary to define the same default section than the one you want to end the document 
  # so that Word agree to not add a page
  # See: https://stackoverflow.com/a/75451251/1719931
  outs[["docx"]] %>%
    officer::body_end_block_section(
      value = officer::block_section(outs[["section_prop"]])
    ) %>%
    officer::body_set_default_section(outs[["section_prop"]]) %>%
    print(target = outfp)
}

#' Save a tmap plot to a Word document
#'
#' @export
tmap2docx <- function(
  gg,
  outfp,
  plot_style = "Normal",
  plot_width = NULL,
  plot_height = NULL,
  plot_unit = "mm",
  myword_prop = word_prop()
) {
  outs <- prepare_docx(myword_prop)

  if (
    (is.null(plot_width) & !is.null(plot_height)) |
      (!is.null(plot_width) & is.null(plot_height))
  ) {
    stop(
      "If you specify either plot_width or plot_height, you must specify both."
    )
  }

  if (is.null(plot_width)) {
    sizes <- compute_max_size(outs[["docx"]])
    plot_width <- sizes[["width"]]
    plot_height <- sizes[["height"]]
    print(paste0("Guessed plot size: ", plot_width, " x ", plot_height, " mm"))
  }

  outs[["docx"]] <- outs[["docx"]] %>%
    body_add_tmap(
      value = gg,
      style = plot_style,
      width = plot_width,
      height = plot_height,
      unit = plot_unit
    )

  return(finalize_docx(outs, outfp))
}

#' Save a base R plot to a Word document
#'
#' @export
plot2docx <- function(
  gg,
  outfp,
  plot_style = "Normal",
  plot_width = 152.4,
  plot_height = 127,
  plot_unit = "mm",
  plot_res = 300,
  myword_prop = word_prop()
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

  if (is.null(plot_width)) {
    sizes <- compute_max_size(outs[["docx"]])
    plot_width <- sizes[["width"]]
    plot_height <- sizes[["height"]]
    print(paste0("Guessed plot size: ", plot_width, " x ", plot_height, " mm"))
  }

  # Initialize Word document
  outs <- prepare_docx(myword_prop)

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
#' @export
ggplot2docx <- function(
  gg,
  outfp,
  plot_style = "Normal",
  plot_width = NULL,
  plot_height = NULL,
  plot_unit = "mm",
  myword_prop = word_prop()
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

  # Prepare docx
  outs <- prepare_docx(myword_prop)
  
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

#' Save a flextable (typically a regression table generated by modelsummary) to a .docx file
#'
#' @export
flextable2docx <- function(
  tbl,
  outfp,
  footer = NULL,
  font_name = "Aptos",
  font_size = 12,
  align = "left",
  padding = NULL,
  column_width = NULL,
  layout_autofit = TRUE,
  myword_prop = word_prop()
) {
  # Prepare docx
  outs <- prepare_docx(myword_prop)

  # Define table layout
  layout <- ifelse(layout_autofit, "autofit", "fixed")

  # Define flextable properties
  if (!is.null(footer)) {
    tbl %<>%
      flextable::add_footer_lines(footer)
  }

  tbl %<>%
    flextable::font(fontname = font_name, part = "all") %>%
    flextable::fontsize(size = font_size, part = "all") %>%
    flextable::set_table_properties(layout = layout, align = align)
  
  if(!is.null(padding)) {
    tbl %<>% flextable::padding(
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
