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

#' Search keywords in text and produce
#'
#' @param string String to search in
#' @param pattern A regex pattern
#' @param tag_open HTML tag to put before a keyword
#' @param tag_close HTML tag to put after a keyword
#'
#' @return A list with the following elements:
#' tab: Table contain start and end position of each match
#' html: A HTML for visual inspection
#'
#' @export
keyword_search <- function(string,
                           pattern,
                           tag_open = paste0(
                             "<span style='background-color:yellow; ",
                             "font-weight: bold'>"
                           ),
                           tag_close = "</span>") {
  stopifnot("stringr_regex" %in% class(pattern))
  outl <- list()
  # Locate matches
  res <- string %>% stringr::str_locate_all(pattern)
  for (i in seq(1, length(res))) {
    outl[[i]] <- list()
    if (nrow(res[[i]]) == 0 || is.na(res[[i]][1, "start"])) {
      outl[[i]]$html <- string[[i]]
      outl[[i]]$tab <- NA
      outl[[i]]$nmatches <- 0
      next
    }
    # Extract located matches
    outl[[i]]$tab <- as.data.frame(res[[i]])
    outl[[i]]$tab$matches <- apply(
      X = outl[[i]]$tab,
      MARGIN = 1,
      FUN = \(x) stringr::str_sub(string[[i]], x[1], x[2])
    )
    # Build nmatches
    outl[[i]]$nmatches <- nrow(outl[[i]]$tab)
    # Build HTML
    start <- 1
    end <- outl[[i]]$tab[1, "start"] - 1
    outl[[i]]$html <- stringr::str_sub(string[[i]], start, end)
    for (rown in seq(1, nrow(outl[[i]]$tab))) {
      start <- outl[[i]]$tab[rown, "start"]
      end <- outl[[i]]$tab[rown, "end"]
      start2 <- end + 1
      if (rown < nrow(outl[[i]]$tab)) {
        end2 <- outl[[i]]$tab[rown + 1, "start"] - 1
      } else {
        end2 <- nchar(string[[i]])
      }
      outl[[i]]$html <- paste0(
        outl[[i]]$html,
        tag_open,
        stringr::str_sub(string[[i]], start, end),
        tag_close,
        stringr::str_sub(string[[i]], start2, end2)
      )
    }
  }
  return(outl)
}
