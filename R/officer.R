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

# Source - https://stackoverflow.com/a/79864017
# Posted by stefan
# Retrieved 2026-01-10, License - CC BY-SA 4.0
#' @export
body_add_tmap <- function(x,
                          value,
                          width = 6,
                          height = 5,
                          res = 300,
                          style = "Normal",
                          scale = 1,
                          pos = "after",
                          unit = "in",
                          ...) {
  stopifnot(inherits(value, "tmap"))
  args <- sys.call()
  if ("units" %in% names(args[-1])) {
    cli::cli_abort(c("Found a {.arg units} argument. Did you mean {.arg unit}?"))
  }
  unit <- officer:::check_unit(unit, c("in", "cm", "mm"))
  file <- tempfile(fileext = ".png")
  ragg::agg_png(
    filename = file, width = width, height = height,
    scaling = scale, units = unit, res = res, background = "transparent",
    ...
  )
  print(value)
  dev.off()
  on.exit(unlink(file))
  officer::body_add_img(x,
                        src = file, style = style, width = width,
                        height = height, pos = pos, unit = unit
  )
}