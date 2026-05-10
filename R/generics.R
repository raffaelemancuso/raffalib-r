# ---------------------------------------------------------------------- #
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
# ---------------------------------------------------------------------- #

#' It will return a character string representing the name of the method that would be dispatched by the input generic given that generic and its arguments.
#' See: https://stackoverflow.com/a/42742370/1719931
#'
#'
#' @export 
#' @examples
#' findMethod(as.ts, iris)
#' findMethod(print, iris)
#' findMethod(print, Sys.time())
#' findMethod(print, 22)
#' findMethod(print, ordered(3))
#' findMethod(`[`, BOD, 1:2, "Time")
find_method <- function(generic, ...) {
  ch <- deparse(substitute(generic))
  f <- X <- function(x, ...) UseMethod("X")
  for(m in methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
  X(...)
}

#' Save time-stamped backup of an object
#'
#' @export
save_backup <- function(obj, out_dir, file_stem) {
  timestamp <- strftime(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  fn <- paste0(file_stem, "_", timestamp, ".rds")
  fp <- file.path(out_dir, fn)
  print(paste0("Saving to ", fp))
  saveRDS(obj, fp)
}

# --- WORKAROUNDS --- #

#' Print what are the character variables in a data frame
#' Used to debug issues due to https://github.com/easystats/parameters/issues/1142
#'
#' @export
print_char_vars <- function(df) {
  for (col in colnames(df)) {
    cl <- class(df[[col]])
    if (cl == "character") {
      print(col)
    }
  }
}

#' Convert character variables to factor
#' Works around https://github.com/easystats/parameters/issues/1142
#'
#' @export
char2factor <- function(df) {
  for (col in colnames(df)) {
    cl <- class(df[[col]])
    print(paste0(col, "-> ", cl))
    if (length(cl) == 1 & cl == "character") {
      print(paste0("Converting ", col, " from character to factor"))
      df[[col]] %<>% as.factor()
    }
  }
  return(df)
}
