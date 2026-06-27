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

#' Find the S3 method a generic would dispatch
#'
#' Returns the name of the method that would be dispatched by `generic` for the
#' supplied arguments, without actually calling it. Handy for debugging S3
#' dispatch. Adapted from <https://stackoverflow.com/a/42742370/1719931>.
#'
#' @param generic A generic function, passed unquoted (e.g. `print`).
#' @param ... Arguments that would be passed to `generic`; dispatch is resolved
#'   on the first one.
#' @return A length-one character vector with the name of the dispatched method.
#' @importFrom utils methods
#' @export
#' @examples
#' find_method(print, iris)
#' find_method(print, Sys.time())
#' find_method(print, ordered(3))
find_method <- function(generic, ...) {
  ch <- deparse(substitute(generic))
  f <- X <- function(x, ...) UseMethod("X")
  for(m in methods(ch)) assign(sub(ch, "X", m, fixed = TRUE), "body<-"(f, value = m))
  X(...)
}

#' Read the most recent time-stamped backup of an object
#'
#' Companion to [save_backup()]. Searches `dirpath` for files named
#' `"<filestem>_YYYY-MM-DD_HH-MM-SS.rds"` and reads back the most recent one
#' (newest time stamp by natural sort).
#'
#' @param dirpath Directory to search for backups.
#' @param filestem The file stem used when the backup was written.
#' @return The object stored in the most recent matching `.rds` file.
#' @seealso [save_backup()]
#' @importFrom stringr str_subset str_sort
#' @importFrom glue glue
#' @export
read_backup <- function(dirpath, filestem) {
  infp <- dirpath %>% list.files() %>%
    str_subset(glue(
      "{filestem}_\\d{{4}}-\\d{{2}}-\\d{{2}}_\\d{{2}}-\\d{{2}}-\\d{{2}}\\.rds"
    )) %>%
    str_sort(numeric = TRUE, decreasing = TRUE) %>%
    head(1)
  if(length(infp)==0) {
    stop(glue("No file found in {infp}"))
  }
  infp <- file.path(dirpath, infp)
  cat(glue("Reading \"{infp}\""))
  return(readRDS(infp))
}


#' Save a time-stamped backup of an object
#'
#' Writes `obj` to `"<file_stem>_YYYY-MM-DD_HH-MM-SS.rds"` inside `out_dir`, so
#' successive calls never overwrite one another. Read the latest one back with
#' [read_backup()].
#'
#' @param obj The object to serialise.
#' @param out_dir Destination directory.
#' @param file_stem File-name stem; the time stamp and `.rds` extension are
#'   appended automatically.
#' @return Invisibly, the return value of [saveRDS()]; called for its side
#'   effect of writing the file.
#' @seealso [read_backup()]
#' @export
save_backup <- function(obj, out_dir, file_stem) {
  timestamp <- strftime(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  fn <- paste0(file_stem, "_", timestamp, ".rds")
  fp <- file.path(out_dir, fn)
  print(paste0("Saving to ", fp))
  saveRDS(obj, fp)
}

#' Split a range of indices into consecutive batches
#'
#' Creates a list of batches covering `1:total_size`, each batch a list with the
#' batch number, start index and end index. Useful for processing data in chunks
#' (e.g. datasets too large to handle at once). For `total_size = 10` and
#' `batch_size = 3` the batches span 1-3, 4-6, 7-9 and 10-10.
#'
#' @param total_size The total number of items to be processed.
#' @param batch_size The number of items in each batch.
#' @return A list of batches; each element is a list with `batch_number`,
#'   `batch_start` and `batch_end`.
#' @examples
#' gen_batches(10, 3)
#' gen_batches(25, 4)
#' gen_batches(100, 20)
#' gen_batches(7, 2)
#' gen_batches(15, 5)
#' @export
gen_batches <- function(total_size, batch_size) {
  batch_starts <- seq(1, total_size, by = batch_size)
  batch_ends <- pmin(batch_starts + batch_size - 1, total_size)
  batch_number <- seq_along(batch_starts)
  # zip vectors
  batches <- mapply(function(start, end, num) {
    list(batch_number = num, batch_start = start, batch_end = end)
  }, batch_starts, batch_ends, batch_number, SIMPLIFY = FALSE)
  return(batches)
}

