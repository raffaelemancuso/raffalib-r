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

#' Generate batches of indices for a given total size and batch size.
#' This function creates a list of batches, where each batch is a list containing the batch number, start index, and end index.
#' For example, if total_size is 10 and batch_size is 3, the function will return a list of batches:
#' - Batch 1: start index 1, end index 3
#' - Batch 2: start index 4, end index 6
#' - Batch 3: start index 7, end index 9
#' - Batch 4: start index 10, end index 10
#' This function is useful for processing data in chunks, especially when dealing with large datasets that cannot be loaded into memory all at once.
#' @param total_size The total number of items to be processed.
#' @param batch_size The number of items to be processed in each batch.
#' @return A list of batches, where each batch is a list containing the batch number, start index, and end index.
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

