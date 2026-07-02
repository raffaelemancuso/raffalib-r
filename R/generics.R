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

