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
#' Before writing, the candidate backup is serialised to a temporary file and
#' its SHA-256 checksum is compared with the most recent existing backup with
#' the same `file_stem`: when they match, the object is unchanged and the save
#' is refused with a warning, so re-running a script section does not litter
#' `out_dir` with identical copies. (`.rds` serialisation is byte-deterministic
#' for an identical object within an R version, so equal checksums mean equal
#' backups; when serialisation does change — e.g. after an R upgrade — the
#' checksums differ and a new backup is written, so the check can only err on
#' the side of saving.)
#'
#' @param obj The object to serialise.
#' @param out_dir Destination directory.
#' @param file_stem File-name stem; the time stamp and `.rds` extension are
#'   appended automatically.
#' @return Invisibly, the path of the written backup file — or, when the save
#'   is refused because the newest same-stem backup is identical, the path of
#'   that existing backup.
#' @seealso [read_backup()]
#' @export
save_backup <- function(obj, out_dir, file_stem) {
  stopifnot(dir.exists(out_dir))

  # serialise first, so the candidate can be hashed before touching out_dir
  tmp_fp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_fp), add = TRUE)
  saveRDS(obj, tmp_fp)

  # most recent existing backup with the same stem (read_backup's pattern,
  # anchored so one stem cannot match inside another, e.g. "pis" in "ai_pis")
  latest_fn <- out_dir %>% list.files() %>%
    str_subset(glue(
      "^{file_stem}_\\d{{4}}-\\d{{2}}-\\d{{2}}_\\d{{2}}-\\d{{2}}-\\d{{2}}\\.rds$"
    )) %>%
    str_sort(numeric = TRUE, decreasing = TRUE) %>%
    head(1)
  if (length(latest_fn) == 1) {
    latest_fp <- file.path(out_dir, latest_fn)
    if (identical(
      cli::hash_file_sha256(tmp_fp),
      cli::hash_file_sha256(latest_fp)
    )) {
      warning(glue(
        "Refusing to save backup \"{file_stem}\": the object is identical ",
        "(same SHA-256) to the most recent backup \"{latest_fp}\"."
      ))
      return(invisible(latest_fp))
    }
  }

  timestamp <- strftime(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  fn <- paste0(file_stem, "_", timestamp, ".rds")
  fp <- file.path(out_dir, fn)
  print(paste0("Saving to ", fp))
  file.copy(tmp_fp, fp, overwrite = TRUE)
  return(invisible(fp))
}

