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
#' `"<filestem>_YYYY-MM-DD_HH-MM-SS.rds"` (any [saveRDS()] compression) or
#' `".qs2"` and reads back the most recent one (newest time stamp by natural
#' sort), choosing the reader from the extension. The stem is matched exactly
#' (anchored), so one stem cannot match inside another (e.g. `"pis"` inside
#' `"ai_pis"`).
#'
#' @param dirpath Directory to search for backups.
#' @param filestem The file stem used when the backup was written.
#' @return The object stored in the most recent matching `.rds` file.
#' @seealso [save_backup()]
#' @importFrom stringr str_subset str_sort
#' @importFrom glue glue
#' @export
read_backup <- function(dirpath, filestem) {
  infp <- .list_backups(dirpath, filestem) %>% head(1)
  if(length(infp)==0) {
    stop(glue("No \"{filestem}\" backup found in \"{dirpath}\""))
  }
  infp <- file.path(dirpath, infp)
  cat(glue("Reading \"{infp}\""))
  if (endsWith(infp, ".qs2")) {
    if (!requireNamespace("qs2", quietly = TRUE)) {
      stop(glue(
        "Backup \"{infp}\" was written with format = \"qs2\": ",
        "install the qs2 package to read it."
      ))
    }
    return(qs2::qs_read(infp))
  }
  return(readRDS(infp))
}


# File names (not paths) of the existing backups of `filestem` in `dirpath`,
# newest first, across all backup formats (.rds and .qs2). The stem is
# matched exactly (anchored), so one stem cannot match inside another
# (e.g. "pis" inside "ai_pis").
.list_backups <- function(dirpath, filestem) {
  dirpath %>% list.files() %>%
    str_subset(glue(
      "^{filestem}_\\d{{4}}-\\d{{2}}-\\d{{2}}_\\d{{2}}-\\d{{2}}-\\d{{2}}\\.(rds|qs2)$"
    )) %>%
    str_sort(numeric = TRUE, decreasing = TRUE)
}


#' Save a time-stamped backup of an object
#'
#' Writes `obj` to `"<file_stem>_YYYY-MM-DD_HH-MM-SS.rds"` inside `out_dir`, so
#' successive calls never overwrite one another. Read the latest one back with
#' [read_backup()].
#'
#' With `refuse_identical = TRUE` (the default), the candidate backup is
#' serialised to a temporary file before writing and its SHA-256 checksum is
#' compared with the most recent existing backup with the same `file_stem`:
#' when they match, the object is unchanged and the save is refused with a
#' warning, so re-running a script section does not litter `out_dir` with
#' identical copies. (`.rds` serialisation is byte-deterministic for an
#' identical object within an R version, so equal checksums mean equal
#' backups; when serialisation does change — e.g. after an R upgrade — the
#' checksums differ and a new backup is written, so the check can only err on
#' the side of saving.) With `refuse_identical = FALSE` the check — and the
#' temporary serialisation — are skipped entirely and a new backup is always
#' written.
#'
#' After a successful save, the backups of `file_stem` are rotated: when more
#' than `max_backups` exist, the oldest ones are deleted until `max_backups`
#' remain. A refused save never rotates. Other stems in `out_dir` are never
#' touched.
#'
#' @param obj The object to serialise.
#' @param out_dir Destination directory.
#' @param file_stem File-name stem; the time stamp and `.rds` extension are
#'   appended automatically.
#' @param refuse_identical Whether to compare the candidate's SHA-256 with the
#'   newest same-stem backup and refuse the save on a match. Default `TRUE`.
#' @param max_backups Maximum number of backups kept for `file_stem`: after a
#'   successful save, the oldest backups beyond this count are deleted.
#'   Default `5`; `NULL` deactivates the rotation and keeps every backup.
#' @param format Serialisation format. One of:
#'   * `"qs2"` (default) — [qs2::qs_save()], multithreaded: by far the
#'     fastest and the smallest, but the backup can only be read where the
#'     `qs2` package is available. Files get a `.qs2` extension instead of
#'     `.rds`.
#'   * `"rds_zstd"` — [saveRDS()] with Zstandard compression (needs
#'     R >= 4.5): gzip-sized files at roughly half the write time, readable
#'     with plain [readRDS()].
#'   * `"rds_gzip"` — [saveRDS()] with its default gzip compression.
#'   * `"rds_uncompressed"` — [saveRDS()] without compression: fastest of the
#'     rds variants, largest files.
#'
#'   All four formats are byte-deterministic, so the `refuse_identical`
#'   comparison works with each. The comparison is bytewise, however, so an
#'   object identical to the newest backup but saved under a different
#'   `format` is written as a new backup, not refused.
#' @return Invisibly, the path of the written backup file — or, when the save
#'   is refused because the newest same-stem backup is identical, the path of
#'   that existing backup.
#' @seealso [read_backup()]
#' @export
save_backup <- function(obj, out_dir, file_stem, refuse_identical = TRUE,
                        max_backups = 5,
                        format = c("qs2", "rds_zstd", "rds_gzip",
                                   "rds_uncompressed")) {
  format <- match.arg(format)
  stopifnot(dir.exists(out_dir))
  stopifnot(
    is.null(max_backups) ||
      (is.numeric(max_backups) && length(max_backups) == 1 && max_backups >= 1)
  )
  if (format == "qs2" && !requireNamespace("qs2", quietly = TRUE)) {
    stop("format = \"qs2\" needs the qs2 package: install it first.")
  }

  ext <- if (format == "qs2") ".qs2" else ".rds"
  write_fun <- switch(
    format,
    rds_zstd         = function(o, f) saveRDS(o, f, compress = "zstd"),
    rds_gzip         = function(o, f) saveRDS(o, f, compress = "gzip"),
    rds_uncompressed = function(o, f) saveRDS(o, f, compress = FALSE),
    qs2              = function(o, f) {
      qs2::qs_save(o, f, nthreads = max(1L, parallel::detectCores() - 2L))
    }
  )

  tmp_fp <- NULL
  if (isTRUE(refuse_identical)) {
    # serialise first, so the candidate can be hashed before touching out_dir
    tmp_fp <- tempfile(fileext = ext)
    on.exit(unlink(tmp_fp), add = TRUE)
    write_fun(obj, tmp_fp)

    latest_fn <- .list_backups(out_dir, file_stem) %>% head(1)
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
  }

  timestamp <- strftime(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  fn <- paste0(file_stem, "_", timestamp, ext)
  fp <- file.path(out_dir, fn)
  print(paste0("Saving to ", fp))
  if (is.null(tmp_fp)) {
    write_fun(obj, fp)
  } else {
    # the candidate is already serialised: move it into place
    file.copy(tmp_fp, fp, overwrite = TRUE)
  }

  # rotate: keep at most `max_backups` backups of this stem, oldest out first
  if (!is.null(max_backups)) {
    fns <- .list_backups(out_dir, file_stem)
    if (length(fns) > max_backups) {
      for (old_fn in fns[(max_backups + 1):length(fns)]) {
        old_fp <- file.path(out_dir, old_fn)
        print(paste0("Removing old backup ", old_fp))
        file.remove(old_fp)
      }
    }
  }

  return(invisible(fp))
}

