# Protect and verify the frozen inputs the single-config migration verifies against.
#
# Why: the migration's issues re-run scripts 5 to 9 from the calibration fits, predictions
# and intermediate files on disk instead of refitting (27 h), so those files must not
# change while it is in progress. `lock` makes every file under data/ and output/
# read-only, so a script that tries to overwrite one fails with "Permission denied"
# instead of replacing it silently; `verify` checks them against the recorded inventory.
#
# Usage, from the repository root (any OS):
#   Rscript tools/frozen.R lock | unlock | verify | record
#   record  writes tests/frozen_inputs.md5 (md5 of every file under data/ and output/, in
#           the "md5  path" format of md5sum)
# Plain R (tools::md5sum, Sys.chmod), replacing tools/frozen.sh (I07, 2026-10-13).

INVENTORY <- "tests/frozen_inputs.md5"

frozen_files <- function() {
  f <- list.files(c("data", "output"), recursive = TRUE, full.names = TRUE)
  sort(f[!dir.exists(f)], method = "radix")
}

# Read-only for everyone, or writable by the owner again; on Windows this sets or clears
# the read-only attribute.
set_writable <- function(files, writable) {
  mode <- file.mode(files)
  new  <- if (writable) mode | as.octmode("200") else mode & !as.octmode("222")
  ok   <- Sys.chmod(files, new, use_umask = FALSE)
  if (!all(ok)) stop("frozen: could not change the mode of ", sum(!ok), " file(s)")
}

frozen_verify <- function(inventory = INVENTORY) {
  rec   <- read.table(inventory, colClasses = "character", col.names = c("md5", "path"))
  found <- file.exists(rec$path)
  got   <- unname(tools::md5sum(rec$path[found]))
  bad   <- c(rec$path[!found], rec$path[found][got != rec$md5[found]])
  if (length(bad)) {
    cat("frozen: ", length(bad), " of ", nrow(rec), " files missing or changed:\n  ",
        paste(bad, collapse = "\n  "), "\n", sep = "")
    return(invisible(FALSE))
  }
  cat("frozen: all", nrow(rec), "files match", inventory, "\n")
  invisible(TRUE)
}

if (sys.nframe() == 0) {
  cmd <- commandArgs(TRUE)[1]
  if (identical(cmd, "lock")) {
    set_writable(frozen_files(), FALSE); cat("frozen: data/ and output/ files read-only\n")
  } else if (identical(cmd, "unlock")) {
    set_writable(frozen_files(), TRUE); cat("frozen: data/ and output/ files writable\n")
  } else if (identical(cmd, "record")) {
    f <- frozen_files()
    writeLines(paste0(unname(tools::md5sum(f)), "  ", f), INVENTORY)
    cat("frozen:", length(f), "files recorded in", INVENTORY, "\n")
  } else if (identical(cmd, "verify")) {
    if (!frozen_verify()) quit(status = 1)
  } else {
    cat("usage: Rscript tools/frozen.R lock|unlock|verify|record\n"); quit(status = 2)
  }
}
