# Compare the pipeline's outputs with an anchor's MD5SUMS.txt, byte for byte.
#
# Why: anchors are keyed by file name, but the pipeline writes to data/ and output/
# subdirectories. tests/anchors/live_paths.tsv maps each anchor file name to where the
# pipeline writes it, relative to the repository or to a run directory (config.R
# run$dir), so one checker serves every anchor and every run.
#
# Usage, from the repository root (any OS):
#   Rscript tools/check_anchors.R <anchor-dir> [run-dir] [--all]
# Without a run directory the repository's own files are checked. Each anchored file gets
# one result: MATCH, DIFFER, NOT PRODUCED (absent), SEEDED (put in the run directory by
# seed_run_dir(), listed in its SEEDED.txt: reporting it as MATCH would claim a
# reproduction that did not happen) or NO MAPPING (missing from live_paths.tsv).
# anchors_ok() turns the results into pass or fail, the one rule used everywhere:
#   default  fail on DIFFER or NO MAPPING, or if no file at all MATCHes (a SEEDED file is not
#            a MATCH; a run of some
#            scripts checks what it produced, but must have produced something: a wrong
#            run directory would otherwise pass with every file NOT PRODUCED);
#   --all    fail unless every file is MATCH (the run must reproduce the whole anchor).
# Exit status 1 on fail. Plain R, replacing tools/check_anchors.sh (I07, 2026-10-13).

check_anchors <- function(anchor, root = NULL) {
  sums <- read.table(file.path(anchor, "MD5SUMS.txt"), colClasses = "character", col.names = c("md5", "path"))
  map  <- read.delim("tests/anchors/live_paths.tsv", comment.char = "#", header = FALSE,
                     col.names = c("anchor", "path"), colClasses = "character")
  seeded <- if (!is.null(root) && file.exists(file.path(root, "SEEDED.txt"))) readLines(file.path(root, "SEEDED.txt")) else character()
  results <- data.frame(file = basename(sums$path), result = "", detail = "", stringsAsFactors = FALSE)
  for (i in seq_len(nrow(sums))) {
    live <- map$path[map$anchor == results$file[i]]
    if (length(live) != 1) {
      results[i, c("result", "detail")] <- c("NO MAPPING", "not in tests/anchors/live_paths.tsv")
      next
    }
    p <- if (is.null(root)) live else file.path(root, live)
    if (live %in% seeded) {
      results[i, c("result", "detail")] <- c("SEEDED", "copied from the baseline, not produced here")
    } else if (!file.exists(p)) {
      results$result[i] <- "NOT PRODUCED"
    } else {
      got <- unname(tools::md5sum(p))
      if (got == sums$md5[i]) {
        results$result[i] <- "MATCH"
      } else {
        results[i, c("result", "detail")] <- c("DIFFER", sprintf("anchor %s, got %s", substr(sums$md5[i], 1, 8), substr(got, 1, 8)))
      }
    }
  }
  cat(sprintf("%-48s %s%s\n", results$file, results$result,
              ifelse(nzchar(results$detail), paste0(" (", results$detail, ")"), "")), sep = "")
  invisible(results)
}

anchors_ok <- function(results, require_all = FALSE) {
  if (require_all) nrow(results) > 0 && all(results$result == "MATCH")
  else any(results$result == "MATCH") && !any(results$result %in% c("DIFFER", "NO MAPPING"))
}

if (sys.nframe() == 0) {
  a <- commandArgs(TRUE)
  require_all <- "--all" %in% a
  a <- a[a != "--all"]
  if (!length(a)) { cat("usage: Rscript tools/check_anchors.R <anchor-dir> [run-dir] [--all]\n"); quit(status = 2) }
  if (!anchors_ok(check_anchors(a[1], if (length(a) > 1) a[2] else NULL), require_all)) quit(status = 1)
}
