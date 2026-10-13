# Compare the pipeline's outputs with an anchor's MD5SUMS.txt, byte for byte.
#
# Why: anchors are keyed by file name, but the pipeline writes to data/ and output/
# subdirectories. tests/anchors/live_paths.tsv maps each anchor file name to where the
# pipeline writes it, relative to the repository or to a run directory (config.R
# run$dir), so one checker serves every anchor and every run.
#
# Usage, from the repository root (any OS):
#   Rscript tools/check_anchors.R <anchor-dir> [run-dir]
# Without a run directory the repository's own files are checked. Files absent are
# reported as "not produced"; in a run directory, files put there by seed_run_dir() (listed
# in its SEEDED.txt) are reported as "seeded from the baseline": reporting them as MATCH
# would claim a reproduction that did not happen. Exit status 1 if any compared file
# differs. Plain R, replacing tools/check_anchors.sh (I07, 2026-10-13).

check_anchors <- function(anchor, root = NULL) {
  sums <- read.table(file.path(anchor, "MD5SUMS.txt"), colClasses = "character", col.names = c("md5", "path"))
  map  <- read.delim("tests/anchors/live_paths.tsv", comment.char = "#", header = FALSE,
                     col.names = c("anchor", "path"), colClasses = "character")
  seeded <- if (!is.null(root) && file.exists(file.path(root, "SEEDED.txt"))) readLines(file.path(root, "SEEDED.txt")) else character()
  ok <- TRUE
  for (i in seq_len(nrow(sums))) {
    name <- basename(sums$path[i])
    live <- map$path[map$anchor == name]
    status <- if (length(live) != 1) { ok <- FALSE; "NO MAPPING in live_paths.tsv" } else {
      p <- if (is.null(root)) live else file.path(root, live)
      if (live %in% seeded) "seeded from the baseline, not produced here"
      else if (!file.exists(p)) "not produced"
      else {
        got <- unname(tools::md5sum(p))
        if (got == sums$md5[i]) "MATCH"
        else { ok <- FALSE; sprintf("DIFFER (anchor %s, got %s)", substr(sums$md5[i], 1, 8), substr(got, 1, 8)) }
      }
    }
    cat(sprintf("%-48s %s\n", name, status))
  }
  invisible(ok)
}

if (sys.nframe() == 0) {
  a <- commandArgs(TRUE)
  if (!length(a)) { cat("usage: Rscript tools/check_anchors.R <anchor-dir> [run-dir]\n"); quit(status = 2) }
  if (!check_anchors(a[1], if (length(a) > 1) a[2] else NULL)) quit(status = 1)
}
