# Run provenance logging.
#
# Records what code and what data produced each run, plus a human note, as one
# markdown file per run under runs/. Plain text so it diffs in git and stays
# readable without any service running.
#
# Usage, at the top and bottom of a pipeline script:
#
#   source("R/run_manifest.R")
#   run_start("4_calibration_model", note = Sys.getenv("RUN_NOTE"),
#             inputs = c("data/calibration_modern_lct_interp_bluesky.RDS"))
#   ... the script ...
#   run_end(outputs = list.files("output/calibration", full.names = TRUE))
#
# Launch with a note:  RUN_NOTE="first interp calibration" Rscript scripts/4_...R

.run <- new.env(parent = emptyenv())

.sh <- function(cmd) tryCatch(trimws(paste(system(cmd, intern = TRUE), collapse = " ")),
                              error = function(e) NA_character_, warning = function(w) NA_character_)

.file_row <- function(p, hash = TRUE) {
  if (!file.exists(p)) return(sprintf("  - %s (MISSING)", p))
  sz <- file.info(p)$size
  h  <- if (hash && sz < 2e9) unname(tools::md5sum(p)) else "(not hashed)"
  sprintf("  - %s | %.1f MB | md5 %s", p, sz / 1048576, h)
}

run_start <- function(script, note = NULL, inputs = character(), config = list(),
                      hash_inputs = TRUE, runs_dir = "runs") {
  .run$script <- script
  .run$runs_dir <- runs_dir   # path_runs(paths): runs/, or the run directory's own (I07)
  .run$t0     <- Sys.time()
  .run$note   <- if (is.null(note) || !nzchar(note)) "(none given)" else note
  .run$inputs <- inputs
  .run$config <- config
  .run$hash   <- hash_inputs
  .run$commit <- .sh("git rev-parse --short HEAD")
  .run$branch <- .sh("git branch --show-current")
  .run$dirty  <- nzchar(.sh("git status --porcelain -- scripts R 2>/dev/null"))
  if (isTRUE(.run$dirty))
    warning("run_manifest: scripts/ or R/ have uncommitted changes; ",
            "this run is not reproducible from a commit", immediate. = TRUE)
  invisible(NULL)
}

run_end <- function(outputs = character(), status = "completed") {
  t1 <- Sys.time()
  stamp <- format(.run$t0, "%Y-%m-%d_%H%M")
  path  <- file.path(.run$runs_dir, sprintf("%s_%s.md", stamp, .run$script))
  dir.create(.run$runs_dir, recursive = TRUE, showWarnings = FALSE)

  pkgs <- c("mgcv", "terra", "raster", "sp", "dplyr", "gratia", "ggplot2")
  pkgv <- vapply(pkgs, function(p)
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), "")

  thr <- Sys.getenv(c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS"))

  L <- c(
    sprintf("# Run: %s", .run$script), "",
    sprintf("**Note.** %s", .run$note), "",
    "## Provenance", "",
    sprintf("- when: %s to %s (%.1f min)", format(.run$t0, "%Y-%m-%d %H:%M:%S"),
            format(t1, "%H:%M:%S"), as.numeric(difftime(t1, .run$t0, units = "mins"))),
    sprintf("- status: %s", status),
    sprintf("- commit: %s (%s)%s", .run$commit, .run$branch,
            if (!identical(.run$commit, .sh("git rev-parse --short HEAD")))
              sprintf(" [HEAD moved to %s during the run; the commit above is the one that ran]",
                      .sh("git rev-parse --short HEAD")) else ""),
    sprintf("- working tree: %s", if (isTRUE(.run$dirty))
            "**DIRTY** - scripts/ or R/ had uncommitted changes" else "clean"),
    sprintf("- host: %s | cores used: %s", .sh("hostname"),
            Sys.getenv("RUN_CORES", "(unset)")),
    sprintf("- BLAS/OMP/MKL threads: %s", paste(thr, collapse = " / ")),
    sprintf("- %s", R.version.string),
    sprintf("- packages: %s", paste(sprintf("%s %s", names(pkgv), pkgv), collapse = ", ")),
    "")

  if (length(.run$config)) L <- c(L, "## Config", "",
    vapply(names(.run$config), function(k)
      sprintf("- %s: %s", k, paste(.run$config[[k]], collapse = ", ")), ""), "")

  L <- c(L, "## Inputs", "",
         if (length(.run$inputs)) vapply(.run$inputs, .file_row, "", hash = .run$hash)
         else "  (none declared)", "",
         "## Outputs", "",
         if (length(outputs)) vapply(outputs, .file_row, "", hash = FALSE)
         else "  (none declared)", "")

  writeLines(L, path)
  message("run manifest written: ", path)
  invisible(path)
}
