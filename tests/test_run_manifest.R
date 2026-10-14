# Tests for R/run_manifest.R (issue I05). Run from the repository root:
#   Rscript tests/test_run_manifest.R
# Each case runs a small script with Rscript, as the pipeline does, in a scratch copy of R/
# and config.R, so nothing is written to the repository's runs/ or data/.
rscript <- file.path(R.home("bin"), "Rscript")
work <- tempfile("run_manifest_test_")
dir.create(file.path(work, "R"), recursive = TRUE)
dir.create(file.path(work, "scripts"))
dir.create(file.path(work, "data"))
invisible(file.copy(list.files("R", full.names = TRUE), file.path(work, "R")))
invisible(file.copy("config.R", work))
setwd(work)
source("R/run_manifest.R")
base_config <- readLines("config.R")

# Write a small pipeline script and run it; returns its exit status and output.
run_toy <- function(name, body, args = character(), env = character()) {
  f <- file.path("scripts", paste0(name, ".R"))
  writeLines(c("source('R/run_manifest.R')", body), f)
  out <- suppressWarnings(system2(rscript, c(f, args), stdout = TRUE, stderr = TRUE, env = env))
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"), out = out)
}
# The manifests of a script, oldest first, and the newest one's record.
manifests <- function(script, runs = "runs") sort(list.files(file.path(runs, script), full.names = TRUE))
latest <- function(script, runs = "runs") { m <- manifests(script, runs); read_manifest(m[length(m)]) }
# A config file made from config.R with one substitution.
config_variant <- function(file, from, to) {
  stopifnot(sum(grepl(from, base_config, fixed = TRUE)) == 1)
  writeLines(sub(from, to, base_config, fixed = TRUE), file)
  file
}
# MD5 by a tool other than R, so the recorded hashes are checked independently.
md5_tool <- function(f) {
  if (nzchar(Sys.which("md5sum"))) return(strsplit(system2("md5sum", shQuote(f), stdout = TRUE), " ")[[1]][1])
  if (nzchar(Sys.which("md5")))    return(system2("md5", c("-q", shQuote(f)), stdout = TRUE))
  NA_character_
}
check <- function(what, ok) { if (!isTRUE(ok)) stop("FAILED: ", what, call. = FALSE); cat("ok:", what, "\n") }

write.csv(data.frame(month = 1:12, albedo = 0.8), "data/albedo_glacier_monthly.csv", row.names = FALSE)

# The toy producer of albedo_differences (script 7a's product) and its consumer (script 8).
producer_body <- c(
  "run = run_begin('7a_alb_diff_full'); paths = run$paths",
  "glacier = read_input(run, path_file(paths, 'glacier_albedo'), read.csv)",
  "write_output(run, data.frame(diff = 1:3), path_file(paths, 'albedo_differences'))",
  "pdf(path_figure(paths, 'toy.pdf')); plot(1)   # left open: run_finish() must close it",
  "run_finish(run)")
consumer_body <- c(
  "run = run_begin('8_radiative'); paths = run$paths",
  "d = read_input(run, path_file(paths, 'albedo_differences'))",
  "run_finish(run)")
product <- "data/ALB_diffs_bluesky.RDS"

# --- a producer and a consumer that agree ----------------------------------------------
dir.create("figures", showWarnings = FALSE)
writeLines("old", "figures/left_by_an_earlier_run.pdf")
Sys.setFileTime("figures/left_by_an_earlier_run.pdf", Sys.time() - 3600)
r <- run_toy("7a_alb_diff_full", producer_body)
m <- latest("7a_alb_diff_full")
check("producer exits 0 and its manifest is completed", r$status == 0 && m$status == "completed")
check("one manifest per run", length(manifests("7a_alb_diff_full")) == 1)
check("original input hashed, not checked",
      length(m$inputs) == 1 && is.null(m$inputs[[1]]$check) && m$inputs[[1]]$md5 == unname(tools::md5sum("data/albedo_glacier_monthly.csv")))
tool <- md5_tool(product)
check("output MD5 matches an independent tool", is.na(tool) || m$outputs[[1]]$md5 == tool)
figs <- vapply(m$figures, `[[`, "", "path")
check("new figure recorded, closed and complete; old figure not claimed",
      identical(figs, "figures/toy.pdf") && m$figures[[1]]$md5 == unname(tools::md5sum("figures/toy.pdf")))
check("the whole config is recorded", identical(m$config, config_record(load_config())))
check("the session is recorded", is.character(m$session$r) && "jsonlite" %in% names(m$session$packages))
check("the manifest is readable Markdown with the record at the end",
      grepl("^# Run of 7a_alb_diff_full: completed", readLines(manifests("7a_alb_diff_full")[1])[1]))

r <- run_toy("8_radiative", consumer_body)
m <- latest("8_radiative")
check("consumer passes the check", r$status == 0 && m$status == "completed" && m$inputs[[1]]$check == "ok")

r <- run_toy("8_radiative", c("run = run_begin('8_radiative'); paths = run$paths",
                              "for (month in 1:3) d = read_input(run, path_file(paths, 'albedo_differences'))",
                              "run_finish(run)"))
check("a file read several times unchanged is recorded once", r$status == 0 && length(latest("8_radiative")$inputs) == 1)

r <- run_toy("toy_details", c("run = run_begin('toy_details')",
                              "seed = run_record(run, 'random_seed', list(setting = 'random', base = 123L))$base",
                              "stopifnot(identical(seed, 123L))", "run_finish(run)"))
m <- latest("toy_details")
check("run_record() keeps a fact in the record and the summary, and returns it",
      r$status == 0 && identical(m$details$random_seed$base, 123L) &&
      any(grepl('random_seed: {"setting":"random","base":123}', readLines(manifests("toy_details")[1]), fixed = TRUE)))

# --- the consistency check stops ... ---------------------------------------------------
saveRDS(data.frame(diff = 4:6), product)
r <- run_toy("8_radiative", consumer_body)
m <- latest("8_radiative")
check("... when the file differs from what the producer wrote",
      r$status == 1 && m$status == "failed" && grepl("differs from what the latest run of 7a_alb_diff_full", m$error))
invisible(run_toy("7a_alb_diff_full", producer_body))

alt <- config_variant(tempfile(fileext = ".R"), "earth_area_m2 = 5.101e14", "earth_area_m2 = 5.2e14")
r <- run_toy("8_radiative", consumer_body, c("--config", alt))
m <- latest("8_radiative")
check("... when a nested analysis setting differs, naming it with old and new values",
      r$status == 1 && m$status == "failed" &&
      grepl("forcing.earth_area_m2: [510100000000000] -> [520000000000000]", m$error, fixed = TRUE))

alt_ages <- config_variant(tempfile(fileext = ".R"), "ages = c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)",
                           "ages = c(50, 500, 2000, 4000, 6000, 9000, 10000, 12000)")
r <- run_toy("8_radiative", consumer_body, c("--config", alt_ages))
check("... when one element of a vector setting differs",
      r$status == 1 && grepl("coarse.ages:", latest("8_radiative")$error, fixed = TRUE))

unlink(product)
r <- run_toy("8_radiative", consumer_body)
check("... when an input is missing", r$status == 1 && grepl("does not exist", latest("8_radiative")$error))

invisible(run_toy("7a_alb_diff_full", c("run = run_begin('7a_alb_diff_full')", "run_finish(run)")))
saveRDS(data.frame(diff = 1:3), product)
r <- run_toy("8_radiative", consumer_body)
check("... when the producer's latest run did not write the file (no search through older runs)",
      r$status == 1 && grepl("did not write it", latest("8_radiative")$error))
invisible(run_toy("7a_alb_diff_full", producer_body))

dir.create("output/prediction", recursive = TRUE, showWarnings = FALSE)
saveRDS(1, "output/prediction/paleo_interp_predict_gam_summary_bluesky.RDS")
r <- run_toy("7_plot_preds", c("run = run_begin('7_plot_preds')",
                               "x = read_input(run, path_prediction(run$paths, 'summary'))", "run_finish(run)"))
check("... when no run of the producer is recorded",
      r$status == 1 && grepl("no run of 6_prediction_model is recorded", latest("7_plot_preds")$error))

# --- ... and passes ---------------------------------------------------------------------
pres <- config_variant(tempfile(fileext = ".R"), "tail        = list(x = c(-166, -50), y = c(12, 82))",
                       "tail        = list(x = c(-160, -50), y = c(12, 82))")
r <- run_toy("8_radiative", consumer_body, c("--config", pres))
check("a presentation-only change passes", r$status == 0 && latest("8_radiative")$inputs[[1]]$check == "ok")

allow <- tempfile(fileext = ".R")
writeLines(sub("allow_stale = FALSE", "allow_stale = TRUE",
               sub("earth_area_m2 = 5.101e14", "earth_area_m2 = 5.2e14", base_config, fixed = TRUE),
               fixed = TRUE), allow)
r <- run_toy("8_radiative", consumer_body, c("--config", allow))
m <- latest("8_radiative")
check("run$allow_stale lets a difference through and records it",
      r$status == 0 && m$status == "completed" && m$inputs[[1]]$check == "stale (allowed)" &&
      length(m$stale_inputs) == 1 && any(grepl("earth_area_m2", unlist(m$stale_inputs[[1]]$problems))))

# --- failures leave a record --------------------------------------------------------------
writeLines("list(analysis = ", "broken_config.R")
r <- run_toy("toy_bad_config", c("run = run_begin('toy_bad_config')"), c("--config", "broken_config.R"))
m <- latest("toy_bad_config")
check("an unreadable config is recorded as failed, with the error",
      r$status == 1 && m$status == "failed" && grepl("could not be read", m$error))

r <- run_toy("toy_error_before", c("run = run_begin('toy_error_before')", "stop('broke before writing')",
                                   "write_output(run, 1, 'data/never.RDS')"))
m <- latest("toy_error_before")
check("an error before any output: failed, no outputs",
      r$status == 1 && m$status == "failed" && grepl("broke before writing", m$error) && !length(m$outputs))

r <- run_toy("toy_error_after", c("run = run_begin('toy_error_after')", "write_output(run, 1, 'data/toy1.RDS')",
                                  "stop('broke after writing')", "cat('not reached')"))
m <- latest("toy_error_after")
check("an error after an output: failed, the earlier output recorded, the rest not run",
      r$status == 1 && m$status == "failed" && length(m$outputs) == 1 && !any(grepl("not reached", r$out)))

r <- run_toy("toy_killed", c("run = run_begin('toy_killed')", "write_output(run, 1, 'data/toy2.RDS')",
                             "tools::pskill(Sys.getpid(), tools::SIGKILL)"))
m <- latest("toy_killed")
check("a killed run is left 'started', with what it wrote so far", m$status == "started" && length(m$outputs) == 1)

writeLines(c("source('R/run_manifest.R')", "run = run_begin('toy_sourced')", "stop('broke under source')"),
           "scripts/toy_sourced.R")
out <- suppressWarnings(system2(rscript, c("-e", shQuote("source('scripts/toy_sourced.R')")), stdout = TRUE, stderr = TRUE))
check("a script run with source() fails the same way",
      identical(attr(out, "status"), 1L) && latest("toy_sourced")$status == "failed")

# --- the session is given back as it was ----------------------------------------------------
r <- run_toy("toy_session", c(
  "pdf(file.path(tempdir(), 'already_open.pdf'))   # a device the caller had open",
  "options(error = NULL); before = list(error = getOption('error'), device = getOption('device'), devices = dev.list())",
  "run = run_begin('toy_session')",
  "png(path_figure(run$paths, 's.png')); plot(1)   # left open: run_finish() closes it",
  "run_finish(run)",
  "stopifnot(identical(getOption('error'), before$error), identical(getOption('device'), before$device),",
  "          identical(dev.list(), before$devices))",
  "cat('SESSION OK')"))
check("run_finish() restores the error handler and default device and closes only the run's devices",
      r$status == 0 && any(grepl("SESSION OK", r$out)) && latest("toy_session")$status == "completed")

# --- figures: only this run's ------------------------------------------------------------
writeLines(c("source('R/run_manifest.R')", "run = run_begin('toy_fig_a')",
             "Sys.sleep(1); png(path_figure(run$paths, 'a.png')); plot(1); dev.off()",
             "plot(2)   # no device open: Rplots.pdf",
             "Sys.sleep(3); run_finish(run)"), "scripts/toy_fig_a.R")
writeLines(c("source('R/run_manifest.R')", "run = run_begin('toy_fig_b')",
             "Sys.sleep(2); png(path_figure(run$paths, 'b.png')); plot(3); dev.off()",
             "Sys.sleep(3); run_finish(run)"), "scripts/toy_fig_b.R")
for (s in c("a", "b")) system2(rscript, sprintf("scripts/toy_fig_%s.R", s), wait = FALSE, stdout = FALSE, stderr = FALSE)
for (i in 1:120) {
  if (all(vapply(c("toy_fig_a", "toy_fig_b"), function(s) length(manifests(s)) == 1 &&
                 latest(s)$status == "completed", TRUE))) break
  Sys.sleep(0.5)
}
figs_of <- function(s) vapply(latest(s)$figures, `[[`, "", "path")
check("two runs writing figures side by side each list only their own (and Rplots.pdf only if they used it)",
      setequal(figs_of("toy_fig_a"), c("Rplots.pdf", "figures/a.png")) && identical(figs_of("toy_fig_b"), "figures/b.png"))

# --- two runs started together ------------------------------------------------------------
writeLines(c("source('R/run_manifest.R')", "run = run_begin('toy_parallel')", "Sys.sleep(2)", "run_finish(run)"),
           "scripts/toy_parallel.R")
for (i in 1:2) system2(rscript, "scripts/toy_parallel.R", wait = FALSE, stdout = FALSE, stderr = FALSE)
for (s in 1:120) {
  done <- manifests("toy_parallel")
  if (length(done) == 2 && all(vapply(done, function(f) read_manifest(f)$status == "completed", TRUE))) break
  Sys.sleep(0.5)
}
check("two runs started together get one manifest each", length(manifests("toy_parallel")) == 2)

# --- a file over 2 GB is hashed --------------------------------------------------------------
big_writer <- "function(x, path) { con = file(path, 'wb'); seek(con, 2.1e9); writeBin(as.raw(1), con); close(con) }"
r <- run_toy("toy_big", c("run = run_begin('toy_big')",
                          sprintf("write_output(run, NULL, 'data/big.bin', %s)", big_writer), "run_finish(run)"))
m <- latest("toy_big")
tool <- md5_tool("data/big.bin")
check("a file over 2 GB is hashed, and the hash matches an independent tool",
      r$status == 0 && file.size("data/big.bin") > 2e9 && nchar(m$outputs[[1]]$md5) == 32 &&
      (is.na(tool) || m$outputs[[1]]$md5 == tool))
unlink("data/big.bin")

# --- uncommitted changes, and no git -------------------------------------------------------
quiet_run <- function(...) invisible(system2("git", c(...), stdout = FALSE, stderr = FALSE))
if (nzchar(Sys.which("git"))) {
  quiet_run("init", "-q"); quiet_run("add", "-A")
  quiet_run("-c", "user.name=test", "-c", "user.email=test@example.org", "commit", "-q", "-m", "test")
  r <- run_toy("toy_git", c("run = run_begin('toy_git')", "run_finish(run)"))
  quiet_run("add", "scripts/toy_git.R")
  quiet_run("-c", "user.name=test", "-c", "user.email=test@example.org", "commit", "-q", "-m", "toy")
  r <- run_toy("toy_git", c("run = run_begin('toy_git')", "run_finish(run)"))
  m <- latest("toy_git")
  check("a committed checkout is clean, with the full commit", identical(m$code$dirty, FALSE) && nchar(m$code$commit) == 40)
  outside <- config_variant(tempfile(fileext = ".R"), "allow_stale = FALSE", "allow_stale = FALSE")
  r <- run_toy("toy_git", c("run = run_begin('toy_git')", "run_finish(run)"), c("--config", outside))
  m <- latest("toy_git")
  check("a config file outside the checkout does not blind the git check",
        identical(m$code$dirty, FALSE) && nzchar(m$code$branch) && m$config_md5 == unname(tools::md5sum(outside)))
  writeLines("# untracked", "R/new_helper.R")
  r <- run_toy("toy_git", c("run = run_begin('toy_git')", "run_finish(run)"))
  m <- latest("toy_git")
  check("an untracked file in R/ counts as an uncommitted change",
        isTRUE(m$code$dirty) && any(grepl("R/new_helper.R", unlist(m$code$uncommitted))))
  unlink("R/new_helper.R")
  writeLines(c(base_config, "# edited"), "config.R")
  r <- run_toy("toy_git", c("run = run_begin('toy_git')", "run_finish(run)"))
  check("an edit to config.R counts as an uncommitted change", isTRUE(latest("toy_git")$code$dirty))
  writeLines(base_config, "config.R")
}
r <- run_toy("toy_nogit", c("run = run_begin('toy_nogit')", "run_finish(run)"), env = "PATH=/nonexistent")
m <- latest("toy_nogit")
check("without git the run goes on and says the code version is unknown",
      r$status == 0 && m$status == "completed" && is.null(m$code$commit) &&
      any(grepl("unknown (git not available)", readLines(manifests("toy_nogit")[1]), fixed = TRUE)))

cat("test_run_manifest: all tests passed\n")
