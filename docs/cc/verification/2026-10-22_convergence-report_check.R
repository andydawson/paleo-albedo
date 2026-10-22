# Check for the convergence-report change (2026-10-22): script 4 prints fit_convergence(fit)
# instead of bam's fixed converged flag, and the helper gives a sensible line for every real
# fit. Run from the worktree root with one argument: a folder of saved calibration fits.
source('R/model_registry.R')

# 1. Script 4 no longer prints the fixed flag; it prints the new line at both places, each
#    after the fit is saved.
script4 = readLines('scripts/4_calibration_model.R')
invisible(parse('scripts/4_calibration_model.R'))   # stops if script 4 no longer parses
stopifnot(!any(grepl('fit$converged', script4, fixed = TRUE)),
          sum(grepl('print(fit_convergence(fit))', script4, fixed = TRUE)) == 2,
          all(grepl('write_output(run, fit,', script4[which(grepl('print(fit_convergence(fit))', script4, fixed = TRUE)) - 1], fixed = TRUE)))

# 2. Every saved fit of a full run: one line each, and a count of any that did not converge.
fits_dir = commandArgs(trailingOnly = TRUE)[1]
files = list.files(fits_dir, pattern = '^calibration_.*\\.RDS$', full.names = TRUE)
stopifnot(length(files) > 0)
lines = character(0)
for (f in files) {
  fit = readRDS(f)
  stopifnot(inherits(fit, 'bam'))
  lines[basename(f)] = fit_convergence(fit)
  rm(fit); invisible(gc())
}
stopifnot(all(grepl('^fitting loop: ', lines)))
cat(sprintf('%s: %s', names(lines), lines), sep = '\n')
cat(sprintf('\n%d fits; reached the step limit: %d; smoothing parameters not fully converged: %d; Hessian not positive definite: %d; not available: %d\n',
            length(lines), sum(grepl('reached its limit', lines)), sum(grepl('smoothing parameters: NOT', lines)),
            sum(grepl('NOT positive definite', lines)), sum(grepl('not available', lines))))

# 3. The failure wordings, on altered copies of a real fit.
fit = readRDS(files[1])
at_limit = fit; at_limit$iter = at_limit$control$maxit
no_outer = fit; no_outer$outer.info = NULL
negative = fit; negative$outer.info$hess = -abs(negative$outer.info$hess)
stopifnot(grepl('reached its limit of 500 steps, probably NOT converged', fit_convergence(at_limit)),
          grepl('smoothing parameters: not available$', fit_convergence(no_outer)),
          grepl('Hessian NOT positive definite', fit_convergence(negative)))
cat('failure wordings: OK\n')
