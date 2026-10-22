# Tests for fit_convergence() in R/model_registry.R, on made-up fits holding only the fields it
# reads. Run from the repository root: Rscript tests/test_model_registry.R
source("R/model_registry.R")

converged = list(iter = 37L, control = list(maxit = 500),
                 outer.info = list(conv = "full convergence", iter = 5L, grad = c(1e-4, -7.7e-4),
                                   hess = diag(c(1.1, 3))))
at_limit = converged; at_limit$iter = 500L
no_optimiser = converged; no_optimiser$outer.info = NULL
no_conv = converged; no_conv$outer.info$conv = NULL
step_failed = converged; step_failed$outer.info$conv = "step failed"
not_pos_def = converged; not_pos_def$outer.info$hess = matrix(-1)   # also the 1x1 case

stopifnot(
  identical(fit_convergence(converged),
            "fitting loop: 37 of 500 steps; smoothing parameters: full convergence after 5 iterations; largest gradient 7.7e-04; Hessian positive definite (smallest eigenvalue 1.1)"),
  startsWith(fit_convergence(at_limit),
             'fitting loop: reached its limit of 500 steps, probably NOT converged (see R\'s "algorithm did not converge" warning); '),
  identical(fit_convergence(no_optimiser), "fitting loop: 37 of 500 steps; smoothing parameters: not available"),
  identical(fit_convergence(no_conv), "fitting loop: 37 of 500 steps; smoothing parameters: not available"),
  grepl("smoothing parameters: NOT converged: step failed after 5 iterations", fit_convergence(step_failed), fixed = TRUE),
  grepl("Hessian NOT positive definite (smallest eigenvalue -1)", fit_convergence(not_pos_def), fixed = TRUE),
  # a fit that converged says NOT nowhere
  !grepl("NOT", fit_convergence(converged))
)
cat("test_model_registry: all tests passed\n")
