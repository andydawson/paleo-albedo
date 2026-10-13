# Tests for R/calendar.R. Run: tools/rscript.sh tests/test_calendar.R
source("R/calendar.R")
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

stopifnot(
  identical(MONTHS, c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")),
  identical(month_to_number(MONTHS), 1:12),
  # the trap this file exists for: a subset must map by name, not by position
  identical(month_to_number(c("feb", "may", "aug", "nov")), c(2L, 5L, 8L, 11L)),
  identical(month_to_number(rev(MONTHS)), 12:1),
  identical(month_to_number("jul"), 7L),
  identical(check_months(c("nov", "feb")), c("nov", "feb")),   # order is kept
  fails(check_months(character(0))),
  fails(check_months(c("jan", "January"))),
  fails(check_months(c("feb", "feb"))),
  fails(month_to_number("Feb")),
  # coarse-slice labels as scripts 7, 8 and 9 had them typed out
  identical(period_labels(c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)),
            c("0.05 - 0.5 ka", "0.5 - 2 ka", "2 - 4 ka", "4 - 6 ka", "6 - 8 ka", "8 - 10 ka", "10 - 12 ka")),
  identical(ages_ka(c(50, 500, 2000, 4000, 6000, 8000, 10000, 12000)),
            c("0.05", "0.5", "2", "4", "6", "8", "10", "12"))
)
cat("test_calendar: all tests passed\n")
