# The calendar shared by every pipeline script.
#
# Why: the month list was typed out separately in seven scripts (four times in script 4),
# and script 8 derived each month's kernel band from its position in that list, so any
# subset or reordering of months would have read the wrong band (a four-month run would
# have given November the January kernel). Defining the calendar once, and mapping a
# month to its number by name, makes month selections safe before they become a setting
#. Written 2026-10-12.

# The twelve months, in calendar order, as the scripts and data name them.
MONTHS <- tolower(month.abb)

# Check a month selection: known names only, no duplicates, not empty. Returns it unchanged
# (in the order given), so it can wrap any assignment.
check_months <- function(months) {
  if (!length(months)) stop("month selection is empty")
  unknown_months <- setdiff(months, MONTHS)
  if (length(unknown_months)) stop("unknown month name(s): ", paste(unknown_months, collapse = ", "),
                            "; expected some of ", paste(MONTHS, collapse = ", "))
  duplicate_months <- unique(months[duplicated(months)])
  if (length(duplicate_months)) stop("month(s) selected more than once: ", paste(duplicate_months, collapse = ", "))
  months
}

# Calendar number of each month (jan = 1 ... dec = 12), by name, whatever the order or
# subset of the selection it comes from. Used to pick monthly bands from rasters and
# kernel files. Not called month_number(): script 8 has a loop variable of that name,
# which would overwrite the function on the first iteration.
month_to_number <- function(month) match(check_months(month), MONTHS)

# Labels for the coarse time slices, built from their ages in years BP so a change of
# ages cannot leave the labels behind. Scripts 7, 8 and 9 had the
# same seven period labels typed out five times.
#   ages_ka(c(50, 500, 2000))        "0.05" "0.5" "2"
#   period_labels(c(50, 500, 2000))  "0.05 - 0.5 ka" "0.5 - 2 ka"
ages_ka <- function(ages) as.character(ages / 1000)
period_labels <- function(ages) {
  ka <- ages_ka(ages)
  paste(head(ka, -1), "-", ka[-1], "ka")
}
