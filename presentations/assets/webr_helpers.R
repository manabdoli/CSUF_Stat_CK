# --- Minimal "CourseKata-style" helpers (WebR-friendly) ----------------------

# A formula-friendly tally():
# - tally(x)  (vector)
# - tally(~x, data)
# - tally(y ~ x, data)
# - tally(~y + x, data)  (same as y ~ x)
tally <- function(formula_or_x, data = NULL) {

  # helper: table -> data.frame with nice names
  .as_df <- function(tab) {
    df <- as.data.frame(tab, stringsAsFactors = FALSE)
    if (!"Freq" %in% names(df)) names(df)[ncol(df)] <- "Freq"
    df
  }

  # Case 1: vector passed directly
  if (!inherits(formula_or_x, "formula")) {
    return(.as_df(table(formula_or_x, useNA = "ifany")))
  }

  f <- formula_or_x

  # Accept ~y + x as y ~ x (CourseKata sometimes uses ~y + x)
  if (length(f) == 3) {
    y_expr <- f[[2]]
    x_expr <- f[[3]]
  } else if (length(f) == 2) {
    y_expr <- NULL
    x_expr <- f[[2]]
  } else {
    stop("Unsupported formula.")
  }

  if (is.null(data)) stop("When using a formula, please supply `data = ...`.")

  env <- list2env(as.list(data), parent = parent.frame())

  x <- eval(x_expr, env)
  if (is.null(y_expr)) {
    return(.as_df(table(x, useNA = "ifany")))
  } else {
    y <- eval(y_expr, env)
    return(.as_df(table(y, x, useNA = "ifany")))
  }
}

# Lightweight select() / filter() / arrange() / mutate() / desc()
# (Enough for Ch 2.8–2.11 activities; not full dplyr.)
select <- function(.data, ...) {
  cols <- as.character(match.call(expand.dots = FALSE)$`...`)
  cols <- cols[cols != ""]
  .data[, cols, drop = FALSE]
}
head_ck <- function(x, n = 6) utils::head(x, n = n)  # avoid masking base head()

filter <- function(.data, condition) {
  env <- list2env(as.list(.data), parent = parent.frame())
  keep <- eval(substitute(condition), env)
  .data[keep, , drop = FALSE]
}

desc <- function(x) -xtfrm(x)

arrange <- function(.data, by) {
  ord <- order(eval(substitute(by), .data, parent.frame()), na.last = TRUE)
  .data[ord, , drop = FALSE]
}

mutate <- function(.data, ...) {
  dots <- match.call(expand.dots = FALSE)$`...`
  for (nm in names(dots)) {
    .data[[nm]] <- eval(dots[[nm]], .data, parent.frame())
  }
  .data
}

five_num <- function(x) {
  x <- x[is.na(x) == FALSE]
  q <- as.numeric(quantile(x, probs = c(0, .25, .5, .75, 1)))
  names(q) <- c("min", "Q1", "median", "Q3", "max")
  q
}

iqr_val <- function(x) {
  q <- five_num(x)
  unname(q["Q3"] - q["Q1"])
}

outlier_flags <- function(x) {
  q <- five_num(x)
  iqr <- q["Q3"] - q["Q1"]
  lo <- q["Q1"] - 1.5 * iqr
  hi <- q["Q3"] + 1.5 * iqr
  list(lower = lo, upper = hi, is_outlier = (x < lo) | (x > hi))
}
