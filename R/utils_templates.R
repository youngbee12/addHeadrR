#' Template helpers for addins
#'
#' @keywords internal
#NULL

#' Header template
#'
#' @param level Header level (1, 2, or 3).
#' @keywords internal
addHeaderR_header_template <- function(level = 1) {
  level <- as.integer(level)
  if (is.na(level) || !level %in% c(1L, 2L, 3L)) {
    stop("Unsupported header level: ", level, call. = FALSE)
  }

  c(
    switch(
      as.character(level),
      "1" = "# 1. Title ----",
      "2" = "## 1.1 Subtitle ----",
      "3" = "### 1.1.1 Subsection ----"
    ),
    ""
  )
}

#' Parse a numbered RStudio section header line
#'
#' @param line A single line.
#' @return A list with elements \code{level} and \code{nums}, or NULL.
#' @keywords internal
addHeaderR_parse_numbered_header_line <- function(line) {
  if (!grepl("----\\s*$", line)) return(NULL)

  m3 <- regexec("^###\\s*(\\d+)\\.(\\d+)\\.(\\d+)\\b", line)
  r3 <- regmatches(line, m3)[[1]]
  if (length(r3) == 4) {
    return(list(level = 3L, nums = as.integer(r3[2:4])))
  }

  m2 <- regexec("^##\\s*(\\d+)\\.(\\d+)\\b", line)
  r2 <- regmatches(line, m2)[[1]]
  if (length(r2) == 3) {
    return(list(level = 2L, nums = as.integer(r2[2:3])))
  }

  m1 <- regexec("^#(?!#)\\s*(\\d+)\\b", line, perl = TRUE)
  r1 <- regmatches(line, m1)[[1]]
  if (length(r1) == 2) {
    return(list(level = 1L, nums = as.integer(r1[2])))
  }

  NULL
}

#' Compute next header number at a position
#'
#' @param lines Character vector of file lines.
#' @param insert_row 1-based row where insertion begins.
#' @param level Header level (1, 2, or 3).
#' @return Integer vector of length \code{level}.
#' @keywords internal
addHeaderR_next_header_number <- function(lines, insert_row, level) {
  insert_row <- as.integer(insert_row)
  level <- as.integer(level)
  if (is.na(insert_row) || insert_row < 1L) insert_row <- 1L

  parsed <- lapply(lines, addHeaderR_parse_numbered_header_line)

  l1_rows <- which(vapply(parsed, function(x) !is.null(x) && x$level == 1L, logical(1)))
  l2_rows <- which(vapply(parsed, function(x) !is.null(x) && x$level == 2L, logical(1)))
  l3_rows <- which(vapply(parsed, function(x) !is.null(x) && x$level == 3L, logical(1)))

  l1_nums <- if (length(l1_rows)) vapply(parsed[l1_rows], function(x) x$nums[[1]], integer(1)) else integer()

  if (level == 1L) {
    return(c(if (length(l1_nums)) max(l1_nums, na.rm = TRUE) else 0L) + 1L)
  }

  prev_l1_row <- max(l1_rows[l1_rows < insert_row], 0L)
  parent1 <- if (prev_l1_row > 0L) parsed[[prev_l1_row]]$nums[[1]] else 1L

  if (level == 2L) {
    next_l1_row <- min(l1_rows[l1_rows > prev_l1_row], length(lines) + 1L)
    scope_rows <- (prev_l1_row + 1L):(next_l1_row - 1L)
    scope_rows <- scope_rows[scope_rows >= 1L & scope_rows <= length(lines)]

    l2_in_scope <- intersect(l2_rows, scope_rows)
    l2_nums2 <- integer()
    if (length(l2_in_scope)) {
      l2_parent <- vapply(parsed[l2_in_scope], function(x) x$nums[[1]], integer(1))
      l2_second <- vapply(parsed[l2_in_scope], function(x) x$nums[[2]], integer(1))
      l2_nums2 <- l2_second[l2_parent == parent1]
    }
    next2 <- (if (length(l2_nums2)) max(l2_nums2, na.rm = TRUE) else 0L) + 1L
    return(c(parent1, next2))
  }

  prev_l2_row <- max(l2_rows[l2_rows < insert_row], 0L)
  if (prev_l2_row > 0L) {
    parent1 <- parsed[[prev_l2_row]]$nums[[1]]
    parent2 <- parsed[[prev_l2_row]]$nums[[2]]
  } else {
    parent2 <- 1L
  }

  next_l1_row <- min(l1_rows[l1_rows > prev_l1_row], length(lines) + 1L)
  l2_after <- l2_rows[l2_rows > prev_l2_row & l2_rows < next_l1_row]
  next_l2_row <- if (length(l2_after)) min(l2_after) else next_l1_row

  scope_rows <- (prev_l2_row + 1L):(next_l2_row - 1L)
  scope_rows <- scope_rows[scope_rows >= 1L & scope_rows <= length(lines)]

  l3_in_scope <- intersect(l3_rows, scope_rows)
  l3_nums3 <- integer()
  if (length(l3_in_scope)) {
    l3_parent1 <- vapply(parsed[l3_in_scope], function(x) x$nums[[1]], integer(1))
    l3_parent2 <- vapply(parsed[l3_in_scope], function(x) x$nums[[2]], integer(1))
    l3_third <- vapply(parsed[l3_in_scope], function(x) x$nums[[3]], integer(1))
    l3_nums3 <- l3_third[l3_parent1 == parent1 & l3_parent2 == parent2]
  }
  next3 <- (if (length(l3_nums3)) max(l3_nums3, na.rm = TRUE) else 0L) + 1L
  c(parent1, parent2, next3)
}

#' Parse function signature from text
#'
#' @param text Character string containing an R function definition.
#' @return A list with elements \code{name} and \code{args}.
#' @keywords internal
addHeaderR_parse_function_signature <- function(text) {
  exprs <- tryCatch(parse(text = text), error = function(e) NULL)
  if (is.null(exprs) || length(exprs) < 1) {
    stop("Could not parse selected text as R code.", call. = FALSE)
  }

  find_fun <- function(expr) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) && length(expr) >= 3) {
      lhs <- expr[[2]]
      rhs <- expr[[3]]
      if (is.symbol(lhs) && is.call(rhs) && identical(rhs[[1]], as.name("function"))) {
        return(list(name = as.character(lhs), fun = rhs))
      }
    }
    if (is.call(expr) && identical(expr[[1]], as.name("function"))) {
      return(list(name = "", fun = expr))
    }
    NULL
  }

  parsed <- NULL
  for (expr in as.list(exprs)) {
    parsed <- find_fun(expr)
    if (!is.null(parsed)) break
  }
  if (is.null(parsed)) {
    stop("Selection must contain a function definition like `name <- function(...) {}`.", call. = FALSE)
  }

  args <- character()
  fun <- parsed$fun
  if (length(fun) >= 2) {
    formals <- fun[[2]]
    if (is.pairlist(formals)) {
      args <- names(formals)
      args <- args[!is.na(args) & nzchar(args)]
    }
  }

  list(name = parsed$name, args = unique(args))
}

#' Build a roxygen2 template
#'
#' @param name Function name (may be empty).
#' @param args Character vector of argument names.
#' @return A character string with roxygen lines.
#' @keywords internal
addHeaderR_roxygen_template <- function(name, args) {
  title <- if (nzchar(name)) tools::toTitleCase(gsub("_", " ", name)) else "Title"
  args <- args[args != ""]

  param_lines <- if (length(args)) {
    unlist(lapply(args, function(arg) sprintf("#' @param %s ", arg)), use.names = FALSE)
  } else {
    character()
  }

  lines <- c(
    sprintf("#' %s", title),
    "#'",
    "#' Description.",
    "#'",
    param_lines,
    if (length(param_lines)) "#'" else NULL,
    "#' @return ",
    "#' @export"
  )

  paste(lines, collapse = "\n")
}
