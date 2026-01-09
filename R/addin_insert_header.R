#' Insert a header template
#'
#' Inserts a structured header at the cursor.
#'
#' @param level Header level (1, 2, or 3).
#' @return Invisibly, NULL.
#' @keywords internal
insert_header <- function(level = 1) {
  if (!rstudioapi::isAvailable()) {
    stop("This addin requires RStudio.", call. = FALSE)
  }

  ctx <- rstudioapi::getActiveDocumentContext()
  selection <- ctx$selection[[1]]
  start <- selection$range$start
  insert_row <- if (is.list(start) && !is.null(start$row)) {
    start$row
  } else if (is.atomic(start) && length(start) >= 1) {
    unname(start[[1]])
  } else {
    1L
  }
  nums <- addHeaderR_next_header_number(ctx$contents, insert_row, level)

  header <- switch(
    as.character(as.integer(level)),
    "1" = c(sprintf("# %d. Title ----", nums[[1]]), ""),
    "2" = c(sprintf("## %d.%d Subtitle ----", nums[[1]], nums[[2]]), ""),
    "3" = c(sprintf("### %d.%d.%d Subsection ----", nums[[1]], nums[[2]], nums[[3]]), ""),
    addHeaderR_header_template(level)
  )

  rstudioapi::insertText(
    location = selection$range$start,
    text = paste(header, collapse = "\n")
  )

  invisible(NULL)
}

#' Insert header level 1
#'
#' @return Invisibly, NULL.
#' @export
insert_header_level1 <- function() {
  insert_header(1)
}

#' Insert header level 2
#'
#' @return Invisibly, NULL.
#' @export
insert_header_level2 <- function() {
  insert_header(2)
}

#' Insert header level 3
#'
#' @return Invisibly, NULL.
#' @export
insert_header_level3 <- function() {
  insert_header(3)
}
