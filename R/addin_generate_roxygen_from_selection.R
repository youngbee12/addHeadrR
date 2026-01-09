#' Generate roxygen2 template from selected function text
#'
#' Select an R function definition in the editor, then run this addin to insert a
#' roxygen2 template above it.
#'
#' @return Invisibly, NULL.
#' @export
generate_roxygen_from_selection <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("This addin requires RStudio.", call. = FALSE)
  }

  ctx <- rstudioapi::getActiveDocumentContext()
  selection <- ctx$selection[[1]]
  selected_text <- selection$text

  if (!nzchar(selected_text)) {
    stop("Select a function definition first, then run the addin.", call. = FALSE)
  }

  parsed <- addHeaderR_parse_function_signature(selected_text)
  roxygen <- addHeaderR_roxygen_template(parsed$name, parsed$args)

  rstudioapi::insertText(
    location = selection$range$start,
    text = paste0(roxygen, "\n")
  )

  invisible(NULL)
}
