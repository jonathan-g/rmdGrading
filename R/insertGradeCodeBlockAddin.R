#' Insert Rmarkdown code block for grading
#'
#' Inserts a code block with knitr `output.class
#' with `knitr` chunk options to use HTML classes
#' "`grade-src`", "`grade-out`", "`grade-msg`", "`grade-warn`",
#' and "`grade-err`" for styling source code, output, messages,
#' warnings, and errors, respectively.
#'
#' @export
#'
insertGradeCommentAddin <- function() {
  doc <- rstudioapi::getSourceEditorContext()
  row <- rstudioapi::primary_selection(doc)$range$start['row']
  col <- rstudioapi::primary_selection(doc)$range$start['column']
  rstudioapi::setCursorPosition(c(row, Inf), id = doc$id)
  new_pos <- rstudioapi::primary_selection(doc)
  if (new_pos$range$start['column'] > 1) {
    rstudioapi::insertText(new_pos$start, "\n", doc$id)
    row <- row + 1
  }
  rstudioapi::insertText(
    location = c(row, 1),
    text = paste(
      "",
      "```{r }",
      "#| class.source: \"grade-src\"",
      "#| class.output: \"grade-out\"",
      "#| class.message: \"grade-msg\"",
      "#| class.warning: \"grade-warn\"",
      "#| class.error: \"grade-err\"",
      "",
      "```",
      "",
      sep = "\n"
    ),
    id = doc$id
  )
  rstudioapi::setCursorPosition(c(row + 8, 1), id = doc$id)
}
