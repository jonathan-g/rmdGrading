#' Insert YAML block to include `grading.css`
#'
#' Adds a format block in the document header YAML to include
#' `grading.css` in HTML output.
#'
#' @export
#'
insertGradeCSSBlockAddin <- function() {
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
      "format:",
      "  html:",
      "  css: \"css/grading.css\"",
      "",
      sep = "\n"
    ),
    id = doc$id
  )
  rstudioapi::setCursorPosition(c(row + 5, 1), id = doc$id)
}

find_root <- function(path = ".") {
  project <- rstudioapi::getActiveProject()
  if (! is.null(project)) return(project)

  rstudio_root <- rprojroot::find_rstudio_root_file(path = path)
  if (!is.null(rstudio_root)) return(rstudio_root)

  quarto_root <- rprojroot::find_root(
    criterion = has_file("_quarto.yml"),
    path = path
  )
  if (! is.null(quarto_root)) return(quarto_root)

  qmd_root <- rprojroot::find_root(
    criterion = has_file_pattern("*.qmd"),
    path = path
  )
  if (! is.null(qmd_root)) return(qmd_root)

  rmd_root <- rprojroot::find_root(
    criterion = has_file_pattern("*.rmd"),
    path = path
  )
  if (! is.null(rmd_root)) return(rmd_root)

  return(NULL)
}

copy_css_files <- function(root = NULL, overwrite = FALSE) {
  if (is.null(root)) {
    root <- find_root(".")
  }
  if (! is.null(root) && dir.exists(root)) {
    css_dir <- file.path(root, "css")
    if (! dir.exists(css_dir)) {
      dir.create(css_dir)
    }
    src_dir <- system.file("css", package = "rmdGrading")
    if (length(src_dir) > 0) {
      for (sd in src_dir) {
        src_files <- list.files(sd, recursive = TRUE,
                                full.names = FALSE,
                                no.. = TRUE)
        for (s in src_files) {
          base <- basename(s)
          base_t <- file.path(css_dir, base)
          if (! dir.exists(base_t)) {
            dir.create(base_t)
          }
          file.copy(file.path(sd, s), file.path(css_dir, s),
                    overwrite = overwrite)
        }
      }
    }
  }
}

#' Sets up CSS for grading comments
#'
#' Creates a file `css/grading.css' in the project root directory and
#'   inserts YAML block at the cursor position.
#'
#' @export
#'
setupGradeCSSAddin <- function() {
  root <- find_root()
  if (! is.null(root)) {
    copy_css_files(root)
  } else {
    warning("No appropriate context for copying CSS files.")
  }
  ctx <- getSourceEditorContext()
  if (! is.null(ctx) && ! is.null(ctx$path) && ! nchar(ctx$path) > 0) {
    ext <- ctx$path |>
      stringr::str_match("\\.(?<ext>[^.]*)$")|>
      (\(x) {x['ext']})() |>
      stringr::str_to_lower()
    if (!is.null(ext) && !is.na(ext))
    if (ext %in% c('yml', 'rmd', 'qmd')) {
      insertGradeCSSBlockAddin()
    }
  }
}
