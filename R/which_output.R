#' which_output
#'
#' @return values are text, latex, html, markdown_html markdown, pandoc, and rst
#' @export
#'
#' @examples
#'
#' which_output()
#'
which_output <- function() {
  in_opt <- get_opt("output")
  in_formats <- c("text", "markdown", "md", "pandoc", "rst", "html", "docx", "word", "latex")

  #pmatch(in_opt, in_formats, dup = TRUE)

  if (is.null(in_opt)) in_opt <- ""
  in_formats <- in_formats[pmatch(in_opt, in_formats)]

  in_formats <- switch(in_formats,
                       md = "markdown",
                       word = "docx",
                       in_formats)

  if (is.na(in_formats)) {
    out <- knitr:::out_format()

    if (is.null(out)) {
      # Das hier ist mein eigenes Ausgabe-Format.
      if (options()$prompt[1] == "HTML> ")  {
        in_formats <-   "html"
      }  else {
        in_formats <-   "text"
      }
    } else {
      in_formats <- knitr::pandoc_to()
      # if(in_formats=="html")  in_formats <- "markdown_html"
      # else if(in_formats=="beamer")  in_formats <- "latex"
      # else if(in_formats=="gfm")  in_formats <- "markdown_html"   #github_document
      # else if(in_formats=="gfm-yaml_metadata_block")  in_formats <- "markdown_html"   #github_document
      #
      in_formats <- switch(in_formats,
             html = "markdown_html",
             beamer = "latex",
             guess_in_format(in_formats)
              )
    }
  }
  in_formats[1]
}

guess_in_format <- function(x) {
  if (any(grepl("gfm", x))) "markdown_html"
  else "text"
}
