

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print summary information about a lofi font
#' @param x lofi font object
#' @param ... other arguments ignored
#' 
#' @importFrom stats median
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.lofifont <- function(x, ...) {
  cat("[lofi font]\n")
  cat("  codepoints : ", nrow(x$glyph_info), "\n")
  cat("  num coords : ", nrow(x$coords), "\n")
  cat(
    "  width      : ", 
    min   (x$glyph_info$width), 
    median(x$glyph_info$width), 
    max   (x$glyph_info$width), 
    "(min/median/max)\n"
  )
  cat("  line height: ", x$line_height, "\n")
  invisible(x)
}

