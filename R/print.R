

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print summary information about a lofi font
#' @param x lofi font object
#' @param ... other arguments ignored
#' 
#' @importFrom stats median
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.lofifont <- function(x, ...) {
  
  widths <- sort(unique(x$glyph_info$width))
  
  if (length(widths) == 1) {
    width_txt <- ''
  } else {
    width_txt <- sprintf(
      "  Width min/median/max = %i, %i, %i",
      min   (x$glyph_info$width), 
      median(x$glyph_info$width), 
      max   (x$glyph_info$width)
    )
  }
  
  
  msg <- sprintf(
    "[lofifont] %i x %i. %i codepoints.%s\n", 
    median(x$glyph_info$width), 
    x$line_height,
    nrow(x$glyph_info),
    width_txt
  )
  cat(msg)
  
  invisible(x)
}

