

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print summary information about a lofi font
#' @param x lofi font object
#' @param ... other arguments ignored
#' 
#' @return None
#' @examples
#' font <- get_lofi_font('unscii-8')
#' print(font)
#' @importFrom stats median
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.lofi <- function(x, ...) {
  
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
  
  type <- ifelse(inherits(x, 'lofi-bitmap'), 'bitmap', 'vector')
  
  msg <- sprintf(
    "[lofi %s font] %i x %i. %i codepoints.%s\n", 
    type,
    median(x$glyph_info$width), 
    x$line_height,
    nrow(x$glyph_info),
    width_txt
  )
  cat(msg)
  
  invisible(x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot a lofi raster
#' @param x lofi raster rendering
#' @param interpolate default: FALSE
#' @param ... extra arguments passed to \code{plot()}
#' @return None
#' @examples
#' ras <- bitmap_text_raster("Hi")
#' plot(ras)
#' @importFrom graphics par
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"plot.lofi-raster" <- function(x, interpolate = FALSE, ...) {
  oldpar <- graphics::par(mai = c(0, 0, 0, 0))
  on.exit(graphics::par(oldpar))
  class(x) <- setdiff(class(x), "lofi-raster")
  plot(x, interpolate = interpolate, ...)
  invisible(x)
}

