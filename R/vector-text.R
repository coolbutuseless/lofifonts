
globalVariables(c('x', 'xoffset', 'stroke_idx', 'point_idx'))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create data.frame of glyph information for the given text.
#'
#' Text input can contain multiple lines separated by carriage returns
#'
#' @param text Single text string. Can include carriage returns.
#' @param font Name of vector font. One of 
#' \code{c("arcade", "gridfont", "gridfont_smooth")}
#' @param dx Additional character spacing in the horizontal direction. Default: 0
#' @param dy Additional character spacing in the vertical direction. Default: 0
#' @param missing Codepoint to use if glyph not available in font. default: Codepoint
#'        for '?'
#'
#' @return data.frame of stroke information
#' \describe{
#'   \item{\code{char_idx}}{The index of the character within the provided \code{text} string}
#'   \item{\code{codepoint}}{Unicode codepoint (integer)}
#'   \item{\code{stroke_idx}}{Index of the stroke within each character}
#'   \item{\code{point_idx}}{Index of the point within each stroke}
#'   \item{\code{x}}{Pixel coordinate x value for display}
#'   \item{\code{y}}{Pixel coordinate y value for display}
#'   \item{\code{width}}{Width of vector character}
#'   \item{\code{height}}{Height of vector character}
#'   \item{\code{x0}}{Original untransformed x-coordinate}
#'   \item{\code{y0}}{Original untransformed y-coordinate}
#'   \item{\code{line}}{Line number within input \code{text} where this character appears}
#' }
#' @examples
#' vector_text_coords('Hi')
#' @family vector text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_coords <- function(text, font = c('gridfont', 'gridfont_smooth', 'arcade'), 
                               dx = 0, dy = 0, missing = utf8ToInt('?'), line_height = NULL) {

  
  stopifnot(length(text) == 1)
  
  if (nchar(text) == 0) {
    return(data.frame())
  }
  
  # arcade is only lower case. gridfont is only uppercase
  if (font == 'arcade') {
    text <- toupper(text)
  } else {
    text <- tolower(text)
  }
  
  
  vector <- vectors[[font]]
  if (is.null(vector)) {
    stop("No such vector font: ", font)
  }
  
  codepoints <- utf8ToInt(text)
  
  # Remove carriage returns and calculate lines
  is_cr      <- codepoints == 10
  line       <- cumsum(is_cr)[!is_cr]
  codepoints <- codepoints[!is_cr]
  linebreak  <- c(which(diff(line) > 0), length(codepoints))
  
  idxs <- vector$codepoint_to_idx[codepoints + 1L]
  
  # Determine what char should be used for missing
  if (is.character(missing)) {
    missing <- utf8ToInt(missing)[[1]]
  }
  missing <- missing %||% vector$font_info$default_char %||% utf8ToInt('?')
  
  idxs[is.na(idxs)] <- missing
  
  widths   <- vector$width  [idxs]
  lens     <- vector$npoints[idxs]
  row_idxs <- vector$rows   [idxs]
  row_idxs <- unlist(row_idxs, recursive = FALSE, use.names = FALSE)
  
  res <- vector$coords[row_idxs, ]
  
  # xoffset needs to reset to 0 after every linebreak
  xoffset <- integer(0)
  linestart <- c(0L, linebreak[-length(linebreak)]) + 1L
  for (i in seq_along(linestart)) {
    if (linestart[i] == linebreak[i]) {
      xoffset <- c(xoffset, 0L)
    } else {
      this_offset <- cumsum(widths[seq(linestart[i] + 1, linebreak[i])])
      xoffset <- c(xoffset, 0L, this_offset)
    }
  }
  xoffset
  res$xoffset <- rep.int(xoffset, lens)
  
  
  res$char_idx  <- rep.int(seq_along(idxs), lens)
  res$codepoint <- rep.int(codepoints, lens)
  res$x0        <- res$x
  res$y0        <- res$y
  res$line      <- rep.int(line, lens)
  
  line_height <- line_height %||% vector$font_info$line_height
  res$y <- res$y + (max(res$line) - res$line) * line_height
  
  res$x <- res$x + res$xoffset
  
  res <- res[, c('char_idx', 'codepoint', 'stroke_idx', 'point_idx', 'x', 'y', 'line', 'x0', 'y0')]
  class(res) <- c('tbl_df', 'tbl', 'data.frame')
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw a line on a matrix with bresenham
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
line <- function(mat, x1, y1,  x2,  y2) {
  
  xdelta <- abs(x2 - x1)
  ydelta <- abs(y2 - y1)
  
  if (xdelta > ydelta) {
    x <- x1:x2
    y <- seq(y1, y2, length.out = length(x))
  } else {
    y <- y1:y2
    x <- seq(x1, x2, length.out = length(y))
  }

  x <- as.integer(round(x))
  y <- as.integer(round(y))
  y <- nrow(mat) - y + 1L
  mat[cbind(y, x)] <- 1L

  mat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a binary matrix of the rendered text
#' 
#' @inheritParams vector_text_coords
#' @param scale Scale factor for text rendering. Numeric value greater than zero.
#'        Default: 1
#' 
#' @return Binary matrix rendering of the font
#' @examples
#' vector_text_matrix("Hi")
#' @family vector text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_matrix <- function(text, font = c('gridfont', 'gridfont_smooth', 'arcade'), 
                               scale = 1, dx = NULL, dy = NULL, missing = utf8ToInt('?')) {
  
  stopifnot(length(text) == 1)
  font <- match.arg(font)
  
  if (is.null(dx) && scale < 2) {
    dx <- 1L
  }
  if (is.null(dy) && scale < 2) {
    dy <- 1L
  }
  
  dx <- dx %||% 0
  dy <- dy %||% 0
  
  
  df <- vector_text_coords(text = text, font = font, dx = dx, dy = dy, missing = missing)
  
  df <- df[!is.na(df$x) & !is.na(df$y), ]
  df$x <- df$x * scale + 1L
  df$y <- df$y * scale + 1L
  
  width  <- max(df$x)
  height <- max(df$y)
  
  mat <- matrix(0L, nrow = height, ncol = width)
  
  df$j <- with(df, interaction(char_idx, stroke_idx, drop = TRUE))
  strokes <- split(df, df$j)
  
  for (stroke in strokes) {
    for (i in seq_len(nrow(stroke) - 1)) {
      mat <- line(mat, stroke$x[i], stroke$y[i], stroke$x[i + 1L], stroke$y[i + 1L])
    }
  }
  
  
  mat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a raster image of the rendered text
#' 
#' @inheritParams vector_text_matrix
#' 
#' @return Raster image of rendered text
#' @examples
#' ras <- vector_text_raster("Hi")
#' plot(ras, interpolate = FALSE)
#' @family vector text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_raster <- function(text, font = c('gridfont', 'gridfont_smooth', 'arcade'), 
                               scale = 10, dx = NULL, dy = NULL, missing = utf8ToInt('?')) {
  
  stopifnot(length(text) == 1)
  font <- match.arg(font)
  
  mat <- vector_text_matrix(text = text, font = font, scale = scale, dx = dx, dy = dy, missing = missing)
  as.raster(1L - mat)
}


