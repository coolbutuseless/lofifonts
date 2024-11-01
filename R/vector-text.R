
globalVariables(c('x', 'xoffset', 'stroke_idx'))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create data.frame of glyph information for the given text.
#'
#' Text input can contain multiple lines separated by carriage returns
#'
#' @inheritParams bitmap_text_coords
#' 
#' @param font Name of vector font, or a vector 'lofi' font object.  Default: 'gridfont_smooth'.
#'   Use \code{get_lofi_names('vector')} to retrieve a list of all valid
#'   vector fonts included in this package.  
#'
#' @return data.frame of stroke information
#' \describe{
#'   \item{\code{char_idx}}{The index of the character within the provided \code{text} string}
#'   \item{\code{codepoint}}{Unicode codepoint (integer)}
#'   \item{\code{stroke_idx}}{Index of the stroke within each character}
#'   \item{\code{x}}{Pixel coordinate x value for display}
#'   \item{\code{y}}{Pixel coordinate y value for display}
#'   \item{\code{line}}{Line number within input \code{text} where this character appears}
#'   \item{\code{x0}}{Original untransformed x-coordinate}
#'   \item{\code{y0}}{Original untransformed y-coordinate}
#' }
#' @examples
#' vector_text_coords('Hi')
#' @family vector text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_coords <- function(text, font = c('gridfont_smooth', 'gridfont', 'arcade'), 
                               dx = 0L, dy = 0L, missing = utf8ToInt('?')) {

  
  stopifnot(length(text) == 1)
  
  if (nchar(text) == 0) {
    return(data.frame())
  }
  
  if (inherits(font, 'lofi')) {
    assert_lofi_vector(font)
    lofi <- font 
  } else {
    font <- match.arg(font)
    # arcade is only lower case. gridfont is only uppercase
    if (font == 'arcade') {
      text <- toupper(text)
    } else {
      text <- tolower(text)
    }
    
    lofi <- vector_fonts[[font]]
  }
  
  
  if (is.null(lofi)) {
    stop("No such vector font: ", font)
  }
  
  
  res <- lofi_text_coords(text, lofi = lofi, dx = dx, dy = dy, missing = missing)
  
  res <- res[, c('char_idx', 'codepoint', 'stroke_idx', 'x', 'y', 'line', 'x0', 'y0')]
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
vector_text_matrix <- function(text, font = c('gridfont_smooth', 'gridfont', 'arcade'), 
                               scale = 1, dx = NULL, dy = NULL, missing = utf8ToInt('?')) {
  
  stopifnot(length(text) == 1)
  
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
vector_text_raster <- function(text, font = c('gridfont_smooth', 'gridfont', 'arcade'), 
                               scale = 10, dx = NULL, dy = NULL, missing = utf8ToInt('?')) {
  
  stopifnot(length(text) == 1)
  
  mat <- vector_text_matrix(text = text, font = font, scale = scale, dx = dx, dy = dy, missing = missing)
  as.raster(1L - mat)
}


