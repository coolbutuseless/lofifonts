
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
vector_text_coords <- function(text, font = 'gridfont_smooth', 
                               dx = 0L, dy = 0L, missing = utf8ToInt('?')) {

  
  stopifnot(length(text) == 1)
  
  if (nchar(text) == 0) {
    return(data.frame())
  }
  
  if (inherits(font, 'lofi')) {
    assert_lofi_vector(font)
    lofi <- font 
  } else {
    stopifnot(font %in% c('gridfont_smooth', 'gridfont', 'arcade', 'asteroids'))
    # arcade is only lower case. gridfont is only uppercase
    if (font %in% c('arcade', 'asteroids')) {
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
  class(res) <- c('lofi-vector-coords', 'tbl_df', 'tbl', 'data.frame')
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
#' @param scale_coords Scale factor for text rendering. Numeric value greater than zero.
#'        Default: 1
#' @param scale_matrix Integer size scale factor. Default: 1.  Must be an integer value >= 1.
#'        Scale up the matrix or raster result by this factor after rendering 
#'        the coordinates.
#' 
#' @return Binary matrix rendering of the font
#' @examples
#' vector_text_matrix("Hi")
#' @family vector text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_matrix <- function(text, font = 'gridfont_smooth', 
                               scale_coords = 1, scale_matrix = 1,
                               dx = NULL, dy = NULL, missing = utf8ToInt('?')) {
  
  stopifnot(length(text) == 1)
  
  if (is.null(dx) && scale_coords < 2) {
    dx <- 1L
  }
  if (is.null(dy) && scale_coords < 2) {
    dy <- 1L
  }
  
  dx <- dx %||% 0
  dy <- dy %||% 0
  
  
  df <- vector_text_coords(text = text, font = font, dx = dx, dy = dy, missing = missing)
  
  df$x <- df$x * scale_coords + 1L
  df$y <- df$y * scale_coords + 1L
  
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
  
  # Scale the rendered matrix
  if (scale_matrix > 1) {
    scale_matrix <- as.integer(scale_matrix)
    mat <- kronecker(mat, matrix(1L, scale_matrix, scale_matrix))
  }
  
  class(mat) <- union('lofi-matrix', class(mat))
  mat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a raster image of the rendered text
#' 
#' @inheritParams vector_text_matrix
#' @param fg,bg foreground and background colors. Default: black text on 
#'        a white background
#' 
#' @return Raster image of rendered text
#' @examples
#' ras <- vector_text_raster("big hi", fg = 'red')
#' plot(ras)
#' @family vector text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_raster <- function(text, font = 'gridfont_smooth', 
                               scale_coords = 10, scale_matrix = 1,
                               dx = NULL, dy = NULL, missing = utf8ToInt('?'),
                               bg = '#FFFFFF', fg = '#000000') {
  
  stopifnot(length(text) == 1)
  
  mat <- vector_text_matrix(text = text, font = font, 
                            scale_coords = scale_coords, 
                            scale_matrix = scale_matrix,
                            dx = dx, dy = dy, missing = missing)
  
  ras <- grDevices::as.raster(1L - mat)
  
  ii <- (ras == '#000000')
  ras[ ii] <- fg
  ras[!ii] <- bg
  
  class(ras) <- union('lofi-raster', class(ras))
  ras
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert a matrix to a native raster
# 
# Need to:
#   - transpose
#   - switch dimensions
#   - ensure it's an integer matrix
#   - has a class of nativeRaster
#
# This isn't especially fast.  Use nara::matrix_to_mr() if you want a bit more
# speed or have a pre-allocated nativeraster to copy into.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
matrix_to_nr_slow <- function(mat) {
  mat <- t(mat)
  dim(mat) <- rev(dim(mat))
  mode(mat) <- 'integer'
  class(mat) <- 'nativeRaster'
  
  mat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a nativeraster of the rendered text
#' 
#' @inheritParams vector_text_coords
#' @inheritParams bitmap_text_nativeraster
#' @param scale_coords Scale factor for text rendering. Numeric value greater than zero.
#'        Default: 1
#' @param scale_matrix Integer size scale factor. Default: 1.  Must be an integer value >= 1.
#'        Scale up the matrix or raster result by this factor after rendering 
#'        the coordinates.
#' 
#' @return Binary native raster rendering of the font
#' @examples
#' vector_text_nativeraster("Hi")
#' @family vector text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_nativeraster <- function(text, font = 'gridfont_smooth', 
                               scale_coords = 1, scale_matrix = 1,
                               dx = NULL, dy = NULL, missing = utf8ToInt('?'), 
                               bg = 0L, fg = -16777216L) {
  
  stopifnot(length(text) == 1)
  
  mat <- vector_text_matrix(text = text, font = font, 
                            scale_coords = scale_coords, 
                            scale_matrix = scale_matrix,
                            dx = dx, dy = dy, missing = missing)
  
  # Replace binary 0/1 with nativeRaster integer colors
  ii <- mat == 0
  mat[ ii] <- as.integer(bg)
  mat[!ii] <- as.integer(fg)
  
  # Convert from matrix to nativeraster
  nr <- matrix_to_nr_slow(mat)
  class(nr) <- union(class(nr), 'lofi-nativeraster')
  nr
}


