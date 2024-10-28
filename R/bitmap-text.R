
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a data.frame of pixel coordinate information of the rendered text
#'
#' @param text Single text string. Can include carriage returns.
#' @param font Name of bitmap font. One of the following:
#' \itemize{
#'   \item{spleen: "spleen-12x24", "spleen-16x32", "spleen-32x64", "spleen-5x8", "spleen-6x12", "spleen-8x16"}
#'   \item{tamzen: "Tamzen10x20b", "Tamzen10x20r", "Tamzen5x9b", "Tamzen5x9r", "Tamzen6x12b", "Tamzen6x12r", "Tamzen7x13b", "Tamzen7x13r", "Tamzen7x14b", "Tamzen7x14r", "Tamzen8x15b", "Tamzen8x15r", "Tamzen8x16b", "Tamzen8x16r"}
#'   \item{"unifomt" (the default)}
#'   \item{unscii: "unscii-8", "unscii-8-thin"}
#' }
#' @param line_height Integer value for the vertical distance between multiple lines
#'        of text.  Use this to override the font's lineheight.
#'        Default: NULL means to use the font's built-in lineheight.
#' @param missing Codepoint (integer) to use if glyph not found in font. 
#'        Default: NULL means to use the default specified by the font internally.
#'        Otherwise it will default to the codepoint for '?'
#'
#' @return data.frame of coordinate information
#' \describe{
#'   \item{\code{char_idx}}{The index of the character within the provided \code{text} string}
#'   \item{\code{codepoint}}{Unicode codepoint (integer)}
#'   \item{\code{x}}{Pixel coordinate x value for display}
#'   \item{\code{y}}{Pixel coordinate y value for display}
#'   \item{\code{line}}{Line number within input \code{text} where this character appears}
#'   \item{\code{x0}}{Original untransformed x-coordinate}
#'   \item{\code{y0}}{Original untransformed y-coordinate}
#' }
#' @examples
#' bitmap_text_coords('Hi')
#' @family bitmap text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_text_coords <- function(text, font = "unifont", line_height = NULL, missing = NULL) {
  
  stopifnot(length(text) == 1)
  
  if (!font %in% names(bitmaps)) {
    stop("No such bitmap font: ", font)
  }
  bitmap <- bitmaps[[font]]
  
  codes <- utf8ToInt(text)
  dfs   <- vector('list', length(codes))

  line_height <- line_height %||% bitmap$font_info$line_height
  yoffset     <- 0
  xoffset     <- 0

  line <- 1L
  
  for (i in seq_along(codes)) {
    code <- codes[i]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If it's a carriage return (i.e. \n) then recalculate the offsets
    # and go to the next character
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (code == 10) {
      xoffset <- 0
      yoffset <- yoffset - line_height
      line    <- line + 1L
      next
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get the character data for the given utf8 code
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    idx <- bitmap$idx[code + 1L]
    if (is.null(idx) || is.na(idx) || idx > length(bitmap$chars)) {
      if (is.character(missing)) {
        missing <- utf8ToInt(missing)[[1]]
      }
      code <- missing %||% bitmap$font_info$default_char %||% utf8ToInt('?') # Default to question mark
      idx <- bitmap$idx[code + 1L]
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get this character
    # Offset the x,y coords based upon the position of the character
    # add the coords data.frame to the list of all data.frames
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    this_bdf_char <- bitmap$chars[[idx]]
    this_df      <- this_bdf_char$coords
    this_df$x0   <- this_df$x
    this_df$y0   <- this_df$y
    # If this is a 'space' then often this will have no coords
    if (!is.null(this_df) && nrow(this_df) > 0) {
      this_df$x         <- this_df$x + xoffset
      this_df$y         <- this_df$y + yoffset
      this_df$codepoint <- code
      this_df$char_idx  <- i
      this_df$line      <- line
      dfs[[i]]          <- this_df
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Even if a character has no coordinates (e.g. 'space') always add the
    # character dwidth to the xoffset for positioning the next character
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xoffset <- xoffset + this_bdf_char$dwidth

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bind all the data.frames of coordinates together.
  # Shift the entire set of coordinates "up" so that it nominally starts
  # at y = 0
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res_df   <- do.call(rbind, dfs)
  res_df$y <- res_df$y - yoffset

  
  res_df <- res_df[, c('char_idx', 'codepoint', 'x', 'y', 'line', 'x0', 'y0')]
  
  
  class(res_df) <- c('tbl_df', 'tbl', 'data.frame')
  res_df
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a data.frame of (x,y) coords into a matrix
#'
#' @param df data.frame with x and y coords
#'
#' @return matrix to hold the coords
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords_to_mat <- function(df) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # y coords can sometimes be negative because of descenders/offsets
  # so push them all to be at least "1", so that (x,y) coords can be used
  # as matrix indices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (any(df$y < 1)) {
    df$y <- df$y + abs(min(df$y)) + 1L
  }
  if (any(df$x < 1)) {
    df$x <- df$x + abs(min(df$x)) + 1L
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a matrix of the appropriate size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  height <- max(df$y)
  width  <- max(df$x)
  mat    <- matrix(0L, height, width)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set all the pixel locations to 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat[ (df$x - 1) * height + df$y ] <- 1L

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Pixel coordinates have (x, y) on the bottom left, but matrices have
  # origin on the top-left, so invert the matrix in the 'y' direction so
  # that it comes out the right way up
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat[rev(seq(nrow(mat))), , drop = FALSE]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a binary matrix of the rendered text
#'
#' @inheritParams bitmap_text_coords
#' @param scale Integer size scale factor. Default: 1.  Must be an integer value >= 1.
#'        Scale up the matrix or raster result by this factor
#'
#' @return Binary matrix representation of the rendered text
#' @examples
#' bitmap_text_matrix('Hi')
#' @family bitmap text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_text_matrix <- function(text, font = "unifont", line_height = NULL, scale = 1,
                               missing = NULL) {
  
  stopifnot(length(text) == 1)
  
  scale <- as.integer(scale)
  stopifnot(scale >= 1)
  
  df <- bitmap_text_coords(text, font, line_height = line_height, missing = missing)
  mat <- coords_to_mat(df)  
  
  if (scale > 1) {
    mat <- kronecker(mat, matrix(1L, scale, scale))
  }
  
  mat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a raster image of the rendered text
#' 
#' @inheritParams bitmap_text_matrix
#' 
#' @return Raster image representation of the rendered text
#' @examples
#' ras <- bitmap_text_raster('Hi')
#' plot(ras, interpolate = FALSE)
#' @family bitmap text functions
#' @importFrom grDevices as.raster
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_text_raster <- function(text, font = "unifont", line_height = NULL, scale = 1, 
                               missing = NULL) {
  stopifnot(length(text) == 1)
  
  mat <- bitmap_text_matrix(text = text, font = font, scale = scale, line_height = line_height,
                            missing = missing)
  mat <- 1L - mat
  grDevices::as.raster(mat)
}


