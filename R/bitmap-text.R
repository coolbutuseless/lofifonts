


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Internal: Common function for extracting data.frames from a lofi font
#' This works with both bitmap and vector fonts
#' 
#' @param text string
#' @param lofi lofi font object
#' @param dx,dy extra spacing offsets for each character
#' @param missing which character to use if any codepoint is not available in 
#'        this font
#' @return data.frame of coordinates/lines for this text
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lofi_text_coords <- function(text, lofi, dx, dy, missing) {
  
  stopifnot(inherits(lofi, 'lofi'))
  
  codepoints <- utf8ToInt(text)
  
  # Remove carriage returns and calculate lines
  is_cr      <- codepoints == 10
  line       <- cumsum(is_cr)[!is_cr]
  codepoints <- codepoints[!is_cr]
  linebreak  <- which(diff(line) > 0) + 1L
  
  idxs <- lofi$codepoint_to_idx[codepoints + 1L]
  
  if (anyNA(idxs)) {
    # Determine what char should be used for missing
    missing <- missing %||% lofi$default_codepoint %||% utf8ToInt('?')
    if (is.character(missing)) {
      missing <- utf8ToInt(missing)[[1]]
    }
    
    if (!missing %in% lofi$glyph_info$codepoint) {
      stop("Codepoint for missing glyphs is not part of this font! Codepoint = ", missing)  
    }
    
    idxs[is.na(idxs)] <- missing
  }
  
  
  glyphs  <- lofi$glyph_info[idxs, , drop = FALSE]
  starts  <- glyphs$row_start
  ends    <- glyphs$row_end  
  widths  <- glyphs$width    
  npoints <- glyphs$npoints  
  
  starts <- starts[npoints > 0]
  ends   <- ends  [npoints > 0]
  
  row_idxs <- mapply(seq.int, starts, ends, SIMPLIFY = FALSE)
  row_idxs <- unlist(row_idxs, recursive = FALSE, use.names = FALSE)
  
  res <- lofi$coords[row_idxs, ]
  if (is.raw(res$x)) {
    res$x <- as.integer(res$x)
    res$y <- as.integer(res$y)
  }
  
  # adjust widths if requested
  widths <- widths + as.integer(dx)
  
  # xoffset needs to reset to 0 after every linebreak
  xoffset     <- cumsum_cut(widths, linebreak)
  res$xoffset <- rep.int(xoffset, npoints)
  
  res$char_idx  <- rep.int(seq_along(idxs), npoints)
  res$codepoint <- rep.int(codepoints, npoints)
  res$x0        <- res$x
  res$y0        <- res$y
  res$line      <- rep.int(line, npoints)
  
  line_height <- lofi$line_height %||% (max(res$y0) + 1L)
  res$y <- res$y + (max(res$line) - res$line) * (line_height + as.integer(dy))
  
  res$x <- res$x + res$xoffset
  
  res
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a data.frame of pixel coordinate information of the rendered text
#'
#' @param text Single text string. Can include carriage returns to split text 
#'        over multiple lines.
#' @param font Name of bitmap font, or a 'lofi' font object.  Default: 'unifont'.
#'   Use \code{get_lofi_names('bitmap')} to retrieve a list of all valid
#'   bitmap fonts included in this package.  To create a 'lofi' font object
#'   use \code{\link{convert_bm_font_to_lofi}()}
#' @param dx Additional character spacing in the horizontal direction. Default: 0
#' @param dy Additional character spacing in the vertical direction i.e. between 
#'        rows of text. Default: 0
#' @param missing Codepoint to use if glyph not found in font. 
#'        Default: NULL means to use the default specified by the font internally.
#'        Otherwise it will default to the codepoint for '?'
#'
#' @return data.frame of coordinate information
#' \describe{
#'   \item{\code{char_idx}}{The index of the glyph within the provided \code{text} string}
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
bitmap_text_coords <- function(text, font = "unifont", dx = 0L, dy = 0L, missing = NULL) {
  
  stopifnot(length(text) == 1)
  
  if (nchar(text) == 0) {
    return(data.frame())
  }
  
  if (inherits(font, 'lofi')) {
    assert_lofi_bitmap(font)
    lofi <- font
  } else {
    lofi <- bitmap_fonts[[font]]
  }
  if (is.null(lofi)) {
    stop("No such bitmap font: ", font)
  }
  
  res <- lofi_text_coords(text, lofi = lofi, dx = dx, dy = dy, missing = missing)
  
  res <- res[, c('char_idx', 'codepoint', 'x', 'y', 'line', 'x0', 'y0')]
  class(res) <- c('lofi-bitmap-coords', 'tbl_df', 'tbl', 'data.frame')
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Internal: Convert a data.frame of (x,y) coords into a matrix
#'
#' @param df data.frame with x and y coords
#' @return matrix to hold the coords
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords_to_mat <- function(df) {

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
  mat <- mat[rev(seq(nrow(mat))), , drop = FALSE]
  
  mat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a binary matrix of the rendered text
#'
#' @inheritParams bitmap_text_coords
#' @param scale_matrix Integer size scale factor. Default: 1.  Must be an integer value >= 1.
#'        Scale up the matrix or raster result by this factor
#'
#' @return Binary matrix representation of the rendered text
#' @examples
#' bitmap_text_matrix('Hi')
#' @family bitmap text functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_text_matrix <- function(text, font = "unifont", dx = 0L, dy = 0L, scale_matrix = 1,
                               missing = NULL) {
  
  stopifnot(length(text) == 1)
  
  df <- bitmap_text_coords(text, font, dx = dx, dy = dy, missing = missing)
  mat <- coords_to_mat(df)  
  
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
#' @inheritParams bitmap_text_matrix
#' 
#' @return Raster image representation of the rendered text
#' @examples
#' ras <- bitmap_text_raster('Hi')
#' plot(ras)
#' @family bitmap text functions
#' @importFrom grDevices as.raster
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_text_raster <- function(text, font = "unifont", dx = 0L, dy = 0L, scale_matrix = 1, 
                               missing = NULL) {
  stopifnot(length(text) == 1)
  
  mat <- bitmap_text_matrix(text = text, font = font, scale_matrix = scale_matrix, dx = dx, dy = dy,
                            missing = missing)
  mat <- 1L - mat
  
  ras <- grDevices::as.raster(mat)
  class(ras) <- union('lofi-raster', class(ras))
  ras
}


