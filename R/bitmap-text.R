


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Common funtion for extracting data.frames from a lofi font
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
  linebreak  <- c(which(diff(line) > 0), length(codepoints))
  
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
  
  
  glyphs <- lofi$glyph_info[idxs, , drop = FALSE]
  starts <- glyphs$row_start
  ends   <- glyphs$row_end  
  widths <- glyphs$width    
  lens   <- glyphs$npoints  
  
  row_idxs <- mapply(seq.int, starts, ends, SIMPLIFY = FALSE)
  row_idxs <- unlist(row_idxs, recursive = FALSE, use.names = FALSE)
  
  res <- lofi$coords[row_idxs, ]
  
  # adjust widths if requested
  widths <- widths + as.integer(dx)
  
  # xoffset needs to reset to 0 after every linebreak
  xoffset <- integer(0)
  linestart <- c(0L, linebreak[-length(linebreak)]) + 1L
  for (i in seq_along(linestart)) {
    if (linestart[i] == linebreak[i]) {
      xoffset <- c(xoffset, 0L)
    } else {
      this_offset <- cumsum(widths[seq(linestart[i], linebreak[i] - 1)])
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
  
  line_height <- lofi$line_height %||% (max(res$y0) + 1L)
  res$y <- res$y + (max(res$line) - res$line) * (line_height + as.integer(dy))
  
  res$x <- res$x + res$xoffset
  
  res
}


assert_lofi <- function(lofi) {
  stopifnot(exprs = {
    inherits(lofi, 'lofi')
     all(c("coords", "codepoint_to_idx", "line_height", "default_codepoint", "glyph_info") %in% names(lofi))
     
     is.data.frame(lofi$coords)
     nrow(lofi$coords) > 0
     
     is.atomic(lofi$codepoint_to_idx)
     length(lofi$codepoint_to_idx) > 0
     
     is.numeric(lofi$default_codepoint)
     length(lofi$default_codepoint) == 1
     
     is.numeric(lofi$line_height)
     length(lofi$line_height) == 1
     
     is.data.frame(lofi$glyph_info)
     all(c("codepoint", "npoints", "row_start", "row_end", "width") %in% colnames(lofi$glyph_info))
     nrow(lofi$glyph_info) > 0
  })
  TRUE
}

assert_lofi_vector <- function(lofi) {
  assert_lofi(lofi)
  stopifnot(exprs = {
    all(c("stroke_idx", "x", "y") %in% names(lofi$coords))
  })
  TRUE
}

assert_lofi_bitmap <- function(lofi) {
  assert_lofi(lofi)
  stopifnot(exprs = {
    all(c("x", "y") %in% names(lofi$coords))
  })
  TRUE
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
  class(res) <- c('tbl_df', 'tbl', 'data.frame')
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a data.frame of (x,y) coords into a matrix
#'
#' @param df data.frame with x and y coords
#' @return matrix to hold the coords
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords_to_mat <- function(df) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # y coords can sometimes be negative because of descenders/offsets
  # so push them all to be at least "1", so that (x,y) coords can be used
  # as matrix indices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  df <- df[!is.na(df$x) & !is.na(df$y),]
  
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
bitmap_text_matrix <- function(text, font = "unifont", dx = 0L, dy = 0L, scale = 1,
                               missing = NULL) {
  
  stopifnot(length(text) == 1)
  
  scale <- as.integer(scale)
  stopifnot(scale >= 1)
  
  df <- bitmap_text_coords(text, font, dx = dx, dy = dy, missing = missing)
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
bitmap_text_raster <- function(text, font = "unifont", dx = 0L, dy = 0L, scale = 1, 
                               missing = NULL) {
  stopifnot(length(text) == 1)
  
  mat <- bitmap_text_matrix(text = text, font = font, scale = scale, dx = dx, dy = dy,
                            missing = missing)
  mat <- 1L - mat
  grDevices::as.raster(mat)
}


