

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a 'bittermelon' 'bm_font' to a lofi font 
#' 
#' @param font font object of class \code{bm_font}
#' @return lofi font object
#' @examplesIf interactive() && requireNamespace('bittermelon', quietly = TRUE)
#' filename <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' bmfont <- bittermelon::read_hex(filename)
#' lofi <- convert_bm_font_to_lofi(bmfont)
#' lofi
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_bm_font_to_lofi <- function(font) {
  
  if (!inherits(font, "bm_font")) {
    stop("Must be a 'bm_font' object")
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse codepoints from the names in the font
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  codepoints <- names(font) 
  codepoints <- gsub("U\\+", "", codepoints, ignore.case = TRUE)
  codepoints <- strtoi(codepoints, base = 16)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a mapping from codepoint to position within the font
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  idx <- integer(0)
  idx[codepoints + 1L] <- seq_along(codepoints)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a data.frame of coordinates for each font
  # Also parse out the width 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dfs <- lapply(seq_along(font), function(i) {
    # print(i)
    char <- font[[i]]
    mat <- unclass(char)
    # coords <- arrayInd(as.logical(mat), dim(mat))
    coords <- arrayInd(which(as.logical(mat)), dim(mat))
    coords <- coords[, 2:1, drop = FALSE]
    coords <- as.data.frame(coords)
    names(coords) <- c('x', 'y')
    
    if (nrow(coords) == 0) {
      coords <- data.frame(x = NA_integer_, y = NA_integer_)
    }
    
    coords$width <- ncol(mat)
    coords
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # How many points in each character?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  npoints <- vapply(dfs, nrow, integer(1))
  npoints <- unname(npoints)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine all coordinates
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- do.call(rbind, dfs)
  coords <- coords[, c('x', 'y')]
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # At which rows in the data.frame does a given codepoint start/finish
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  row_ends    <- cumsum(npoints)
  row_starts  <- row_ends[-length(row_ends)] + 1L
  row_starts  <- c(1L, row_starts)
  
  row_starts <- unname(row_starts)
  row_ends   <- unname(row_ends)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Width of each character
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  widths <- vapply(dfs, function(df) {
    df$width[1]
  }, integer(1))
  widths <- unname(widths)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LineHeight = max font height
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  line_height <- nrow(font[[1]])
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # A data.frame of information about each glyph
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  glyphs <- data.frame(
    codepoint        = codepoints,
    npoints          = npoints,
    row_start        = row_starts,
    row_end          = row_ends,
    width            = widths
  )
  row.names(glyphs) <- NULL
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Full font spec
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lofi <- list(
    coords            = coords,
    codepoint_to_idx  = idx,
    line_height       = line_height,
    default_codepoint = utf8ToInt('?'),
    glyph_info        = glyphs
  )
  
  class(lofi) <- c('lofifont', 'lofifont-bitmap')
  lofi
}



if (FALSE) {
  filename <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
  bmfont <- bittermelon::read_hex(filename)
  
  filename <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon", mustWork = TRUE)
  bmfont <- bittermelon::read_yaff(filename)
  
  lofi <- convert_bm_font_to_lofi(bmfont)
  lofi
  
  # grid::grid.newpage()
  
  bitmap_text_raster('hello', font = lofi) |>
    plot(interpolate = FALSE)
  
  
  df <- bitmap_text_coords('hello', font = lofi) 
}

