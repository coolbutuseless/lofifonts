
globalVariables(c('x', 'xoffset', 'stroke_idx', 'point_idx'))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create data.frame of glyph information for the given line of text.
#'
#' @inheritParams vector_text_coords
#' @param missing char to use if codepoint is missing
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
#'
#' @importFrom utils head
#' @family vector text functions
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_coords_single_row <- function(text, font, dx = 0, missing) {

  stopifnot(length(text) == 1)

  if (nchar(text) == 0) {
    return(data.frame())
  }

  if (is.character(missing)) {
    missing <- utf8ToInt(missing)[[1]]
  }

  font_df <- switch(
    font,
    gridfont        = gridfont,
    gridfont_smooth = gridfont_smooth,
    arcade          = arcade,
    stop("No such font: ", font)
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # split text into characters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (font == 'arcade') {
    text <- toupper(text)
  } else {
    text <- tolower(text)
  }
  
  codepoint <- utf8ToInt(text)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Replace any unknown chars with a blank space
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bad_idx <- !(codepoint %in% font_df$codepoint)
  if (length(bad_idx) > 0) {
    if (!missing %in% font_df$codepoint) {
      missing <- utf8ToInt('?')
    }
    codepoint[bad_idx] <- missing
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Merge the text info with the data.frame for each character from `arcade_df`
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- data.frame(
    codepoint = codepoint,
    char_idx  = seq_along(codepoint),
    stringsAsFactors = FALSE
  )

  string_df <- merge(string_df, font_df, sort = FALSE, all.x = TRUE)
  string_df$x0 <- string_df$x
  string_df$y0 <- string_df$y

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure correct ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- with(string_df, string_df[order(char_idx, stroke_idx, point_idx),])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the character offset
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  width_df <- subset(string_df, stroke_idx == 1 & point_idx == 1)
  width_df <- width_df[, c('char_idx', 'width')]
  width_df$xoffset <- c(0, cumsum(head(width_df$width, -1) + dx))
  width_df$width <- NULL

  string_df <- merge(string_df, width_df, sort = FALSE, all.x = TRUE)
  string_df <- transform(string_df, x = x + xoffset)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure correct ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- with(string_df, string_df[order(char_idx, stroke_idx, point_idx),])
  string_df$xoffset <- NULL

  string_df
}



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
                               dx = 0, dy = 0, missing = utf8ToInt('?')) {

  stopifnot(length(text) == 1)
  font <- match.arg(font)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the text at "\n" boundaries
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  texts  <- strsplit(text, "\n")[[1]]
  nchars <- cumsum(nchar(texts))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a string for each line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dfs <- lapply(texts, vector_text_coords_single_row, font = font, dx = dx, missing = missing)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update line numbering and character indices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(dfs)) {
    if (nrow(dfs[[i]]) == 0) next
    dfs[[i]]$line     <- i
    if (i > 1) {
      dfs[[i]]$char_idx <- dfs[[i]]$char_idx + nchars[i-1]
    }
  }

  font_df <- switch(
    font,
    gridfont        = gridfont,
    gridfont_smooth = gridfont_smooth,
    arcade          = arcade,
    stop("No such font: ", font)
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # combined all data.frames for each line, offset the y for each line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- do.call(rbind, dfs)
  res$y <- res$y - (res$line - 1) * (font_df$height[1] + dy)

  # Reposition so that bottom of text is (1, 1)
  res$y <- res$y - min(res$y, na.rm = TRUE)
  
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


