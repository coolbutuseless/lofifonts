
source("data-raw/bdf-read.R")

source("data-raw/parse-arcade.R")
source("data-raw/parse-bdfs.R")
source("data-raw/parse-gridfont.R")
source("data-raw/parse-unscii.R")
source("data-raw/parse-unifont-hex.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add the  Supplementary Multilingual Plane to the 
#   Basic Multilingual Plane
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
idx1 <- bitmaps1$unifont$idx
idx2 <- unifont_upper$idx

# renumber the index to start after the BDF unifont reading
# And we'll remove the codepoint '32' that's part of the upper font.
idx <- idx2 - 1L + length(bitmaps1$unifont$chars)

# copy the Plane0 indices
idx[seq_along(idx1)] <- idx1

chars <- unifont_upper$chars[-1]
length(chars)
length(unifont_upper$chars)

bitmaps1$unifont$idx <- idx
bitmaps1$unifont$chars <- c(bitmaps1$unifont$chars, chars)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add in unscii to the bitmaps
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmaps1[['unscii-8']]       <- unscii_8
bitmaps1[['unscii-8-thin']]  <- unscii_8_thin




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reshape bitmap fonts to a single data.frame with index offsets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compact_bitmap <- function(font) {
  # font   <- bitmaps[['unscii-8']]
  dfs    <- lapply(font$chars, \(x) x$coords)
  lens   <- vapply(dfs, nrow, integer(1))
  coords <- do.call(rbind, dfs)
  
  row_ends    <- cumsum(lens)
  row_starts  <- c(1L, head(row_ends, -1) + 1L)
  
  idx_to_rows <- mapply(seq.int, row_starts, row_ends, SIMPLIFY = FALSE)
  
  widths <- vapply(font$chars, \(char) {
    char$coords$width[nrow(char$coords)]
  }, integer(1))
  coords$width <- NULL
  
  coords <- coords[, c('x', 'y')]
  
  glyph_info <- data.frame(
    codepoint  = which(!is.na(font$idx)) - 1L,
    npoints    = lens,
    row_start  = row_starts,
    row_end    = row_ends,
    width      = widths
  )
  row.names(glyph_info) <- NULL
  
  
  default_char <- font$font_info$default_char
  if (is.null(default_char)) {
    default_char <- utf8ToInt('?')
  }
  if (is.character(default_char)) {
    default_codepoint <- utf8ToInt(default_char)
  } else {
    default_codepoint <- as.integer(default_char)
  }  
  
  class(coords) <- c('tbl_df', 'tbl', 'data.frame')
  
  res <- list(
    coords            = coords,
    codepoint_to_idx  = font$idx,
    line_height       = font$font_info$line_height,
    default_codepoint = default_codepoint,
    glyph_info        = glyph_info
  )
  class(res) <- c('lofi', 'lofi-bitmap')
  res
}

bitmaps2 <- lapply(seq_along(bitmaps1), function(i) {
  print(names(bitmaps1)[[i]])
  compact_bitmap(bitmaps1[[i]])
})
names(bitmaps2) <- tolower(names(bitmaps1))
bitmap_fonts <- bitmaps2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# insert yoffsets 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (nm in names(bitmap_fonts)) {
  bitmap_fonts[[nm]]$name <- nm
  if (nm %in% names(yoffs)) {
    bitmap_fonts[[nm]]$baseline_offset <- yoffs[[nm]]
  } else {
    bitmap_fonts[[nm]]$baseline_offset <- 0L
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Assert no zeros or negatives in coords
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (lofi in bitmap_fonts) {
  xmin <- min(lofi$coords$x, na.rm = TRUE)
  ymin <- min(lofi$coords$y, na.rm = TRUE)
  cat(xmin, ymin, "\n")
  stopifnot(xmin > 0)
  stopifnot(ymin > 0)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assemble vector font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font <- arcade

vector_font_compact <- function(font) {
  codepoints <- unique(font$codepoint)
  row_idx <- lapply(codepoints, \(codepoint) {
    unname(which(font$codepoint == codepoint))
  })
  idx <- integer(0)
  idx[codepoints + 1] <- seq_along(codepoints)
  
  widths <- subset(font, stroke_idx == 1 & point_idx == 1)
  
  height <- font$height[1]
  
  font$width  <- NULL
  font$height <- NULL
  
  npoints <- rle(font$codepoint)$lengths
  npoints <- unname(npoints)
  
  row_starts <- vapply(row_idx, min, integer(1))
  row_ends   <- vapply(row_idx, max, integer(1))
  
  glyph_info <- data.frame(
    codepoint = codepoints,
    npoints   = npoints,
    row_start = row_starts,
    row_end   = row_ends,
    width     = widths$width
  )
  row.names(glyph_info) <- NULL
  
  font$codepoint <- NULL
  font$point_idx <- NULL
  
  class(font) <- c('tbl_df', 'tbl', 'data.frame')
  
  res <- list(
    coords            = font,
    codepoint_to_idx  = idx,
    line_height       = height,
    default_codepoint = utf8ToInt('?'),
    glyph_info        = glyph_info,
    baseline_offset   = 0
  )
  class(res) <- c('lofi', 'lofi-vector')
  res
}

vector_fonts <- list(
  arcade          = vector_font_compact(arcade),
  gridfont        = vector_font_compact(gridfont),
  gridfont_smooth = vector_font_compact(gridfont_smooth)
)

vector_fonts$arcade$baseline_offset <- 3
vector_fonts$gridfont$baseline_offset <- 3
vector_fonts$gridfont_smooth$baseline_offset <- 3


for (nm in names(vector_fonts)) {
  vector_fonts[[nm]]$name <- nm
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove some small floating point negative values e.g. -2.1e-17
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- vector_fonts$gridfont_smooth$coords$x
small_negs <- which(!is.na(x) & x < 0)
vector_fonts$gridfont_smooth$coords$x[small_negs] <- 0


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Assert no negatives in vector coords
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (lofi in vector_fonts) {
  xmin <- min(lofi$coords$x, na.rm = TRUE)
  ymin <- min(lofi$coords$y, na.rm = TRUE)
  cat(xmin, ymin, "\n")
  stopifnot(xmin >= 0)
  stopifnot(ymin >= 0)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save all the font data for internal use only
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
usethis::use_data(
  bitmap_fonts,
  vector_fonts,
  internal = TRUE, overwrite = TRUE, compress = 'bzip2'
)
# file.size("R/sysdata.rda")/1024/1024

