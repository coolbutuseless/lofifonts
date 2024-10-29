
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
  row_starts  <- c(1, head(row_ends, -1) + 1)
  
  idx_to_rows <- mapply(seq.int, row_starts, row_ends, SIMPLIFY = FALSE)
  
  widths <- vapply(font$chars, \(char) {
    char$coords$width[nrow(char$coords)]
  }, integer(1))
  coords$width <- NULL
  
  coords <- coords[, c('x', 'y')]
  
  list(
    font_info        = font$font_info,
    coords           = coords,
    codepoint_to_idx = font$idx,
    row_start        = row_starts,
    row_end          = row_ends,
    npoints          = lens,
    width            = widths
  )
}

bitmaps2 <- lapply(seq_along(bitmaps1), function(i) {
  print(names(bitmaps1)[[i]])
  compact_bitmap(bitmaps1[[i]])
})
names(bitmaps2) <- names(bitmaps1)
bitmaps <- bitmaps2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save all the font data for internal use only
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
usethis::use_data(
  bitmaps,
  arcade,
  gridfont, gridfont_smooth,
  internal = TRUE, overwrite = TRUE, compress = 'bzip2'
)
file.size("R/sysdata.rda")/1024/1024



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the vectors of codepoints for each bitmap font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdf_names

font_info <- list()
font_info$bitmap <- lapply(bitmaps, function(bdf) {
  cp <- which(!is.na(bdf$idx)) - 1L
  list(codepoints = sort(cp))
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract the codepoints from the vector fonts
# Assemble all the font information
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
font_info$vector <- list()
font_info$vector$arcade          <- list(codepoints = unique(arcade$codepoint))
font_info$vector$gridfont        <- list(codepoints = unique(gridfont$codepoint))
font_info$vector$gridfont_smooth <- list(codepoints = unique(gridfont_smooth$codepoint))

font_names <- list(
  bitmap = names(font_info$bitmap),
  vector = names(font_info$vector)
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save the font information for the user of the package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
usethis::use_data(
  font_info,
  font_names,
  internal = FALSE,
  overwrite = TRUE
)
