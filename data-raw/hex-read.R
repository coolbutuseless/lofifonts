

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Byte to coordinate lookup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xs8 <- lapply(0:255, \(i) {
  which(rev(intToBits(i)[1:8]) > 0)
}) |> setNames(
  toupper(sprintf("%02x", 0:255))
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Byte to coordinate lookup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xs16 <- lapply(0:65535, \(i) {
  which(rev(intToBits(i)[1:16]) > 0)
}) |> setNames(
  toupper(sprintf("%04x", 0:65535))
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert 8 byte hex to a data.frame of coordinates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_8x8_to_coords <- function(hex, codepoint) {
  hex <- stringr::str_sub(hex, seq(1, 15, 2), seq(2, 16, 2))
  coords <- lapply(8:1, \(i) {
    x <- xs8[[hex[[i]]]]
    if (length(x) == 0) {
      data.frame(codepoint = integer(0), x = integer(0), y = integer(0))
    } else {
      data.frame(codepoint = codepoint, x = x          , y = 9L - i)
    }
  }) 
  char_coords <- do.call(rbind, coords)
  if (nrow(char_coords) > 0) {
    char_coords$width <- 0L
    char_coords$width[nrow(char_coords)] <- 8L
  }
  
  char_coords
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert 16 byte hex to a data.frame of coordinates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_8x16_to_coords <- function(hex, codepoint) {
  
  hex <- stringr::str_sub(hex, seq(1, 31, 2), seq(2, 32, 2))
  coords <- lapply(16:1, \(i) {
    x <- xs8[[hex[[i]]]]
    if (length(x) == 0) {
      data.frame(codepoint = integer(0), x = integer(0), y = integer(0))
    } else {
      data.frame(codepoint = codepoint, x = x, y = 17L - i)
    }
  }) 
  char_coords <- do.call(rbind, coords)
  if (nrow(char_coords) > 0) {
    char_coords$width <- 0L
    char_coords$width[nrow(char_coords)] <- 8L
  }
  
  char_coords
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert 32 byte hex (a 16x16bit character) to a data.frame of coordinates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_16x16_to_coords <- function(hex, codepoint) {
  
  hexwords <- stringr::str_sub(hex, seq(1, 63, 4), seq(4, 64, 4))
  coords <- lapply(16:1, \(i) {
    x <- xs16[[hexwords[[i]]]]
    if (length(x) == 0) {
      data.frame(codepoint = integer(0), x = integer(0), y = integer(0))
    } else {
      data.frame(codepoint = codepoint, x = x, y = 17L - i)
    }
  }) 
  char_coords <- do.call(rbind, coords)
  if (nrow(char_coords) > 0) {
    char_coords$width <- 0L
    char_coords$width[nrow(char_coords)] <- 16L
  }
  
  char_coords
}


if (FALSE) {
  code <- '01000E'
  hex <- '000000000000700008000800082008200FFC0924092409240924092400000000'
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read an 8x8 hex font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_hex_8x8 <- function(hex_file) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load hexfile and split codepoint from hex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hex_raw  <- readLines(hex_file) 
  hex_raw  <- strsplit(hex_raw, ":")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert codes to index into 'chars'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  codes <- vapply(hex_raw, \(x) x[1], character(1))
  codes <- strtoi(codes, base = 16)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert hex to data.frames
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chars <- lapply(seq_along(hex_raw), \(i) {
    codepoint <- codes[[i]]
    hex <- hex_raw[[i]][2]
    list(
      coords = hex_8x8_to_coords(hex, codepoint = codepoint),
      dwidth = 8L
    )
  })
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create code -> index lookup
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  max_code <- max(codes)
  idx <- rep(NA_integer_, max_code+1L)
  
  idx[codes + 1] <- seq_along(codes)
  
  list(
    chars = chars, 
    idx   = idx,
    font_info = list(
      line_height = 9L
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reshape font to a single data.frame with index offsets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_hex_8x8_redux <- function(hex_file) {
  font <- read_hex_8x8(hex_file)
  
  font <- bitmaps$`unscii-8`
  dfs <- lapply(font$chars, \(x) x$coords)
  lens <- vapply(dfs, nrow, integer(1))
  
}








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Readh hex 8x16 font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_hex_8x16 <- function(hex_file) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load hexfile and split codepoint from hex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hex_raw  <- readLines(hex_file) 
  hex_raw  <- strsplit(hex_raw, ":")
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert codes to index into 'chars'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  codes <- vapply(hex_raw, \(x) x[1], character(1))
  codes <- strtoi(codes, base = 16)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert hex to data.frames
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chars <- lapply(seq_along(hex_raw), \(i) {
    codepoint <- codes[i]
    hex <- hex_raw[[i]][2]
    list(
      coords = hex_8x16_to_coords(hex, codepoint = codepoint),
      dwidth = 8L
    )
  })
  
  # print(codes)
  max_code <- max(codes)
  # print(max_code)
  idx <- rep(NA_integer_, max_code+1L)
  
  idx[codes + 1] <- seq_along(codes)
  
  list(
    chars = chars, 
    idx   = idx,
    font_info = list(
      line_height = 16L
    )
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Readh hex font which is 16 high, but may be multiple widths wide
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_hex_Nx16 <- function(hex_file) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load hexfile and split codepoint from hex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hex_raw  <- readLines(hex_file) 
  hex_raw  <- strsplit(hex_raw, ":")
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert codes to index into 'chars'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  codes <- vapply(hex_raw, \(x) x[1], character(1))
  codes <- strtoi(codes, base = 16)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert hex to data.frames
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  idxs <- seq_along(hex_raw)
  # idxs <- 1:20
  
  chars <- lapply(idxs, \(i) {
    codepoint <- codes[i]
    hex <- hex_raw[[i]][2]
    
    if (nchar(hex) == 32) {
      list(
        coords = hex_8x16_to_coords(hex, codepoint = codepoint),
        dwidth = 8L
      )
    } else if (nchar(hex) == 64) {
      list(
        coords = hex_16x16_to_coords(hex, codepoint = codepoint),
        dwidth = 16L
      )
    } else {
      stop("Unknown hex length: ", nchar(hex), " ", hex)
    }
  })
  
  # print(codes)
  max_code <- max(codes)
  # print(max_code)
  idx <- rep(NA_integer_, max_code+1L)
  
  idx[codes + 1] <- seq_along(codes)
  
  list(
    chars = chars, 
    idx   = idx,
    font_info = list(
      line_height = 16L
    )
  )
}



if (FALSE) {
  
  unifont_upper <- read_hex_Nx16("data-raw/unifont/unifont_upper-16.0.01.hex.gz")
  
  
}


