## code to prepare `DATASET` dataset goes here

source("data-raw/bdf-read.R")

bdf_files <- list.files("data-raw/fonts/", pattern = "bdf$", full.names = TRUE)
bdf_files

bdf_names <- basename(tools::file_path_sans_ext(bdf_files))
bdf_names[bdf_names == 'unifont-16.0.01'] <- 'unifont'


bitmaps1 <- lapply(bdf_files, function(f) {
  message(f)
  
  codepoint_from_startchar <- grepl("Tamzen", f)
  
  read_bdf(f, codepoint_from_startchar = codepoint_from_startchar)
  }) |>
  setNames(bdf_names)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BDF yoffset. Should be a single value across the fonts i'm packaging
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_bdf_yoff <- function(bdf_file) {
  lns <- readLines(bdf_file)
  lns <- grep("BBX", lns, value = TRUE)
  lns <- sort(unique(lns))
  yoff <- strsplit(lns, " ")
  yoff <- vapply(yoff, \(x) x[5], character(1))
  yoff <- abs(unique(as.integer(yoff)))
  
  if (length(yoff) != 1) {
    stop("Bad yoff:", bdf_file, deparse1(yoff))
  }
  
  yoff
}

yoffs <- vapply(bdf_files, get_bdf_yoff, integer(1)) |>
  setNames(tolower(bdf_names))
