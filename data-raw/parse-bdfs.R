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



