


if (FALSE) {
  source("data-raw/hex-read.R")
  unifont_upper <- read_hex_Nx16("data-raw/unifont/unifont_upper-16.0.01.hex.gz")
  saveRDS(unifont_upper, "data-raw/unifont/unifont_upper-16.0.01.rds")
}


unifont_upper <- readRDS("data-raw/unifont/unifont_upper-16.0.01.rds")
