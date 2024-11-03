
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Lagged cumulative sum which resets at the given cut points
#' 
#' Author: June Choe
#' 
#' @param x Vector of values. 
#' @param cut Vector of cut points. Usually integer. Must be sorted.
#' @return Vector of lagged cumulative sum with sum resetting to zero at
#'         the cut points
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cumsum_cut <- function(x, cut) {
  cut <- c(1L, cut) # 'cut' will always be sorted
  lagged_cumsum <- c(0L, cumsum(x)[-length(x)])
  offsets <- lagged_cumsum[cut]
  offset_vec <- offsets[findInterval(seq_along(x), cut)]
  lagged_cumsum - offset_vec
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Naive R version: Lagged cumulative sum which resets at the given cut points
#' 
#' Author: Mike Cheng
#' 
#' @param x Vector of values. Usually sorted.
#' @param cut Vector of cut points. Usually integer. Must be sorted.
#' @return vector of lagged cumulative sum with sum resetting to zero at
#'         the cut points
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cumsum_cut_naive <- function(x, cut) {
  res <- integer(length(x))
  cut <- sort(cut)
  
  acc <- 0L
  for (i in 2:length(x)) {
    if (i %in% cut) {
      res[i] = 0L
    } else {
      res[i] = res[i - 1L] + x[i - 1L]
    }
  }
  
  res
}


if (FALSE) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # C version. Mike Cheng
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  code <- r"(
SEXP cumsum_cut_c(SEXP x_, SEXP cut_) {
  int nx = length(x_);
  int nc = length(cut_);
  int *x = INTEGER(x_);
  int *cut = INTEGER(cut_);

  SEXP res_ = PROTECT(allocVector(INTSXP, nx)); 
  int *res = INTEGER(res_);
  res[0] = 0;

  int ci = 0;

  if (cut[ci] == 1) {
    // ignore
    ci++;
  }

  for (int i = 1; i < nx; i++) {
    if (ci < nc && i == cut[ci] - 1) {
       res[i] = 0;
       ci++;
       // skip repeated elements
       while(ci < nc && cut[ci] == cut[ci - 1]) ci++;
    } else {
       res[i] = res[i - 1] + x[i - 1];
    }
  }


  UNPROTECT(1);
  return res_;
}
)"
}


