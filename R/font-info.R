


#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' #' List of included bitmap fonts. 
#' #' 
#' #' Each font is an object with class 'lofifont'
#' #' 
#' #' \describe{
#' #'   \item{coords}{data.frame}
#' #' }
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' "bitmap_fonts"
#' 
#' 
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' #' List of included vector fonts. 
#' #' 
#' #' Each font is an object with class 'lofifont'
#' #' 
#' #' \describe{
#' #'   \item{coords}{data.frame}
#' #' }
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' "vector_fonts"



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Return the names of all included fonts
#' @param type font type. Either 'bitmap' or 'vector'.
#' @return List of two elements: names of bitmap fonts, names of vector fonts
#' @examples
#' get_lofi_names('bitmap')
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_lofi_names <- function(type) {
  if (type == 'bitmap') {
    names(bitmap_fonts)
  } else {
    names(vector_fonts)
  }
}
