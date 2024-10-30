


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
#' @return List of two elements: names of bitmap fonts, names of vector fonts
#' @examples
#' get_lofifont_names()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_lofifont_names <- function() {
  list(
    bitmap = names(bitmap_fonts),
    vector = names(vector_fonts)
  )
}
