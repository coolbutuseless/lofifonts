

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Return the names of all included fonts
#' @param type font type. Either 'bitmap', 'vector' or 'all'
#' @return List of two elements: names of bitmap fonts, names of vector fonts
#' @examples
#' get_lofi_names('bitmap')
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_lofi_names <- function(type) {
  
  switch(
    type,
    bitmap = names(bitmap_fonts),
    vector = names(vector_fonts),
    all    = c(names(bitmap_fonts), names(vector_fonts)),
    
    # default
    stop("'type' must be one of 'bitmap', 'vector', 'all'")
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fetch an included 'lofi' font
#' @param font_name Name of font e.g. 'unifont'
#' @return 'lofi' font object. See \code{\link{lofi}} for details of this
#'         data structure
#' @examples
#' get_lofi_font('unifont')
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_lofi_font <- function(font_name) {
  bitmap_fonts[[font_name]] %||% 
    vector_fonts[[font_name]] %||% 
    stop("No such font: ", font_name)
}

