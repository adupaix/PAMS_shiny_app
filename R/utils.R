#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2026-08-03
#'@email : amael.dupaix@ird.fr
#'#***************************************************************************
#'@description: Functions for the shiny app
#'
#'#***************************************************************************

format_and_correct_objectives <- function(x){
  x <- stringr::str_trim(x)
  if (length(x) > 1 && 'Unclear' %in% x){
    x <- x[which(x != 'Unclear')]
  }
  return(x)
}

split_duplicate_rows <- function(data,
                                 split_col,
                                 sep = ";",
                                 trim_whitespace = TRUE) {
  
  # Input validation
  stopifnot(
    is.data.frame(data),
    is.character(split_col), split_col %in% names(data),
    is.character(sep)
  )
  
  # Process the split column
  data <- data |>
    # Split rows on separator — duplicates all other columns automatically
    tidyr::separate_rows(dplyr::all_of(split_col), sep = sep) |>
    # Trim whitespace
    dplyr::mutate(dplyr::across(dplyr::all_of(split_col), stringr::str_trim))
  
  return(data)
}