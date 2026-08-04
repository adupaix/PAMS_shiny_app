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
