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

#' Count the rows coded with each category of a multi-valued variable
#'
#' Splits each value of `x` on `sep`, trims whitespace and counts, for each
#' category, the number of elements of `x` containing it. Matching is exact:
#' "Others" does not match "Other industries", and labels containing regex
#' or escaped characters (e.g. "\\n", "(") are matched literally.
#'
#' A row coded twice with the same category counts once; missing (NA) or
#' empty values count for no category. Values absent from `categories` are
#' ignored.
#'
#' @param x character vector, one element per coding unit (e.g. one
#'   participant group), with `sep`-separated categories
#' @param categories character vector of the categories to count
#' @param sep separator between categories in `x`
#' @return named integer vector (names = `categories`, same order)
#' @export
count_category_occurrences <- function(x, categories, sep = ";") {
  stopifnot(is.character(categories))
  x <- as.character(x)

  # unique() so that a category repeated within a row counts once
  values_per_row <- lapply(strsplit(x, sep, fixed = TRUE),
                           function(values) unique(trimws(values)))
  # factor() drops values that are not in `categories` (and NA)
  counts <- table(factor(unlist(values_per_row), levels = categories))

  stats::setNames(as.integer(counts), categories)
}