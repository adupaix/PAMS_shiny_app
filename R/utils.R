#' @title Format and correct the engagement objectives of one article
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Trims the engagement objectives coded for one article and drops "Unclear"
#' when at least one other objective is coded.
#'
#' @param x character vector of the objectives coded for one article (one
#'   element of `strsplit(engagement_objectives, ";")`)
#' @return character vector of the corrected objectives
#' @export
format_and_correct_objectives <- function(x){
  x <- stringr::str_trim(x)
  if (length(x) > 1 && 'Unclear' %in% x){
    x <- x[which(x != 'Unclear')]
  }
  return(x)
}

#' @title Split a multi-valued column into one row per value
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Splits `split_col` on `sep` into one row per value (all other columns are
#' duplicated) and trims whitespace around the values.
#'
#' @param data data frame
#' @param split_col name of the column to split
#' @param sep separator between values in `split_col`
#' @return `data` with one row per value of `split_col`
#' @export
split_duplicate_rows <- function(data,
                                 split_col,
                                 sep = ";") {
  
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

#' @title Count the rows coded with each category of a multi-valued variable
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
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
#'   participant group), with `sep`-separated categories; or a list with one
#'   character vector of (already split) categories per coding unit
#' @param categories character vector of the categories to count
#' @param sep separator between categories in `x` (ignored if `x` is a list)
#' @return named integer vector (names = `categories`, same order)
#' @export
count_category_occurrences <- function(x, categories, sep = ";") {
  stopifnot(is.character(categories))
  if (!is.list(x)) x <- strsplit(as.character(x), sep, fixed = TRUE)

  # unique() so that a category repeated within a row counts once
  values_per_row <- lapply(x, function(values) unique(trimws(values)))
  # factor() drops values that are not in `categories` (and NA)
  counts <- table(factor(unlist(values_per_row), levels = categories))

  stats::setNames(as.integer(counts), categories)
}

#' @title Merge the depth levels of the impact, lever and challenge variables
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Merges the coded depth levels two by two: "Detailed" and "Evaluated"
#' become "Detailed/Evaluated", "Mentioned" and "Discussed" become
#' "Mentioned/Discussed". Other values (e.g. "No") are kept unchanged.
#'
#' @param x character vector of coded values (see `depth_levels`)
#' @return character vector of merged values, same length as `x`
#' @export
merge_depth_levels <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("Detailed", "Evaluated") ~ "Detailed/Evaluated",
    x %in% c("Mentioned", "Discussed") ~ "Mentioned/Discussed",
    TRUE ~ x
  )
}

#' @title List the rows coded with each category of a multi-valued variable
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' For each category, returns the indices of the elements of `x` whose
#' `sep`-separated values contain it (exact match after trimming whitespace).
#' The output is the set list expected by ggVennDiagram::ggVennDiagram().
#'
#' @param x character vector, one element per coding unit (e.g. one article),
#'   with `sep`-separated categories
#' @param categories character vector of the categories; if named, the names
#'   are used as set names
#' @param sep separator between categories in `x`
#' @return named list of integer vectors (row indices of `x`), one per
#'   category
#' @export
list_rows_by_category <- function(x, categories, sep = ";") {
  stopifnot(is.character(categories))
  values_per_row <- lapply(strsplit(as.character(x), sep, fixed = TRUE),
                           trimws)
  set_names <- if (is.null(names(categories))) categories else names(categories)

  stats::setNames(
    lapply(categories, function(category) {
      which(vapply(values_per_row, function(values) category %in% values,
                   logical(1)))
    }),
    set_names
  )
}
