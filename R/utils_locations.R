#' @title Split the study site locations of the case studies
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' One row per case study x coded location. `study_site_location` is split on
#' ";" and trimmed; missing, empty and "NA" values, and codes longer than 3
#' characters, are dropped. Codes are converted to numbers (so that "090"
#' and "90" match).
#'
#' NOTE: this is the rule of qmd/plot_workflow.qmd (location x driver and
#' year x location plots). The maps use the rule of
#' scripts/load_sysrev_data.R (`case_study_locations`), which also drops
#' codes absent from data/location_code.csv.
#'
#' @param case_study_data data frame with one row per case study and the
#'   columns `project_id`, `article_id` and `study_site_location`
#'   (";"-separated values)
#' @return tibble with the columns `case_study_row` (row index in
#'   `case_study_data`), `project_id`, `article_id` and `location` (numeric)
#' @importFrom ggplot2 .data
#' @export
split_study_site_locations <- function(case_study_data) {
  stopifnot(is.data.frame(case_study_data),
            all(c("project_id", "article_id", "study_site_location") %in%
                  names(case_study_data)))

  tibble::tibble(
    case_study_row = seq_len(nrow(case_study_data)),
    project_id = case_study_data$project_id,
    article_id = case_study_data$article_id,
    location = case_study_data$study_site_location
  ) |>
    tidyr::separate_rows("location", sep = ";") |>
    dplyr::mutate(location = stringr::str_trim(.data$location)) |>
    dplyr::filter(!is.na(.data$location), .data$location != "",
                  .data$location != "NA", nchar(.data$location) <= 3) |>
    dplyr::mutate(location = as.numeric(.data$location)) |>
    dplyr::distinct()
}

#' @title Add the location names to a table of location codes
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Joins the location names of `location_codes` (Annexe 1 - Codes pour
#' study_site_location) to `data` by code. Sub-groups are labelled
#' "Sub-group (Group)" to keep labels unique, and labels are wrapped at 40
#' characters. `location_label` is a factor ordered by code; codes absent
#' from `location_codes` keep their code as label and come last.
#'
#' @param data data frame with a numeric column `location`
#' @param location_codes data frame read from data/location_code.csv, with
#'   the columns `code`, `location_name_group` and `location_name_subgroup`
#' @return `data` with a factor column `location_label`
#' @importFrom ggplot2 .data
#' @export
add_location_label <- function(data, location_codes) {
  stopifnot(is.data.frame(data), "location" %in% names(data),
            all(c("code", "location_name_group", "location_name_subgroup") %in%
                  names(location_codes)))

  location_codes <- location_codes |>
    dplyr::mutate(
      location_label = dplyr::if_else(
        .data$location_name_group == .data$location_name_subgroup,
        .data$location_name_group,
        paste0(.data$location_name_subgroup, " (",
               .data$location_name_group, ")")
      ),
      location_label = stringr::str_wrap(.data$location_label, width = 40)
    ) |>
    dplyr::arrange(.data$code)

  data |>
    dplyr::left_join(
      location_codes |> dplyr::select(dplyr::all_of(c("code",
                                                      "location_label"))),
      by = c("location" = "code")
    ) |>
    dplyr::mutate(
      location_label = dplyr::coalesce(.data$location_label,
                                       as.character(.data$location)),
      location_label = factor(
        .data$location_label,
        levels = unique(c(location_codes$location_label,
                          as.character(sort(unique(.data$location)))))
      )
    ) |>
    droplevels()
}

#' @title Get the publication year of the included articles
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' One row per included article with its publication year (integer; `NA`
#' when `Publication.Year` is missing or not a number).
#'
#' @param included_papers data frame with one row per included article and
#'   the columns `project_id`, `article_id` and `Publication.Year`
#' @return tibble with the columns `project_id`, `article_id` and
#'   `publication_year`
#' @export
get_article_years <- function(included_papers) {
  stopifnot(is.data.frame(included_papers),
            all(c("project_id", "article_id", "Publication.Year") %in%
                  names(included_papers)))

  tibble::tibble(
    project_id = included_papers$project_id,
    article_id = included_papers$article_id,
    publication_year = suppressWarnings(
      as.integer(included_papers$Publication.Year)
    )
  ) |>
    dplyr::distinct()
}
