#' @title Heatmap of the articles per publication year and driver of change
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = article x driver of change: number of included articles per
#' publication year with at least one case study coded with each driver
#' (exact value of `drivers_of_change`; "Unrelevant" is excluded). Articles
#' without a publication year are dropped.
#'
#' NOTE: only the case studies with at least one usable location (see
#' [split_study_site_locations()]) are used, as in the original
#' qmd/plot_workflow.qmd code where drivers were taken from the location x
#' driver table.
#'
#' @param case_study_data data frame with one row per case study and the
#'   columns `project_id`, `article_id`, `study_site_location` and
#'   `drivers_of_change` (";"-separated values)
#' @param included_papers data frame with one row per included article and
#'   the columns `project_id`, `article_id` and `Publication.Year`
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_year_by_driver <- function(case_study_data, included_papers) {
  stopifnot("drivers_of_change" %in% names(case_study_data))

  # Case studies with at least one usable location
  located_rows <- unique(
    split_study_site_locations(case_study_data)$case_study_row
  )

  # Articles per publication year x driver
  year_driver_counts <- tibble::tibble(
    project_id = case_study_data$project_id[located_rows],
    article_id = case_study_data$article_id[located_rows],
    driver = case_study_data$drivers_of_change[located_rows]
  ) |>
    tidyr::separate_rows("driver", sep = ";") |>
    dplyr::mutate(driver = stringr::str_trim(.data$driver)) |>
    dplyr::filter(!is.na(.data$driver), .data$driver != "",
                  .data$driver != "NA", .data$driver != "Unrelevant") |>
    dplyr::distinct() |>
    dplyr::inner_join(get_article_years(included_papers),
                      by = c("project_id", "article_id")) |>
    dplyr::filter(!is.na(.data$publication_year)) |>
    dplyr::count(.data$publication_year, .data$driver, name = "n_articles")

  ggplot2::ggplot(year_driver_counts,
                  ggplot2::aes(x = .data$publication_year, y = .data$driver,
                               fill = .data$n_articles)) +
    ggplot2::geom_tile(color = "white") +
    # viridis (colour-blind friendly)
    ggplot2::scale_fill_viridis_c(name = "Articles", limits = c(0, NA)) +
    ggplot2::labs(x = "Publication year", y = "Drivers of change") +
    ggplot2::theme_minimal()
}
