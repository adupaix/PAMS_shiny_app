#' @title Heatmap of the articles per publication year and location
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = article x location: number of included articles per
#' publication year with at least one case study coded with each location
#' (see [split_study_site_locations()]). Articles without a publication year
#' are dropped. Only the locations of one `area` are shown: land (codes
#' < 200) or sea (codes >= 200).
#'
#' @param case_study_data data frame with one row per case study and the
#'   columns `project_id`, `article_id` and `study_site_location`
#'   (";"-separated values)
#' @param included_papers data frame with one row per included article and
#'   the columns `project_id`, `article_id` and `Publication.Year`
#' @param location_codes data frame read from data/location_code.csv (see
#'   [add_location_label()])
#' @param area "land" (location codes < 200) or "sea" (codes >= 200)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_year_by_location <- function(case_study_data, included_papers,
                                  location_codes, area = c("land", "sea")) {
  area <- match.arg(area)

  # Articles per publication year x location
  year_location_counts <- split_study_site_locations(case_study_data) |>
    dplyr::select(dplyr::all_of(c("project_id", "article_id",
                                  "location"))) |>
    dplyr::distinct() |>
    dplyr::inner_join(get_article_years(included_papers),
                      by = c("project_id", "article_id")) |>
    dplyr::filter(!is.na(.data$publication_year)) |>
    dplyr::count(.data$publication_year, .data$location,
                 name = "n_articles") |>
    add_location_label(location_codes)

  # NOTE: land / sea split on the location code (codes >= 200 = sea)
  year_location_counts <- if (area == "land") {
    dplyr::filter(year_location_counts, .data$location < 200)
  } else {
    dplyr::filter(year_location_counts, .data$location >= 200)
  }

  ggplot2::ggplot(year_location_counts,
                  ggplot2::aes(x = .data$publication_year,
                               y = .data$location_label,
                               fill = .data$n_articles)) +
    ggplot2::geom_tile(color = "white") +
    # viridis (colour-blind friendly)
    ggplot2::scale_fill_viridis_c(name = "Articles", limits = c(0, NA)) +
    ggplot2::labs(x = "Publication year", y = "Location") +
    ggplot2::theme_minimal()
}
