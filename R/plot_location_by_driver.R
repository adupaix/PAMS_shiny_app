#' @title Heatmap of the case studies per location and driver of change
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study x location x driver of change: number of case
#' studies coded with each location (see [split_study_site_locations()]) and
#' each driver (exact value of `drivers_of_change`). A case study with
#' several locations or drivers contributes every location x driver pair.
#' Only the locations of one `area` are shown: land (codes < 200) or sea
#' (codes >= 200).
#'
#' @param case_study_data data frame with one row per case study and the
#'   columns `project_id`, `article_id`, `study_site_location` and
#'   `drivers_of_change` (";"-separated values)
#' @param location_codes data frame read from data/location_code.csv (see
#'   [add_location_label()])
#' @param area "land" (location codes < 200) or "sea" (codes >= 200)
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_location_by_driver <- function(case_study_data, location_codes,
                                    area = c("land", "sea")) {
  area <- match.arg(area)
  stopifnot("drivers_of_change" %in% names(case_study_data))

  # One row per case study x driver
  case_study_drivers <- tibble::tibble(
    case_study_row = seq_len(nrow(case_study_data)),
    driver = case_study_data$drivers_of_change
  ) |>
    tidyr::separate_rows("driver", sep = ";") |>
    dplyr::mutate(driver = stringr::str_trim(.data$driver)) |>
    dplyr::filter(!is.na(.data$driver), .data$driver != "",
                  .data$driver != "NA")

  # Case studies per location x driver (location x driver pairs crossed
  # within each case study)
  case_driver_counts <- split_study_site_locations(case_study_data) |>
    dplyr::inner_join(case_study_drivers, by = "case_study_row",
                      relationship = "many-to-many") |>
    dplyr::distinct() |>
    dplyr::count(.data$location, .data$driver, name = "n_case_studies") |>
    add_location_label(location_codes)

  # NOTE: land / sea split on the location code (codes >= 200 = sea)
  case_driver_counts <- if (area == "land") {
    dplyr::filter(case_driver_counts, .data$location < 200)
  } else {
    dplyr::filter(case_driver_counts, .data$location >= 200)
  }

  ggplot2::ggplot(case_driver_counts,
                  ggplot2::aes(x = .data$location_label, y = .data$driver,
                               fill = .data$n_case_studies)) +
    ggplot2::geom_tile(color = "white") +
    # viridis (colour-blind friendly)
    ggplot2::scale_fill_viridis_c(name = "Case studies", limits = c(0, NA)) +
    ggplot2::labs(x = "Location", y = "Drivers of change") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1))
}
