#' @title Bar plot of the drivers of change of the case studies
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study (one row of `case_study_data`). For each driver
#' of change, number of case studies whose `drivers_of_change` includes it
#' (exact match, see [count_category_occurrences()]). A case study can be
#' coded with several drivers, so bars can sum to more than the number of
#' case studies. Bars are coloured by driver type (`groups`).
#'
#' @param case_study_data data frame with one row per case study and a
#'   character column `drivers_of_change` (";"-separated values)
#' @param categories drivers to plot, in display order (top to bottom)
#' @param groups type of each driver (named by driver), used as colour:
#'   "Primary", "Secondary" or "None"
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_drivers_of_change <- function(case_study_data,
                                   categories = driver_levels,
                                   groups = driver_groups) {
  stopifnot(is.data.frame(case_study_data),
            "drivers_of_change" %in% names(case_study_data),
            all(categories %in% names(groups)))

  drivers_df <- tibble::tibble(
    driver = categories,
    color = unname(groups[categories]),
    nb_case_studies = unname(count_category_occurrences(
      case_study_data$drivers_of_change, categories
    ))
  )

  # coord_flip() draws the first level at the bottom: reverse the limits so
  # that `categories` reads top to bottom
  ggplot2::ggplot(drivers_df,
                  ggplot2::aes(x = .data$driver, y = .data$nb_case_studies,
                               fill = .data$color)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(categories)) +
    # RColorBrewer "Blues" shades, grey for "None"
    ggplot2::scale_fill_manual("", values = c(Primary = "#6BAED6",
                                              Secondary = "#08519C",
                                              None = "grey")) +
    ggplot2::xlab("Drivers of change") +
    ggplot2::ylab("Number of case studies") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey")
    )
}
