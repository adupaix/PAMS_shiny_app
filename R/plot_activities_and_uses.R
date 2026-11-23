#' @title Bar plot of the activities and uses of the case studies
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study (one row of `case_study_data`). For each activity
#' or use, percentage of the case studies with at least one activity coded
#' whose `activities_and_uses` includes it (exact match, see
#' [count_category_occurrences()]). A case study can be coded with several
#' activities, so percentages may sum to more than 100 %.
#'
#' Bars are sorted by decreasing percentage (highest on top), the blue
#' categories first, then the grey ones (`other_categories`); ties are in
#' alphabetical order.
#'
#' @param case_study_data data frame with one row per case study and a
#'   character column `activities_and_uses` (";"-separated values)
#' @param categories activities to plot
#' @param other_categories categories drawn in grey instead of blue, below
#'   the blue ones
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_activities_and_uses <- function(case_study_data,
                                     categories = activity_levels,
                                     other_categories = activity_other) {
  stopifnot(is.data.frame(case_study_data),
            "activities_and_uses" %in% names(case_study_data))

  # Denominator = case studies with at least one activity coded (same
  # convention as the engagement objectives plots)
  n_coded_case_studies <- sum(
    !is.na(case_study_data$activities_and_uses) &
      trimws(case_study_data$activities_and_uses) != ""
  )

  # Display order (top to bottom): blue categories by decreasing count, then
  # grey ones by decreasing count
  activities_df <- tibble::tibble(
    activity = categories,
    color = ifelse(categories %in% other_categories, "Other", "Known"),
    nb_case_studies = unname(count_category_occurrences(
      case_study_data$activities_and_uses, categories
    ))
  ) |>
    dplyr::mutate(
      percent = 100 * .data$nb_case_studies / n_coded_case_studies
    ) |>
    dplyr::arrange(.data$color, dplyr::desc(.data$nb_case_studies),
                   .data$activity)

  # coord_flip() draws the first level at the bottom: reverse the limits so
  # that the sorted activities read top to bottom
  ggplot2::ggplot(activities_df,
                  ggplot2::aes(x = .data$activity, y = .data$percent,
                               fill = .data$color)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(activities_df$activity)) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(scale = 1), limits = c(0, NA)
    ) +
    ggplot2::scale_fill_manual("", values = c(Known = "#08519C",
                                              Other = "grey")) +
    ggplot2::xlab("Activities and uses") +
    ggplot2::ylab("Coded case studies (%)") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey"),
      legend.position = "none"
    )
}
