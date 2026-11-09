#' @title Bar plot of the activities and uses of the case studies
#' @author Amaël Dupaix (amael.dupaix@ird.fr)
#'
#' @description
#' Coding unit = case study (one row of `case_study_data`). For each activity
#' or use, number of case studies whose `activities_and_uses` includes it
#' (exact match, see [count_category_occurrences()]). A case study can be
#' coded with several activities, so bars can sum to more than the number of
#' case studies.
#'
#' @param case_study_data data frame with one row per case study and a
#'   character column `activities_and_uses` (";"-separated values)
#' @param categories activities to plot, in display order (top to bottom)
#' @param other_categories categories drawn in grey instead of blue
#' @return a ggplot object
#' @importFrom ggplot2 .data
#' @export
plot_activities_and_uses <- function(case_study_data,
                                     categories = activity_levels,
                                     other_categories = activity_other) {
  stopifnot(is.data.frame(case_study_data),
            "activities_and_uses" %in% names(case_study_data))

  activities_df <- tibble::tibble(
    activity = categories,
    color = ifelse(categories %in% other_categories, "Other", "Known"),
    nb_case_studies = unname(count_category_occurrences(
      case_study_data$activities_and_uses, categories
    ))
  )

  # coord_flip() draws the first level at the bottom: reverse the limits so
  # that `categories` reads top to bottom
  ggplot2::ggplot(activities_df,
                  ggplot2::aes(x = .data$activity, y = .data$nb_case_studies,
                               fill = .data$color)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(categories)) +
    ggplot2::scale_fill_manual("", values = c(Known = "#08519C",
                                              Other = "grey")) +
    ggplot2::xlab("Activities and uses") +
    ggplot2::ylab("Number of case studies") +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white",
                                               color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey"),
      legend.position = "none"
    )
}
